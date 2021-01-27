/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.linker

import scala.concurrent._

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir.ClassKind
import org.scalajs.ir.EntryPointsInfo
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Trees.MemberNamespace._
import org.scalajs.ir.Types._

import org.scalajs.logging._

import org.scalajs.junit.async._

import org.scalajs.linker.interface._
import org.scalajs.linker.standard._

import org.scalajs.linker.testutils._
import org.scalajs.linker.testutils.IRAssertions._
import org.scalajs.linker.testutils.LinkingUtils._
import org.scalajs.linker.testutils.TestIRBuilder._

class OptimizerTest {
  import scala.concurrent.ExecutionContext.Implicits.global

  import OptimizerTest._

  /** Generic code for the three methods below.
   *
   *  Check that `clone()` is correctly inlined (or correctly *not* inlined)
   *  when the receiver can be an array, in several scenarios.
   *
   *  Correct outcomes are:
   *  - The call is not inlined at all, or
   *  - It inlines `java.lang.Object.clone()`, which results in a `Clone` node.
   *
   *  A potential incorrect outcome, which has happened in the past (#3778), is
   *  to inline a method `clone()` from *another* class (for example, because
   *  `j.l.Object.clone()` is not otherwise reachable).
   */
  private def testCloneOnArrayInliningGeneric(inlinedWhenOnObject: Boolean,
      customMemberDefs: List[MemberDef]): Future[Unit] = {

    val thisFoo = This()(ClassType("Foo"))
    val intArrayTypeRef = ArrayTypeRef(IntRef, 1)
    val intArrayType = ArrayType(intArrayTypeRef)
    val anArrayOfInts = ArrayValue(intArrayTypeRef, List(IntLiteral(1)))
    val newFoo = New("Foo", NoArgConstructorName, Nil)

    val reachCloneMethodName = m("reachClone", Nil, O)
    val anArrayMethodName = m("anArray", Nil, intArrayTypeRef)
    val anObjectMethodName = m("anObject", Nil, O)

    def callCloneOn(receiver: Tree): Tree =
      Apply(EAF, receiver, cloneMethodName, Nil)(AnyType)

    val fooMemberDefs = List(
        trivialCtor("Foo"),

        // @noinline def witness(): AnyRef = throw null
        MethodDef(EMF, witnessMethodName, NON, Nil, AnyType, Some {
          Throw(Null())
        })(EOH.withNoinline(true), None),

        // @noinline def reachClone(): Object = clone()
        MethodDef(EMF, reachCloneMethodName, NON, Nil, AnyType, Some {
          Apply(EAF, thisFoo, cloneMethodName, Nil)(AnyType)
        })(EOH.withNoinline(true), None),

        // @noinline def anArray(): Array[Int] = Array(1)
        MethodDef(EMF, anArrayMethodName, NON, Nil, intArrayType, Some {
          anArrayOfInts
        })(EOH.withNoinline(true), None),

        // @noinline def anObject(): AnyRef = Array(1)
        MethodDef(EMF, anObjectMethodName, NON, Nil, AnyType, Some {
          anArrayOfInts
        })(EOH.withNoinline(true), None)
    ) ::: customMemberDefs

    val classDefs = Seq(
        classDef("Foo",
            superClass = Some(ObjectClass),
            interfaces = List("java.lang.Cloneable"),
            memberDefs = fooMemberDefs
        ),
        mainTestClassDef(Block(
            // new Foo().reachClone() -- make Foo.clone() reachable for sure
            Apply(EAF, newFoo, reachCloneMethodName, Nil)(AnyType),
            // Array(1).clone() -- test with an exact static type of I[] -> inline
            callCloneOn(anArrayOfInts),
            // new Foo().anArray().clone() -- test with a static type of I[] -> inline
            callCloneOn(Apply(EAF, newFoo, anArrayMethodName, Nil)(intArrayType)),
            // new Foo().anObject().clone() -- test with a static type of Object -> inlinedWhenOnObject
            callCloneOn(Apply(EAF, newFoo, anObjectMethodName, Nil)(AnyType))
        ))
    )

    for (moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers)) yield {
      val linkedClass = moduleSet.modules.flatMap(_.classDefs)
        .find(_.className == MainTestClassName).get
      linkedClass.hasNot("any call to Foo.witness()") {
        case Apply(_, receiver, MethodIdent(`witnessMethodName`), _) =>
          receiver.tpe == ClassType("Foo")
      }.hasExactly(if (inlinedWhenOnObject) 1 else 0, "IsInstanceOf node") {
        case IsInstanceOf(_, _) => true
      }.hasExactly(if (inlinedWhenOnObject) 1 else 0, "Throw node") {
        case Throw(_) => true
      }.hasExactly(if (inlinedWhenOnObject) 3 else 2, "built-in <clone>() operations") {
        case Clone(_) => true
      }.hasExactly(if (inlinedWhenOnObject) 0 else 1, "call to clone() (not inlined)") {
        case Apply(_, _, MethodIdent(`cloneMethodName`), _) => true
      }
    }
  }

  /** Test array `clone()` inlining when `j.l.Object.clone()` is not otherwise
   *  reachable.
   */
  @Test
  def testCloneOnArrayInliningNonObjectCloneOnly_Issue3778(): AsyncResult = await {
    testCloneOnArrayInliningGeneric(inlinedWhenOnObject = false, List(
        // @inline override def clone(): AnyRef = witness()
        MethodDef(EMF, cloneMethodName, NON, Nil, AnyType, Some {
          Apply(EAF, This()(ClassType("Foo")), witnessMethodName, Nil)(AnyType)
        })(EOH.withInline(true), None)
    ))
  }

  /** Test array `clone()` inlining when `j.l.Object.clone()` is the only
   *  reachable `clone()` method.
   *
   *  In that case, it will be inlined even when called on a receiver whose
   *  static type is no more precise than `Object`.
   */
  @Test
  def testCloneOnArrayInliningObjectCloneOnly_Issue3778(): AsyncResult = await {
    testCloneOnArrayInliningGeneric(inlinedWhenOnObject = true, Nil)
  }

  /** Test array `clone()` inlining when `j.l.Object.clone()` and another
   *  `clone()` method are reachable.
   */
  @Test
  def testCloneOnArrayInliningObjectCloneAndAnotherClone_Issue3778(): AsyncResult = await {
    testCloneOnArrayInliningGeneric(inlinedWhenOnObject = false, List(
        // @inline override def clone(): AnyRef = witness()
        MethodDef(EMF, cloneMethodName, NON, Nil, AnyType, Some {
          Block(
              Apply(EAF, This()(ClassType("Foo")), witnessMethodName, Nil)(AnyType),
              ApplyStatically(EAF, This()(ClassType("Foo")),
                  ObjectClass, cloneMethodName, Nil)(AnyType)
          )
        })(EOH.withInline(true), None)
    ))
  }

  /** Makes sure that a hello world does not need java.lang.Class after
   *  optimizations.
   */
  @Test
  def testHelloWorldDoesNotNeedClassClass(): AsyncResult = await {
    val classDefs = Seq(
        mainTestClassDef({
          predefPrintln(StringLiteral("Hello world!"))
        })
    )

    for (moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers, TestIRRepo.fulllib)) yield {
      assertFalse(moduleSet.modules.flatMap(_.classDefs).exists(_.className == ClassClass))
    }
  }

  @Test
  def testOptimizerDoesNotEliminateRequiredStaticField_Issue4021(): AsyncResult = await {
    val StringType = ClassType(BoxedStringClass)
    val fooGetter = m("foo", Nil, T)
    val classDefs = Seq(
        classDef(
            MainTestClassName,
            kind = ClassKind.Class,
            superClass = Some(ObjectClass),
            memberDefs = List(
                trivialCtor(MainTestClassName),
                // static var foo: java.lang.String
                FieldDef(EMF.withNamespace(PublicStatic).withMutable(true),
                    "foo", NON, StringType),
                // static def foo(): java.lang.String = Test::foo
                MethodDef(EMF.withNamespace(MemberNamespace.PublicStatic),
                    fooGetter, NON, Nil, StringType, Some({
                      SelectStatic(MainTestClassName, "foo")(StringType)
                    }))(EOH, None),
                // static def main(args: String[]) { println(Test::foo()) }
                mainMethodDef({
                  consoleLog(ApplyStatic(EAF, MainTestClassName, fooGetter, Nil)(StringType))
                })
            )
        )
    )

    for (moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers)) yield {
      val mainClassDef = moduleSet.modules.flatMap(_.classDefs)
        .find(_.className == MainTestClassName).get
      assertTrue(mainClassDef.fields.exists {
        case FieldDef(_, FieldIdent(name), _, _) => name == FieldName("foo")
        case _                                   => false
      })
    }
  }

  @Test
  def testOptimizerDoesNotEliminateRequiredLabeledBlockEmittedByDotty_Issue4171(): AsyncResult = await {
    /* For the following source code:
     *
     * (null: Any) match {
     *   case (_: Int) | (_: String) =>
     * }
     *
     * the dotty compiler generates the following IR:
     *
     * matchResult1: {
     *   val x1: any = null;
     *   matchAlts1: {
     *     matchAlts2: {
     *       if (x1.isInstanceOf[java.lang.Integer]) {
     *         return@matchAlts2 (void 0)
     *       };
     *       if (x1.isInstanceOf[java.lang.String]) {
     *         return@matchAlts2 (void 0)
     *       };
     *       return@matchAlts1 (void 0)
     *     };
     *     return@matchResult1 (void 0)
     *   };
     *   throw new scala.MatchError().<init>;Ljava.lang.Object;V(x1)
     * }
     *
     * The optimizer used to erroneously get rid of the `matchAlts1` labeled
     * block, although it could not remove the `return@matchAlts1`. This led to
     * a crash in the Emitter.
     *
     * This test reproduces that exact IR by hand, and verifies that the
     * optimized code can be linked all the way to the Emitter.
     */

    val matchResult1 = LabelIdent("matchResult1")
    val x1 = LocalIdent("x1")
    val matchAlts1 = LabelIdent("matchAlts1")
    val matchAlts2 = LabelIdent("matchAlts2")

    val classDefs = Seq(
        mainTestClassDef(Block(
            Labeled(matchResult1, NoType, Block(
                VarDef(x1, NON, AnyType, mutable = false, Null()),
                Labeled(matchAlts1, NoType, Block(
                    Labeled(matchAlts2, NoType, Block(
                        If(IsInstanceOf(VarRef(x1)(AnyType), ClassType(BoxedIntegerClass)), {
                          Return(Undefined(), matchAlts2)
                        }, Skip())(NoType),
                        If(IsInstanceOf(VarRef(x1)(AnyType), ClassType(BoxedStringClass)), {
                          Return(Undefined(), matchAlts2)
                        }, Skip())(NoType),
                        Return(Undefined(), matchAlts1)
                    )),
                    Return(Undefined(), matchResult1)
                )),
                Throw(New("java.lang.Exception", NoArgConstructorName, Nil))
            ))
        ))
    )

    testLink(classDefs, MainTestModuleInitializers)
  }

}

object OptimizerTest {
  private val cloneMethodName = m("clone", Nil, O)
  private val witnessMethodName = m("witness", Nil, O)

  private final class StoreModuleSetLinkerBackend(
      originalBackend: LinkerBackend)
      extends LinkerBackend {

    @volatile
    private var _moduleSet: ModuleSet = _

    val coreSpec: CoreSpec = originalBackend.coreSpec

    val symbolRequirements: SymbolRequirement = originalBackend.symbolRequirements

    override def injectedIRFiles: Seq[IRFile] = originalBackend.injectedIRFiles

    def emit(moduleSet: ModuleSet, output: OutputDirectory, logger: Logger)(
        implicit ec: ExecutionContext): Future[Report] = {
      _moduleSet = moduleSet
      originalBackend.emit(moduleSet, output, logger)
    }

    def moduleSet: ModuleSet = {
      if (_moduleSet == null)
        throw new IllegalStateException("Cannot access moduleSet before emit is called")
      _moduleSet
    }
  }

  def linkToModuleSet(classDefs: Seq[ClassDef],
      moduleInitializers: List[ModuleInitializer])(
      implicit ec: ExecutionContext): Future[ModuleSet] = {

    linkToModuleSet(classDefs, moduleInitializers, TestIRRepo.minilib)
  }

  def linkToModuleSet(classDefs: Seq[ClassDef],
      moduleInitializers: List[ModuleInitializer], stdlib: Future[Seq[IRFile]])(
      implicit ec: ExecutionContext): Future[ModuleSet] = {

    val config = StandardConfig()
    val frontend = StandardLinkerFrontend(config)
    val backend = new StoreModuleSetLinkerBackend(StandardLinkerBackend(config))
    val linker = StandardLinkerImpl(frontend, backend)

    val classDefsFiles = classDefs.map(MemClassDefIRFile(_))
    val output = MemOutputDirectory()

    stdlib.flatMap { stdLibFiles =>
      linker.link(stdLibFiles ++ classDefsFiles, moduleInitializers,
          output, new ScalaConsoleLogger(Level.Error))
    }.map { _ =>
      backend.moduleSet
    }
  }
}
