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
import org.scalajs.ir.Traversers.Traverser
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
      customMethodDefs: List[MethodDef]): Future[Unit] = {

    val thisFoo = This()(ClassType("Foo"))
    val intArrayTypeRef = ArrayTypeRef(IntRef, 1)
    val intArrayType = ArrayType(intArrayTypeRef)
    val anArrayOfInts = ArrayValue(intArrayTypeRef, List(IntLiteral(1)))
    val newFoo = New("Foo", NoArgConstructorName, Nil)

    val reachCloneMethodName = m("reachClone", Nil, O)
    val anArrayMethodName = m("anArray", Nil, intArrayTypeRef)
    val anObjectMethodName = m("anObject", Nil, O)

    def callCloneOn(receiver: Tree): Tree =
      consoleLog(Apply(EAF, receiver, cloneMethodName, Nil)(AnyType))

    val fooMethodDefs = List(
        trivialCtor("Foo"),

        // @noinline def witness(): AnyRef = throw null
        MethodDef(EMF, witnessMethodName, NON, Nil, AnyType, Some {
          Throw(Null())
        })(EOH.withNoinline(true), UNV),

        // @noinline def reachClone(): Object = clone()
        MethodDef(EMF, reachCloneMethodName, NON, Nil, AnyType, Some {
          Apply(EAF, thisFoo, cloneMethodName, Nil)(AnyType)
        })(EOH.withNoinline(true), UNV),

        // @noinline def anArray(): Array[Int] = Array(1)
        MethodDef(EMF, anArrayMethodName, NON, Nil, intArrayType, Some {
          anArrayOfInts
        })(EOH.withNoinline(true), UNV),

        // @noinline def anObject(): AnyRef = Array(1)
        MethodDef(EMF, anObjectMethodName, NON, Nil, AnyType, Some {
          anArrayOfInts
        })(EOH.withNoinline(true), UNV)
    ) ::: customMethodDefs

    val classDefs = Seq(
        classDef("Foo",
            superClass = Some(ObjectClass),
            interfaces = List("java.lang.Cloneable"),
            methods = fooMethodDefs
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
        })(EOH.withInline(true), UNV)
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
        })(EOH.withInline(true), UNV)
    ))
  }

  /** Makes sure that a hello world does not need java.lang.Class after
   *  optimizations.
   */
  @Test
  def testHelloWorldDoesNotNeedClassClass(): AsyncResult = await {
    val classDefs = Seq(
        mainTestClassDef({
          systemOutPrintln(str("Hello world!"))
        })
    )

    for {
      moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers,
          stdlib = TestIRRepo.javalib)
    } yield {
      assertFalse(findClass(moduleSet, ClassClass).isDefined)
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
            fields = List(
              // static var foo: java.lang.String
              FieldDef(EMF.withNamespace(PublicStatic).withMutable(true),
                  FieldName(MainTestClassName, "foo"), NON, StringType)
            ),
            methods = List(
                trivialCtor(MainTestClassName),
                // static def foo(): java.lang.String = Test::foo
                MethodDef(EMF.withNamespace(MemberNamespace.PublicStatic),
                    fooGetter, NON, Nil, StringType, Some({
                      SelectStatic(FieldName(MainTestClassName, "foo"))(StringType)
                    }))(EOH, UNV),
                // static def main(args: String[]) { println(Test::foo()) }
                mainMethodDef({
                  consoleLog(ApplyStatic(EAF, MainTestClassName, fooGetter, Nil)(StringType))
                })
            )
        )
    )

    for (moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers)) yield {
      val mainClassDef = findClass(moduleSet, MainTestClassName).get
      assertTrue(mainClassDef.fields.exists {
        case FieldDef(_, FieldIdent(name), _, _) => name == FieldName(MainTestClassName, "foo")
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

  @Test
  def testCaptureElimination(): AsyncResult = await {
    val sideEffect = m("sideEffect", List(I), I)

    val x = LocalName("x")
    val x2 = LocalName("x2")
    val x4 = LocalName("x4")

    val classDefs = Seq(
        classDef(
            MainTestClassName,
            kind = ClassKind.Class,
            superClass = Some(ObjectClass),
            methods = List(
                trivialCtor(MainTestClassName),
                // @noinline static def sideEffect(x: Int): Int = x
                MethodDef(EMF.withNamespace(PublicStatic), sideEffect, NON,
                    List(paramDef(x, IntType)), IntType, Some(VarRef(x)(IntType)))(
                    EOH.withNoinline(true), UNV),
                /* static def main(args: String[]) {
                 *   console.log(arrow-lambda<
                 *     x1 = sideEffect(1),
                 *     x2 = sideEffect(2),
                 *     ...
                 *     x5 = sideEffect(5),
                 *   >() = {
                 *     console.log(x4);
                 *     console.log(x2);
                 *   });
                 * }
                 */
                mainMethodDef({
                  val closure = Closure(
                    arrow = true,
                    (1 to 5).toList.map(i => paramDef(LocalName("x" + i), IntType)),
                    Nil,
                    None,
                    Block(
                      consoleLog(VarRef(x4)(IntType)),
                      consoleLog(VarRef(x2)(IntType))
                    ),
                    (1 to 5).toList.map(i => ApplyStatic(EAF, MainTestClassName, sideEffect, List(int(i)))(IntType))
                  )
                  consoleLog(closure)
                })
            )
        )
    )

    for (moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers)) yield {
      var lastSideEffectFound = 0
      var closureParams = List.empty[List[LocalName]]
      traverseMainMethod(moduleSet) {
        case ApplyStatic(_, MainTestClassName, MethodIdent(`sideEffect`), List(IntLiteral(i))) =>
          assertEquals("wrong side effect ordering", lastSideEffectFound + 1, i)
          lastSideEffectFound = i
        case c: Closure =>
          closureParams :+= c.captureParams.map(_.name.name)
        case _ =>
      }
      assertEquals("wrong number of side effect calls", 5, lastSideEffectFound)
      assertEquals("wrong closure params", List(List(x2, x4)), closureParams)
    }
  }

  @Test
  def testOptimizeDynamicImport(): AsyncResult = await {
    val thunkMethodName = m("thunk", Nil, O)
    val implMethodName = m("impl", Nil, O)

    val memberMethodName = m("member", Nil, O)

    val SMF = EMF.withNamespace(MemberNamespace.PublicStatic)

    val classDefs = Seq(
        mainTestClassDef({
          consoleLog(ApplyDynamicImport(EAF, "Thunk", thunkMethodName, Nil))
        }),
        classDef("Thunk",
            superClass = Some(ObjectClass),
            optimizerHints = EOH.withInline(true),
            methods = List(
                trivialCtor("Thunk"),
                MethodDef(EMF, implMethodName, NON, Nil, AnyType, Some {
                  SelectJSNativeMember("Holder", memberMethodName)
                })(EOH, UNV),
                MethodDef(SMF, thunkMethodName, NON, Nil, AnyType, Some {
                  val inst = New("Thunk", NoArgConstructorName, Nil)
                  Apply(EAF, inst, implMethodName, Nil)(AnyType)
                })(EOH, UNV)
            )
        ),
        classDef("Holder", kind = ClassKind.Interface,
            jsNativeMembers = List(
                JSNativeMemberDef(SMF, memberMethodName, JSNativeLoadSpec.Import("foo", List("bar")))
            )
        )
    )

    val linkerConfig = StandardConfig()
      .withModuleKind(ModuleKind.ESModule)

    for {
      moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers,
          config = linkerConfig)
    } yield {
      assertFalse(findClass(moduleSet, "Thunk").isDefined)

      var foundJSImport = false
      val main = findClass(moduleSet, MainTestClassName).get
      val traverser = new Traverser {
        override def traverse(tree: Tree): Unit = tree match {
          case tree: ApplyDynamicImport => fail(s"found ApplyDynamicImport " + tree)
          case tree: JSImportCall       => foundJSImport = true
          case tree                     => super.traverse(tree)
        }
      }

      main.methods.foreach(traverser.traverseMethodDef(_))

      assertTrue(foundJSImport)
    }
  }

  @Test
  def testFoldLiteralClosureCaptures(): AsyncResult = await {
    val classDefs = Seq(
      mainTestClassDef({
        consoleLog(Closure(true, List(paramDef("x", IntType)), Nil, None, {
          BinaryOp(BinaryOp.Int_+, VarRef("x")(IntType), int(2))
        }, List(int(3))))
      })
    )

    for {
      moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers)
    } yield {
      traverseMainMethod(moduleSet) {
        case c: Closure =>
          assertTrue(s"captures remaining: $c", c.captureParams.isEmpty)
          assertTrue(s"body is not a constant: $c", c.body.isInstanceOf[IntLiteral])
        case _ =>
      }
    }
  }

  @Test
  def testSupportImplicitClosureCaptures(): AsyncResult = await {
    val calc = m("calc", Nil, I)

    val classDefs = Seq(
      classDef(
          MainTestClassName,
          kind = ClassKind.Class,
          superClass = Some(ObjectClass),
          methods = List(
              // @noinline static def calc(): Int = 1
              MethodDef(EMF.withNamespace(PublicStatic), calc, NON, Nil,
                  IntType, Some(int(1)))(EOH.withNoinline(true), UNV),
              mainMethodDef(Block(
                VarDef("x", NON, IntType, mutable = false,
                    ApplyStatic(EAF, MainTestClassName, calc, Nil)(IntType)),
                consoleLog(Closure(true, List(paramDef("y", IntType)), Nil, None,
                    VarRef("y")(IntType), List(VarRef("x")(IntType))))
              ))
          )
      )
    )

    for {
      moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers)
    } yield {
      traverseMainMethod(moduleSet) {
        case c: Closure =>
          c.captureValues match {
            case List(VarRef(LocalIdent(name))) =>
              assertEquals(s"unexpected capture name: $c", c.captureParams.head.name.name, name)

            case _ =>
              fail(s"unexpected capture values: $c")
          }

        case _ =>
      }
    }
  }

  private def commonClassDefsForFieldRemovalTests(classInline: Boolean,
      witnessMutable: Boolean): Seq[ClassDef] = {
    val methodName = m("method", Nil, I)

    val witnessType = ClassType("Witness")

    Seq(
      classDef("Witness", kind = ClassKind.Interface),
      classDef("Foo", kind = ClassKind.Class, superClass = Some(ObjectClass),
          fields = List(
            // x: Witness
            FieldDef(EMF.withMutable(witnessMutable), FieldName("Foo", "x"), NON, witnessType),

            // y: Int
            FieldDef(EMF, FieldName("Foo", "y"), NON, IntType)
          ),
          methods = List(
            // def this() = {
            //   this.x = null
            //   this.y = 5
            // }
            MethodDef(EMF.withNamespace(Constructor), NoArgConstructorName, NON, Nil, NoType, Some(Block(
              Assign(Select(This()(ClassType("Foo")), FieldName("Foo", "x"))(witnessType), Null()),
              Assign(Select(This()(ClassType("Foo")), FieldName("Foo", "y"))(IntType), int(5))
            )))(EOH, UNV),

            // def method(): Int = this.y
            MethodDef(EMF, methodName, NON, Nil, IntType, Some {
              Select(This()(ClassType("Foo")), FieldName("Foo", "y"))(IntType)
            })(EOH, UNV)
          ),
          optimizerHints = EOH.withInline(classInline)
      ),
      mainTestClassDef({
        consoleLog(Apply(EAF, New("Foo", NoArgConstructorName, Nil), methodName, Nil)(IntType))
      })
    )
  }

  @Test
  def removeUnusedFields(): AsyncResult = await {
    val classDefs = commonClassDefsForFieldRemovalTests(classInline = false, witnessMutable = false)

    for {
      moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers)
    } yield {
      findClass(moduleSet, "Foo").get.fields match {
        case List(FieldDef(_, FieldIdent(name), _, _)) if name == FieldName("Foo", "y") =>
          // ok

        case fields =>
          fail(s"Unexpected fields: $fields")
      }

      assertFalse(findClass(moduleSet, "Witness").isDefined)
    }
  }

  @Test
  def removeUnusedFieldsInline(): AsyncResult = await {
    val classDefs = commonClassDefsForFieldRemovalTests(classInline = true, witnessMutable = false)

    for {
      moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers)
    } yield {
      assertFalse(findClass(moduleSet, "Foo").isDefined)
      assertFalse(findClass(moduleSet, "Witness").isDefined)
    }
  }

  @Test
  def removeUnusedMutableFieldsInline(): AsyncResult = await {
    val classDefs = commonClassDefsForFieldRemovalTests(classInline = true, witnessMutable = true)

    for {
      moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers)
    } yield {
      assertFalse(findClass(moduleSet, "Foo").isDefined)
      assertFalse(findClass(moduleSet, "Witness").isDefined)
    }
  }

  @Test
  def removeUnreachableLinkTimeIfBranch(): AsyncResult = await {
    val methodName = m("method", Nil, I)
    val methodBody = LinkTimeIf(
        LinkTimeTree.BinaryOp(
          LinkTimeOp.Boolean_==,
          LinkTimeTree.Property("core/productionMode", BooleanType),
          LinkTimeTree.BooleanConst(true)),
        int(1), int(0))(IntType)
    val classDefs = Seq(
      classDef("Foo", kind = ClassKind.Class, superClass = Some(ObjectClass),
          methods = List(
            trivialCtor("Foo"),
            MethodDef(EMF, methodName, NON, Nil, IntType, Some(methodBody))(EOH, UNV)
          )),
      mainTestClassDef({
        consoleLog(Apply(EAF, New("Foo", NoArgConstructorName, Nil), methodName, Nil)(IntType))
      })
    )
    for {
      moduleSet <- linkToModuleSet(
        classDefs, MainTestModuleInitializers,
        config = StandardConfig().withSemantics((_.withProductionMode(true)))
      )
    } yield {
      findClass(moduleSet, ClassName("Foo")).get
        .methods.find(_.name.name == methodName).get
        .body.get match {
          case IntLiteral(1) => // ok
          case t =>
            fail(s"Unexpected body: $t")
        }
    }
  }

  @Test
  def removeUnreachableCalleeByLinkTimeIf(): AsyncResult = await {
    val methodName = m("method", Nil, I)
    val classDefs = Seq(
      classDef("Foo", kind = ClassKind.Class, superClass = Some(ObjectClass),
        methods = List(
          trivialCtor("Foo"),
          // def method(): Int = 0
          MethodDef(EMF, methodName, NON, Nil, IntType, Some(int(0)))(EOH, UNV))
      ),
      mainTestClassDef({
        LinkTimeIf(
          LinkTimeTree.BinaryOp(
            LinkTimeOp.Boolean_==,
            LinkTimeTree.Property("core/productionMode", BooleanType),
            LinkTimeTree.BooleanConst(true)),
          consoleLog(str("prod")),
          consoleLog(Apply(EAF, New("Foo", NoArgConstructorName, Nil),
              methodName, Nil)(IntType))
        )(NoType)
      })
    )

    for {
      moduleSet <- linkToModuleSet(
        classDefs, MainTestModuleInitializers,
        config = StandardConfig().withSemantics((_.withProductionMode(true)))
      )
    } yield {
      assertFalse(findClass(moduleSet, ClassName("Foo")).isDefined)
    }
  }

  def inlineFlagsTestCommon(optimizerHints: OptimizerHints, applyFlags: ApplyFlags,
      expectInline: Boolean): AsyncResult = await {
    val classDefs = Seq(
        classDef(
            MainTestClassName,
            kind = ClassKind.Class,
            superClass = Some(ObjectClass),
            methods = List(
                trivialCtor(MainTestClassName),
                MethodDef(EMF.withNamespace(PublicStatic), witnessMethodName, NON, Nil, AnyType, Some({
                  // Non-trivial body to ensure no inlining by heuristics.
                  Block(consoleLog(str("something")), str("result"))
                }))(optimizerHints, UNV),
                mainMethodDef({
                  consoleLog({
                    ApplyStatic(applyFlags, MainTestClassName, witnessMethodName, Nil)(AnyType)
                  })
                })
            )
        )
    )

    for {
      moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers)
    } yield {
      val didInline = !findClass(moduleSet, MainTestClassName).get
        .methods.exists(_.name.name == witnessMethodName)

      assertEquals(expectInline, didInline)
    }
  }

  @Test
  def inlineFlagsTestDefault(): AsyncResult =
    inlineFlagsTestCommon(EOH, EAF, expectInline = false)

  @Test
  def inlineFlagsTestInlineDefSite(): AsyncResult =
    inlineFlagsTestCommon(EOH.withInline(true), EAF, expectInline = true)

  @Test
  def inlineFlagsTestNoinlineDefSite(): AsyncResult =
    inlineFlagsTestCommon(EOH.withNoinline(true), EAF, expectInline = false)

  @Test
  def inlineFlagsTestInlineCallSite(): AsyncResult =
    inlineFlagsTestCommon(EOH, EAF.withInline(true), expectInline = true)

  @Test
  def inlineFlagsTestNoinlineDefSiteNoOverride(): AsyncResult =
    inlineFlagsTestCommon(EOH.withNoinline(true), EAF.withInline(true), expectInline = false)

  @Test
  def inlineFlagsTestNoinlineCallSite(): AsyncResult =
    inlineFlagsTestCommon(EOH, EAF.withNoinline(true), expectInline = false)

  @Test
  def inlineFlagsTestNoinlineCallSiteOverride(): AsyncResult =
    inlineFlagsTestCommon(EOH.withInline(true), EAF.withNoinline(true), expectInline = false)
}

object OptimizerTest {
  private val cloneMethodName = m("clone", Nil, O)
  private val witnessMethodName = m("witness", Nil, O)

  private def findClass(moduleSet: ModuleSet, name: ClassName): Option[LinkedClass] =
    moduleSet.modules.flatMap(_.classDefs).find(_.className == name)

  private def traverseMainMethod(moduleSet: ModuleSet)(f: Tree => Unit) = {
    val mainClassDef = findClass(moduleSet, MainTestClassName).get
    val mainMethodDef = mainClassDef.methods
      .find(m => m.name.name == MainMethodName && m.flags.namespace == MemberNamespace.PublicStatic).get

    new Traverser {
      override def traverse(tree: Tree): Unit = {
        f(tree)
        super.traverse(tree)
      }
    }.traverseMethodDef(mainMethodDef)
  }
}
