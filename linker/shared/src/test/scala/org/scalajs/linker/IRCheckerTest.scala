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
import scala.util.{Failure, Success}

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir.{ClassKind, EntryPointsInfo}
import org.scalajs.ir.Names._
import org.scalajs.ir.Transformers._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._
import org.scalajs.ir.WellKnownNames._

import org.scalajs.logging._

import org.scalajs.junit.async._

import org.scalajs.linker.interface._
import org.scalajs.linker.interface.unstable.IRFileImpl
import org.scalajs.linker.standard._
import org.scalajs.linker.frontend.Refiner
import org.scalajs.linker.backend.emitter.PrivateLibHolder

import org.scalajs.linker.testutils._
import org.scalajs.linker.testutils.TestIRBuilder._
import org.scalajs.linker.testutils.CapturingLogger.LogLines

class IRCheckerTest {
  import scala.concurrent.ExecutionContext.Implicits.global

  import IRCheckerTest._

  @Test
  def testMethodCallOnClassWithNoInstances(): AsyncResult = await {
    val FooClass = ClassName("Foo")
    val BarClass = ClassName("Bar")

    val methMethodName = m("meth", List(ClassRef(FooClass)), V)
    val nullBarMethodName = m("nullBar", Nil, ClassRef(BarClass))

    def callMethOn(receiver: Tree): Tree =
      Apply(EAF, receiver, methMethodName, List(Null()))(VoidType)

    val classDefs = Seq(
        // LFoo will be dropped by base linking
        classDef("Foo", superClass = Some(ObjectClass)),

        classDef("Bar",
            superClass = Some(ObjectClass),
            methods = List(
                trivialCtor("Bar"),

                /* This method is called, but unreachable because there are no
                 * instances of `Bar`. It will therefore not make `Foo` reachable.
                 */
                MethodDef(EMF, methMethodName, NON,
                    List(paramDef("foo", ClassType("Foo", nullable = true))), VoidType,
                    Some(Skip()))(
                    EOH, UNV)
            )
        ),

        classDef(MainTestClassName,
            superClass = Some(ObjectClass),
            methods = List(
                trivialCtor(MainTestClassName),
                MethodDef(EMF.withNamespace(MemberNamespace.PublicStatic),
                    nullBarMethodName, NON, Nil, ClassType("Bar", nullable = true),
                    Some(Null()))(
                    EOH, UNV),
                mainMethodDef(Block(
                    callMethOn(ApplyStatic(EAF, MainTestClassName,
                        nullBarMethodName, Nil)(ClassType("Bar", nullable = true))),
                    callMethOn(Null()),
                    callMethOn(UnaryOp(UnaryOp.Throw, Null()))
                ))
            )
        )
    )

    testLinkNoIRError(classDefs, mainModuleInitializers("Test"))
  }

  @Test
  def argumentTypeMismatch(): AsyncResult = await {
    val A = ClassName("A")
    val B = ClassName("B")
    val C = ClassName("C")
    val D = ClassName("D")

    val fooMethodName = m("foo", List(ClassRef(B)), V)

    val results = for (receiverClassName <- List(A, B, C, D)) yield {
      val receiverClassRef = ClassRef(receiverClassName)
      val receiverType = ClassType(receiverClassName, nullable = true)

      val testMethodName = m("test", List(receiverClassRef, ClassRef(C), ClassRef(D)), V)

      val newD = New(D, NoArgConstructorName, Nil)

      val classDefs = Seq(
        classDef(
          "A",
          kind = ClassKind.Interface,
          interfaces = Nil,
          methods = List(
            MethodDef(EMF, fooMethodName, NON,
                List(paramDef("x", ClassType(B, nullable = true))), VoidType, Some(Skip()))(
                EOH, UNV)
          )
        ),
        classDef("B", kind = ClassKind.Interface, interfaces = List("A")),
        classDef(
          "C",
          kind = ClassKind.Class,
          superClass = Some(ObjectClass),
          interfaces = List("A"),
          methods = List(trivialCtor("C"))
        ),

        classDef(
          "D",
          kind = ClassKind.Class,
          superClass = Some("C"),
          interfaces = List("B"),
          methods = List(
            trivialCtor("D", "C"),
            MethodDef(
              EMF.withNamespace(MemberNamespace.PublicStatic),
              testMethodName,
              NON,
              List(
                paramDef("x", receiverType),
                paramDef("c", ClassType(C, nullable = true)),
                paramDef("d", ClassType(D, nullable = true))
              ),
              VoidType,
              Some(Block(
                Apply(EAF, VarRef("x")(receiverType), fooMethodName,
                    List(VarRef("c")(ClassType(C, nullable = true))))(VoidType),
                Apply(EAF, VarRef("x")(receiverType), fooMethodName,
                    List(VarRef("d")(ClassType(D, nullable = true))))(VoidType)
              ))
            )(EOH, UNV)
          )
        ),

        mainTestClassDef(
          ApplyStatic(EAF, D, testMethodName, List(newD, newD, newD))(VoidType)
        )
      )

      for (log <- testLinkIRErrors(classDefs, MainTestModuleInitializers)) yield {
        log.assertContainsError(
            "B expected but C found for tree of type org.scalajs.ir.Trees$VarRef")
        log.assertNotContains("B expected but D found")
      }
    }

    Future.sequence(results)
  }

  @Test
  def missingJSNativeLoadSpec(): AsyncResult = await {
    val classDefs = Seq(
      classDef("A", kind = ClassKind.NativeJSClass, superClass = Some(ObjectClass)),
      classDef("B", kind = ClassKind.NativeJSClass, superClass = Some(ObjectClass)),
      classDef("C", kind = ClassKind.NativeJSModuleClass, superClass = Some(ObjectClass)),

      classDef("D", kind = ClassKind.JSClass, superClass = Some("A"), jsConstructor = Some(trivialJSCtor())),

      mainTestClassDef(Block(
        LoadJSConstructor("B"),
        LoadJSModule("C"),
        LoadJSConstructor("D")
      ))
    )

    for (log <- testLinkIRErrors(classDefs, MainTestModuleInitializers)) yield {
      log.assertContainsError(
          "Cannot load JS constructor of native JS class B without native load spec")
      log.assertContainsError(
          "Cannot load JS module of native JS module class C without native load spec")
      log.assertContainsError(
          "Native super class A must have a native load spec")
    }
  }

  @Test
  def jsClassConstructorBodyMustBeExpr1_Issue4491(): AsyncResult = await {
    val classDefs = Seq(
      JSObjectLikeClassDef,

      classDef(
        "Foo",
        kind = ClassKind.JSClass,
        superClass = Some(JSObjectLikeClass),
        jsConstructor = Some(
          JSConstructorDef(JSCtorFlags, Nil, None, JSConstructorBody(
            Nil,
            JSSuperConstructorCall(Nil),
            Nil
          ))(EOH, UNV)
        )
      ),

      mainTestClassDef(Block(
        LoadJSConstructor("Foo")
      ))
    )

    for (log <- testLinkIRErrors(classDefs, MainTestModuleInitializers)) yield {
      log.assertContainsError(
          "any expected but void found for JS constructor body")
    }
  }

  @Test
  def jsClassConstructorBodyMustBeExpr2_Issue4491(): AsyncResult = await {
    val classDefs = Seq(
      JSObjectLikeClassDef,

      classDef(
        "Foo",
        kind = ClassKind.JSClass,
        superClass = Some(JSObjectLikeClass),
        jsConstructor = Some(
          JSConstructorDef(JSCtorFlags, Nil, None, JSConstructorBody(
            Nil,
            JSSuperConstructorCall(Nil),
            VarDef("x", NON, IntType, mutable = false, int(5)) :: Nil
          ))(EOH, UNV)
        )
      ),

      mainTestClassDef(Block(
        LoadJSConstructor("Foo")
      ))
    )

    for (log <- testLinkIRErrors(classDefs, MainTestModuleInitializers)) yield {
      log.assertContainsError(
          "any expected but void found for JS constructor body")
    }
  }

  @Test
  def unaryOpNonNullableArguments(): AsyncResult = await {
    import UnaryOp._

    // List of ops that take non-nullable reference types as argument
    val ops = List(
      Class_name,
      Class_isPrimitive,
      Class_isInterface,
      Class_isArray,
      Class_componentType,
      Class_superClass,
      Array_length,
      GetClass,
      Clone,
      UnwrapFromThrowable
    )

    val results = for (op <- ops) yield {
      val classDefs = Seq(
        mainTestClassDef(UnaryOp(op, Null()))
      )

      for (log <- testLinkIRErrors(classDefs, MainTestModuleInitializers)) yield {
        log.assertContainsError("expected but null found")
      }
    }

    Future.sequence(results)
  }

  @Test
  def arrayOpsNullOrNothing(): AsyncResult = await {
    val classDefs = Seq(
      mainTestClassDef(
        Block(
          ArraySelect(Null(), int(1))(NothingType),
          ArraySelect(UnaryOp(UnaryOp.Throw, Null()), int(1))(NothingType),
          UnaryOp(UnaryOp.Array_length, UnaryOp(UnaryOp.Throw, Null()))
        )
      )
    )

    testLinkNoIRError(classDefs, MainTestModuleInitializers)
  }

  @Test
  def arrayAssignCovariant(): AsyncResult = await {
    val classDefs = Seq(
      classDef("Foo", superClass = Some(ObjectClass)),
      mainTestClassDef(
        Assign(
          ArraySelect(
            ArrayValue(ArrayTypeRef.of(ClassRef("Foo")), Nil),
            int(1)
          )(ClassType("Foo", true)),
          int(1) // not a Foo, but OK.
        )
      )
    )

    testLinkNoIRError(classDefs, MainTestModuleInitializers)
  }

  @Test
  def arrayNoAssignCovariantPrimitive(): AsyncResult = await {
    val classDefs = Seq(
      mainTestClassDef(
        Assign(
          ArraySelect(
            ArrayValue(ArrayTypeRef.of(IntRef), Nil),
            int(1)
          )(IntType),
          str("foo")
        )
      )
    )

    for (log <- testLinkIRErrors(classDefs, MainTestModuleInitializers)) yield {
      log.assertContainsError("int expected but string found for tree of type")
    }
  }

  def immutableFieldAssignTestClassDefs(parent: Boolean): Seq[ClassDef] = {
    val ctorBodyUnderTest =
      Assign(Select(thisFor("Bar"), FieldName("Foo", "fooFld"))(IntType), int(1))

    Seq(
      classDef(
        "Foo",
        superClass = Some(ObjectClass),
        fields = List(FieldDef(EMF, FieldName("Foo", "fooFld"), NON, IntType))
      ),
      classDef(
        "Bar",
        superClass = Some(if (parent) "Foo" else ObjectClass),
        methods = List(
          MethodDef(
              EMF.withNamespace(MemberNamespace.Constructor),
              NoArgConstructorName, NON, Nil, VoidType,
              Some(ctorBodyUnderTest))(EOH, UNV)
        )
      ),
      mainTestClassDef(New("Bar", NoArgConstructorName, Nil))
    )
  }

  @Test
  def noImmutableAssignNonParent(): AsyncResult = await {
    val classDefs = immutableFieldAssignTestClassDefs(parent = false)

    for {
      log <- testLinkIRErrors(classDefs, MainTestModuleInitializers, postOptimizer = true)
    } yield {
      log.assertContainsError("Foo expected but Bar! found for tree of type org.scalajs.ir.Trees$VarRef")
    }
  }

  @Test
  def allowImmutableAssignParent(): AsyncResult = await {
    val classDefs = immutableFieldAssignTestClassDefs(parent = true)
    testLinkNoIRError(classDefs, MainTestModuleInitializers, postOptimizer = true)
  }

  @Test
  def badRecordSelect(): AsyncResult = await {
    val recordValue = RecordValue(
      RecordType(List(
        RecordType.Field("i", NON, IntType, false),
        RecordType.Field("s", NON, StringType, false)
      )),
      List(int(1), str("foo")))

    val classDefs = Seq(
      mainTestClassDef(Block(
        RecordSelect(recordValue, "i")(StringType),
        RecordSelect(recordValue, "x")(StringType)
      ))
    )

    for {
      log <- testLinkIRErrors(classDefs, MainTestModuleInitializers, postOptimizer = true)
    } yield {
      log.assertContainsError("Record select of field type int typed as string")
      log.assertContainsError("Record select of non-existent field: x")
    }
  }

  @Test
  def badRecordValue(): AsyncResult = await {
    val classDefs = Seq(
      mainTestClassDef(Block(
        RecordValue(RecordType(Nil), List(int(1))),
        RecordValue(
            RecordType(List(RecordType.Field("i", NON, IntType, false))),
            List(str("foo"))
        )
      ))
    )

    for {
      log <- testLinkIRErrors(classDefs, MainTestModuleInitializers, postOptimizer = true)
    } yield {
      log.assertContainsError("Mismatched size for record fields / elements: 0 fields vs 1 elements")
      log.assertContainsError("int expected but string found")
    }
  }

}

object IRCheckerTest {
  /** Version of the minilib where we have replaced every node requiring
   *  desugaring by a placeholder.
   *
   *  We need this to directly feed to the IR checker post-optimizer, since
   *  nodes requiring desugaring are rejected at that point.
   */
  private lazy val minilibRequiringNoDesugaring: Future[Seq[IRFile]] = {
    import scala.concurrent.ExecutionContext.Implicits.global

    TestIRRepo.minilib.map { stdLibFiles =>
      for (irFile <- stdLibFiles) yield {
        val irFileImpl = IRFileImpl.fromIRFile(irFile)

        val patchedTreeFuture = irFileImpl.tree.map { tree =>
          new ClassTransformer {
            override def transform(tree: Tree): Tree = tree match {
              case tree: LinkTimeProperty => zeroOf(tree.tpe)
              case tree: LinkTimeIf       => zeroOf(tree.tpe)
              case tree: NewLambda        => UnaryOp(UnaryOp.Throw, Null())
              case _                      => super.transform(tree)
            }
          }.transformClassDef(tree)
        }

        new IRFileImpl(irFileImpl.path, irFileImpl.version) {
          /** Entry points information for this file. */
          def entryPointsInfo(implicit ec: ExecutionContext): Future[EntryPointsInfo] =
            irFileImpl.entryPointsInfo(ec)

          /** IR Tree of this file. */
          def tree(implicit ec: ExecutionContext): Future[ClassDef] =
            patchedTreeFuture
        }
      }
    }
  }

  def testLinkNoIRError(classDefs: Seq[ClassDef],
      moduleInitializers: List[ModuleInitializer],
      postOptimizer: Boolean = false)(
      implicit ec: ExecutionContext): Future[Unit] = {
    link(classDefs, moduleInitializers, new ScalaConsoleLogger(Level.Error), postOptimizer)
  }

  def testLinkIRErrors(classDefs: Seq[ClassDef],
      moduleInitializers: List[ModuleInitializer],
      postOptimizer: Boolean = false)(
      implicit ec: ExecutionContext): Future[LogLines] = {

    val logger = new CapturingLogger

    link(classDefs, moduleInitializers, logger, postOptimizer).transform {
      case Success(_) => Failure(new AssertionError("IR checking did not fail"))
      case Failure(_) => Success(logger.allLogLines)
    }
  }

  private def link(classDefs: Seq[ClassDef],
      moduleInitializers: List[ModuleInitializer],
      logger: Logger, postOptimizer: Boolean)(
      implicit ec: ExecutionContext): Future[Unit] = {
    val baseConfig = StandardConfig()
      .withCheckIR(true)
      .withOptimizer(false)

    val config = {
      /* Disable RuntimeLongs to workaround the Refiner disabling IRChecks in this case.
       * TODO: Remove once we run IRChecks post optimizer all the time.
       */
      if (postOptimizer) baseConfig.withESFeatures(_.withAllowBigIntsForLongs(true))
      else baseConfig
    }

    val noSymbolRequirements = SymbolRequirement
      .factory("IRCheckerTest")
      .none()

    if (postOptimizer) {
      minilibRequiringNoDesugaring.flatMap { stdLibFiles =>
        val refiner = new Refiner(CommonPhaseConfig.fromStandardConfig(config), checkIR = true)

        Future.traverse(stdLibFiles)(f => IRFileImpl.fromIRFile(f).tree).flatMap { stdLibClassDefs =>
          val allClassDefs = (
            stdLibClassDefs ++
            classDefs
          )

          refiner.refine(allClassDefs.map(c => (c, UNV)), moduleInitializers,
              noSymbolRequirements, logger)
        }
      }.map(_ => ())
    } else {
      TestIRRepo.minilib.flatMap { stdLibFiles =>
        val linkerFrontend = StandardLinkerFrontend(config)
        val irFiles = (
          stdLibFiles ++
          classDefs.map(MemClassDefIRFile(_)) ++
          PrivateLibHolder.files
        )
        linkerFrontend.link(irFiles, moduleInitializers, noSymbolRequirements, logger)
      }.map(_ => ())
    }
  }
}
