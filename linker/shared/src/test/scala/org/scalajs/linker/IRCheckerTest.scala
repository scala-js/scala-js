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

import org.scalajs.ir.ClassKind
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.logging._

import org.scalajs.junit.async._

import org.scalajs.linker.interface._
import org.scalajs.linker.standard._

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
      Apply(EAF, receiver, methMethodName, List(Null()))(NoType)

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
                    List(paramDef("foo", ClassType("Foo", nullable = true))), NoType,
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
                    callMethOn(Throw(Null()))
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
                List(paramDef("x", ClassType(B, nullable = true))), NoType, Some(Skip()))(
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
              NoType,
              Some(Block(
                Apply(EAF, VarRef("x")(receiverType), fooMethodName,
                    List(VarRef("c")(ClassType(C, nullable = true))))(NoType),
                Apply(EAF, VarRef("x")(receiverType), fooMethodName,
                    List(VarRef("d")(ClassType(D, nullable = true))))(NoType)
              ))
            )(EOH, UNV)
          )
        ),

        mainTestClassDef(
          ApplyStatic(EAF, D, testMethodName, List(newD, newD, newD))(NoType)
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

      classDef("D", kind = ClassKind.JSClass, superClass = Some("A"), jsConstructor = Some(trivialJSCtor)),

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
          "any expected but <notype> found for JS constructor body")
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
          "any expected but <notype> found for JS constructor body")
    }
  }

  @Test
  def arrayOpsNull(): AsyncResult = await {
    val classDefs = Seq(
      mainTestClassDef(
        Block(
          ArraySelect(Null(), int(1))(NothingType),
          ArrayLength(Null()),
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

}

object IRCheckerTest {
  def testLinkNoIRError(classDefs: Seq[ClassDef],
      moduleInitializers: List[ModuleInitializer])(
      implicit ec: ExecutionContext): Future[Unit] = {
    link(classDefs, moduleInitializers, new ScalaConsoleLogger(Level.Error))
  }

  def testLinkIRErrors(classDefs: Seq[ClassDef],
      moduleInitializers: List[ModuleInitializer])(
      implicit ec: ExecutionContext): Future[LogLines] = {

    val logger = new CapturingLogger

    link(classDefs, moduleInitializers, logger).transform {
      case Success(_) => Failure(new AssertionError("IR checking did not fail"))
      case Failure(_) => Success(logger.allLogLines)
    }
  }

  private def link(classDefs: Seq[ClassDef],
      moduleInitializers: List[ModuleInitializer],
      logger: Logger)(implicit ec: ExecutionContext): Future[Unit] = {
    val config = StandardConfig()
      .withCheckIR(true)
      .withOptimizer(false)
    val linkerFrontend = StandardLinkerFrontend(config)

    val noSymbolRequirements = SymbolRequirement
      .factory("IRCheckerTest")
      .none()

    TestIRRepo.minilib.flatMap { stdLibFiles =>
      val irFiles = (
          stdLibFiles ++
          classDefs.map(MemClassDefIRFile(_))
      )

      linkerFrontend.link(irFiles, moduleInitializers, noSymbolRequirements, logger)
    }.map(_ => ())
  }
}
