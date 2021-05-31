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
            memberDefs = List(
                trivialCtor("Bar"),

                /* This method is called, but unreachable because there are no
                 * instances of `Bar`. It will therefore not make `Foo` reachable.
                 */
                MethodDef(EMF, methMethodName, NON,
                    List(paramDef("foo", ClassType("Foo"))), NoType,
                    Some(Skip()))(
                    EOH, None)
            )
        ),

        classDef(MainTestClassName,
            superClass = Some(ObjectClass),
            memberDefs = List(
                trivialCtor(MainTestClassName),
                MethodDef(EMF.withNamespace(MemberNamespace.PublicStatic),
                    nullBarMethodName, NON, Nil, ClassType("Bar"),
                    Some(Null()))(
                    EOH, None),
                mainMethodDef(Block(
                    callMethOn(ApplyStatic(EAF, MainTestClassName,
                        nullBarMethodName, Nil)(ClassType("Bar"))),
                    callMethOn(Null()),
                    callMethOn(Throw(Null()))
                ))
            )
        )
    )

    testLinkNoIRError(classDefs, mainModuleInitializers("Test"))
  }

  @Test
  def testDuplicateMembers(): AsyncResult = await {
    val FooClass = ClassName("Foo")
    val FooType = ClassType(FooClass)
    val BoxedStringType = ClassType(BoxedStringClass)

    val stringCtorName = MethodName.constructor(List(ClassRef(BoxedStringClass)))
    val babarMethodName = MethodName("babar", List(IntRef), IntRef)

    val callPrimaryCtorBody: Tree = {
      ApplyStatically(EAF.withConstructor(true), This()(FooType),
          FooClass, NoArgConstructorName, Nil)(NoType)
    }

    def babarMethodBody(paramName: String): Tree = {
      BinaryOp(BinaryOp.Int_+,
          Select(This()(FooType), FooClass, "foobar")(IntType),
          VarRef(paramName)(IntType))
    }

    val classDefs = Seq(
        classDef("Foo",
            superClass = Some(ObjectClass),
            memberDefs = List(
                trivialCtor("Foo"),

                // Duplicate fields

                FieldDef(EMF, "foobar", NON, IntType),
                FieldDef(EMF, "foobar", NON, BoxedStringType),

                // Duplicate constructors

                MethodDef(EMF.withNamespace(MemberNamespace.Constructor),
                    stringCtorName, NON, List(paramDef("x", BoxedStringType)),
                    NoType, Some(callPrimaryCtorBody))(
                    EOH, None),

                MethodDef(EMF.withNamespace(MemberNamespace.Constructor),
                    stringCtorName, NON, List(paramDef("y", BoxedStringType)),
                    NoType, Some(callPrimaryCtorBody))(
                    EOH, None),

                // Duplicate methods

                MethodDef(EMF, babarMethodName, NON, List(paramDef("x", IntType)),
                    IntType, Some(babarMethodBody("x")))(
                    EOH, None),

                MethodDef(EMF, babarMethodName, NON, List(paramDef("y", IntType)),
                    IntType, Some(babarMethodBody("y")))(
                    EOH, None)
            )
        ),

        mainTestClassDef(Block(
            VarDef("foo", NON, FooType, mutable = false,
                New(FooClass, stringCtorName, List(str("hello")))),
            Apply(EAF, VarRef("foo")(FooType), babarMethodName, List(int(5)))(IntType)
        ))
    )

    for (log <- testLinkIRErrors(classDefs, MainTestModuleInitializers)) yield {
      log.assertContainsError(
          "Duplicate definition of field 'foobar' in class 'Foo'")
      log.assertContainsError(
          "Duplicate definition of constructor method '<init>(java.lang.String)void' in class 'Foo'")
      log.assertContainsError(
          "Duplicate definition of method 'babar(int)int' in class 'Foo'")
    }
  }

  @Test
  def missingJSNativeLoadSpec(): AsyncResult = await {
    val classDefs = Seq(
      classDef("A", kind = ClassKind.NativeJSClass, superClass = Some(ObjectClass)),
      classDef("B", kind = ClassKind.NativeJSClass, superClass = Some(ObjectClass)),
      classDef("C", kind = ClassKind.NativeJSModuleClass, superClass = Some(ObjectClass)),

      classDef("D", kind = ClassKind.JSClass, superClass = Some("A")),

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
        memberDefs = List(
          JSMethodDef(EMF, str("constructor"), Nil, None, Block(
            JSSuperConstructorCall(Nil)
          ))(EOH, None)
        )
      ),

      mainTestClassDef(Block(
        LoadJSConstructor("Foo")
      ))
    )

    for (log <- testLinkIRErrors(classDefs, MainTestModuleInitializers)) yield {
      log.assertContainsError(
          "Expression tree has type NoType")
      log.assertContainsError(
          "Invalid expression tree")
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
        memberDefs = List(
          JSMethodDef(EMF, str("constructor"), Nil, None, Block(
            JSSuperConstructorCall(Nil),
            VarDef("x", NON, IntType, mutable = false, int(5))
          ))(EOH, None)
        )
      ),

      mainTestClassDef(Block(
        LoadJSConstructor("Foo")
      ))
    )

    for (log <- testLinkIRErrors(classDefs, MainTestModuleInitializers)) yield {
      log.assertContainsError(
          "Expression tree has type NoType")
      log.assertContainsError(
          "Invalid expression tree")
    }
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

    // We cannot use `transform` because of 2.11.
    link(classDefs, moduleInitializers, logger).failed.recoverWith {
      case _: NoSuchElementException =>
        Future.failed(new AssertionError("IR checking did not fail"))
    }.map { _ =>
      logger.allLogLines
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
