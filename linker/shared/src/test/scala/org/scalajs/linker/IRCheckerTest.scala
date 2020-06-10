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
                New(FooClass, stringCtorName, List(StringLiteral("hello")))),
            Apply(EAF, VarRef("foo")(FooType), babarMethodName, List(int(5)))(IntType)
        ))
    )

    for (log <- testLinkIRErrors(classDefs, MainTestModuleInitializers)) yield {
      log.assertContainsLogLine(
          "Duplicate definition of field 'foobar' in class 'Foo'")
      log.assertContainsLogLine(
          "Duplicate definition of constructor method '<init>(java.lang.String)void' in class 'Foo'")
      log.assertContainsLogLine(
          "Duplicate definition of method 'babar(int)int' in class 'Foo'")
    }
  }

}

object IRCheckerTest {
  def testLinkNoIRError(classDefs: Seq[ClassDef],
      moduleInitializers: List[ModuleInitializer])(
      implicit ec: ExecutionContext): Future[Unit] = {

    LinkingUtils.expectSuccess(LinkingUtils.linkOnly(
        classDefs, moduleInitializers,
        StandardConfig().withOptimizer(false)))
  }

  def testLinkIRErrors(classDefs: Seq[ClassDef],
      moduleInitializers: List[ModuleInitializer])(
      implicit ec: ExecutionContext): Future[TestLogger] = {

    val failedResult = LinkingUtils.expectFailure(LinkingUtils.linkOnly(
        classDefs, moduleInitializers,
        StandardConfig().withOptimizer(false)))

    for (result <- failedResult) yield {
      val exception = result.exception
      assertTrue(exception.getMessage(),
          exception.getMessage().contains("IR checking error"))
      result.log
    }
  }
}
