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

package org.scalajs.linker.checker

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir.ClassKind
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.linker.testutils.TestIRBuilder._

class ClassDefCheckerTest {
  import ClassDefCheckerTest.assertError

  @Test
  def javaLangObjectNoSuperClass(): Unit = {
    assertError(
        classDef(ObjectClass, superClass = Some("Parent")),
        "java.lang.Object cannot have a superClass")
  }

  @Test
  def javaLangObjectKind(): Unit = {
    assertError(
        classDef(ObjectClass, kind = ClassKind.Interface),
        "java.lang.Object must be a Class")
  }

  @Test
  def javaLangObjectNoInterfaces(): Unit = {
    assertError(
        classDef(ObjectClass, interfaces = List("Parent")),
        "java.lang.Object may not implement any interfaces")
  }

  @Test
  def hijackedClassesKinds(): Unit = {
    assertError(
        classDef(BoxedIntegerClass, kind = ClassKind.Class, superClass = Some(ObjectClass)),
        "java.lang.Integer must be a HijackedClass")

    assertError(
        classDef("A", kind = ClassKind.HijackedClass, superClass = Some(ObjectClass)),
        "A must not be a HijackedClass")
  }

  @Test
  def missingSuperClass(): Unit = {
    val kinds = Seq(
        ClassKind.Class,
        ClassKind.ModuleClass,
        ClassKind.HijackedClass,
        ClassKind.JSClass,
        ClassKind.JSModuleClass,
        ClassKind.NativeJSClass,
        ClassKind.NativeJSModuleClass
    )

    for (kind <- kinds) {
      val name = if (kind == ClassKind.HijackedClass) BoxedIntegerClass else ClassName("A")
      assertError(
          classDef(name, kind = kind, memberDefs = requiredMemberDefs(name, kind)),
          "missing superClass")
    }
  }

  @Test
  def interfaceNoSuperClass(): Unit = {
    assertError(
        classDef("A", kind = ClassKind.Interface, superClass = Some("B")),
        "interfaces may not have a superClass")
  }

  @Test
  def noDuplicateFields(): Unit = {
    assertError(
        classDef("A", superClass = Some(ObjectClass),
            memberDefs = List(
              FieldDef(EMF, "foobar", NON, IntType),
              FieldDef(EMF, "foobar", NON, BooleanType)
            )),
        "duplicate field 'foobar'")
  }

  @Test
  def noDuplicateMethods(): Unit = {
    val babarMethodName = MethodName("babar", List(IntRef), IntRef)

    assertError(
        classDef("A", superClass = Some(ObjectClass),
          memberDefs = List(
              MethodDef(EMF, babarMethodName, NON, List(paramDef("x", IntType)),
                      IntType, None)(EOH, None),
              MethodDef(EMF, babarMethodName, NON, List(paramDef("y", IntType)),
                      IntType, None)(EOH, None)
            )),
        "duplicate method 'babar(int)int'")
  }

  @Test
  def noDuplicateConstructors(): Unit = {
    val BoxedStringType = ClassType(BoxedStringClass)
    val stringCtorName = MethodName.constructor(List(T))

    val FooClass = ClassName("Foo")
    val FooType = ClassType(FooClass)

    val callPrimaryCtorBody: Tree = {
      ApplyStatically(EAF.withConstructor(true), This()(FooType),
          FooClass, NoArgConstructorName, Nil)(NoType)
    }

    assertError(
        classDef(FooClass, superClass = Some(ObjectClass),
            memberDefs = List(
              trivialCtor(FooClass),
              MethodDef(EMF.withNamespace(MemberNamespace.Constructor),
                  stringCtorName, NON, List(paramDef("x", BoxedStringType)),
                  NoType, Some(callPrimaryCtorBody))(
                  EOH, None),
              MethodDef(EMF.withNamespace(MemberNamespace.Constructor),
                  stringCtorName, NON, List(paramDef("y", BoxedStringType)),
                  NoType, Some(callPrimaryCtorBody))(
                  EOH, None)
          )),
        "duplicate constructor method '<init>(java.lang.String)void'")
  }

  @Test
  def noDuplicateVarDef(): Unit = {
    val body = Block(
      VarDef("x", NoOriginalName, IntType, mutable = false, int(1)),
      While(BooleanLiteral(true), {
        VarDef("x", NoOriginalName, IntType, mutable = false, int(1))
      })
    )

    assertError(
        classDef("A", kind = ClassKind.Interface, memberDefs = List(mainMethodDef(body))),
        "Duplicate local variable name x."
    )
  }

  @Test
  def noDuplicateVarDefForIn(): Unit = {
    val body = Block(
      VarDef("x", NoOriginalName, IntType, mutable = false, int(1)),
      ForIn(JSObjectConstr(Nil), "x", NoOriginalName, Skip())
    )

    assertError(
        classDef("A", kind = ClassKind.Interface, memberDefs = List(mainMethodDef(body))),
        "Duplicate local variable name x."
    )
  }

  @Test
  def noDuplicateVarDefTryCatch(): Unit = {
    val body = Block(
      VarDef("x", NoOriginalName, IntType, mutable = false, int(1)),
      TryCatch(Skip(), "x", NoOriginalName, Skip())(NoType)
    )

    assertError(
        classDef("A", kind = ClassKind.Interface, memberDefs = List(mainMethodDef(body))),
        "Duplicate local variable name x."
    )
  }
}

private object ClassDefCheckerTest {
  private def assertError(clazz: ClassDef, expectMsg: String) = {
    var seen = false
    val reporter = new ErrorReporter {
      def reportError(msg: String)(implicit ctx: ErrorReporter.ErrorContext) = {
        assertEquals("unexpected error reported", expectMsg, msg)
        assertFalse("error reported multiple times", seen)
        seen = true
      }
    }

    new ClassDefChecker(clazz, reporter).checkClassDef()
    assertTrue("no errors reported", seen)
  }
}
