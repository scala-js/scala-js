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

package org.scalajs.testsuite.jsinterop

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.junit.Assert._
import org.junit.Test

/* !!! This is mostly copy-pasted in `ModulesWithGlobalFallbackTest.scala` in
 * `src/test/scala/`, with a version with global fallbacks.
 */
class ModulesTest {
  import ModulesTest._

  @Test def testImportModuleItself(): Unit = {
    val m = NamespaceImport
    assertEquals("object", js.typeOf(m))

    assertEquals(5, m.ssum(2))
    assertEquals(13, m.ssum(2, 3))
    assertEquals("value", m.strConstant)
    assertEquals("value", m.strConstantAsDef)

    /* Potentially, this could be "optimized" by importing `stringify` as a
     * global symbol if we are emitting ES2015 modules.
     */
    assertEquals(5, NamespaceImport.ssum(2))
    assertEquals(13, NamespaceImport.ssum(2, 3))
    assertEquals("value", NamespaceImport.strConstant)
    assertEquals("value", NamespaceImport.strConstantAsDef)
  }

  @Test def testImportLegacyModuleItselfAsDefault(): Unit = {
    val m = DefaultAsSelf
    assertEquals("object", js.typeOf(m))

    assertEquals(1, m.x)
    assertEquals("foo", m.y)

    /* Potentially, this could be "optimized" by importing `stringify` as a
     * global symbol if we are emitting ES2015 modules.
     */
    assertEquals(1, DefaultAsSelf.x)
    assertEquals("foo", DefaultAsSelf.y)
  }

  @Test def testImportDefaultFunction(): Unit = {
    assertEquals(5, defaultFunction())
  }

  @Test def testImportFunctionInModule(): Unit = {
    assertEquals(5, NativeMembers.ssum(2))
    assertEquals(13, NativeMembers.ssum(2, 3))
  }

  @Test def testImportFieldInModule(): Unit = {
    assertEquals("string", js.typeOf(NativeMembers.strConstant))
    assertEquals("string", js.typeOf(NativeMembers.strConstantAsDef))
  }

  @Test def testImportFunctionInModulePackageObject(): Unit = {
    assertEquals(5, modulestestpackageobject.ssum(2))
    assertEquals(13, modulestestpackageobject.ssum(2, 3))
  }

  @Test def testImportFieldInModulePackageObject(): Unit = {
    assertEquals("string", js.typeOf(modulestestpackageobject.strConstant))
    assertEquals("string", js.typeOf(modulestestpackageobject.strConstantAsDef))
  }

  @Test def testImportObjectInModule(): Unit = {
    assertTrue((MyBox: Any).isInstanceOf[js.Object])
    assertTrue(MyBox.make(5).isInstanceOf[MyBox[_]])
  }

  @Test def testImportClassInModule(): Unit = {
    val b = new MyBox(1L)

    assertEquals(1L, b.get())
    b.set(5L)
    assertEquals(5L, b.get())
  }

  // #4001
  @Test def testNoImportUnusedSuperClass(): Unit = {
    new ExistentSubClass
    ExistentSubObject
  }

  // #4267
  @Test def testImportSuperClassUsedOnlyInExtendsOfNonNativeJSClass(): Unit = {
    val instance = new ChildOfNativeClass("Bob")
    assertEquals("Hello Bob", instance.x)
  }

  @Test def testClassReferencedOnlyInClassData_Issue4548(): Unit = {
    val cls = classOf[JSClassReferencedOnlyInClassData]
    assertFalse(cls.isInstance(new js.Date()))
  }
}

package object modulestestpackageobject {
  @js.native
  @JSImport(ModulesTest.modulePath, "ssum")
  def ssum(x: Int, y: Int = 1): Int = js.native

  @js.native
  @JSImport(ModulesTest.modulePath, "strConstant")
  val strConstant: String = js.native

  @js.native
  @JSImport(ModulesTest.modulePath, "strConstant")
  def strConstantAsDef: String = js.native
}

object ModulesTest {
  final val modulePath = "../test-classes/modules-test.js"

  @js.native
  @JSImport(modulePath, JSImport.Namespace)
  object NamespaceImport extends js.Object {
    def ssum(x: Int, y: Int = 1): Int = js.native
    val strConstant: String = js.native

    @JSName("strConstant")
    def strConstantAsDef: String = js.native
  }

  @js.native
  @JSImport("../test-classes/modules-test-default-as-self.js", JSImport.Default)
  object DefaultAsSelf extends js.Object {
    val x: Int = js.native
    val y: String = js.native
  }

  @js.native
  @JSImport(modulePath, JSImport.Default)
  def defaultFunction(): Int = js.native

  object NativeMembers {
    @js.native
    @JSImport(modulePath, "ssum")
    def ssum(x: Int, y: Int = 1): Int = js.native

    @js.native
    @JSImport(modulePath, "strConstant")
    val strConstant: String = js.native

    @js.native
    @JSImport(modulePath, "strConstant")
    def strConstantAsDef: String = js.native
  }

  @js.native
  @JSImport(modulePath, "MyBox")
  class MyBox[T](x: T) extends js.Object {
    def get(): T = js.native
    def set(x: T): Unit = js.native
  }

  @js.native
  @JSImport(modulePath, "MyBox")
  object MyBox extends js.Object {
    def make[T](x: T): MyBox[T] = js.native
  }

  // #4001 - Test that unused super-classes are not imported.
  @js.native
  @JSImport("non-existent.js", "Foo")
  class NonExistentSuperClass extends js.Object

  @js.native
  @JSImport(modulePath, "MyBox")
  class ExistentSubClass extends NonExistentSuperClass

  @js.native
  @JSImport(modulePath, JSImport.Namespace)
  object ExistentSubObject extends NonExistentSuperClass

  /* #4268 Test that a super-class only used in an extends from a non-native JS
   * class *is* imported.
   */
  @js.native
  @JSImport(
      "../test-classes/ModulesTestSuperClassUsedOnlyInExtendsOfNonNativeJSClass.js",
      "ModuleTestNativeParentClass")
  class NativeParentClass(x0: String) extends js.Object {
    val x: String = js.native
  }

  class ChildOfNativeClass(name: String)
      extends NativeParentClass("Hello " + name)

  // #4548 Test that a class referenced only in class data *is* imported.
  @js.native
  @JSImport(
      "../test-classes/modules-test-referenced-only-in-classdata.js",
      "JSClassReferencedOnlyInClassData")
  class JSClassReferencedOnlyInClassData() extends js.Object
}
