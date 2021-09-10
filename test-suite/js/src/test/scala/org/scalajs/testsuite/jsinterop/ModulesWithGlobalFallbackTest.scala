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
import scala.scalajs.js.typedarray._

import java.nio.{ByteBuffer, CharBuffer}

import org.junit.Assert._
import org.junit.Assume._
import org.junit.BeforeClass
import org.junit.Test

import org.scalajs.testsuite.utils.Platform._

/* !!! This is mostly copy-pasted from `ModulesTest.scala` in
 * `src/test/require-modules/`. This is the version with global fallbacks.
 */
class ModulesWithGlobalFallbackTest {
  import ModulesWithGlobalFallbackTest._

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

}

object ModulesWithGlobalFallbackTest {
  private object ModuleFallbackImpl extends js.Object {
    /* We cannot use defs, because they require the function call to have a
     * `this` bound to the containing object. But we cannot test fallbacks for
     * native imports for this.
     * Admittedly, this is a bit hacky.
     */
    val ssum: js.Function = { (x: Int, y: js.UndefOr[Int]) =>
      val y2 = y.getOrElse(1)
      x * x + y2 * y2
    }

    val strConstant: String = "value"
  }

  private object DefaultAsSelfFallbackImpl extends js.Object {
    val x: Int = 1
    val y: String = "foo"
  }

  private val defaultFunFallbackImpl: js.Function0[Int] = () => 5

  private class MyBoxFallbackImpl[T](private var x: T) extends js.Object {
    def get(): T = x
    def set(x: T): Unit = this.x = x
  }

  private object MyBoxStaticFallbackImpl extends js.Object {
    def make[T](x: T): MyBoxFallbackImpl[T] = new MyBoxFallbackImpl(x)
  }

  @BeforeClass
  def beforeClass(): Unit = {
    assumeTrue("Assuming that Typed Arrays are supported", typedArrays)

    if (isNoModule) {
      val global = org.scalajs.testsuite.utils.JSUtils.globalObject
      global.ModulesWithGlobalFallbackTest_Module = ModuleFallbackImpl
      global.ModulesWithGlobalFallbackTest_DefaultAsSelf = DefaultAsSelfFallbackImpl
      global.ModulesWithGlobalFallbackTest_DefaultFun = defaultFunFallbackImpl
      global.ModulesWithGlobalFallbackTest_MyBox = js.constructorOf[MyBoxFallbackImpl[_]]
      global.ModulesWithGlobalFallbackTest_MyBoxStatic = MyBoxStaticFallbackImpl
    }
  }

  final val modulePath = "../test-classes/modules-test.js"

  @js.native
  @JSImport(modulePath, JSImport.Namespace,
      globalFallback = "ModulesWithGlobalFallbackTest_Module")
  object NamespaceImport extends js.Object {
    /* In this facade, 50 is not the actual default value for `y`.
     * We intentionally use a different value to check that it is ignored.
     * See #4554.
     */
    def ssum(x: Int, y: Int = 50): Int = js.native
    val strConstant: String = js.native

    @JSName("strConstant")
    def strConstantAsDef: String = js.native
  }

  @js.native
  @JSImport("../test-classes/modules-test-default-as-self.js", JSImport.Default,
      globalFallback = "ModulesWithGlobalFallbackTest_DefaultAsSelf")
  object DefaultAsSelf extends js.Object {
    val x: Int = js.native
    val y: String = js.native
  }

  @js.native
  @JSImport(modulePath, JSImport.Default,
      globalFallback = "ModulesWithGlobalFallbackTest_DefaultFun")
  def defaultFunction(): Int = js.native

  object NativeMembers {
    /* In this facade, 50 is not the actual default value for `y`.
     * We intentionally use a different value to check that it is ignored.
     * See #4554.
     */
    @js.native
    @JSImport(modulePath, "ssum",
        globalFallback = "ModulesWithGlobalFallbackTest_Module.ssum")
    def ssum(x: Int, y: Int = 50): Int = js.native

    @js.native
    @JSImport(modulePath, "strConstant",
        globalFallback = "ModulesWithGlobalFallbackTest_Module.strConstant")
    val strConstant: String = js.native

    @js.native
    @JSImport(modulePath, "strConstant",
        globalFallback = "ModulesWithGlobalFallbackTest_Module.strConstant")
    def strConstantAsDef: String = js.native
  }

  @js.native
  @JSImport(modulePath, "MyBox",
      globalFallback = "ModulesWithGlobalFallbackTest_MyBox")
  class MyBox[T](x: T) extends js.Object {
    def get(): T = js.native
    def set(x: T): Unit = js.native
  }

  @js.native
  @JSImport(modulePath, "MyBox",
      globalFallback = "ModulesWithGlobalFallbackTest_MyBoxStatic")
  object MyBox extends js.Object {
    def make[T](x: T): MyBox[T] = js.native
  }
}
