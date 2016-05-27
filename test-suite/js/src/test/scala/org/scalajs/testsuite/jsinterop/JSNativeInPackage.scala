package org.scalajs.testsuite.jsinterop

import org.junit.Test
import org.junit.Assert._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName

package object packageobjectwithnatives {
  @js.native
  object JSNativeObjectInPackageFoo extends js.Object

  @js.native
  @JSName("JSNativeObjectInPackageBar")
  object JSNativeObjectInPackageBaz extends js.Object

  @js.native
  class JSNativeClassInPackageFoo extends js.Object {
    def foo(): String = js.native
  }

  @js.native
  @JSName("JSNativeClassInPackageBar")
  class JSNativeClassInPackageBaz extends js.Object {
    def baz(): String = js.native
  }
}

class JSNativeInPackage {
  import packageobjectwithnatives._
  import js.Dynamic.global

  @Test def testOvjectDefaultJSName(): Unit = {
    val gJSNativeObjectInPackageFoo = global.JSNativeObjectInPackageFoo
    assertFalse(js.isUndefined(gJSNativeObjectInPackageFoo))
    assertSame(JSNativeObjectInPackageFoo, gJSNativeObjectInPackageFoo)
  }

  @Test def testObjectJSName(): Unit = {
    val gJSNativeObjectInPackageBar = global.JSNativeObjectInPackageBar
    val gJSNativeObjectInPackageBaz = global.JSNativeObjectInPackageBaz
    assertFalse(js.isUndefined(gJSNativeObjectInPackageBar))
    assertTrue(js.isUndefined(gJSNativeObjectInPackageBaz))
    assertSame(JSNativeObjectInPackageBaz, gJSNativeObjectInPackageBar)
  }

  @Test def testClassDefaultJSName(): Unit = {
    val gJSNativeClassInPackageFooCtr = global.JSNativeClassInPackageFoo
    assertFalse(js.isUndefined(gJSNativeClassInPackageFooCtr))
    assertEquals(js.constructorOf[JSNativeClassInPackageFoo],
        gJSNativeClassInPackageFooCtr)

    val gJSNativeClassInPackageFoo =
      js.Dynamic.newInstance(gJSNativeClassInPackageFooCtr)()
    assertEquals("foo", gJSNativeClassInPackageFoo.foo())
    assertEquals("foo", new JSNativeClassInPackageFoo().foo())
  }

  @Test def testClassJSName(): Unit = {
    val gJSNativeClassInPackageBarCtr = global.JSNativeClassInPackageBar
    val gJSNativeClassInPackageBazCtr = global.JSNativeClassInPackageBaz
    assertFalse(js.isUndefined(gJSNativeClassInPackageBarCtr))
    assertSame(js.constructorOf[JSNativeClassInPackageBaz],
        gJSNativeClassInPackageBarCtr)
    assertTrue(js.isUndefined(gJSNativeClassInPackageBazCtr))

    val gJSNativeClassInPackageBar =
      js.Dynamic.newInstance(gJSNativeClassInPackageBarCtr)()
    assertEquals("baz", gJSNativeClassInPackageBar.baz())
    assertEquals("baz", new JSNativeClassInPackageBaz().baz())
  }
}
