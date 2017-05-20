package org.scalajs.testsuite.jsinterop

import org.junit.Test
import org.junit.Assert._

import scala.scalajs.js
import scala.scalajs.js.annotation._

package object packageobjectwithnatives {
  @js.native
  @JSGlobal
  object JSNativeObjectInPackageFoo extends js.Object

  @js.native
  object JSNativeObjectInPackageFooJSNameOmitted extends js.Object

  @js.native
  @JSGlobal("JSNativeObjectInPackageBar")
  object JSNativeObjectInPackageBaz extends js.Object

  @js.native
  @JSName("JSNativeObjectInPackageBar")
  object JSNativeObjectInPackageBazJSName extends js.Object

  @js.native
  @JSGlobal
  class JSNativeClassInPackageFoo extends js.Object {
    def foo(): String = js.native
  }

  @js.native
  class JSNativeClassInPackageFooJSNameOmitted extends js.Object {
    def foo(): String = js.native
  }

  @js.native
  @JSName("JSNativeClassInPackageBar")
  class JSNativeClassInPackageBaz extends js.Object {
    def baz(): String = js.native
  }

  @js.native
  @JSGlobal("JSNativeClassInPackageBar")
  class JSNativeClassInPackageBazJSName extends js.Object {
    def baz(): String = js.native
  }
}

class JSNativeInPackage {
  import packageobjectwithnatives._
  import js.Dynamic.global

  @Test def testObjectDefaultJSGlobal(): Unit = {
    val gJSNativeObjectInPackageFoo = global.JSNativeObjectInPackageFoo
    assertNotEquals("undefined", js.typeOf(gJSNativeObjectInPackageFoo))
    assertSame(JSNativeObjectInPackageFoo, gJSNativeObjectInPackageFoo)
  }

  @Test def testObjectDefaultJSName(): Unit = {
    val gJSNativeObjectInPackageFoo = global.JSNativeObjectInPackageFoo
    assertNotEquals("undefined", js.typeOf(gJSNativeObjectInPackageFoo))
    assertSame(JSNativeObjectInPackageFooJSNameOmitted,
        gJSNativeObjectInPackageFoo)
  }

  @Test def testObjectJSGlobal(): Unit = {
    val gJSNativeObjectInPackageBar = global.JSNativeObjectInPackageBar
    assertNotEquals("undefined", js.typeOf(gJSNativeObjectInPackageBar))
    assertEquals("undefined", js.typeOf(global.JSNativeObjectInPackageBaz))
    assertSame(JSNativeObjectInPackageBaz, gJSNativeObjectInPackageBar)
  }

  @Test def testObjectJSName(): Unit = {
    val gJSNativeObjectInPackageBar = global.JSNativeObjectInPackageBar
    assertNotEquals("undefined", js.typeOf(gJSNativeObjectInPackageBar))
    assertEquals("undefined", js.typeOf(global.JSNativeObjectInPackageBaz))
    assertSame(JSNativeObjectInPackageBazJSName, gJSNativeObjectInPackageBar)
  }

  @Test def testClassDefaultJSGlobal(): Unit = {
    val gJSNativeClassInPackageFooCtr = global.JSNativeClassInPackageFoo
    assertNotEquals("undefined", js.typeOf(gJSNativeClassInPackageFooCtr))
    assertEquals(js.constructorOf[JSNativeClassInPackageFoo],
        gJSNativeClassInPackageFooCtr)

    val gJSNativeClassInPackageFoo =
      js.Dynamic.newInstance(gJSNativeClassInPackageFooCtr)()
    assertEquals("foo", gJSNativeClassInPackageFoo.foo())
    assertEquals("foo", new JSNativeClassInPackageFoo().foo())
  }

  @Test def testClassDefaultJSName(): Unit = {
    val gJSNativeClassInPackageFooCtr = global.JSNativeClassInPackageFoo
    assertNotEquals("undefined", js.typeOf(gJSNativeClassInPackageFooCtr))
    assertEquals(js.constructorOf[JSNativeClassInPackageFooJSNameOmitted],
        gJSNativeClassInPackageFooCtr)

    val gJSNativeClassInPackageFoo =
      js.Dynamic.newInstance(gJSNativeClassInPackageFooCtr)()
    assertEquals("foo", gJSNativeClassInPackageFoo.foo())
    assertEquals("foo", new JSNativeClassInPackageFooJSNameOmitted().foo())
  }

  @Test def testClassJSGlobal(): Unit = {
    val gJSNativeClassInPackageBarCtr = global.JSNativeClassInPackageBar
    assertNotEquals("undefined", js.typeOf(gJSNativeClassInPackageBarCtr))
    assertSame(js.constructorOf[JSNativeClassInPackageBaz],
        gJSNativeClassInPackageBarCtr)
    assertEquals("undefined", js.typeOf(global.JSNativeClassInPackageBaz))

    val gJSNativeClassInPackageBar =
      js.Dynamic.newInstance(gJSNativeClassInPackageBarCtr)()
    assertEquals("baz", gJSNativeClassInPackageBar.baz())
    assertEquals("baz", new JSNativeClassInPackageBaz().baz())
  }

  @Test def testClassJSName(): Unit = {
    val gJSNativeClassInPackageBarCtr = global.JSNativeClassInPackageBar
    assertNotEquals("undefined", js.typeOf(gJSNativeClassInPackageBarCtr))
    assertSame(js.constructorOf[JSNativeClassInPackageBazJSName],
        gJSNativeClassInPackageBarCtr)
    assertEquals("undefined", js.typeOf(global.JSNativeClassInPackageBaz))

    val gJSNativeClassInPackageBar =
      js.Dynamic.newInstance(gJSNativeClassInPackageBarCtr)()
    assertEquals("baz", gJSNativeClassInPackageBar.baz())
    assertEquals("baz", new JSNativeClassInPackageBazJSName().baz())
  }
}
