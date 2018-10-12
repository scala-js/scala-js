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
    assertFalse(js.isUndefined(gJSNativeObjectInPackageFoo))
    assertSame(JSNativeObjectInPackageFoo, gJSNativeObjectInPackageFoo)
  }

  @Test def testObjectDefaultJSName(): Unit = {
    val gJSNativeObjectInPackageFoo = global.JSNativeObjectInPackageFoo
    assertFalse(js.isUndefined(gJSNativeObjectInPackageFoo))
    assertSame(JSNativeObjectInPackageFooJSNameOmitted,
        gJSNativeObjectInPackageFoo)
  }

  @Test def testObjectJSGlobal(): Unit = {
    val gJSNativeObjectInPackageBar = global.JSNativeObjectInPackageBar
    val gJSNativeObjectInPackageBaz = global.JSNativeObjectInPackageBaz
    assertFalse(js.isUndefined(gJSNativeObjectInPackageBar))
    assertTrue(js.isUndefined(gJSNativeObjectInPackageBaz))
    assertSame(JSNativeObjectInPackageBaz, gJSNativeObjectInPackageBar)
  }

  @Test def testObjectJSName(): Unit = {
    val gJSNativeObjectInPackageBar = global.JSNativeObjectInPackageBar
    val gJSNativeObjectInPackageBaz = global.JSNativeObjectInPackageBaz
    assertFalse(js.isUndefined(gJSNativeObjectInPackageBar))
    assertTrue(js.isUndefined(gJSNativeObjectInPackageBaz))
    assertSame(JSNativeObjectInPackageBazJSName, gJSNativeObjectInPackageBar)
  }

  @Test def testClassDefaultJSGlobal(): Unit = {
    val gJSNativeClassInPackageFooCtr = global.JSNativeClassInPackageFoo
    assertFalse(js.isUndefined(gJSNativeClassInPackageFooCtr))
    assertEquals(js.constructorOf[JSNativeClassInPackageFoo],
        gJSNativeClassInPackageFooCtr)

    val gJSNativeClassInPackageFoo =
      js.Dynamic.newInstance(gJSNativeClassInPackageFooCtr)()
    assertEquals("foo", gJSNativeClassInPackageFoo.foo())
    assertEquals("foo", new JSNativeClassInPackageFoo().foo())
  }

  @Test def testClassDefaultJSName(): Unit = {
    val gJSNativeClassInPackageFooCtr = global.JSNativeClassInPackageFoo
    assertFalse(js.isUndefined(gJSNativeClassInPackageFooCtr))
    assertEquals(js.constructorOf[JSNativeClassInPackageFooJSNameOmitted],
        gJSNativeClassInPackageFooCtr)

    val gJSNativeClassInPackageFoo =
      js.Dynamic.newInstance(gJSNativeClassInPackageFooCtr)()
    assertEquals("foo", gJSNativeClassInPackageFoo.foo())
    assertEquals("foo", new JSNativeClassInPackageFooJSNameOmitted().foo())
  }

  @Test def testClassJSGlobal(): Unit = {
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

  @Test def testClassJSName(): Unit = {
    val gJSNativeClassInPackageBarCtr = global.JSNativeClassInPackageBar
    val gJSNativeClassInPackageBazCtr = global.JSNativeClassInPackageBaz
    assertFalse(js.isUndefined(gJSNativeClassInPackageBarCtr))
    assertSame(js.constructorOf[JSNativeClassInPackageBazJSName],
        gJSNativeClassInPackageBarCtr)
    assertTrue(js.isUndefined(gJSNativeClassInPackageBazCtr))

    val gJSNativeClassInPackageBar =
      js.Dynamic.newInstance(gJSNativeClassInPackageBarCtr)()
    assertEquals("baz", gJSNativeClassInPackageBar.baz())
    assertEquals("baz", new JSNativeClassInPackageBazJSName().baz())
  }
}
