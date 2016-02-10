/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.lang

import org.scalajs.testsuite.utils.Platform

import language.implicitConversions

import scala.scalajs.js
import scala.scalajs.runtime.assumingES6

import org.junit.Test
import org.junit.Assert._

class SystemJSTest {

  @Test def identityHashCode_should_survive_if_an_object_is_sealed(): Unit = {
    /* This is mostly forward-checking that, should we have an implementation
     * that seals Scala.js objects, identityHashCode() survives.
     */
    class HasIDHashCodeToBeSealed

    // Seal before the first call to hashCode()
    val x1 = new HasIDHashCodeToBeSealed
    js.Object.seal(x1.asInstanceOf[js.Object])
    val x1FirstHash = x1.hashCode()
    assertEquals(x1FirstHash, x1.hashCode())

    // Seal after the first call to hashCode()
    val x2 = new HasIDHashCodeToBeSealed
    val x2FirstHash = x2.hashCode()
    js.Object.seal(x2.asInstanceOf[js.Object])
    assertEquals(x2FirstHash, x2.hashCode())
  }

  @Test def identityHashCode_for_JS_objects(): Unit = {
    if (assumingES6 || !js.isUndefined(js.Dynamic.global.WeakMap)) {
      /* This test is more restrictive than the spec, but we know our
       * implementation will always pass the test.
       */
      val x1 = new js.Object
      val x2 = new js.Object
      val x1FirstHash = x1.hashCode()
      assertEquals(x1FirstHash, x1.hashCode())
      assertNotEquals(x1.hashCode(), x2.hashCode())
      assertEquals(x1FirstHash, x1.hashCode())

      assertEquals(x1FirstHash, System.identityHashCode(x1))
      assertEquals(x2.hashCode(), System.identityHashCode(x2))
    } else {
      val x1 = new js.Object
      val x1FirstHash = x1.hashCode()
      assertEquals(x1FirstHash, x1.hashCode())
      assertEquals(x1FirstHash, System.identityHashCode(x1))
    }
  }

  @Test def systemProperties(): Unit = {
    def get(key: String): String = java.lang.System.getProperty(key)

    // Defined in System.scala

    assertEquals("1.8", get("java.version"))
    assertEquals("1.8", get("java.vm.specification.version"))
    assertEquals("Oracle Corporation", get("java.vm.specification.vendor"))
    assertEquals("Scala.js", get("java.vm.name"))
    assertEquals("1.8", get("java.specification.version"))
    assertEquals("/", get("file.separator"))
    assertEquals(":", get("path.separator"))
    assertEquals("\n", get("line.separator"))

    // Defined in Build.scala and added via __ScalaJSEnv in ScalaJSPluginInternal

    assertEquals("testtag.value", get("scalajs.testsuite.testtag"))
  }
}
