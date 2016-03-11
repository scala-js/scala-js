/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.jsinterop

import scala.language.implicitConversions

import scala.scalajs.js

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.JSAssert._

class ArrayTest {
  import ArrayTest.VC

  // scala.scalajs.js.Array

  @Test def should_provide_implicit_conversion_from_js_Array_to_ArrayOps_String(): Unit = {
    var propCount = 0
    var propString = ""

    for (item <- js.Array("Sc", "ala", ".", "js")) {
      assertTrue(item.isInstanceOf[String])
      propCount += 1
      propString += item
    }

    assertEquals(4, propCount)
    assertEquals("Scala.js", propString)
  }

  @Test def should_provide_implicit_conversion_from_js_Array_to_ArrayOps_Int(): Unit = {
    var propCount = 0
    var propString = ""

    for (item <- js.Array(7, 3, 5, 7)) {
      assertTrue(item.isInstanceOf[Int])
      propCount += 1
      propString += item
    }

    assertEquals(4, propCount)
    assertEquals("7357", propString)
  }

  @Test def should_provide_implicit_conversion_from_js_Array_to_ArrayOps_Char(): Unit = {
    var propCount = 0
    var propString = ""

    for (item <- js.Array('S', 'c', 'a', 'l', 'a')) {
      assertTrue(item.isInstanceOf[Char])
      propCount += 1
      propString += item
    }

    assertEquals(5, propCount)
    assertEquals("Scala", propString)
  }

  @Test def should_provide_implicit_conversion_from_js_Array_to_ArrayOps_value_class(): Unit = {
    var propCount = 0
    var propString = ""

    for (item <- js.Array(new VC(5), new VC(-4))) {
      assertTrue(item.isInstanceOf[VC])
      propCount += 1
      propString += item
    }

    assertEquals(2, propCount)
    assertEquals("VC(5)VC(-4)", propString)
  }

  // scala.scalajs.js.JSConverters.JSRichGenTraversableOnce

  @Test def should_provide_toJSArray(): Unit = {
    import js.JSConverters._
    assertJSArrayEquals(js.Array("foo", "bar"), List("foo", "bar").toJSArray)
    assertJSArrayEquals(js.Array(1, 2, 3), Iterator(1, 2, 3).toJSArray)
    assertJSArrayEquals(js.Array(0.3, 7.3, 8.9), Array(0.3, 7.3, 8.9).toJSArray)
    assertJSArrayEquals(js.Array(), None.toJSArray)
    // The following fails on 2.10.x
    //assertJSArrayEquals(js.Array("Hello World"), Some("Hello World").toJSArray)
  }
}

object ArrayTest {

  private class VC(val x: Int) extends AnyVal {
    override def toString(): String = s"VC($x)"
  }

}
