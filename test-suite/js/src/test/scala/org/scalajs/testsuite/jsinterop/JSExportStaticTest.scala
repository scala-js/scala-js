/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.jsinterop

import scala.scalajs.js
import js.annotation._

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform._

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

class JSExportStaticTest {
  @Test def toplevel_basic_static_method_export(): Unit = {
    val statics = js.constructorOf[TopLevelStaticExportMethods]

    assertEquals(1, statics.basic())
  }

  @Test def toplevel_overloaded_static_method_export(): Unit = {
    val statics = js.constructorOf[TopLevelStaticExportMethods]

    assertEquals("Hello World", statics.overload("World"))
    assertEquals(2, statics.overload(2))
    assertEquals(9, statics.overload(2, 7))
    assertEquals(10, statics.overload(1, 2, 3, 4))
  }

  @Test def toplevel_renamed_static_method_export(): Unit = {
    val statics = js.constructorOf[TopLevelStaticExportMethods]

    assertEquals(11, statics.renamed(8))
  }

  @Test def toplevel_renamed_overloaded_static_method_export(): Unit = {
    val statics = js.constructorOf[TopLevelStaticExportMethods]

    assertEquals("Hello World", statics.renamedOverload("World"))
    assertEquals(2, statics.renamedOverload(2))
    assertEquals(9, statics.renamedOverload(2, 7))
    assertEquals(10, statics.renamedOverload(1, 2, 3, 4))
  }

  @Test def toplevel_static_method_export_constructor(): Unit = {
    val statics = js.constructorOf[TopLevelStaticExportMethods]

    assertEquals(24, statics.constructor(12))
  }

  @Test def toplevel_static_method_export_uses_unique_object(): Unit = {
    val statics = js.constructorOf[TopLevelStaticExportMethods]

    statics.setMyVar(3)
    assertEquals(3, TopLevelStaticExportMethods.myVar)
    statics.setMyVar(7)
    assertEquals(7, TopLevelStaticExportMethods.myVar)
  }

  @Test def toplevel_static_method_export_also_exists_in_member(): Unit = {
    val statics = js.constructorOf[TopLevelStaticExportMethods]
    assertEquals(15, statics.alsoExistsAsMember(3))

    val obj = new TopLevelStaticExportMethods
    assertEquals(6, obj.alsoExistsAsMember(3))
  }

  @Test def nested_basic_static_method_export(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportMethods]

    assertEquals(1, statics.basic())
  }

  @Test def nested_overloaded_static_method_export(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportMethods]

    assertEquals("Hello World", statics.overload("World"))
    assertEquals(2, statics.overload(2))
    assertEquals(9, statics.overload(2, 7))
    assertEquals(10, statics.overload(1, 2, 3, 4))
  }

  @Test def nested_renamed_static_method_export(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportMethods]

    assertEquals(11, statics.renamed(8))
  }

  @Test def nested_renamed_overloaded_static_method_export(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportMethods]

    assertEquals("Hello World", statics.renamedOverload("World"))
    assertEquals(2, statics.renamedOverload(2))
    assertEquals(9, statics.renamedOverload(2, 7))
    assertEquals(10, statics.renamedOverload(1, 2, 3, 4))
  }

  @Test def nested_static_method_export_constructor(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportMethods]

    assertEquals(24, statics.constructor(12))
  }

  @Test def nested_static_method_export_uses_unique_object(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportMethods]

    statics.setMyVar(3)
    assertEquals(3, JSExportStaticTest.StaticExportMethods.myVar)
    statics.setMyVar(7)
    assertEquals(7, JSExportStaticTest.StaticExportMethods.myVar)
  }

  @Test def nested_static_method_export_also_exists_in_member(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportMethods]
    assertEquals(15, statics.alsoExistsAsMember(3))

    val obj = new JSExportStaticTest.StaticExportMethods
    assertEquals(6, obj.alsoExistsAsMember(3))
  }

}

@ScalaJSDefined
class TopLevelStaticExportMethods extends js.Object {
  def alsoExistsAsMember(x: Int): Int = x * 2
}

object TopLevelStaticExportMethods {
  @JSExportStatic
  def basic(): Int = 1

  @JSExportStatic
  def overload(x: String): String = "Hello " + x

  @JSExportStatic
  def overload(x: Int, y: Int*): Int = x + y.sum

  @JSExportStatic("renamed")
  def renamedMethod(x: Int): Int = x + 3

  @JSExportStatic
  def renamedOverload(x: String): String = "Hello " + x

  @JSExportStatic("renamedOverload")
  def renamedOverloadedMethod(x: Int, y: Int*): Int = x + y.sum

  @JSExportStatic
  def constructor(x: Int): Int = 2 * x

  var myVar: Int = _

  @JSExportStatic
  def setMyVar(x: Int): Unit = myVar = x

  @JSExportStatic
  def alsoExistsAsMember(x: Int): Int = x * 5
}

object JSExportStaticTest {
  @ScalaJSDefined
  class StaticExportMethods extends js.Object {
    def alsoExistsAsMember(x: Int): Int = x * 2
  }

  object StaticExportMethods {
    @JSExportStatic
    def basic(): Int = 1

    @JSExportStatic
    def overload(x: String): String = "Hello " + x

    @JSExportStatic
    def overload(x: Int, y: Int*): Int = x + y.sum

    @JSExportStatic("renamed")
    def renamedMethod(x: Int): Int = x + 3

    @JSExportStatic
    def renamedOverload(x: String): String = "Hello " + x

    @JSExportStatic("renamedOverload")
    def renamedOverloadedMethod(x: Int, y: Int*): Int = x + y.sum

    @JSExportStatic
    def constructor(x: Int): Int = 2 * x

    var myVar: Int = _

    @JSExportStatic
    def setMyVar(x: Int): Unit = myVar = x

    @JSExportStatic
    def alsoExistsAsMember(x: Int): Int = x * 5
  }
}
