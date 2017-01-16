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
  // Methods

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

  // Properties

  @Test def basic_static_prop_readonly(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportProperties]

    assertEquals(1, statics.basicReadOnly)
  }

  @Test def basic_static_prop_readwrite(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportProperties]

    assertEquals(5, statics.basicReadWrite)
    statics.basicReadWrite = 10
    assertEquals(15, statics.basicReadWrite)
  }

  @Test def static_prop_set_wrong_type_throws_classcastexception(): Unit = {
    assumeTrue("assuming compliant asInstanceOfs", hasCompliantAsInstanceOfs)

    val statics = js.constructorOf[JSExportStaticTest.StaticExportProperties]

    assertThrows(classOf[ClassCastException], {
      statics.basicReadWrite = "wrong type"
    })
  }

  @Test def overloaded_static_prop_setter(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportProperties]

    assertEquals("got: ", statics.overloadedSetter)
    statics.overloadedSetter = "foo"
    assertEquals("got: foo", statics.overloadedSetter)
    statics.overloadedSetter = 5
    assertEquals("got: foo10", statics.overloadedSetter)
  }

  @Test def overloaded_static_prop_renamed(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportProperties]

    assertEquals(5, statics.renamed)
    statics.renamed = 10
    assertEquals(15, statics.renamed)
    statics.renamed = "foobar"
    assertEquals(21, statics.renamed)
  }

  @Test def static_prop_constructor(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportProperties]

    assertEquals(102, statics.constructor)
  }

  @Test def static_prop_also_exists_in_member(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportProperties]
    assertEquals("also a member", statics.alsoExistsAsMember)

    val obj = new JSExportStaticTest.StaticExportProperties
    assertEquals(54, obj.alsoExistsAsMember)
  }

  // Fields

  @Test def basic_field(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportFields]

    // Initialization
    assertEquals(5, statics.basicVal)
    assertEquals("hello", statics.basicVar)

    // JS modifies var
    statics.basicVar = "hello world"
    assertEquals("hello world", statics.basicVar)
    assertEquals("hello world", JSExportStaticTest.StaticExportFields.basicVar)

    // Scala modifies var
    JSExportStaticTest.StaticExportFields.basicVar = "modified once more"
    assertEquals("modified once more",
        JSExportStaticTest.StaticExportFields.basicVar)
    assertEquals("modified once more", statics.basicVar)

    // Reset var
    JSExportStaticTest.StaticExportFields.basicVar = "hello"
  }

  @Test def read_tampered_var_causes_class_cast_exception(): Unit = {
    assumeTrue("assuming compliant asInstanceOfs", hasCompliantAsInstanceOfs)

    val statics = js.constructorOf[JSExportStaticTest.StaticExportFields]

    // JS modifies var with an incorrect type
    statics.basicVar = 42
    assertThrows(classOf[ClassCastException], {
      assertEquals(42, JSExportStaticTest.StaticExportFields.basicVar)
    })

    // Reset var
    JSExportStaticTest.StaticExportFields.basicVar = "hello"
  }

  @Test def renamed_field(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportFields]

    // Initialization
    assertEquals(6, statics.renamedVal)
    assertEquals("world", statics.renamedVar)

    // JS modifies var
    statics.renamedVar = "hello world"
    assertEquals("hello world", statics.renamedVar)
    assertEquals("hello world",
        JSExportStaticTest.StaticExportFields.renamedBasicVar)

    // Scala modifies var
    JSExportStaticTest.StaticExportFields.renamedBasicVar = "modified once more"
    assertEquals("modified once more",
        JSExportStaticTest.StaticExportFields.renamedBasicVar)
    assertEquals("modified once more", statics.renamedVar)

    // Reset var
    JSExportStaticTest.StaticExportFields.renamedBasicVar = "world"
  }

  @Test def uninitialized_fields(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportFields]

    assertEquals(0, JSExportStaticTest.StaticExportFields.uninitializedVarInt)
    assertEquals(0, statics.uninitializedVarInt)

    assertEquals(null,
        JSExportStaticTest.StaticExportFields.uninitializedVarString)
    assertEquals(null, statics.uninitializedVarString)

    assertEquals('\0',
        JSExportStaticTest.StaticExportFields.uninitializedVarChar)
    assertEquals(null, statics.uninitializedVarChar)
  }

  @Test def field_also_exists_in_member(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportFields]
    assertEquals("hello", statics.alsoExistsAsMember)

    val obj = new JSExportStaticTest.StaticExportFields
    assertEquals(5, obj.alsoExistsAsMember)
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

  @ScalaJSDefined
  class StaticExportProperties extends js.Object {
    def alsoExistsAsMember: Int = 54
  }

  object StaticExportProperties {
    @JSExportStatic
    def basicReadOnly: Int = 1

    private var basicVar: Int = 5

    @JSExportStatic
    def basicReadWrite: Int = basicVar

    @JSExportStatic
    def basicReadWrite_=(v: Int): Unit = basicVar += v

    private var overloadedSetterVar: String = ""

    @JSExportStatic
    def overloadedSetter: String = "got: " + overloadedSetterVar

    @JSExportStatic
    def overloadedSetter_=(x: String): Unit = overloadedSetterVar += x

    @JSExportStatic
    def overloadedSetter_=(x: Int): Unit = overloadedSetterVar += 2 * x

    private var renamedPropVar: Int = 5

    @JSExportStatic("renamed")
    def renamedProp: Int = renamedPropVar

    @JSExportStatic("renamed")
    def renamedProp_=(v: Int): Unit = renamedPropVar += v

    @JSExportStatic("renamed")
    def renamedOverload_=(x: String): Unit = renamedPropVar += x.length

    @JSExportStatic
    def constructor: Int = 102

    @JSExportStatic
    def alsoExistsAsMember: String = "also a member"
  }

  @ScalaJSDefined
  class StaticExportFields extends js.Object {
    val alsoExistsAsMember: Int = 5
  }

  object StaticExportFields {
    @JSExportStatic
    val basicVal: Int = 5

    @JSExportStatic
    var basicVar: String = "hello"

    @JSExportStatic("renamedVal")
    val renamedBasicVal: Int = 6

    @JSExportStatic("renamedVar")
    var renamedBasicVar: String = "world"

    @JSExportStatic
    var uninitializedVarInt: Int = _

    @JSExportStatic
    var uninitializedVarString: String = _

    @JSExportStatic
    var uninitializedVarChar: Char = _

    @JSExportStatic
    val alsoExistsAsMember: String = "hello"
  }
}
