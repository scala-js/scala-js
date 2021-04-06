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
import js.annotation._

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform._

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

class JSExportStaticTest {
  // Methods

  @Test def toplevelBasicStaticMethodExport(): Unit = {
    val statics = js.constructorOf[TopLevelStaticExportMethods]

    assertEquals(1, statics.basic())
  }

  @Test def toplevelOverloadedStaticMethodExport(): Unit = {
    val statics = js.constructorOf[TopLevelStaticExportMethods]

    assertEquals("Hello World", statics.overload("World"))
    assertEquals(2, statics.overload(2))
    assertEquals(9, statics.overload(2, 7))
    assertEquals(10, statics.overload(1, 2, 3, 4))
  }

  @Test def toplevelRenamedStaticMethodExport(): Unit = {
    val statics = js.constructorOf[TopLevelStaticExportMethods]

    assertEquals(11, statics.renamed(8))
  }

  @Test def toplevelRenamedOverloadedStaticMethodExport(): Unit = {
    val statics = js.constructorOf[TopLevelStaticExportMethods]

    assertEquals("Hello World", statics.renamedOverload("World"))
    assertEquals(2, statics.renamedOverload(2))
    assertEquals(9, statics.renamedOverload(2, 7))
    assertEquals(10, statics.renamedOverload(1, 2, 3, 4))
  }

  @Test def toplevelStaticMethodExportConstructor(): Unit = {
    val statics = js.constructorOf[TopLevelStaticExportMethods]

    assertEquals(24, statics.constructor(12))
  }

  @Test def toplevelStaticMethodExportUsesUniqueObject(): Unit = {
    val statics = js.constructorOf[TopLevelStaticExportMethods]

    statics.setMyVar(3)
    assertEquals(3, TopLevelStaticExportMethods.myVar)
    statics.setMyVar(7)
    assertEquals(7, TopLevelStaticExportMethods.myVar)
  }

  @Test def toplevelStaticMethodExportAlsoExistsInMember(): Unit = {
    val statics = js.constructorOf[TopLevelStaticExportMethods]
    assertEquals(15, statics.alsoExistsAsMember(3))

    val obj = new TopLevelStaticExportMethods
    assertEquals(6, obj.alsoExistsAsMember(3))
  }

  @Test def nestedBasicStaticMethodExport(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportMethods]

    assertEquals(1, statics.basic())
  }

  @Test def nestedOverloadedStaticMethodExport(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportMethods]

    assertEquals("Hello World", statics.overload("World"))
    assertEquals(2, statics.overload(2))
    assertEquals(9, statics.overload(2, 7))
    assertEquals(10, statics.overload(1, 2, 3, 4))
  }

  @Test def nestedRenamedStaticMethodExport(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportMethods]

    assertEquals(11, statics.renamed(8))
  }

  @Test def nestedRenamedOverloadedStaticMethodExport(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportMethods]

    assertEquals("Hello World", statics.renamedOverload("World"))
    assertEquals(2, statics.renamedOverload(2))
    assertEquals(9, statics.renamedOverload(2, 7))
    assertEquals(10, statics.renamedOverload(1, 2, 3, 4))
  }

  @Test def nestedStaticMethodExportConstructor(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportMethods]

    assertEquals(24, statics.constructor(12))
  }

  @Test def nestedStaticMethodExportUsesUniqueObject(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportMethods]

    statics.setMyVar(3)
    assertEquals(3, JSExportStaticTest.StaticExportMethods.myVar)
    statics.setMyVar(7)
    assertEquals(7, JSExportStaticTest.StaticExportMethods.myVar)
  }

  @Test def nestedStaticMethodExportAlsoExistsInMember(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportMethods]
    assertEquals(15, statics.alsoExistsAsMember(3))

    val obj = new JSExportStaticTest.StaticExportMethods
    assertEquals(6, obj.alsoExistsAsMember(3))
  }

  // Properties

  @Test def basicStaticPropReadonly(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportProperties]

    assertEquals(1, statics.basicReadOnly)
  }

  @Test def basicStaticPropReadwrite(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportProperties]

    assertEquals(5, statics.basicReadWrite)
    statics.basicReadWrite = 10
    assertEquals(15, statics.basicReadWrite)
  }

  @Test def staticPropSetWrongTypeThrowsClassCastException(): Unit = {
    assumeTrue("assuming compliant asInstanceOfs", hasCompliantAsInstanceOfs)

    val statics = js.constructorOf[JSExportStaticTest.StaticExportProperties]

    assertThrows(classOf[ClassCastException], {
      statics.basicReadWrite = "wrong type"
    })
  }

  @Test def overloadedStaticPropSetter(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportProperties]

    assertEquals("got: ", statics.overloadedSetter)
    statics.overloadedSetter = "foo"
    assertEquals("got: foo", statics.overloadedSetter)
    statics.overloadedSetter = 5
    assertEquals("got: foo10", statics.overloadedSetter)
  }

  @Test def overloadedStaticPropRenamed(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportProperties]

    assertEquals(5, statics.renamed)
    statics.renamed = 10
    assertEquals(15, statics.renamed)
    statics.renamed = "foobar"
    assertEquals(21, statics.renamed)
  }

  @Test def staticPropConstructor(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportProperties]

    assertEquals(102, statics.constructor)
  }

  @Test def staticPropAlsoExistsInMember(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportProperties]
    assertEquals("also a member", statics.alsoExistsAsMember)

    val obj = new JSExportStaticTest.StaticExportProperties
    assertEquals(54, obj.alsoExistsAsMember)
  }

  // Fields

  @Test def basicField(): Unit = {
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

  @Test def readTamperedVarCausesClassCastException(): Unit = {
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

  @Test def renamedField(): Unit = {
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

  @Test def uninitializedFields(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportFields]

    assertEquals(0, JSExportStaticTest.StaticExportFields.uninitializedVarInt)
    assertEquals(0, statics.uninitializedVarInt)

    assertEquals(null,
        JSExportStaticTest.StaticExportFields.uninitializedVarString)
    assertEquals(null, statics.uninitializedVarString)

    assertEquals('\u0000',
        JSExportStaticTest.StaticExportFields.uninitializedVarChar)
    assertEquals('\u0000', statics.uninitializedVarChar)
  }

  @Test def fieldAlsoExistsInMember(): Unit = {
    val statics = js.constructorOf[JSExportStaticTest.StaticExportFields]
    assertEquals("hello", statics.alsoExistsAsMember)

    val obj = new JSExportStaticTest.StaticExportFields
    assertEquals(5, obj.alsoExistsAsMember)
  }

  // Inherited members

  @Test def testInheritedMembersInECMAScript2015(): Unit = {
    assumeTrue("Requires ECMAScript 2015 semantics", useECMAScript2015Semantics)

    val parent = js.constructorOf[JSExportStaticTest.StaticExportsParent]
    val child = js.constructorOf[JSExportStaticTest.StaticExportsChild]

    assertEquals(5, child.inheritedVal)

    assertEquals("hello", child.inheritedVar)
    parent.inheritedVar = "world"
    assertEquals("world", child.inheritedVar)
    child.inheritedVar = "overwritten"
    assertEquals("overwritten", child.inheritedVar)
    assertEquals("world", parent.inheritedVar)
    parent.inheritedVar = "another"
    assertEquals("overwritten", child.inheritedVar)

    assertEquals(42, child.inheritedProperty)
    parent.inheritedProperty = 53
    assertEquals(53, child.inheritedProperty)
    child.inheritedProperty = 23
    assertEquals(23, child.inheritedProperty)
    assertEquals(23, parent.inheritedProperty)
    parent.inheritedProperty = 123
    assertEquals(123, child.inheritedProperty)

    assertEquals(6, child.inheritedMethod(5))
  }

}

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

  class StaticExportsParent extends js.Object

  object StaticExportsParent {
    @JSExportStatic
    val inheritedVal: Int = 5

    @JSExportStatic
    var inheritedVar: String = "hello"

    private var propVar: Int = 42

    @JSExportStatic
    def inheritedProperty: Int = propVar

    @JSExportStatic
    def inheritedProperty_=(v: Int): Unit = propVar = v

    @JSExportStatic
    def inheritedMethod(x: Int): Int = x + 1
  }

  class StaticExportsChild extends StaticExportsParent
}
