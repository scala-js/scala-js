/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2016, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.jsinterop

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.JSAssert._

/** 2.12+ tests for `@JSOptional`.
 *
 *  This class is basically a copy-paste of `JSOptionalTest`, where
 *  `override val/def`s do not have an explicit result type. Instead, they
 *  are inferred from the superclasses.
 */
class JSOptionalTest212 {
  import JSOptionalTest212._

  @Test def classWithOptional(): Unit = {
    val obj = new ClassWithOptional

    assertEquals(js.undefined, obj.actualUndefined)
    assertTrue(obj.hasOwnProperty("actualUndefined"))

    assertEquals(js.undefined, obj.x)
    assertFalse(obj.hasOwnProperty("x"))

    assertEquals(js.undefined, obj.y)
    assertFalse(js.Object.hasProperty(obj, "y"))

    assertEquals(js.undefined, obj.y2)
    assertFalse(js.Object.hasProperty(obj, "y2"))

    assertEquals(js.undefined, obj.z)
    assertFalse(obj.hasOwnProperty("z"))
    obj.z = Some(3)
    assertEquals(Some(3), obj.z)
  }

  @Test def objectWithOptional(): Unit = {
    val obj = ObjectWithOptional

    assertEquals(js.undefined, obj.actualUndefined)
    assertTrue(obj.hasOwnProperty("actualUndefined"))

    assertEquals(js.undefined, obj.x)
    assertFalse(obj.hasOwnProperty("x"))

    assertEquals(js.undefined, obj.y)
    assertFalse(js.Object.hasProperty(obj, "y"))

    assertEquals(js.undefined, obj.y2)
    assertFalse(js.Object.hasProperty(obj, "y2"))

    assertEquals(js.undefined, obj.z)
    assertFalse(obj.hasOwnProperty("z"))
    obj.z = Some(3)
    assertEquals(Some(3), obj.z)
  }

  @Test def classImplementsTraitWithOptional(): Unit = {
    val obj = new ClassImplementsTraitWithOptional

    assertEquals(js.undefined, obj.x)
    assertFalse(obj.hasOwnProperty("x"))

    assertEquals(js.undefined, obj.y)
    assertFalse(js.Object.hasProperty(obj, "y"))

    assertEquals(js.undefined, obj.y2)
    assertFalse(js.Object.hasProperty(obj, "y2"))

    assertEquals(js.undefined, obj.z)
    assertFalse(obj.hasOwnProperty("z"))
    obj.z = Some(3)
    assertEquals(Some(3), obj.z)
  }

  @Test def anonClassImplementsTraitWithOptional(): Unit = {
    val obj = new TraitWithOptional {}

    assertEquals(js.undefined, obj.x)
    assertFalse(obj.hasOwnProperty("x"))

    assertEquals(js.undefined, obj.y)
    assertFalse(js.Object.hasProperty(obj, "y"))

    assertEquals(js.undefined, obj.y2)
    assertFalse(js.Object.hasProperty(obj, "y2"))

    assertEquals(js.undefined, obj.z)
    assertFalse(obj.hasOwnProperty("z"))
    obj.z = Some(3)
    assertEquals(Some(3), obj.z)
  }

  @Test def classOverrideOptionalWithConcrete(): Unit = {
    val obj = new ClassImplementsTraitWithOptionalOverrideWithConcrete

    assertEquals(42, obj.x)
    assertTrue(obj.hasOwnProperty("x"))

    assertEquals("hello", obj.y)
    assertTrue(obj.hasOwnProperty("y"))

    assertEquals("world", obj.y2)
    assertTrue(js.Object.hasProperty(obj, "y2"))

    assertEquals(Some(5), obj.z)
    assertTrue(obj.hasOwnProperty("z"))
    obj.z = Some(3)
    assertEquals(Some(3), obj.z)
  }

  @Test def anonClassOverrideOptionalWithConcrete(): Unit = {
    val obj = new TraitWithOptional {
      override val x = 42
      override val y = "hello"
      override def y2 = "world"
      z = Some(5)
    }

    assertEquals(42, obj.x)
    assertTrue(obj.hasOwnProperty("x"))

    assertEquals("hello", obj.y)
    assertTrue(obj.hasOwnProperty("y"))

    assertEquals("world", obj.y2)
    assertTrue(js.Object.hasProperty(obj, "y2"))

    assertEquals(Some(5), obj.z)
    assertTrue(obj.hasOwnProperty("z"))
    obj.z = Some(3)
    assertEquals(Some(3), obj.z)
  }
}

object JSOptionalTest212 {
  @ScalaJSDefined
  class ClassWithOptional extends js.Object {
    val actualUndefined: js.UndefOr[Int] = js.undefined
    @JSOptional val x: js.UndefOr[Int] = js.undefined
    @JSOptional def y: js.UndefOr[String] = js.undefined
    @JSOptional def y2: js.UndefOr[String] = js.undefined
    @JSOptional var z: js.UndefOr[Option[Int]] = js.undefined
  }

  @ScalaJSDefined
  object ObjectWithOptional extends js.Object {
    val actualUndefined: js.UndefOr[Int] = js.undefined
    @JSOptional val x: js.UndefOr[Int] = js.undefined
    @JSOptional def y: js.UndefOr[String] = js.undefined
    @JSOptional def y2: js.UndefOr[String] = js.undefined
    @JSOptional var z: js.UndefOr[Option[Int]] = js.undefined
  }

  @ScalaJSDefined
  trait TraitWithOptional extends js.Object {
    @JSOptional val x: js.UndefOr[Int] = js.undefined
    @JSOptional def y: js.UndefOr[String] = js.undefined
    @JSOptional def y2: js.UndefOr[String] = js.undefined
    @JSOptional var z: js.UndefOr[Option[Int]] = js.undefined
  }

  @ScalaJSDefined
  class ClassImplementsTraitWithOptional extends TraitWithOptional

  @ScalaJSDefined
  class ClassImplementsTraitWithOptionalOverrideWithConcrete
      extends TraitWithOptional {
    override val x = 42
    override val y = "hello"
    override def y2 = "world"
    z = Some(5)
  }
}
