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

class JSOptionalTest {
  import JSOptionalTest._

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

  @Test def undefinedInClassIsNotOptional(): Unit = {
    val obj = new UndefinedInClassIsNotOptional

    assertEquals(js.undefined, obj.x)
    assertTrue(obj.hasOwnProperty("x"))

    assertEquals(js.undefined, obj.y)
    assertTrue(js.Object.hasProperty(obj, "y"))

    assertEquals(js.undefined, obj.y2)
    assertTrue(js.Object.hasProperty(obj, "y2"))

    assertEquals(js.undefined, obj.z)
    assertTrue(obj.hasOwnProperty("z"))
    obj.z = Some(3)
    assertEquals(Some(3), obj.z)
  }

  @Test def overrideWithUndefinedInClassIsNotOptional(): Unit = {
    val obj = new OverrideWithUndefinedInClassIsNotOptional

    assertEquals(js.undefined, obj.x)
    assertTrue(obj.hasOwnProperty("x"))

    assertEquals(js.undefined, obj.y)
    assertTrue(js.Object.hasProperty(obj, "y"))

    assertEquals(js.undefined, obj.y2)
    assertTrue(js.Object.hasProperty(obj, "y2"))

    assertEquals(js.undefined, obj.z)
    assertTrue(obj.hasOwnProperty("z"))
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
      override val x: js.UndefOr[Int] = 42
      override val y: js.UndefOr[String] = "hello"
      override def y2: js.UndefOr[String] = "world"
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

  @Test def overrideOptionalWithOptional(): Unit = {
    val obj = new OverrideOptionalWithOptional {}

    assertEquals(js.undefined, obj.x)
    assertFalse(obj.hasOwnProperty("x"))

    assertEquals(js.undefined, obj.y)
    assertFalse(js.Object.hasProperty(obj, "y"))

    assertEquals(js.undefined, obj.y2)
    assertFalse(js.Object.hasProperty(obj, "y2"))
  }

  @Test def overrideOptionalWithOptionalImplicitType(): Unit = {
    val obj = new OverrideOptionalWithOptionalImplicitType {}

    assertEquals(js.undefined, obj.x)
    assertFalse(obj.hasOwnProperty("x"))

    assertEquals(js.undefined, obj.y)
    assertFalse(js.Object.hasProperty(obj, "y"))

    assertEquals(js.undefined, obj.y2)
    assertFalse(js.Object.hasProperty(obj, "y2"))
  }

  @Test def overrideClassAbstractWithOptional(): Unit = {
    @ScalaJSDefined
    abstract class ClassWithAbstracts extends js.Object {
      val x: js.UndefOr[Int]
      def y: js.UndefOr[String]
      def y2: js.UndefOr[String]
      var z: js.UndefOr[Option[Int]]
    }

    @ScalaJSDefined
    trait OverrideClassAbstractWithOptional extends ClassWithAbstracts {
      val x: js.UndefOr[Int] = js.undefined
      def y: js.UndefOr[String] = js.undefined
      val y2: js.UndefOr[String] = js.undefined
      var z: js.UndefOr[Option[Int]] = js.undefined
    }

    val obj = new OverrideClassAbstractWithOptional {}

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

  @Test def overrideTraitAbstractWithOptional(): Unit = {
    @ScalaJSDefined
    trait TraitWithAbstracts extends js.Object {
      val x: js.UndefOr[Int]
      def y: js.UndefOr[String]
      def y2: js.UndefOr[String]
      var z: js.UndefOr[Option[Int]]
    }

    @ScalaJSDefined
    trait OverrideTraitAbstractWithOptional extends TraitWithAbstracts {
      val x: js.UndefOr[Int] = js.undefined
      def y: js.UndefOr[String] = js.undefined
      val y2: js.UndefOr[String] = js.undefined
      var z: js.UndefOr[Option[Int]] = js.undefined
    }

    val obj = new OverrideTraitAbstractWithOptional {}

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

  @Test def polyOptionalMethod(): Unit = {
    @ScalaJSDefined
    trait TraitWithPolyOptionalMethod extends js.Object {
      def foo[A]: js.UndefOr[A] = js.undefined
    }

    val obj = new TraitWithPolyOptionalMethod {}

    assertEquals(js.undefined, obj.foo[Int])
    assertFalse(js.Object.hasProperty(obj, "foo"))
  }

  @Test def traitWithOptionalFunction(): Unit = {
    val obj = new TraitWithOptionalFunction {
      override val f: js.UndefOr[js.Function1[Int, Int]] =
        js.defined((x: Int) => x + 1)
    }

    assertEquals("function", js.typeOf(obj.f))
    assertEquals(6, obj.f.get(5))
  }
}

object JSOptionalTest {
  @ScalaJSDefined
  trait TraitWithOptional extends js.Object {
    val x: js.UndefOr[Int] = js.undefined
    def y: js.UndefOr[String] = js.undefined
    def y2: js.UndefOr[String] = js.undefined
    var z: js.UndefOr[Option[Int]] = js.undefined
  }

  @ScalaJSDefined
  class ClassImplementsTraitWithOptional extends TraitWithOptional

  @ScalaJSDefined
  class UndefinedInClassIsNotOptional extends js.Object {
    val x: js.UndefOr[Int] = js.undefined
    def y: js.UndefOr[String] = js.undefined
    def y2: js.UndefOr[String] = js.undefined
    var z: js.UndefOr[Option[Int]] = js.undefined
  }

  @ScalaJSDefined
  class OverrideWithUndefinedInClassIsNotOptional extends TraitWithOptional {
    override val x: js.UndefOr[Int] = js.undefined
    override def y: js.UndefOr[String] = js.undefined
    override def y2: js.UndefOr[String] = js.undefined
    z = js.undefined
  }

  @ScalaJSDefined
  class ClassImplementsTraitWithOptionalOverrideWithConcrete
      extends TraitWithOptional {
    override val x: js.UndefOr[Int] = 42
    override val y: js.UndefOr[String] = "hello"
    override def y2: js.UndefOr[String] = "world"
    z = Some(5)
  }

  @ScalaJSDefined
  trait OverrideOptionalWithOptional extends TraitWithOptional {
    override val x: js.UndefOr[Int] = js.undefined
    override val y: js.UndefOr[String] = js.undefined
    override def y2: js.UndefOr[String] = js.undefined
  }

  @ScalaJSDefined
  trait OverrideOptionalWithOptionalImplicitType extends TraitWithOptional {
    /* Unlike cases where the rhs is an Int, String, etc., this compiles even
     * in 2.10 and 2.11, because the inferred type is `js.UndefOr[Nothing]`
     * (the type of `js.undefined`) which is truly a subtype of the inherited
     * member's type `js.UndefOr[Int/String]`. Note that in this case, it
     * would be impossible to re-override these fields with a
     * `js.UndefOr[Int/String]` in a subclass, which means this is pretty
     * useless in practice.
     */
    override val x = js.undefined
    override val y = js.undefined
    override def y2 = js.undefined // scalastyle:ignore
  }

  @ScalaJSDefined
  trait TraitWithOptionalFunction extends js.Object {
    val f: js.UndefOr[js.Function1[Int, Int]] = js.undefined
  }
}
