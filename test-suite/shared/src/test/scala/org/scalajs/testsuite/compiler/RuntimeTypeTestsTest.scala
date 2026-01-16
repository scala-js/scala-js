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

package org.scalajs.testsuite.compiler

import java.lang.Cloneable
import java.io.Serializable

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform._

import scala.reflect.{ClassTag, classTag}

class RuntimeTypeTestsTest {
  import RuntimeTypeTestsTest._

  @Test def objectType(): Unit = {
    @inline def testObject(expected: Boolean, value: Any): Unit =
      test(expected, value, classOf[Object], _.isInstanceOf[Object], _.asInstanceOf[Object])

    testObject(true, Some(5))
    testObject(true, 5)
    testObject(true, "hello")
    testObject(true, true)
    testObject(true, ())
    testObject(true, new Array[Int](5))
    testObject(true, new Array[String](5))
    testObject(true, new Array[Object](5))
    testObject(true, new Array[Array[Int]](5))
    testObject(true, new Array[Array[String]](5))
    testObject(true, new Array[Array[Object]](5))

    testNullValue(classOf[Object], _.isInstanceOf[Object], _.asInstanceOf[Object])
  }

  @Test def regularClass(): Unit = {
    @inline def testOption(expected: Boolean, value: Any): Unit =
      test(expected, value, classOf[Option[_]], _.isInstanceOf[Option[_]], _.asInstanceOf[Option[_]])

    testOption(true, Some(5))
    testOption(true, None)
    testOption(false, List(5))
    testOption(false, 5)
    testOption(false, new Array[Int](5))
    testOption(false, new Array[String](5))

    testNullValue(classOf[Option[_]], _.isInstanceOf[Option[_]], _.asInstanceOf[Option[_]])
  }

  @Test def regularInterface(): Unit = {
    @inline def testSeq(expected: Boolean, value: Any): Unit =
      test(expected, value, classOf[Seq[_]], _.isInstanceOf[Seq[_]], _.asInstanceOf[Seq[_]])

    testSeq(false, Some(5))
    testSeq(false, None)
    testSeq(true, List(5))
    testSeq(false, 5)
    testSeq(false, new Array[Int](5))
    testSeq(false, new Array[String](5))

    testNullValue(classOf[Seq[_]], _.isInstanceOf[Seq[_]], _.asInstanceOf[Seq[_]])
  }

  @Test def serializableAndCloneable(): Unit = {
    // Among others, this tests #2094

    class Foo
    class MySerializable extends Serializable
    class MyCloneable extends Cloneable

    @inline def testSerializable(expected: Boolean, value: Any): Unit =
      test(expected, value, classOf[Serializable], _.isInstanceOf[Serializable],
          _.asInstanceOf[Serializable])

    testSerializable(true, 5)
    testSerializable(true, 1.4)
    testSerializable(true, true)
    testSerializable(true, 'Z')
    testSerializable(true, "hello")
    testSerializable(true, List(5))
    testSerializable(true, new MySerializable)
    testSerializable(true, new Array[Int](3))
    testSerializable(true, new Array[String](3))
    testSerializable(true, new Array[Object](3))
    testSerializable(true, new Array[Serializable](3))
    testSerializable(true, new Array[Cloneable](3))
    testSerializable(true, new Array[Array[String]](3))

    testSerializable(false, new Foo)
    testSerializable(false, new MyCloneable)

    @inline def testCloneable(expected: Boolean, value: Any): Unit =
      test(expected, value, classOf[Cloneable], _.isInstanceOf[Cloneable], _.asInstanceOf[Cloneable])

    testCloneable(true, new MyCloneable)
    testCloneable(true, new Array[Int](3))
    testCloneable(true, new Array[String](3))
    testCloneable(true, new Array[Object](3))
    testCloneable(true, new Array[Serializable](3))
    testCloneable(true, new Array[Cloneable](3))
    testCloneable(true, new Array[Array[String]](3))

    testCloneable(false, 5)
    testCloneable(false, 1.4)
    testCloneable(false, true)
    testCloneable(false, 'Z')
    testCloneable(false, "hello")
    testCloneable(false, List(5))
    testCloneable(false, new Foo)
    testCloneable(false, new MySerializable)
  }

  @Test def javaLangNumber(): Unit = {
    @inline def testNumber(expected: Boolean, value: Any): Unit =
      test(expected, value, classOf[Number], _.isInstanceOf[Number], _.asInstanceOf[Number])

    testNumber(false, true)
    testNumber(false, 'A')
    testNumber(false, ())
    testNumber(false, "hello")

    testNumber(true, 1.toByte)
    testNumber(true, 0x100.toShort)
    testNumber(true, 0x10000)
    testNumber(true, 0x100000000L)
    testNumber(true, 1.5f)
    testNumber(true, 1.2)

    class CustomNumber extends Number {
      def intValue(): Int = 0
      def longValue(): Long = 0
      def floatValue(): Float = 0
      def doubleValue(): Double = 0
    }

    testNumber(true, new CustomNumber)
  }

  @Test def primitiveTypes(): Unit = {
    @inline def testBoolean(expected: Boolean, value: Any): Unit =
      test(expected, value, classOf[Boolean], _.isInstanceOf[Boolean], _.asInstanceOf[Boolean])
    @inline def testChar(expected: Boolean, value: Any): Unit =
      test(expected, value, classOf[Char], _.isInstanceOf[Char], _.asInstanceOf[Char])
    @inline def testByte(expected: Boolean, value: Any): Unit =
      test(expected, value, classOf[Byte], _.isInstanceOf[Byte], _.asInstanceOf[Byte])
    @inline def testShort(expected: Boolean, value: Any): Unit =
      test(expected, value, classOf[Short], _.isInstanceOf[Short], _.asInstanceOf[Short])
    @inline def testInt(expected: Boolean, value: Any): Unit =
      test(expected, value, classOf[Int], _.isInstanceOf[Int], _.asInstanceOf[Int])
    @inline def testLong(expected: Boolean, value: Any): Unit =
      test(expected, value, classOf[Long], _.isInstanceOf[Long], _.asInstanceOf[Long])
    @inline def testFloat(expected: Boolean, value: Any): Unit =
      test(expected, value, classOf[Float], _.isInstanceOf[Float], _.asInstanceOf[Float])
    @inline def testDouble(expected: Boolean, value: Any): Unit =
      test(expected, value, classOf[Double], _.isInstanceOf[Double], _.asInstanceOf[Double])

    testBoolean(true, false)
    testByte(true, 65.toByte)
    testShort(true, 654.toShort)
    testInt(true, -4321)
    testLong(true, 684321L)
    testFloat(true, 3.14f)
    testDouble(true, 3.14)

    testFloat(!executingInJVM, 45)
    testDouble(!executingInJVM, 45)
    testInt(!executingInJVM, 3.0f)
    testDouble(!executingInJVM, 3.0f)
    testInt(!executingInJVM, 5.0)
    testFloat(!executingInJVM, 5.0)

    testInt(!executingInJVM, 5.toByte)
    testInt(!executingInJVM, 5.toShort)
    testByte(!executingInJVM, 5)
    testShort(!executingInJVM, 5)
    testInt(!executingInJVM, 0.0)
    testFloat(!executingInJVM, 0.0)
    testFloat(!executingInJVM, -0.0)

    testBoolean(false, 12345)
    testChar(false, 12345)
    testByte(false, 'a')
    testShort(false, 'b')
    testInt(false, 'c')
    testLong(false, 'd')
    testFloat(false, 'e')
    testDouble(false, 'f')

    testFloat(false, 1.2)

    // Special cases for negative 0, NaN and Infinity

    testFloat(true, -0.0f)
    testFloat(true, Float.NaN)
    testFloat(true, Float.PositiveInfinity)

    testDouble(true, -0.0)
    testDouble(true, Double.NaN)
    testDouble(true, Double.PositiveInfinity)

    testFloat(!executingInJVM, -0.0)
    testFloat(!executingInJVM, Double.NaN)
    testFloat(!executingInJVM, Double.PositiveInfinity)

    testDouble(!executingInJVM, -0.0f)
    testDouble(!executingInJVM, Float.NaN)
    testDouble(!executingInJVM, Float.PositiveInfinity)

    testInt(false, -0.0)
    testInt(false, Double.NaN)
    testInt(false, Double.PositiveInfinity)
  }

  @Test def unit(): Unit = {
    /* The tests for `Unit` are special, because casting a non-`()` value `x`
     * to `Unit` is unpredictable. scalac may choose to insert an actual
     * `x.asInstanceOf[BoxedUnit]`, or to simply emit `{ x; () }`, depending
     * on the circumstances.
     *
     * Therefore, we can test *positive* casts to `Unit`, but we cannot test
     * anything when the expected result is negative.
     */

    import scala.runtime.BoxedUnit

    @noinline
    def testNoInline[T: ClassTag](msg: String, expected: Boolean,
        value: Any, targetClass: Class[T], isInstance: Any => Boolean,
        asInstance: Any => Any): Unit = {
      testInline(msg, expected, value, targetClass, isInstance, asInstance)
    }

    @inline
    def testInline[T: ClassTag](msg: String, expected: Boolean,
        value: Any, targetClass: Class[T], isInstance: Any => Boolean,
        asInstance: Any => Any): Unit = {
      assertEquals(msg, expected, isInstance(value))
      assertEquals(msg, expected && !targetClass.isPrimitive(), targetClass.isInstance(value))
      assertEquals(msg, expected, classTag[T].unapply(value).isDefined)

      if (expected) {
        assertAlmostSame(msg, value, asInstance(value))
        if (!targetClass.isPrimitive())
          assertAlmostSame(msg, value, targetClass.cast(value))
      }
    }

    @inline
    def test[T: ClassTag](expected: Boolean, value: Any, targetClass: Class[T],
        isInstance: Any => Boolean, asInstance: Any => Any): Unit = {
      val msg = makeMessage(value, targetClass)
      testNoInline(msg, expected, value, targetClass, isInstance, asInstance)
      testInline(msg, expected, value, targetClass, isInstance, asInstance)
    }

    @inline def testUnit(expected: Boolean, value: Any): Unit =
      test(expected, value, classOf[Unit], _.isInstanceOf[Unit], _.asInstanceOf[Unit])

    @inline def testBoxedUnit(expected: Boolean, value: Any): Unit =
      test(expected, value, classOf[BoxedUnit], _.isInstanceOf[BoxedUnit], _.asInstanceOf[BoxedUnit])

    testUnit(true, ())
    testBoxedUnit(true, ())

    testUnit(false, 12345)
    testBoxedUnit(false, 12345)
  }

  @Test def string(): Unit = {
    @inline def testString(expected: Boolean, value: Any): Unit =
      test(expected, value, classOf[String], _.isInstanceOf[String], _.asInstanceOf[String])

    testString(true, "hello")
    testString(true, "a")

    testString(false, 1)
    testString(false, List(5))

    testNullValue(classOf[String], _.isInstanceOf[String], _.asInstanceOf[String])
  }

  @Test def arrayTypes(): Unit = {
    @inline def testArrayObject(expected: Boolean, value: Any): Unit =
      test(expected, value, classOf[Array[Object]], _.isInstanceOf[Array[Object]],
          _.asInstanceOf[Array[Object]])

    testArrayObject(true, new Array[Object](5))
    testArrayObject(true, new Array[Seq[_]](5))
    testArrayObject(true, new Array[List[_]](5))
    testArrayObject(true, new Array[String](5))
    testArrayObject(true, new Array[Serializable](5))
    testArrayObject(true, new Array[Cloneable](5))

    testArrayObject(true, new Array[Array[Object]](5))
    testArrayObject(true, new Array[Array[Seq[_]]](5))
    testArrayObject(true, new Array[Array[List[_]]](5))
    testArrayObject(true, new Array[Array[Int]](5))

    testArrayObject(false, new Array[Int](5))
    testArrayObject(false, new Array[Long](5))
    testArrayObject(false, List(5))
    testArrayObject(false, new Object)

    testNullValue(classOf[Array[Object]], _.isInstanceOf[Array[Object]], _.asInstanceOf[Array[Object]])

    @inline def testArrayList(expected: Boolean, value: Any): Unit =
      test(expected, value, classOf[Array[List[_]]], _.isInstanceOf[Array[List[_]]],
          _.asInstanceOf[Array[List[_]]])

    testArrayList(true, new Array[List[_]](5))
    testArrayList(true, new Array[::[_]](5))

    testArrayList(false, new Array[Seq[_]](5))
    testArrayList(false, new Array[Object](5))
    testArrayList(false, List(5))
    testArrayList(false, new Object)

    testNullValue(
        classOf[Array[List[_]]], _.isInstanceOf[Array[List[_]]], _.asInstanceOf[Array[List[_]]])

    @inline def testArraySeq(expected: Boolean, value: Any): Unit =
      test(expected, value, classOf[Array[Seq[_]]], _.isInstanceOf[Array[Seq[_]]],
          _.asInstanceOf[Array[Seq[_]]])

    testArraySeq(true, new Array[List[_]](5))
    testArraySeq(true, new Array[Seq[_]](5))
    testArraySeq(true, new Array[IndexedSeq[_]](5))

    testArraySeq(false, new Array[Map[_, _]](5))
    testArraySeq(false, new Array[Object](5))
    testArraySeq(false, List(5))
    testArraySeq(false, new Object)

    testNullValue(classOf[Array[Seq[_]]], _.isInstanceOf[Array[Seq[_]]], _.asInstanceOf[Array[Seq[_]]])

    @inline def testArrayInt(expected: Boolean, value: Any): Unit =
      test(
          expected, value, classOf[Array[Int]], _.isInstanceOf[Array[Int]], _.asInstanceOf[Array[Int]])

    testArrayInt(true, new Array[Int](5))

    testArrayInt(false, new Array[Long](5))
    testArrayInt(false, new Array[Object](5))
    testArrayInt(false, List(5))
    testArrayInt(false, new Object)

    testNullValue(classOf[Array[Int]], _.isInstanceOf[Array[Int]], _.asInstanceOf[Array[Int]])

    @inline def testArrayArrayObject(expected: Boolean, value: Any): Unit = {
      test(expected, value, classOf[Array[Array[Object]]],
          _.isInstanceOf[Array[Array[Object]]], _.asInstanceOf[Array[Array[Object]]])
    }

    testArrayArrayObject(true, new Array[Array[Object]](5))
    testArrayArrayObject(true, new Array[Array[Seq[_]]](5))
    testArrayArrayObject(true, new Array[Array[List[_]]](5))

    testArrayArrayObject(true, new Array[Array[Array[Object]]](5))
    testArrayArrayObject(true, new Array[Array[Array[Seq[_]]]](5))
    testArrayArrayObject(true, new Array[Array[Array[List[_]]]](5))
    testArrayArrayObject(true, new Array[Array[Array[Int]]](5))

    testArrayArrayObject(false, new Array[Array[Int]](5))

    testArrayArrayObject(false, new Array[Object](5))
    testArrayArrayObject(false, new Array[Seq[_]](5))
    testArrayArrayObject(false, new Array[List[_]](5))
    testArrayArrayObject(false, new Array[String](5))
    testArrayArrayObject(false, new Array[Serializable](5))
    testArrayArrayObject(false, new Array[Cloneable](5))

    testArrayArrayObject(false, new Array[Int](5))
    testArrayArrayObject(false, new Array[Long](5))

    testArrayArrayObject(false, List(5))
    testArrayArrayObject(false, new Object)

    testNullValue(classOf[Array[Array[Object]]],
        _.isInstanceOf[Array[Array[Object]]], _.asInstanceOf[Array[Array[Object]]])

    @inline def testArrayArraySeq(expected: Boolean, value: Any): Unit = {
      test(expected, value, classOf[Array[Array[Seq[_]]]],
          _.isInstanceOf[Array[Array[Seq[_]]]], _.asInstanceOf[Array[Array[Seq[_]]]])
    }

    testArrayArraySeq(true, new Array[Array[Seq[_]]](5))
    testArrayArraySeq(true, new Array[Array[List[_]]](5))

    testArrayArraySeq(false, new Array[Array[Object]](5))
    testArrayArraySeq(false, new Array[Seq[_]](5))
    testArrayArraySeq(false, new Array[Array[Array[Seq[_]]]](5))

    testNullValue(classOf[Array[Array[Seq[_]]]],
        _.isInstanceOf[Array[Array[Seq[_]]]], _.asInstanceOf[Array[Array[Seq[_]]]])
  }

  @Test def nothingType(): Unit = {
    // scalac refuses to compile x.isInstanceOf[Nothing], so we fake it
    val isInstance: Any => Boolean = _ => false

    // Force Scala 3 to accept our code
    implicit val ct: ClassTag[Nothing] = ClassTag(classOf[Nothing])

    @inline def testNothing(expected: Boolean, value: Any): Unit =
      test[Nothing](expected, value, classOf[Nothing], isInstance, _.asInstanceOf[Nothing])

    testNothing(false, "a")
    testNothing(false, List(5))

    /* The behavior of casting for null is not consistent, due to how scalac
     * desugars things. This is true both on JS and on the JVM. To test null,
     * we therefore only test the isInstance tests.
     */
    assertFalse(classOf[Nothing].isInstance(null))
  }

  @Test def nullType(): Unit = {
    // scalac refuses to compile x.isInstanceOf[Null], so we fake it
    val isInstance: Any => Boolean = _ => false

    // Force Scala 3 to accept our code
    implicit val ct: ClassTag[Null] = ClassTag(classOf[Null])

    testNullValue(classOf[Null], isInstance, _.asInstanceOf[Null])

    @inline def testNull(expected: Boolean, value: Any): Unit =
      test(expected, value, classOf[Null], isInstance, _.asInstanceOf[Null])

    testNull(false, "a")
    testNull(false, 0)
    testNull(false, false)
    testNull(false, ())
    testNull(false, List(5))
  }

}

object RuntimeTypeTestsTest {

  @noinline
  def assertAlmostSame(msg: String, expected: Any, actual: Any): Unit = {
    /* "Almost" same is like "same", except that we tolerate `equals` for
     * boxed classes, since the instances could be unboxed and reboxed by the
     * monomorphic code in the lambdas.
     *
     * To test whether we are in the situation of a boxed class, we perform
     * a series of `isInstanceOf` on the `expected` value. This appears to be
     * using the very thing we are testing in order to test it, which would be
     * bad. This is fine here because we only use this method to test the
     * *casts*, i.e., `asInstanceOf` and `jl.Class.cast`. The `isInstanceOf` we
     * are using to implement `assertAlmostSame` is tested by other code paths,
     * which do not rely on `assertAlmostSame`.
     */

    expected match {
      case _:Boolean | _:Char | _:Byte | _:Short | _:Int | _:Long | _:Float | _:Double =>
        assertEquals(msg, expected, actual)
      case _ =>
        assertSame(msg, expected, actual)
    }
  }

  private def makeMessage(value: Any, targetClass: Class[_]): String =
    s"$value, $targetClass"

  @noinline
  private def testNoInline[T: ClassTag](msg: String, expected: Boolean,
      value: Any, targetClass: Class[T], isInstance: Any => Boolean,
      asInstance: Any => Any, castAlwaysSucceeds: Boolean): Unit = {
    testInline(msg, expected, value, targetClass, isInstance, asInstance,
        castAlwaysSucceeds)
  }

  @inline
  private def testInline[T: ClassTag](msg: String, expected: Boolean,
      value: Any, targetClass: Class[T], isInstance: Any => Boolean,
      asInstance: Any => Any, castAlwaysSucceeds: Boolean): Unit = {
    assertEquals(msg, expected, isInstance(value))
    assertEquals(msg, expected && !targetClass.isPrimitive(), targetClass.isInstance(value))
    assertEquals(msg, expected, classTag[T].unapply(value).isDefined)

    if (expected || castAlwaysSucceeds) {
      assertAlmostSame(msg, value, asInstance(value))
      if (!targetClass.isPrimitive())
        assertAlmostSame(msg, value, targetClass.cast(value))
    } else {
      if (hasCompliantAsInstanceOfs) {
        assertThrows(classOf[ClassCastException], asInstance(value))
        assertThrows(classOf[ClassCastException], targetClass.cast(value))
      }
    }
  }

  @inline
  def test[T: ClassTag](expected: Boolean, value: Any, targetClass: Class[T],
      isInstance: Any => Boolean, asInstance: Any => Any): Unit = {
    val msg = makeMessage(value, targetClass)
    testNoInline(msg, expected, value, targetClass, isInstance, asInstance,
        castAlwaysSucceeds = false)
    testInline(msg, expected, value, targetClass, isInstance, asInstance,
        castAlwaysSucceeds = false)
  }

  @inline
  def testJS[T: ClassTag](expected: Boolean, value: Any, targetClass: Class[T],
      isInstance: Any => Boolean, asInstance: Any => Any): Unit = {
    val msg = makeMessage(value, targetClass)
    testNoInline(msg, expected, value, targetClass, isInstance, asInstance,
        castAlwaysSucceeds = true)
    testInline(msg, expected, value, targetClass, isInstance, asInstance,
        castAlwaysSucceeds = true)
  }

  @noinline
  private def testNullNoInline[T: ClassTag](msg: String, targetClass: Class[T],
      isInstance: Any => Boolean, asInstance: Any => Any): Unit = {
    testNullInline(msg, targetClass, isInstance, asInstance)
  }

  @inline
  private def testNullInline[T: ClassTag](msg: String, targetClass: Class[T],
      isInstance: Any => Boolean, asInstance: Any => Any): Unit = {
    assertEquals(msg, false, isInstance(null))
    assertEquals(msg, false, targetClass.isInstance(null))

    assertNull(msg, asInstance(null))
    assertNull(msg, targetClass.cast(null))

    assertEquals(msg, None, classTag[T].unapply(null))
  }

  @inline
  def testNullValue[T: ClassTag](targetClass: Class[T],
      isInstance: Any => Boolean, asInstance: Any => Any): Unit = {
    val msg = makeMessage(null, targetClass)
    testNullNoInline(msg, targetClass, isInstance, asInstance)
    testNullInline(msg, targetClass, isInstance, asInstance)
  }

}
