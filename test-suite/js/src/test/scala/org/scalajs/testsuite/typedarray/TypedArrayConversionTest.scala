/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.typedarray

import org.junit.Assert._
import org.junit.Test
import org.scalajs.testsuite.utils.Requires

import scala.scalajs.js
import js.typedarray._

object TypedArrayConversionTest extends Requires.TypedArray

class TypedArrayConversionTest {

  def data(factor: Double): js.Array[Double] =
    js.Array(-1, 1, 2, 3, 4, 5, 6, 7, 8).map((_: Int) * factor)

  def sum(factor: Double): Double = (8 * 9 / 2 - 1) * factor

  @Test def convert_an_Int8Array_to_a_scala_Array_Byte(): Unit = {
    val x = new Int8Array(data(1))
    val y = x.toArray

    assertTrue(y.getClass == classOf[scala.Array[Byte]])
    assertEquals(sum(1), y.sum)

    // Ensure its a copy
    x(0) = 0
    assertEquals(sum(1), y.sum)
  }

  @Test def convert_an_Int16Array_to_a_scala_Array_Short(): Unit = {
    val x = new Int16Array(data(100))
    val y = x.toArray

    assertTrue(y.getClass == classOf[scala.Array[Short]])
    assertEquals(sum(100), y.sum)

    // Ensure its a copy
    x(0) = 0
    assertEquals(sum(100), y.sum)
  }

  @Test def convert_an_Uint16Array_to_a_scala_Array_Char(): Unit = {
    val data = js.Array((1 to 6).map(_ * 10000): _*)
    val sum = (6*7/2*10000).toChar

    val x = new Uint16Array(data)
    val y = x.toArray

    assertTrue(y.getClass == classOf[scala.Array[Char]])
    assertEquals(sum, y.sum)

    // Ensure its a copy
    x(0) = 0
    assertEquals(sum, y.sum)
  }

  @Test def convert_an_Int32Array_to_a_scala_Array_Int(): Unit = {
    val x = new Int32Array(data(10000))
    val y = x.toArray

    assertTrue(y.getClass == classOf[scala.Array[Int]])
    assertEquals(sum(10000), y.sum)

    // Ensure its a copy
    x(0) = 0
    assertEquals(sum(10000), y.sum)
  }

  @Test def convert_a_Float32Array_to_a_scala_Array_Float(): Unit = {
    val x = new Float32Array(data(0.2))
    val y = x.toArray

    assertTrue(y.getClass == classOf[scala.Array[Float]])
    assertEquals(sum(0.2), y.sum, 1E-6)

    // Ensure its a copy
    x(0) = 0
    assertEquals(sum(0.2), y.sum, 1E-6)
  }

  @Test def convert_a_Float64Array_to_a_scala_Array_Double(): Unit = {
    val x = new Float64Array(data(0.2))
    val y = x.toArray

    assertTrue(y.getClass == classOf[scala.Array[Double]])
    assertEquals(sum(0.2), y.sum)

    // Ensure its a copy
    x(0) = 0
    assertEquals(sum(0.2), y.sum)
  }

  @Test def convert_a_scala_Array_Byte__to_an_Int8Array(): Unit = {
    val x = (Byte.MinValue to Byte.MaxValue).map(_.toByte).toArray
    val y = x.toTypedArray

    assertTrue(y.isInstanceOf[Int8Array])
    assertEquals(x.length, y.length)

    for (i <- 0 until y.length)
      assertEquals(x(i), y(i))

    // Ensure its a copy
    x(0) = 0
    assertEquals(Byte.MinValue, y(0))
  }

  @Test def convert_a_scala_Array_Short__to_an_Int16Array(): Unit = {
    val x = ((Short.MinValue to (Short.MinValue + 1000)) ++
            ((Short.MaxValue - 1000) to Short.MaxValue)).map(_.toShort).toArray
    val y = x.toTypedArray

    assertTrue(y.isInstanceOf[Int16Array])
    assertEquals(x.length, y.length)

    for (i <- 0 until y.length)
      assertEquals(x(i), y(i))

    // Ensure its a copy
    x(0) = 0
    assertEquals(Short.MinValue, y(0))
  }

  @Test def convert_a_scala_Array_Char__to_an_Uint16Array(): Unit = {
    val x = ((Char.MaxValue - 1000) to Char.MaxValue).map(_.toChar).toArray
    val y = x.toTypedArray

    assertTrue(y.isInstanceOf[Uint16Array])
    assertEquals(x.length, y.length)

    for (i <- 0 until y.length)
      assertEquals(x(i).toInt, y(i))

    // Ensure its a copy
    x(0) = 0
    assertEquals(Char.MaxValue - 1000, y(0))
  }

  @Test def convert_a_scala_Array_Int__to_an_Int32Array(): Unit = {
    val x = ((Int.MinValue to (Int.MinValue + 1000)) ++
            ((Int.MaxValue - 1000) to Int.MaxValue)).toArray
    val y = x.toTypedArray

    assertTrue(y.isInstanceOf[Int32Array])
    assertEquals(x.length, y.length)

    for (i <- 0 until y.length)
      assertEquals(x(i), y(i))

    // Ensure its a copy
    x(0) = 0
    assertEquals(Int.MinValue, y(0))
  }

  @Test def convert_a_scala_Array_Float__to_a_Float32Array(): Unit = {
    val x = Array[Float](1.0f, 2.0f, -2.3f, 5.3f)
    val y = x.toTypedArray

    assertTrue(y.isInstanceOf[Float32Array])
    assertEquals(x.length, y.length)

    for (i <- 0 until y.length)
      assertEquals(x(i), y(i))

    // Ensure its a copy
    x(0) = 0
    assertEquals(1.0f, y(0))
  }

  @Test def convert_a_scala_Array_Double__to_a_Float64Array(): Unit = {
    val x = Array[Double](1.0, 2.0, -2.3, 5.3)
    val y = x.toTypedArray

    assertTrue(y.isInstanceOf[Float64Array])
    assertEquals(x.length, y.length)

    for (i <- 0 until y.length)
      assertEquals(x(i), y(i))

    // Ensure its a copy
    x(0) = 0
    assertEquals(1.0, y(0))
  }
}
