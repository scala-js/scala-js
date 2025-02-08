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

package org.scalajs.testsuite.typedarray

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform._

import scala.scalajs.js
import js.typedarray._

class TypedArrayConversionTest {

  val data = js.Array[Int](-1, 1, 2, 3, 4, 5, 6, 7, 8)

  def sum(factor: Double): Double = (8 * 9 / 2 - 1) * factor

  @Test def convertInt8ArrayToScalaArrayByte(): Unit = {
    val x = new Int8Array(data.map(_.toByte))
    val y = x.toArray

    assertTrue(y.getClass == classOf[scala.Array[Byte]])
    assertEquals(sum(1), y.sum, 0.0)

    // Ensure its a copy
    x(0) = 0
    assertEquals(sum(1), y.sum, 0.0)
  }

  @Test def convertInt16ArrayToScalaArrayShort(): Unit = {
    val x = new Int16Array(data.map(x => (100 * x).toShort))
    val y = x.toArray

    assertTrue(y.getClass == classOf[scala.Array[Short]])
    assertEquals(sum(100), y.sum, 0.0)

    // Ensure its a copy
    x(0) = 0
    assertEquals(sum(100), y.sum, 0.0)
  }

  @Test def convertUint16ArrayToScalaArrayChar(): Unit = {
    val data = js.Array(1, 2, 3, 4, 5, 6).map(x => 10000 * x)
    val sum = (6*7/2*10000).toChar

    val x = new Uint16Array(data)
    val y = x.toArray

    assertTrue(y.getClass == classOf[scala.Array[Char]])
    assertEquals(sum, y.sum)

    // Ensure its a copy
    x(0) = 0
    assertEquals(sum, y.sum)
  }

  @Test def convertInt32ArrayToScalaArrayInt(): Unit = {
    val x = new Int32Array(data.map(x => 10000 * x))
    val y = x.toArray

    assertTrue(y.getClass == classOf[scala.Array[Int]])
    assertEquals(sum(10000), y.sum, 0.0)

    // Ensure its a copy
    x(0) = 0
    assertEquals(sum(10000), y.sum, 0.0)
  }

  @Test def convertFloat32ArrayToScalaArrayFloat(): Unit = {
    val x = new Float32Array(data.map(x => 0.2f * x.toFloat))
    val y = x.toArray

    assertTrue(y.getClass == classOf[scala.Array[Float]])
    assertEquals(sum(0.2), y.sum, 1E-6)

    // Ensure its a copy
    x(0) = 0
    assertEquals(sum(0.2), y.sum, 1E-6)
  }

  @Test def convertFloat64ArrayToScalaArrayDouble(): Unit = {
    val x = new Float64Array(data.map(x => 0.2 * x.toDouble))
    val y = x.toArray

    assertTrue(y.getClass == classOf[scala.Array[Double]])
    assertEquals(sum(0.2), y.sum, 0.0)

    // Ensure its a copy
    x(0) = 0
    assertEquals(sum(0.2), y.sum, 0.0)
  }

  @Test def convertScalaArrayByteToInt8Array(): Unit = {
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

  @Test def convertScalaArrayShortToInt16Array(): Unit = {
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

  @Test def convertScalaArrayCharToUint16Array(): Unit = {
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

  @Test def convertScalaArrayIntToInt32Array(): Unit = {
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

  @Test def convertScalaArrayFloatToFloat32Array(): Unit = {
    val x = Array[Float](1.0f, 2.0f, -2.3f, 5.3f)
    val y = x.toTypedArray

    assertTrue(y.isInstanceOf[Float32Array])
    assertEquals(x.length, y.length)

    for (i <- 0 until y.length)
      assertEquals(x(i), y(i), 0.0)

    // Ensure its a copy
    x(0) = 0
    assertEquals(1.0f, y(0), 0.0)
  }

  @Test def convertScalaArrayDoubleToFloat64Array(): Unit = {
    val x = Array[Double](1.0, 2.0, -2.3, 5.3)
    val y = x.toTypedArray

    assertTrue(y.isInstanceOf[Float64Array])
    assertEquals(x.length, y.length, 0.0)

    for (i <- 0 until y.length)
      assertEquals(x(i), y(i), 0.0)

    // Ensure its a copy
    x(0) = 0
    assertEquals(1.0, y(0), 0.0)
  }

  @Test def convertScalaArrayToTypedArrayNulls(): Unit = {
    assumeTrue("Assuming compliant nullPointers", hasCompliantNullPointers)

    @noinline def assertNPE[U](body: => U): Unit =
      assertThrows(classOf[NullPointerException], body)

    @noinline def nullOf[T >: Null]: T = null

    assertNPE(nullOf[Array[Byte]].toTypedArray)
    assertNPE(nullOf[Array[Short]].toTypedArray)
    assertNPE(nullOf[Array[Char]].toTypedArray)
    assertNPE(nullOf[Array[Int]].toTypedArray)
    assertNPE(nullOf[Array[Float]].toTypedArray)
    assertNPE(nullOf[Array[Double]].toTypedArray)
  }
}
