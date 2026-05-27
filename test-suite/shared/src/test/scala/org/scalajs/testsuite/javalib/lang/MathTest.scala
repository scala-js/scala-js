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

package org.scalajs.testsuite.javalib.lang

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import java.lang.{Double => JDouble, Math}

// Imported under different names for historical reasons
import org.scalajs.testsuite.utils.AssertExtensions.{assertExactEquals => assertSameDouble}
import org.scalajs.testsuite.utils.AssertExtensions.{assertExactEquals => assertSameFloat}

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform._

import scala.scalajs.LinkingInfo.{linkTimeIf, moduleKind}
import scala.scalajs.LinkingInfo.ModuleKind.MinimalWasmModule

class MathTest {

  @noinline
  private def hideFromOptimizer(x: Int): Int = x

  @Test def absInt(): Unit = {
    assertEquals(0, Math.abs(0))
    assertEquals(156, Math.abs(156))
    assertEquals(156, Math.abs(-156))
    assertEquals(49841354, Math.abs(49841354))
    assertEquals(98433, Math.abs(-98433))
    assertEquals(Int.MaxValue, Math.abs(Int.MaxValue))
    assertEquals(Int.MaxValue, Math.abs(Int.MinValue + 1))
    assertEquals(Int.MinValue, Math.abs(Int.MinValue))
  }

  @Test def absLong(): Unit = {
    assertEquals(0L, Math.abs(0L))
    assertEquals(156L, Math.abs(156L))
    assertEquals(156L, Math.abs(-156L))
    assertEquals(498413546584635135L, Math.abs(498413546584635135L))
    assertEquals(984335433487676L, Math.abs(-984335433487676L))
    assertEquals(Long.MaxValue, Math.abs(Long.MaxValue))
    assertEquals(Long.MaxValue, Math.abs(Long.MinValue + 1L))
    assertEquals(Long.MinValue, Math.abs(Long.MinValue))
  }

  @Test def absFloat(): Unit = {
    assertSameFloat(0.0f, Math.abs(0.0f))
    assertSameFloat(0.0f, Math.abs(-0.0f))
    assertSameFloat(42.156f, Math.abs(42.156f))
    assertSameFloat(42.654f, Math.abs(-42.654f))
    assertSameFloat(Float.PositiveInfinity, Math.abs(Float.PositiveInfinity))
    assertSameFloat(Float.PositiveInfinity, Math.abs(Float.NegativeInfinity))
    assertSameFloat(Float.NaN, Math.abs(Float.NaN))
  }

  @Test def absDouble(): Unit = {
    assertSameDouble(0.0, Math.abs(0.0))
    assertSameDouble(0.0, Math.abs(-0.0))
    assertSameDouble(42.156, Math.abs(42.156))
    assertSameDouble(42.654, Math.abs(-42.654))
    assertSameDouble(Double.PositiveInfinity, Math.abs(Double.PositiveInfinity))
    assertSameDouble(Double.PositiveInfinity, Math.abs(Double.NegativeInfinity))
    assertSameDouble(Double.NaN, Math.abs(Double.NaN))
  }

  @Test def max(): Unit = {
    assertEquals(0, Math.max(0, 0))
    assertEquals(2, Math.max(0, 2))
    assertEquals(2, Math.max(2, 0))
    assertEquals(2.0, Math.max(0.0, 2.0), 0.0)
    assertEquals(2.0, Math.max(2.0, 0.0), 0.0)
    assertTrue(Math.max(0.0, 0.0).equals(0.0))
    assertTrue(Math.max(-0.0, 0.0).equals(0.0))
    assertTrue(Math.max(0.0, -0.0).equals(0.0))
    assertTrue(Math.max(-0.0, -0.0).equals(-0.0))
    assertEquals(Double.PositiveInfinity, Math.max(Double.PositiveInfinity, 0.0), 0.0)
    assertEquals(0.0, Math.max(Double.NegativeInfinity, 0.0), 0.0)
    assertTrue(Math.max(Double.NaN, 0.0).isNaN)
    assertTrue(Math.max(0.0, Double.NaN).isNaN)
    assertEquals(Long.MaxValue, Math.max(Long.MaxValue, 0))
    assertEquals(0L, Math.max(Long.MinValue, 0))
  }

  @Test def min(): Unit = {
    assertEquals(0, Math.min(0, 0))
    assertEquals(0, Math.min(0, 2))
    assertEquals(0, Math.min(2, 0))
    assertEquals(0.0, Math.min(0.0, 2.0), 0.0)
    assertEquals(0.0, Math.min(2.0, 0.0), 0.0)
    assertTrue(Math.min(0.0, 0.0).equals(0.0))
    assertTrue(Math.min(-0.0, 0.0).equals(-0.0))
    assertTrue(Math.min(0.0, -0.0).equals(-0.0))
    assertTrue(Math.min(-0.0, -0.0).equals(-0.0))
    assertEquals(0.0, Math.min(Double.PositiveInfinity, 0.0), 0.0)
    assertEquals(Double.NegativeInfinity, Math.min(Double.NegativeInfinity, 0.0), 0.0)
    assertTrue(Math.min(Double.NaN, 0.0).isNaN)
    assertTrue(Math.min(0.0, Double.NaN).isNaN)
    assertEquals(0L, Math.min(Long.MaxValue, 0))
    assertEquals(Long.MinValue, Math.min(Long.MinValue, 0))
  }

  @Test def floor(): Unit = {
    @noinline def fromBits(bits: Long): Double = JDouble.longBitsToDouble(bits)

    // Basic cases
    assertSameDouble(5.0, Math.floor(5.0))
    assertSameDouble(5.0, Math.floor(5.7))
    assertSameDouble(-6.0, Math.floor(-5.7))
    assertSameDouble(0.0, Math.floor(0.0))
    assertSameDouble(-0.0, Math.floor(-0.0))
    assertSameDouble(0.0, Math.floor(0.5))
    assertSameDouble(-1.0, Math.floor(-0.5))

    // Special values
    assertSameDouble(Double.PositiveInfinity, Math.floor(Double.PositiveInfinity))
    assertSameDouble(Double.NegativeInfinity, Math.floor(Double.NegativeInfinity))
    assertSameDouble(Double.NaN, Math.floor(Double.NaN))

    // Exponent = 19, 2^19 = 524288
    assertSameDouble(524288.0, Math.floor(fromBits(0x4120000000000000L)))
    assertSameDouble(524288.0, Math.floor(fromBits(0x4120000000000001L)))
    assertSameDouble(-524289.0, Math.floor(fromBits(0xc120000000000001L)))

    // Exponent = 20, 2^20 = 1048576
    assertSameDouble(1048576.0, Math.floor(fromBits(0x4130000000000000L)))
    assertSameDouble(1048576.0, Math.floor(fromBits(0x4130000000000001L)))
    assertSameDouble(-1048577.0, Math.floor(fromBits(0xc130000000000001L)))

    // Exponent = 51, 2^51 = 2251799813685248
    assertSameDouble(2251799813685248.0, Math.floor(fromBits(0x4320000000000000L)))
    assertSameDouble(2251799813685248.0, Math.floor(fromBits(0x4320000000000001L)))
    assertSameDouble(-2251799813685249.0, Math.floor(fromBits(0xc320000000000001L)))

    // Exponent = 52. All values are exact integers beyond this point.
    assertSameDouble(4503599627370496.0, Math.floor(fromBits(0x4330000000000000L)))
    assertSameDouble(4503599627370497.0, Math.floor(fromBits(0x4330000000000001L)))
    assertSameDouble(-4503599627370496.0, Math.floor(fromBits(0xc330000000000000L)))
    assertSameDouble(-4503599627370497.0, Math.floor(fromBits(0xc330000000000001L)))
  }

  @Test def ceil(): Unit = {
    @noinline def fromBits(bits: Long): Double = JDouble.longBitsToDouble(bits)

    // Basic cases
    assertSameDouble(5.0, Math.ceil(5.0))
    assertSameDouble(6.0, Math.ceil(5.7))
    assertSameDouble(-5.0, Math.ceil(-5.7))
    assertSameDouble(0.0, Math.ceil(0.0))
    assertSameDouble(-0.0, Math.ceil(-0.0))
    assertSameDouble(1.0, Math.ceil(0.5))
    assertSameDouble(-0.0, Math.ceil(-0.5))

    // Special values
    assertSameDouble(Double.PositiveInfinity, Math.ceil(Double.PositiveInfinity))
    assertSameDouble(Double.NegativeInfinity, Math.ceil(Double.NegativeInfinity))
    assertSameDouble(Double.NaN, Math.ceil(Double.NaN))

    // Exponent = 19, 2^19 = 524288
    assertSameDouble(524288.0, Math.ceil(fromBits(0x4120000000000000L)))
    assertSameDouble(524289.0, Math.ceil(fromBits(0x4120000000000001L)))
    assertSameDouble(-524288.0, Math.ceil(fromBits(0xc120000000000001L)))

    // Exponent = 20, 2^20 = 1048576
    assertSameDouble(1048576.0, Math.ceil(fromBits(0x4130000000000000L)))
    assertSameDouble(1048577.0, Math.ceil(fromBits(0x4130000000000001L)))
    assertSameDouble(-1048576.0, Math.ceil(fromBits(0xc130000000000001L)))

    // Exponent = 51, 2^51 = 2251799813685248
    assertSameDouble(2251799813685248.0, Math.ceil(fromBits(0x4320000000000000L)))
    assertSameDouble(2251799813685249.0, Math.ceil(fromBits(0x4320000000000001L)))
    assertSameDouble(-2251799813685248.0, Math.ceil(fromBits(0xc320000000000001L)))

    // Exponent = 52. All values are exact integers beyond this point.
    assertSameDouble(4503599627370496.0, Math.ceil(fromBits(0x4330000000000000L)))
    assertSameDouble(4503599627370497.0, Math.ceil(fromBits(0x4330000000000001L)))
    assertSameDouble(-4503599627370496.0, Math.ceil(fromBits(0xc330000000000000L)))
    assertSameDouble(-4503599627370497.0, Math.ceil(fromBits(0xc330000000000001L)))
  }

  @Test def cbrt(): Unit = linkTimeIf(moduleKind == MinimalWasmModule) {
    assumeFalse("TODO: Math.cbrt for MinimalWasm", true)
  } {
    assertSameDouble(-0.0, Math.cbrt(-0.0))
    assertSameDouble(0.0, Math.cbrt(0.0))
    assertEquals(3.0, Math.cbrt(27.0), 0.0)
    assertEquals(100.0, Math.cbrt(1000000.0), 0.0)
    assertEquals(1000.0, Math.cbrt(1000000000.0), 0.0)
    assertEquals(-100000000.0, Math.cbrt(-1.0e24), 0.0)
    assertEquals(-4039.0e8, Math.cbrt(-65890311319.0e24), 0.0)
    assertTrue(Math.cbrt(Double.NaN).isNaN)
    assertSameDouble(Double.PositiveInfinity, Math.cbrt(Double.PositiveInfinity))
    assertSameDouble(Double.NegativeInfinity, Math.cbrt(Double.NegativeInfinity))
  }

  @Test def log1p(): Unit = linkTimeIf(moduleKind == MinimalWasmModule) {
    assumeFalse("TODO: Math.log1p for MinimalWasm", true)
  } {
    assertTrue(Math.log1p(-2.0).isNaN)
    assertTrue(Math.log1p(Double.NaN).isNaN)
    assertSameDouble(0.0, Math.log1p(0.0))
    assertSameDouble(-0.0, Math.log1p(-0.0))
    assertTrue(Math.log1p(Double.NaN).isNaN)
    assertSameDouble(Double.PositiveInfinity, Math.log1p(Double.PositiveInfinity))
    assertTrue(Math.log1p(Double.NegativeInfinity).isNaN)
    assertSameDouble(Double.NegativeInfinity, Math.log1p(-1))
  }

  @Test def log10(): Unit = linkTimeIf(moduleKind == MinimalWasmModule) {
    assumeFalse("TODO: Math.log10 for MinimalWasm", true)
  } {
    assertTrue(Math.log10(-230.0).isNaN)
    assertTrue(Math.log10(Double.NaN).isNaN)
    assertSameDouble(Double.NegativeInfinity, Math.log10(0.0))
    assertSameDouble(Double.NegativeInfinity, Math.log10(-0.0))
    assertTrue(Math.log10(Double.NaN).isNaN)
    assertSameDouble(Double.PositiveInfinity, Math.log10(Double.PositiveInfinity))
    assertTrue(Math.log10(Double.NegativeInfinity).isNaN)
  }

  @Test def toDegrees(): Unit = {
    // The precision is not specified for this method
    val epsilon = 1e-14
    assertEquals(57.29577951308232, Math.toDegrees(1.0), epsilon)
    assertEquals(1.0, Math.toDegrees(0.017453292519943295), epsilon)
    assertEquals(360.0, Math.toDegrees(2 * Math.PI), epsilon)
  }

  @Test def toRadians(): Unit = {
    // The precision is not specified for this method
    val epsilon = 1e-14
    assertEquals(0.017453292519943295, Math.toRadians(1.0), epsilon)
    assertEquals(1.0, Math.toRadians(57.29577951308232), epsilon)
    assertEquals(2 * Math.PI, Math.toRadians(360.0), epsilon)
  }

  @Test def signumForDouble(): Unit = {
    assertEquals(1.0, Math.signum(234394.2198273), 0.0)
    assertEquals(-1.0, Math.signum(-124937498.58), 0.0)

    assertSameDouble(0.0, Math.signum(+0.0))
    assertTrue(1 / Math.signum(+0.0) > 0)

    assertSameDouble(-0.0, Math.signum(-0.0))
    assertTrue(1 / Math.signum(-0.0) < 0)

    assertTrue(Math.signum(Double.NaN).isNaN)
  }

  @Test def signumForFloat(): Unit = {
    assertEquals(1.0f, Math.signum(234394.2198273f), 0.0f)
    assertEquals(-1.0f, Math.signum(-124937498.58f), 0.0f)

    assertSameFloat(0.0f, Math.signum(+0.0f))
    assertSameFloat(-0.0f, Math.signum(-0.0f))

    assertTrue(Math.signum(Float.NaN).isNaN)
  }

  @Test def copySignForDouble(): Unit = {
    assertSameDouble(Double.PositiveInfinity, Math.copySign(Double.PositiveInfinity, 1.0))
    assertSameDouble(Double.NegativeInfinity, Math.copySign(Double.PositiveInfinity, -1.0))
    assertSameDouble(-5.0, Math.copySign(5.0, -0.0))
    assertSameDouble(10.5, Math.copySign(-10.5, +0.0))
    assertSameDouble(65.25, Math.copySign(65.25, 52.0))
    assertSameDouble(-65.25, Math.copySign(-65.25, -52.0))
    assertSameDouble(-0.0, Math.copySign(+0.0, -52.0))
    assertSameDouble(Double.NaN, Math.copySign(Double.NaN, -5.0))

    // NaN as sign argument may be considered positive or negative
    val nanSignResult = Math.copySign(-5.0, Double.NaN)
    assertTrue(nanSignResult == 5.0 || nanSignResult == -5.0)
  }

  @Test def copySignForFloat(): Unit = {
    assertSameFloat(Float.PositiveInfinity, Math.copySign(Float.PositiveInfinity, 1.0f))
    assertSameFloat(Float.NegativeInfinity, Math.copySign(Float.PositiveInfinity, -1.0f))
    assertSameFloat(-5.0f, Math.copySign(5.0f, -0.0f))
    assertSameFloat(10.5f, Math.copySign(-10.5f, +0.0f))
    assertSameFloat(65.25f, Math.copySign(65.25f, 52.0f))
    assertSameFloat(-65.25f, Math.copySign(-65.25f, -52.0f))
    assertSameFloat(-0.0f, Math.copySign(+0.0f, -52.0f))
    assertSameFloat(Float.NaN, Math.copySign(Float.NaN, -5.0f))

    // NaN as sign argument may be considered positive or negative
    val nanSignResult = Math.copySign(-5.0f, Float.NaN)
    assertTrue(nanSignResult == 5.0f || nanSignResult == -5.0f)
  }

  @Test def getExponentForFloat(): Unit = {
    // Specials
    assertEquals(-127, Math.getExponent(0.0f))
    assertEquals(-127, Math.getExponent(-0.0f))
    assertEquals(128, Math.getExponent(Float.PositiveInfinity))
    assertEquals(128, Math.getExponent(Float.NegativeInfinity))
    assertEquals(128, Math.getExponent(Float.NaN))

    // Corner cases
    val MinNormal = java.lang.Float.MIN_NORMAL
    val MaxSubnormal = 1.1754942e-38f
    assertEquals(127, Math.getExponent(Float.MaxValue))
    assertEquals(127, Math.getExponent(Float.MinValue))
    assertEquals(-127, Math.getExponent(-Float.MinPositiveValue))
    assertEquals(-127, Math.getExponent(MaxSubnormal))
    assertEquals(-126, Math.getExponent(-MinNormal))

    // Some regular values
    assertEquals(53, Math.getExponent(9007199300000000.0f))
    assertEquals(0, Math.getExponent(1.0f))
  }

  @Test def getExponentForDouble(): Unit = {
    // Specials
    assertEquals(-1023, Math.getExponent(0.0))
    assertEquals(-1023, Math.getExponent(-0.0))
    assertEquals(1024, Math.getExponent(Double.PositiveInfinity))
    assertEquals(1024, Math.getExponent(Double.NegativeInfinity))
    assertEquals(1024, Math.getExponent(Double.NaN))

    // Corner cases
    val MinNormal = java.lang.Double.MIN_NORMAL
    val MaxSubnormal = 2.225073858507201e-308
    assertEquals(1023, Math.getExponent(Double.MaxValue))
    assertEquals(1023, Math.getExponent(Double.MinValue))
    assertEquals(-1023, Math.getExponent(-Double.MinPositiveValue))
    assertEquals(-1023, Math.getExponent(MaxSubnormal))
    assertEquals(-1022, Math.getExponent(-MinNormal))

    // Some regular values
    assertEquals(52, Math.getExponent(9007199254740991.0))
    assertEquals(0, Math.getExponent(1.0))
  }

  @Test def nextUpForDouble(): Unit = {
    // Specials
    assertSameDouble(Double.MinPositiveValue, Math.nextUp(0.0))
    assertSameDouble(Double.MinPositiveValue, Math.nextUp(-0.0))
    assertSameDouble(Double.PositiveInfinity, Math.nextUp(Double.PositiveInfinity))
    assertSameDouble(Double.MinValue, Math.nextUp(Double.NegativeInfinity))
    assertSameDouble(Double.NaN, Math.nextUp(Double.NaN))

    // Corner cases
    val MinNormal = java.lang.Double.MIN_NORMAL
    val MaxSubnormal = 2.225073858507201e-308
    assertSameDouble(Double.PositiveInfinity, Math.nextUp(Double.MaxValue))
    assertSameDouble(-1.7976931348623155e+308, Math.nextUp(Double.MinValue))
    assertSameDouble(-0.0, Math.nextUp(-Double.MinPositiveValue))
    assertSameDouble(MinNormal, Math.nextUp(MaxSubnormal))
    assertSameDouble(-MaxSubnormal, Math.nextUp(-MinNormal))

    // Try very hard to produce non-canonical NaN's that are corner cases
    @noinline def fromBits(bits: Long): Double = java.lang.Double.longBitsToDouble(bits)

    for (bits <- List(Long.MaxValue, -1L, 0x7ff0000000000001L, 0xfff0000000000001L))
      assertSameDouble(Double.NaN, Math.nextUp(fromBits(bits)))

    // Random values
    assertSameDouble(9007199254740992.0, Math.nextUp(9007199254740991.0))
    assertSameDouble(9007199254740994.0, Math.nextUp(9007199254740992.0))
    assertSameDouble(1.0000000000000002, Math.nextUp(1.0))
  }

  @Test def nextUpForFloat(): Unit = {
    // Specials
    assertSameFloat(Float.MinPositiveValue, Math.nextUp(0.0f))
    assertSameFloat(Float.MinPositiveValue, Math.nextUp(-0.0f))
    assertSameFloat(Float.PositiveInfinity, Math.nextUp(Float.PositiveInfinity))
    assertSameFloat(Float.MinValue, Math.nextUp(Float.NegativeInfinity))
    assertSameFloat(Float.NaN, Math.nextUp(Float.NaN))

    // Corner cases
    val MinNormal = java.lang.Float.MIN_NORMAL
    val MaxSubnormal = 1.1754942e-38f
    assertSameFloat(Float.PositiveInfinity, Math.nextUp(Float.MaxValue))
    assertSameFloat(-3.4028233e38f, Math.nextUp(Float.MinValue))
    assertSameFloat(-0.0f, Math.nextUp(-Float.MinPositiveValue))
    assertSameFloat(MinNormal, Math.nextUp(MaxSubnormal))
    assertSameFloat(-MaxSubnormal, Math.nextUp(-MinNormal))

    // Try very hard to produce non-canonical NaN's that are corner cases
    @noinline def fromBits(bits: Int): Float = java.lang.Float.intBitsToFloat(bits)

    for (bits <- List(Int.MaxValue, -1, 0x7f800001, 0xff800001))
      assertSameFloat(Float.NaN, Math.nextUp(fromBits(bits)))

    // Random values
    assertSameFloat(9007200300000000.0f, Math.nextUp(9007199300000000.0f))
    assertSameFloat(1.0000001f, Math.nextUp(1.0f))
  }

  @Test def nextAfterForDouble(): Unit = {
    assertSameDouble(Double.NaN, Math.nextAfter(Double.NaN, Double.NaN))
    assertSameDouble(Double.NaN, Math.nextAfter(1.0, Double.NaN))
    assertSameDouble(Double.NaN, Math.nextAfter(Double.NaN, 1.0))

    assertSameDouble(0.0, Math.nextAfter(0.0, 0.0))
    assertSameDouble(-0.0, Math.nextAfter(0.0, -0.0))
    assertSameDouble(0.0, Math.nextAfter(-0.0, 0.0))
    assertSameDouble(-0.0, Math.nextAfter(-0.0, -0.0))

    assertSameDouble(Double.PositiveInfinity,
        Math.nextAfter(Double.PositiveInfinity, Double.PositiveInfinity))
    assertSameDouble(Double.NegativeInfinity,
        Math.nextAfter(Double.NegativeInfinity, Double.NegativeInfinity))

    assertSameDouble(Double.NegativeInfinity,
        Math.nextAfter(Double.MinValue, Double.NegativeInfinity))
    assertSameDouble(Double.PositiveInfinity,
        Math.nextAfter(-Double.MinValue, Double.PositiveInfinity))
    assertSameDouble(Double.MaxValue,
        Math.nextAfter(Double.PositiveInfinity, Double.NegativeInfinity))
    assertSameDouble(Double.MinValue,
        Math.nextAfter(Double.NegativeInfinity, Double.PositiveInfinity))
    assertSameDouble(Double.PositiveInfinity,
        Math.nextAfter(Double.MaxValue, Double.PositiveInfinity))
    assertSameDouble(Double.NegativeInfinity,
        Math.nextAfter(-Double.MaxValue, Double.NegativeInfinity))

    assertSameDouble(1.0, Math.nextAfter(1.0, 1.0))
    assertSameDouble(1.0000000000000002, Math.nextAfter(1.0, 2.0))
    assertSameDouble(0.9999999999999999, Math.nextAfter(1.0, 0.5))
  }

  @Test def nextAfterForFloat(): Unit = {
    assertSameFloat(Float.NaN, Math.nextAfter(Float.NaN, Double.NaN))
    assertSameFloat(Float.NaN, Math.nextAfter(1.0f, Double.NaN))
    assertSameFloat(Float.NaN, Math.nextAfter(Float.NaN, 1.0))

    assertSameFloat(0.0f, Math.nextAfter(0.0f, 0.0))
    assertSameFloat(-0.0f, Math.nextAfter(0.0f, -0.0))
    assertSameFloat(0.0f, Math.nextAfter(-0.0f, 0.0))
    assertSameFloat(-0.0f, Math.nextAfter(-0.0f, -0.0))

    assertSameFloat(Float.PositiveInfinity,
        Math.nextAfter(Float.PositiveInfinity, Double.PositiveInfinity))
    assertSameFloat(Float.NegativeInfinity,
        Math.nextAfter(Float.NegativeInfinity, Double.NegativeInfinity))

    assertSameFloat(Float.NegativeInfinity,
        Math.nextAfter(Float.MinValue, Double.NegativeInfinity))
    assertSameFloat(Float.PositiveInfinity,
        Math.nextAfter(-Float.MinValue, Double.PositiveInfinity))
    assertSameFloat(Float.MaxValue,
        Math.nextAfter(Float.PositiveInfinity, Double.NegativeInfinity))
    assertSameFloat(Float.MinValue,
        Math.nextAfter(Float.NegativeInfinity, Double.PositiveInfinity))
    assertSameFloat(Float.PositiveInfinity,
        Math.nextAfter(Float.MaxValue, Double.PositiveInfinity))
    assertSameFloat(Float.NegativeInfinity,
        Math.nextAfter(-Float.MaxValue, Double.NegativeInfinity))

    assertSameFloat(1.0f, Math.nextAfter(1.0f, 1.0))
    assertSameFloat(1.0000001f, Math.nextAfter(1.0f, 2.0))
    assertSameFloat(0.99999994f, Math.nextAfter(1.0f, 0.5))
  }

  @Test def ulpForDouble(): Unit = {
    @noinline
    def test(expected: Double, value: Double): Unit =
      assertSameDouble(s"for value $value", expected, Math.ulp(value))

    // Specials

    test(Double.MinPositiveValue, 0.0)
    test(Double.MinPositiveValue, -0.0)
    test(Double.NaN, Double.NaN)
    test(Double.PositiveInfinity, Double.PositiveInfinity)
    test(Double.PositiveInfinity, Double.NegativeInfinity)

    // Other corner cases

    test(Double.MinPositiveValue, Double.MinPositiveValue)
    test(Double.MinPositiveValue, -Double.MinPositiveValue)
    test(Double.MinPositiveValue, 2.2250738585072009e-308) // max subnormal value
    test(Double.MinPositiveValue, -2.2250738585072009e-308)
    test(Double.MinPositiveValue, 2.2250738585072014e-308) // min normal value
    test(Double.MinPositiveValue, -2.2250738585072014e-308)
    test(Double.MinPositiveValue, 4.4501477170144023e-308) // max value with MinPosValue result
    test(Double.MinPositiveValue, -4.4501477170144023e-308)
    test(1.0e-323, 4.450147717014403e-308) // min value with non-MinPosValue result
    test(1.0e-323, -4.450147717014403e-308)
    test(1.9958403095347198e292, Double.MaxValue)
    test(1.9958403095347198e292, -Double.MaxValue)

    // Some normal values

    test(4.440892098500626e-16, 3.4)
    test(4.440892098500626e-16, -3.4)
    test(4.1718496795330275e93, 3.423e109)
    test(4.1718496795330275e93, -3.423e109)

    // Some subnormal values

    test(Double.MinPositiveValue, 3.4e-317)
    test(Double.MinPositiveValue, -3.4e-317)
    test(Double.MinPositiveValue, 3.423e-319)
    test(Double.MinPositiveValue, -3.423e-319)
  }

  @Test def ulpForFloat(): Unit = {
    @noinline
    def test(expected: Float, value: Float): Unit =
      assertSameFloat(s"for value $value", expected, Math.ulp(value))

    // Specials

    test(Float.MinPositiveValue, 0.0f)
    test(Float.MinPositiveValue, -0.0f)
    test(Float.NaN, Float.NaN)
    test(Float.PositiveInfinity, Float.PositiveInfinity)
    test(Float.PositiveInfinity, Float.NegativeInfinity)

    // Other corner cases

    test(Float.MinPositiveValue, Float.MinPositiveValue)
    test(Float.MinPositiveValue, -Float.MinPositiveValue)
    test(Float.MinPositiveValue, 1.1754942e-38f) // max subnormal value
    test(Float.MinPositiveValue, -1.1754942e-38f)
    test(Float.MinPositiveValue, 1.17549435e-38f) // min normal value
    test(Float.MinPositiveValue, -1.17549435e-38f)
    test(Float.MinPositiveValue, 2.3509886e-38f) // max value with MinPosValue result
    test(Float.MinPositiveValue, -2.3509886e-38f)
    test(2.8e-45f, 2.3509887e-38f) // min value with non-MinPosValue result
    test(2.8e-45f, -2.3509887e-38f)
    test(2.028241e31f, Float.MaxValue)
    test(2.028241e31f, -Float.MaxValue)

    // Some normal values

    test(2.3841858e-7f, 3.4f)
    test(2.3841858e-7f, -3.4f)
    test(3.1691265e29f, 3.423e36f)
    test(3.1691265e29f, -3.423e36f)

    // Some subnormal values

    test(Float.MinPositiveValue, 3.4e-40f)
    test(Float.MinPositiveValue, -3.4e-40f)
    test(Float.MinPositiveValue, 3.42e-43f)
    test(Float.MinPositiveValue, -3.42e-43f)
  }

  @Test def hypot(): Unit = linkTimeIf(moduleKind == MinimalWasmModule) {
    assumeFalse("TODO: Math.hypot for MinimalWasm", true)
  } {
    assertEquals(0.0, Math.hypot(0.0, 0.0), 0.01)
    assertEquals(5.0, Math.hypot(3.0, 4.0), 0.01)
    assertTrue(Math.hypot(3.0, Double.NaN).isNaN)
    assertTrue(Math.hypot(Double.NaN, 3.0).isNaN)
    assertEquals(Double.PositiveInfinity, Math.hypot(Double.NegativeInfinity, 4.0), 0.0)
    assertEquals(Double.PositiveInfinity, Math.hypot(4.0, Double.NegativeInfinity), 0.0)
    assertEquals(Double.PositiveInfinity, Math.hypot(Double.PositiveInfinity, 4.0), 0.0)
    assertEquals(Double.PositiveInfinity, Math.hypot(4.0, Double.PositiveInfinity), 0.0)
    assertSameDouble(0.0, Math.hypot(-0.0, -0.0))
    assertSameDouble(0.0, Math.hypot(0.0, -0.0))
    assertSameDouble(0.0, Math.hypot(-0.0, 0.0))
  }

  @Test def expm1(): Unit = linkTimeIf(moduleKind == MinimalWasmModule) {
    assumeFalse("TODO: Math.expm1 for MinimalWasm", true)
  } {
    assertTrue(1 / Math.expm1(-0.0) < 0)
    assertTrue(1 / Math.expm1(0.0) > 0)
    assertSameDouble(-0.0, Math.expm1(-0.0))
    assertSameDouble(0.0, Math.expm1(0.0))
    assertEquals(19.085536923187668, Math.expm1(3.0), 0.01)
    assertEquals(3269016.3724721107, Math.expm1(15.0), 0.01)
    assertEquals(Double.PositiveInfinity, Math.expm1(1.8e10), 0.0)
    assertEquals(Double.PositiveInfinity, Math.expm1(Double.PositiveInfinity), 0.0)
    assertEquals(-1.0, Math.expm1(Double.NegativeInfinity), 0.01)
    assertEquals(4.9e-324, Math.expm1(4.9e-324), 0.01)
    assertTrue(Math.expm1(Double.NaN).isNaN)
  }

  @Test def sinh(): Unit = linkTimeIf(moduleKind == MinimalWasmModule) {
    assumeFalse("TODO: Math.sinh for MinimalWasm", true)
  } {
    assertEquals(Double.NegativeInfinity, Math.sinh(-1234.56), 0.0)
    assertEquals(Double.PositiveInfinity, Math.sinh(1234.56), 0.0)
    assertSameDouble(0.0, Math.sinh(0.0))
    assertSameDouble(-0.0, Math.sinh(-0.0))
    assertEquals(Double.PositiveInfinity, Math.sinh(Double.PositiveInfinity), 0.0)
    assertEquals(Double.NegativeInfinity, Math.sinh(Double.NegativeInfinity), 0.0)
    assertTrue(Math.sinh(Double.NaN).isNaN)
  }

  @Test def cosh(): Unit = linkTimeIf(moduleKind == MinimalWasmModule) {
    assumeFalse("TODO: Math.cosh for MinimalWasm", true)
  } {
    assertEquals(Double.PositiveInfinity, Math.cosh(-1234.56), 0.0)
    assertEquals(Double.PositiveInfinity, Math.cosh(1234.56), 0.0)
    assertEquals(1.0, Math.cosh(-0.0), 0.01)
    assertEquals(1.0, Math.cosh(0.0), 0.01)
    assertEquals(Double.PositiveInfinity, Math.cosh(Double.PositiveInfinity), 0.0)
    assertEquals(Double.PositiveInfinity, Math.cosh(Double.NegativeInfinity), 0.0)
    assertTrue(Math.cosh(Double.NaN).isNaN)
  }

  @Test def tanh(): Unit = linkTimeIf(moduleKind == MinimalWasmModule) {
    assumeFalse("TODO: Math.tanh for MinimalWasm", true)
  } {
    assertEquals(-1.0, Math.tanh(-1234.56), 0.01)
    assertEquals(-1.0, Math.tanh(-120.56), 0.01)
    assertEquals(1.0, Math.tanh(1234.56), 0.01)
    assertSameDouble(0.0, Math.tanh(0.0))
    assertSameDouble(-0.0, Math.tanh(-0.0))
    assertEquals(1.0, Math.tanh(Double.PositiveInfinity), 0.01)
    assertEquals(-1.0, Math.tanh(Double.NegativeInfinity), 0.01)
    assertTrue(Math.tanh(Double.NaN).isNaN)
  }

  @Test def rintForDouble(): Unit = {
    import Math.rint

    val intLimit = (1L << 52).toDouble
    val halfIntLimit = (1L << 51).toDouble
    val doubleIntLimit = (1L << 53).toDouble

    // Specials
    assertSameDouble(+0.0, rint(+0.0))
    assertSameDouble(-0.0, rint(-0.0))
    assertSameDouble(Double.PositiveInfinity, rint(Double.PositiveInfinity))
    assertSameDouble(Double.NegativeInfinity, rint(Double.NegativeInfinity))
    assertSameDouble(Double.NaN, rint(Double.NaN))

    // Positive values
    assertSameDouble(+0.0, rint(Double.MinPositiveValue))
    assertSameDouble(+0.0, rint(java.lang.Double.MIN_NORMAL))
    assertSameDouble(+0.0, rint(0.1))
    assertSameDouble(+0.0, rint(0.5))
    assertSameDouble(1.0, rint(0.5000000000000001))
    assertSameDouble(1.0, rint(0.999))
    assertSameDouble(1.0, rint(1.4999999999999998))
    assertSameDouble(2.0, rint(1.5))
    assertSameDouble(2.0, rint(2.0))
    assertSameDouble(2.0, rint(2.1))
    assertSameDouble(2.0, rint(2.5))
    assertSameDouble(3.0, rint(2.75))
    assertSameDouble(3.0, rint(3.25))
    assertSameDouble(4.0, rint(3.5))
    assertSameDouble(4.0, rint(3.75))
    assertSameDouble(halfIntLimit - 2.0, rint(halfIntLimit - 1.5))
    assertSameDouble(halfIntLimit - 1.0, rint(halfIntLimit - 1.25))
    assertSameDouble(halfIntLimit - 1.0, rint(halfIntLimit - 1.0))
    assertSameDouble(halfIntLimit - 1.0, rint(halfIntLimit - 0.75))
    assertSameDouble(halfIntLimit, rint(halfIntLimit - 0.5))
    assertSameDouble(halfIntLimit, rint(halfIntLimit - 0.25))
    assertSameDouble(halfIntLimit, rint(halfIntLimit))
    assertSameDouble(halfIntLimit, rint(halfIntLimit + 0.25))
    assertSameDouble(halfIntLimit, rint(halfIntLimit + 0.5))
    assertSameDouble(halfIntLimit + 1.0, rint(halfIntLimit + 0.75))
    assertSameDouble(halfIntLimit + 1.0, rint(halfIntLimit + 1.0))
    assertSameDouble(halfIntLimit + 1.0, rint(halfIntLimit + 1.25))
    assertSameDouble(halfIntLimit + 2.0, rint(halfIntLimit + 1.5))
    assertSameDouble(intLimit - 2.0, rint(intLimit - 1.5))
    assertSameDouble(intLimit - 1.0, rint(intLimit - 1.0))
    assertSameDouble(intLimit, rint(intLimit - 0.5))
    assertSameDouble(intLimit, rint(intLimit))

    val largeIntegers = List(
      // corner cases just above intLimit
      intLimit + 1.0,
      intLimit + 2.0,
      intLimit + 3.0,
      intLimit + 4.0,
      // corner cases around doubleIntLimit
      doubleIntLimit - 4.0,
      doubleIntLimit - 3.0,
      doubleIntLimit - 2.0,
      doubleIntLimit - 1.0,
      doubleIntLimit,
      doubleIntLimit + 2.0,
      doubleIntLimit + 4.0,
      doubleIntLimit + 6.0,
      doubleIntLimit + 8.0,
      doubleIntLimit + 16.0,
      Double.MaxValue
    )
    for (x <- largeIntegers)
      assertSameDouble(x, rint(x))

    // Negative values
    assertSameDouble(-0.0, rint(-Double.MinPositiveValue))
    assertSameDouble(-0.0, rint(-java.lang.Double.MIN_NORMAL))
    assertSameDouble(-0.0, rint(-0.1))
    assertSameDouble(-0.0, rint(-0.5))
    assertSameDouble(-1.0, rint(-0.5000000000000001))
    assertSameDouble(-1.0, rint(-0.999))
    assertSameDouble(-1.0, rint(-1.4999999999999998))
    assertSameDouble(-2.0, rint(-1.5))
    assertSameDouble(-2.0, rint(-2.0))
    assertSameDouble(-2.0, rint(-2.1))
    assertSameDouble(-2.0, rint(-2.5))
    assertSameDouble(-3.0, rint(-2.75))
    assertSameDouble(-3.0, rint(-3.25))
    assertSameDouble(-4.0, rint(-3.5))
    assertSameDouble(-4.0, rint(-3.75))
    assertSameDouble(-(halfIntLimit - 2.0), rint(-(halfIntLimit - 1.5)))
    assertSameDouble(-(halfIntLimit - 1.0), rint(-(halfIntLimit - 1.25)))
    assertSameDouble(-(halfIntLimit - 1.0), rint(-(halfIntLimit - 1.0)))
    assertSameDouble(-(halfIntLimit - 1.0), rint(-(halfIntLimit - 0.75)))
    assertSameDouble(-halfIntLimit, rint(-(halfIntLimit - 0.5)))
    assertSameDouble(-halfIntLimit, rint(-(halfIntLimit - 0.25)))
    assertSameDouble(-halfIntLimit, rint(-halfIntLimit))
    assertSameDouble(-halfIntLimit, rint(-(halfIntLimit + 0.25)))
    assertSameDouble(-halfIntLimit, rint(-(halfIntLimit + 0.5)))
    assertSameDouble(-(halfIntLimit + 1.0), rint(-(halfIntLimit + 0.75)))
    assertSameDouble(-(halfIntLimit + 1.0), rint(-(halfIntLimit + 1.0)))
    assertSameDouble(-(halfIntLimit + 1.0), rint(-(halfIntLimit + 1.25)))
    assertSameDouble(-(halfIntLimit + 2.0), rint(-(halfIntLimit + 1.5)))
    assertSameDouble(-(intLimit - 2.0), rint(-(intLimit - 1.5)))
    assertSameDouble(-(intLimit - 1.0), rint(-(intLimit - 1.0)))
    assertSameDouble(-intLimit, rint(-(intLimit - 0.5)))
    assertSameDouble(-intLimit, rint(-intLimit))

    for (x <- largeIntegers)
      assertSameDouble(-x, rint(-x))
  }

  @Test def addExact(): Unit = {
    assertEquals(0, Math.addExact(0, 0))
    assertEquals(1, Math.addExact(0, 1))
    assertEquals(1, Math.addExact(1, 0))
    assertEquals(-1, Math.addExact(0, -1))
    assertEquals(-1, Math.addExact(-1, 0))
    assertEquals(0, Math.addExact(1, -1))
    assertEquals(0, Math.addExact(-1, 1))
    assertEquals(Int.MinValue, Math.addExact(Int.MinValue, 0))
    assertEquals(Int.MinValue, Math.addExact(0, Int.MinValue))
    assertEquals(-2147483647, Math.addExact(Int.MinValue, 1))
    assertEquals(-2147483647, Math.addExact(1, Int.MinValue))
    assertEquals(Int.MinValue, Math.addExact(-1, -2147483647))
    assertEquals(Int.MinValue, Math.addExact(-2147483647, -1))
    assertEquals(Int.MaxValue, Math.addExact(Int.MaxValue, 0))
    assertEquals(Int.MaxValue, Math.addExact(0, Int.MaxValue))
    assertEquals(2147483646, Math.addExact(Int.MaxValue, -1))
    assertEquals(2147483646, Math.addExact(-1, Int.MaxValue))
    assertEquals(Int.MaxValue, Math.addExact(2147483646, 1))
    assertEquals(Int.MaxValue, Math.addExact(1, 2147483646))
    assertEquals(Int.MinValue, Math.addExact(-1073741824, -1073741824))

    assertThrows(classOf[ArithmeticException], Math.addExact(Int.MinValue, -1))
    assertThrows(classOf[ArithmeticException], Math.addExact(-1, Int.MinValue))
    assertThrows(classOf[ArithmeticException], Math.addExact(Int.MinValue, Int.MinValue))
    assertThrows(classOf[ArithmeticException], Math.addExact(Int.MaxValue, 1))
    assertThrows(classOf[ArithmeticException], Math.addExact(1, Int.MaxValue))
    assertThrows(classOf[ArithmeticException], Math.addExact(Int.MaxValue, Int.MaxValue))
    assertThrows(classOf[ArithmeticException], Math.addExact(1073741824, 1073741824))

    assertEquals(0L, Math.addExact(0L, 0L))
    assertEquals(1L, Math.addExact(0L, 1L))
    assertEquals(1L, Math.addExact(1L, 0L))
    assertEquals(-1L, Math.addExact(0L, -1L))
    assertEquals(-1L, Math.addExact(-1L, 0L))
    assertEquals(0L, Math.addExact(1L, -1L))
    assertEquals(0L, Math.addExact(-1L, 1L))
    assertEquals(Long.MinValue, Math.addExact(Long.MinValue, 0))
    assertEquals(Long.MinValue, Math.addExact(0, Long.MinValue))
    assertEquals(-9223372036854775807L, Math.addExact(Long.MinValue, 1))
    assertEquals(-9223372036854775807L, Math.addExact(1, Long.MinValue))
    assertEquals(Long.MinValue, Math.addExact(-9223372036854775807L, -1))
    assertEquals(Long.MinValue, Math.addExact(-1, -9223372036854775807L))
    assertEquals(Long.MaxValue, Math.addExact(Long.MaxValue, 0))
    assertEquals(Long.MaxValue, Math.addExact(0, Long.MaxValue))
    assertEquals(9223372036854775806L, Math.addExact(Long.MaxValue, -1))
    assertEquals(9223372036854775806L, Math.addExact(-1, Long.MaxValue))
    assertEquals(Long.MaxValue, Math.addExact(9223372036854775806L, 1))
    assertEquals(Long.MaxValue, Math.addExact(1, 9223372036854775806L))
    assertEquals(Long.MinValue, Math.addExact(-4611686018427387904L, -4611686018427387904L))

    assertThrows(classOf[ArithmeticException], Math.addExact(Long.MinValue, -1))
    assertThrows(classOf[ArithmeticException], Math.addExact(-1, Long.MinValue))
    assertThrows(classOf[ArithmeticException], Math.addExact(Long.MinValue, Long.MinValue))
    assertThrows(classOf[ArithmeticException], Math.addExact(Long.MaxValue, 1))
    assertThrows(classOf[ArithmeticException], Math.addExact(1, Long.MaxValue))
    assertThrows(classOf[ArithmeticException], Math.addExact(Long.MaxValue, Long.MaxValue))
    assertThrows(
        classOf[ArithmeticException], Math.addExact(4611686018427387904L, 4611686018427387904L))
  }

  @Test def subtractExact(): Unit = {
    assertEquals(0, Math.subtractExact(0, 0))
    assertEquals(1, Math.subtractExact(1, 0))
    assertEquals(-1, Math.subtractExact(0, 1))
    assertEquals(0, Math.subtractExact(1, 1))
    assertEquals(1, Math.subtractExact(0, -1))
    assertEquals(-1, Math.subtractExact(-1, 0))
    assertEquals(0, Math.subtractExact(-1, -1))
    assertEquals(Int.MinValue, Math.subtractExact(Int.MinValue, 0))
    assertEquals(Int.MaxValue, Math.subtractExact(Int.MaxValue, 0))
    assertEquals(-2147483647, Math.subtractExact(Int.MinValue, -1))
    assertEquals(2147483646, Math.subtractExact(Int.MaxValue, 1))
    assertEquals(Int.MinValue, Math.subtractExact(-1, Int.MaxValue))
    assertEquals(Int.MaxValue, Math.subtractExact(0, -Int.MaxValue))
    assertEquals(-2147483647, Math.subtractExact(0, Int.MaxValue))
    assertEquals(Int.MaxValue, Math.subtractExact(-1, Int.MinValue))
    assertEquals(Int.MinValue, Math.subtractExact(-1073741824, 1073741824))
    assertThrows(classOf[ArithmeticException], Math.subtractExact(0, Int.MinValue))
    assertThrows(classOf[ArithmeticException], Math.subtractExact(Int.MinValue, 1))
    assertThrows(classOf[ArithmeticException], Math.subtractExact(Int.MinValue, Int.MaxValue))
    assertThrows(classOf[ArithmeticException], Math.subtractExact(-2, Int.MaxValue))
    assertThrows(classOf[ArithmeticException], Math.subtractExact(Int.MaxValue, -1))
    assertThrows(classOf[ArithmeticException], Math.subtractExact(Int.MaxValue, Int.MinValue))
    assertThrows(classOf[ArithmeticException], Math.subtractExact(1073741824, -1073741824))

    assertEquals(0L, Math.subtractExact(0L, 0L))
    assertEquals(1L, Math.subtractExact(1L, 0L))
    assertEquals(-1L, Math.subtractExact(0L, 1L))
    assertEquals(0L, Math.subtractExact(1L, 1L))
    assertEquals(1L, Math.subtractExact(0L, -1L))
    assertEquals(-1L, Math.subtractExact(-1L, 0L))
    assertEquals(0L, Math.subtractExact(-1L, -1L))
    assertEquals(Long.MinValue, Math.subtractExact(Long.MinValue, 0))
    assertEquals(Long.MaxValue, Math.subtractExact(Long.MaxValue, 0))
    assertEquals(-9223372036854775807L, Math.subtractExact(Long.MinValue, -1))
    assertEquals(9223372036854775806L, Math.subtractExact(Long.MaxValue, 1))
    assertEquals(Long.MinValue, Math.subtractExact(-1, Long.MaxValue))
    assertEquals(Long.MaxValue, Math.subtractExact(0, -Long.MaxValue))
    assertEquals(-9223372036854775807L, Math.subtractExact(0, Long.MaxValue))
    assertEquals(Long.MaxValue, Math.subtractExact(-1, Long.MinValue))
    assertEquals(Long.MinValue, Math.subtractExact(-4611686018427387904L, 4611686018427387904L))

    assertThrows(classOf[ArithmeticException], Math.subtractExact(0, Long.MinValue))
    assertThrows(classOf[ArithmeticException], Math.subtractExact(Long.MinValue, 1))
    assertThrows(classOf[ArithmeticException], Math.subtractExact(Long.MinValue, Long.MaxValue))
    assertThrows(classOf[ArithmeticException], Math.subtractExact(Long.MinValue, 1))
    assertThrows(classOf[ArithmeticException], Math.subtractExact(-2, Long.MaxValue))
    assertThrows(classOf[ArithmeticException], Math.subtractExact(Long.MaxValue, -1))
    assertThrows(classOf[ArithmeticException], Math.subtractExact(Long.MaxValue, Long.MinValue))
    assertThrows(
        classOf[ArithmeticException], Math.subtractExact(4611686018427387904L, -4611686018427387904L))
  }

  @Test def multiplyExactIntInt(): Unit = {
    for (n <- Seq(Int.MinValue, -1, 0, 1, Int.MaxValue)) {
      assertEquals(0, Math.multiplyExact(n, 0))
      assertEquals(0, Math.multiplyExact(0, n))
      assertEquals(n, Math.multiplyExact(n, 1))
      assertEquals(n, Math.multiplyExact(1, n))
    }
    assertEquals(Int.MaxValue, Math.multiplyExact(-2147483647, -1))
    assertEquals(Int.MaxValue, Math.multiplyExact(-1, -2147483647))
    assertEquals(2147483646, Math.multiplyExact(1073741823, 2))
    assertEquals(2147483646, Math.multiplyExact(2, 1073741823))
    assertEquals(Int.MinValue, Math.multiplyExact(1073741824, -2))
    assertEquals(Int.MinValue, Math.multiplyExact(-2, 1073741824))

    assertThrows(classOf[ArithmeticException], Math.multiplyExact(Int.MinValue, -1))
    assertThrows(classOf[ArithmeticException], Math.multiplyExact(-1, Int.MinValue))
    assertThrows(classOf[ArithmeticException], Math.multiplyExact(Int.MinValue, Int.MinValue))
    assertThrows(classOf[ArithmeticException], Math.multiplyExact(Int.MaxValue, Int.MaxValue))
    assertThrows(classOf[ArithmeticException], Math.multiplyExact(Int.MinValue, Int.MaxValue))
    assertThrows(classOf[ArithmeticException], Math.multiplyExact(Int.MaxValue, Int.MinValue))
    assertThrows(classOf[ArithmeticException], Math.multiplyExact(1073741824, 2))
    assertThrows(classOf[ArithmeticException], Math.multiplyExact(2, 1073741824))
    assertThrows(classOf[ArithmeticException], Math.multiplyExact(1073741825, -2))
    assertThrows(classOf[ArithmeticException], Math.multiplyExact(-2, 1073741825))
  }

  @Test def multiplyExactLongLong(): Unit = {
    for (n <- Seq(Long.MinValue, -1L, 0L, 1L, Long.MaxValue)) {
      assertEquals(0L, Math.multiplyExact(n, 0L))
      assertEquals(0L, Math.multiplyExact(0L, n))
      assertEquals(n, Math.multiplyExact(n, 1L))
      assertEquals(n, Math.multiplyExact(1L, n))
    }
    assertEquals(0L, Math.multiplyExact(Long.MinValue, 0L))
    assertEquals(0L, Math.multiplyExact(0L, Long.MinValue))
    assertEquals(Long.MaxValue, Math.multiplyExact(-9223372036854775807L, -1L))
    assertEquals(Long.MaxValue, Math.multiplyExact(-1L, -9223372036854775807L))
    assertEquals(9223372036854775806L, Math.multiplyExact(4611686018427387903L, 2L))
    assertEquals(9223372036854775806L, Math.multiplyExact(2L, 4611686018427387903L))
    assertEquals(Long.MinValue, Math.multiplyExact(4611686018427387904L, -2L))
    assertEquals(Long.MinValue, Math.multiplyExact(-2L, 4611686018427387904L))

    assertThrows(classOf[ArithmeticException], Math.multiplyExact(Long.MinValue, -1L))
    assertThrows(classOf[ArithmeticException], Math.multiplyExact(-1L, Long.MinValue))
    assertThrows(classOf[ArithmeticException], Math.multiplyExact(Long.MinValue, Long.MinValue))
    assertThrows(classOf[ArithmeticException], Math.multiplyExact(Long.MaxValue, Long.MaxValue))
    assertThrows(classOf[ArithmeticException], Math.multiplyExact(Long.MinValue, Long.MaxValue))
    assertThrows(classOf[ArithmeticException], Math.multiplyExact(Long.MaxValue, Long.MinValue))
    assertThrows(classOf[ArithmeticException], Math.multiplyExact(4611686018427387904L, 2L))
    assertThrows(classOf[ArithmeticException], Math.multiplyExact(2L, 4611686018427387904L))
    assertThrows(classOf[ArithmeticException], Math.multiplyExact(4611686018427387905L, -2L))
    assertThrows(classOf[ArithmeticException], Math.multiplyExact(-2L, 4611686018427387905L))
  }

  @Test def multiplyExactLongInt(): Unit = {
    for (n <- Seq(Long.MinValue, -1L, 0L, 1L, Long.MaxValue)) {
      assertEquals(0L, Math.multiplyExact(n, 0))
      assertEquals(n, Math.multiplyExact(n, 1))
    }
    for (n <- Seq(Int.MinValue, -1, 0, 1, Int.MaxValue)) {
      assertEquals(0L, Math.multiplyExact(0L, n))
      assertEquals(n.toLong, Math.multiplyExact(1L, n))
    }
    assertEquals(Long.MaxValue, Math.multiplyExact(-9223372036854775807L, -1))
    assertEquals(2147483648L, Math.multiplyExact(-1L, Int.MinValue))
    assertEquals(31284307708346368L, Math.multiplyExact(-14567891L, Int.MinValue))
    assertEquals(9223372036854775806L, Math.multiplyExact(4611686018427387903L, 2))
    assertEquals(922337202L, Math.multiplyExact(2L, 461168601))
    assertEquals(Long.MinValue, Math.multiplyExact(4611686018427387904L, -2))
    assertEquals(-4294967294L, Math.multiplyExact(-2L, Int.MaxValue))
    assertEquals(-6415938107894138L, Math.multiplyExact(-2987654L, Int.MaxValue))

    assertThrows(classOf[ArithmeticException], Math.multiplyExact(Long.MinValue, -1))
    assertThrows(classOf[ArithmeticException], Math.multiplyExact(-12345678910L, Int.MinValue))
    assertThrows(classOf[ArithmeticException], Math.multiplyExact(Long.MinValue, Int.MinValue))
    assertThrows(classOf[ArithmeticException], Math.multiplyExact(Long.MaxValue, Int.MaxValue))
    assertThrows(classOf[ArithmeticException], Math.multiplyExact(Long.MinValue, Int.MaxValue))
    assertThrows(classOf[ArithmeticException], Math.multiplyExact(Long.MaxValue, Int.MinValue))
    assertThrows(classOf[ArithmeticException], Math.multiplyExact(4611686018427387904L, 2))
    assertThrows(classOf[ArithmeticException], Math.multiplyExact(29876541321L, 461168601))
    assertThrows(classOf[ArithmeticException], Math.multiplyExact(4611686018427387905L, -2))
    assertThrows(classOf[ArithmeticException], Math.multiplyExact(-29876541321L, 461168601))
  }

  @Test def incrementExact(): Unit = {
    assertEquals(Int.MaxValue, Math.incrementExact(Int.MaxValue - 1))
    assertEquals(Long.MaxValue, Math.incrementExact(Long.MaxValue - 1))

    assertThrows(classOf[ArithmeticException], Math.incrementExact(Int.MaxValue))
    assertThrows(classOf[ArithmeticException], Math.incrementExact(Long.MaxValue))
  }

  @Test def decrementExact(): Unit = {
    assertEquals(Int.MinValue, Math.decrementExact(Int.MinValue + 1))
    assertEquals(Long.MinValue, Math.decrementExact(Long.MinValue + 1))

    assertThrows(classOf[ArithmeticException], Math.decrementExact(Int.MinValue))
    assertThrows(classOf[ArithmeticException], Math.decrementExact(Long.MinValue))
  }

  @Test def negateExact(): Unit = {
    assertEquals(Int.MaxValue, Math.negateExact(Int.MinValue + 1))
    assertEquals(Int.MinValue + 1, Math.negateExact(Int.MaxValue))
    assertEquals(Long.MaxValue, Math.negateExact(Long.MinValue + 1))
    assertEquals(Long.MinValue + 1, Math.negateExact(Long.MaxValue))

    assertThrows(classOf[ArithmeticException], Math.negateExact(Int.MinValue))
    assertThrows(classOf[ArithmeticException], Math.negateExact(Long.MinValue))
  }

  @Test def toIntExact(): Unit = {
    assertEquals(Int.MinValue, Math.toIntExact(-2147483648L))
    assertEquals(Int.MaxValue, Math.toIntExact(2147483647L))

    assertThrows(classOf[ArithmeticException], Math.toIntExact(-2147483649L))
    assertThrows(classOf[ArithmeticException], Math.toIntExact(2147483648L))
  }

  @Test def testMultiplyFull(): Unit = {
    @inline def test(expected: Long, x: Int, y: Int): Unit = {
      assertEquals(expected, Math.multiplyFull(x, y))
      assertEquals(expected, Math.multiplyFull(x, hideFromOptimizer(y)))
      assertEquals(expected, Math.multiplyFull(hideFromOptimizer(x), y))
      assertEquals(expected, Math.multiplyFull(hideFromOptimizer(x), hideFromOptimizer(y)))
    }

    test(2641928036408725662L, 1942041231, 1360387202)
    test(54843908448922272L, 1565939409, 35023008)
    test(510471553407128558L, 1283300489, 397780222)
    test(-1211162085735907941L, -1990140693, 608581137)
    test(-1197265696701533712L, -584098468, 2049766884)
    test(203152587796496856L, -1809591416, -112264341)
    test(-1869763755321108598L, 1235591906, -1513253483)
    test(-737954189546644064L, 675415792, -1092592442)
    test(-2570904460570261986L, 1639253754, -1568338309)
    test(1106623967126000400L, 2088029790, 529984760)
    test(1407516248272451352L, -869881054, -1618055988)
    test(-2120367337662071940L, -1558894530, 1360173698)
    test(-1464086284066637244L, -1417313902, 1033000722)
    test(36729253163312334L, -1673852034, -21942951)
    test(-3197007331876781046L, 1876799847, -1703435418)
    test(461794994386945009L, -246001091, -1877207099)
    test(-1206231192496917804L, 867896526, -1389832954)
    test(-1739671893103255929L, -1083992841, 1604873969)
    test(-409626127116780624L, 240101424, -1706054551)
    test(-3083566560548370936L, -1568530113, 1965895672)
    test(-1205028798380605000L, -1201743532, 1002733750)
    test(-1328689065035027168L, 929349664, -1429697687)
    test(-124212693522020684L, 80893862, -1535502082)
    test(-82341860111074830L, -243230690, 338534007)
    test(-846837059701860202L, 1959770926, -432110227)
    test(335728245390354432L, 506816728, 662425344)
    test(745294755971022170L, 1521993302, 489683335)
    test(-2370525755201631608L, 2023520366, -1171485988)
    test(-1039854583047715776L, 593162592, -1753068378)
    test(-152985384388127808L, -635946432, 240563319)
    test(-678107568956539050L, 649113254, -1044667575)
    test(-3064094283703186444L, -1890896836, 1620444979)
    test(1240687269228318870L, -1080325230, -1148438669)
    test(-46551523496333580L, 27167878, -1713476610)
    test(-2500430606368427103L, 2023288183, -1235825241)
    test(92963399778762084L, 896198732, 103730787)
    test(2469065794894324667L, 2105111101, 1172890967)
    test(172558569988357136L, -142945148, -1207166332)
    test(335684786634110970L, -1647598405, -203741874)
    test(2406859843746696240L, 2049365815, 1174441296)
    test(3100973294006114952L, 1991928152, 1556769651)
    test(-335912134649077352L, 866240524, -387781598)
    test(84303320581066207L, 75666091, 1114149277)
    test(-2623126349572207976L, 1426933667, -1838295928)
    test(59139945163750590L, 149344270, 395997417)
    test(-105764175098643999L, 68726447, -1538915217)
    test(8595303129864000L, 726092025, 11837760)
    test(-2958527843471399088L, 1536412078, -1925608296)
    test(1532625839159904477L, 867021537, 1767690621)
    test(384402376484481316L, 1207235521, 318415396)
    test(-219376614576542698L, 1816299166, -120782203)
    test(-672138807810988440L, 531516745, -1264567512)
    test(-193351903065245331L, 170858169, -1131651499)
    test(71263251057597648L, 51058196, 1395725988)
    test(-774312974742971385L, 1958551603, -395349795)
    test(-1846593638370672048L, 1190143097, -1551572784)
    test(240083094242536384L, 1404614968, 170924488)
    test(-130950827889833280L, -115480554, 1133964320)
    test(128954457719585228L, 735993884, 175211317)
    test(364779990580792000L, -668489125, -545678272)
    test(107252402494512045L, 759517757, 141211185)
    test(3038084150893069044L, -1924640913, -1578519988)
    test(760804294233336624L, -728394552, -1044494762)
    test(1171051779605774913L, 848233701, 1380576813)
    test(-1805862307837393080L, -1385644986, 1303264780)
    test(172227703288618734L, -104999826, -1640266559)
    test(150448013961014407L, 163398103, 920745169)
    test(-671469201380991232L, 650262784, -1032612073)
    test(-1325861126942924945L, -1773644581, 747534845)
    test(987406376890116568L, -1626507773, -607071416)
    test(2918138947401192144L, 1695881208, 1720721318)
    test(-2590993826910153940L, -1397240042, 1854365570)
    test(954644624447419276L, -1516139806, -629654746)
    test(407510452326678620L, -384747652, -1059162935)
    test(149866317537821404L, 1530355444, 97929091)
    test(922044716091910632L, 968149268, 952378674)
    test(-3508732521573808284L, 1825364562, -1922209182)
    test(1701723136959404304L, 894776752, 1901841027)
    test(-2435876799625512705L, -1276062909, 1908900245)
    test(-516933170985379201L, 657063047, -786732983)
    test(123334479976750576L, 313765817, 393078128)
    test(-1072624004420456775L, -894199299, 1199535725)
    test(301682711612188737L, 330918981, 911651277)
    test(1790992996470651507L, -1115945231, -1604911197)
    test(-2750453268538140155L, 1878389719, -1464261245)
    test(758285757353272504L, 1259684942, 601964612)
    test(-218581674312137400L, -161533394, 1353167100)
    test(-1824007072461951836L, -1244277844, 1465916219)
    test(-92753167730460334L, -65368843, 1418920138)
    test(-2326636630979491248L, 1124395877, -2069232624)
    test(-7380586257943446L, 29715454, -248375349)
    test(31319707234597638L, 491995506, 63658523)
    test(-1196559502630778250L, -1752963990, 682592175)
    test(166065559841839548L, -911521074, -182185102)
    test(-1222260378510810100L, 1071539812, -1140657925)
    test(57800571165871464L, -257569032, -224408077)
    test(332444627169725608L, 1247224172, 266547614)
    test(217903869180130650L, 1069161915, 203808110)
    test(920425054266935850L, -901689546, -1020778225)
    test(-507632632656614388L, 864632142, -587108214)
  }

  @Test def testMultiplyHigh(): Unit = {
    def test(expected: Long, x: Long, y: Long): Unit =
      assertEquals(expected, Math.multiplyHigh(x, y))

    test(-2514789262281153376L, 8217931296694472096L, -5644933286224084859L)
    test(-298247406641127011L, -8034902747807161194L, 684724352445702293L)
    test(242644198957550459L, 717019025263929004L, 6242505821226454837L)
    test(-1089698470915011537L, -7558081430876177893L, 2659588811568490384L)
    test(138675986327040026L, 2362930226177876193L, 1082605148727562445L)
    test(-1260260349245855816L, -3350308785473442797L, 6938972380570262589L)
    test(-1799534229489533301L, -4097805274432763180L, 8100811327075225922L)
    test(437623091041087696L, -2968271773754119013L, -2719670493975918294L)
    test(-107841114219899514L, 2013609532543228156L, -987936043452088475L)
    test(2757621741022067854L, -7005993850636185311L, -7260803191272031988L)
    test(-187671345159116030L, 1781219534362173574L, -1943570237881252419L)
    test(-515018730942796014L, 6085558843030314089L, -1561141543105626636L)
    test(-119091959391883575L, 7423442237814967910L, -295935339127164155L)
    test(18351865713513547L, -1886460125362775846L, -179453657960126825L)
    test(3928100041033091765L, 8449838094261471293L, 8575389888485029447L)
    test(-7404756889594137L, -89549316594063561L, 1525345591296625693L)
    test(714591873345926311L, -2929853068304815970L, -4499165349746322236L)
    test(1305977852854305585L, -5568549492657237090L, -4326268312655360053L)
    test(-2435010516398991446L, 6443930667478151719L, -6970592660082469124L)
    test(2031324595328562735L, 5390460907312723801L, 6951413911530987604L)
    test(34713245667458599L, -535353692461820541L, -1196118319182197181L)
    test(255381044848343425L, -3176530727082196631L, -1483048388428836603L)
    test(6566871520624982L, -33326351213089011L, -3634883324950494373L)
    test(156130078476475485L, 687410849583778615L, 4189767446364284457L)
    test(1647679448547038188L, 4460502251200507739L, 6814102850116870938L)
    test(-2241611115434343963L, 5633894511267143863L, -7339581257068946568L)
    test(-93572860194426351L, -1075368508503119813L, 1605137764964203383L)
    test(1663347345126188661L, -6330756750592024018L, -4846710115399342760L)
    test(-1686630202076061136L, 5124142056960069542L, -6071813649745693328L)
    test(728105493712673843L, -8079843401135830331L, -1662306437683128283L)
    test(-2030727779883712688L, 4452689522888653156L, -8412963770845872378L)
    test(734253555387491804L, 5835084770836409518L, 2321232330529258387L)
    test(2018627311798804222L, -7211950082779933827L, -5163250018863045382L)
    test(-1244560006523295051L, -7326211205612788508L, 3133690700470219958L)
    test(-492070935033321215L, 1614944457187625808L, -5620692751550184667L)
    test(319340972880203566L, 2310036532484690677L, 2550090059672932009L)
    test(1766280783448332865L, 5949345770128658249L, 5476590340096838859L)
    test(2757208297958468913L, -5707089944199929572L, -8911987777945981523L)
    test(408328069441815717L, 1242541635079749093L, 6062028975489127199L)
    test(-77985829287979398L, -7943526433115400350L, 181101510313367840L)
    test(-230121117022373017L, -780391911062895469L, 5439555807140802418L)
    test(2588662639521587653L, 7451684432618227097L, 6408268846625040081L)
    test(861249002493118404L, 1744344496585548181L, 9107856827493957233L)
    test(-2703044944335540474L, 8052570526613861366L, -6192106997771248181L)
    test(-2975059248415970510L, 6503508572335523474L, -8438546047759521035L)
    test(-370291189062632935L, -8722964233277178137L, 783067156383574516L)
    test(-90473002639507852L, 852694261922564555L, -1957245873225555126L)
    test(-218977334338454381L, -1819563432425194345L, 2219993418476586419L)
    test(-1087231185918604076L, -2941838679159182506L, 6817462690146034563L)
    test(-1170480051005916145L, -2771463765488827700L, 7790665067735548924L)
    test(-371145713487913188L, 3224241917397787909L, -2123423169279885562L)
    test(-502492608136209963L, 1568228348895174267L, -5910716094215359887L)
    test(1445926343733049503L, -7706328512722939071L, -3461133686196008644L)
    test(-1374053009197983052L, -8787832166727089323L, 2884306814637966447L)
    test(-1910150305525172307L, 8663815092401732543L, -4067036686787486282L)
    test(2074971709256543740L, 8092193156887080609L, 4730049238662438083L)
    test(953725989108917020L, 8492699833366153401L, 2071560232049848145L)
    test(334989155711573307L, 1093268576921704206L, 5652279186765632978L)
    test(129011196343964709L, 1000276763122669782L, 2379178052852915387L)
    test(239042793587178901L, 3208737625070847213L, 1374235525371105170L)
    test(127809344420152430L, -7696730067895344868L, -306320508313194466L)
    test(-2506455997163955037L, -5731747797284935902L, 8066641092198683254L)
    test(3016086034985660469L, -6992699346126002928L, -7956436339922591224L)
    test(-1527917483534567268L, -8938885845855254814L, 3153089016969294968L)
    test(-1268939936756528050L, 5537112727075101653L, -4227439716695399205L)
    test(-37535014067603004L, -8605247800544091240L, 80462389271855887L)
    test(-2710920384572235679L, -7926242046619125682L, 6309125338878172023L)
    test(-3331830886924716794L, 6823617049086893513L, -9007163096323738999L)
    test(1854911433578401793L, -4644835313936852982L, -7366693150982113934L)
    test(-3840461794042836575L, 8006480391435326631L, -8848334396141248546L)
    test(-1212641710132993432L, -7017377545321262459L, 3187699555205380404L)
    test(946047090630044138L, -5829622550331878687L, -2993588077419595837L)
    test(3518955178043574292L, -7909090733489625033L, -8207424565425867851L)
    test(1231895337081111773L, 2841977238766797132L, 7996002817598962425L)
    test(-1649686524869089287L, -3558405071306300052L, 8551962049372852642L)
    test(1156466789444347220L, -8077807627762096372L, -2640945152160624636L)
    test(-284428196958678125L, 7604654143237097972L, -689942508603024688L)
    test(24530734973246035L, -4976536915346383672L, -90929133590073966L)
    test(915668791878818L, -4915702564252847L, -3436153355352311231L)
    test(-59487608720960501L, 2234272329433906652L, -491145452224512365L)
    test(-935777346233643464L, 2234022931260640741L, -7726888105936443458L)
    test(-539196324963981948L, 1233384294780865907L, -8064328899098291942L)
    test(-302740552339519239L, 1652272762436229815L, -3379936785683182277L)
    test(-1602328337662720444L, -5891195966699023422L, 5017273391344774367L)
    test(1971437877011804292L, 6123334000940359947L, 5939021122948580484L)
    test(3518273874050862283L, -7935043146462869940L, -8178997459486413381L)
    test(989386049294028022L, 3631504400505165814L, 5025727419987895939L)
    test(1075600553777136761L, 8162668046881939535L, 2430740540606242760L)
    test(555876997051543592L, -1422006546765159905L, -7211022146415941068L)
    test(1442987791832810570L, 3172003226122803882L, 8391676993961733131L)
    test(122174343239443206L, 592078109511582332L, 3806455273225175653L)
    test(-555975358284841098L, -2610695041141095892L, 3928430928909536969L)
    test(1217820260754824228L, -2566343358431797989L, -8753629401971345682L)
    test(-843540703271762806L, 2010390971620435041L, -7740076278033066915L)
    test(28227414827282063L, 1691814723551530731L, 307778322255183098L)
    test(-3487482743675782331L, 8885183126228404590L, -7240447464066348779L)
    test(-641218088086423374L, -5793475349478143447L, 2041673650588512538L)
    test(491218135799199820L, -3483174304311045377L, -2601470510458659970L)
    test(-61083956648009538L, -331097881159246733L, 3403223576515274855L)
    test(-1760654512150512675L, -6642702867806073297L, 4889326503714183951L)
  }

  @Test def floorDiv(): Unit = {
    assertEquals(0, Math.floorDiv(0, 1))
    assertEquals(0, Math.floorDiv(0, -1))
    assertEquals(1, Math.floorDiv(1, 1))
    assertEquals(-1, Math.floorDiv(1, -1))
    assertEquals(0, Math.floorDiv(1, 2))
    assertEquals(-1, Math.floorDiv(1, -2))
    assertEquals(-1, Math.floorDiv(-1, 2))
    assertEquals(0, Math.floorDiv(-1, -2))
    assertEquals(0, Math.floorDiv(1, Int.MaxValue))
    assertEquals(-1, Math.floorDiv(1, Int.MinValue))
    assertEquals(-1, Math.floorDiv(-1, Int.MaxValue))
    assertEquals(0, Math.floorDiv(-1, Int.MinValue))
    assertEquals(Int.MaxValue, Math.floorDiv(Int.MaxValue, 1))
    assertEquals(-Int.MaxValue, Math.floorDiv(Int.MaxValue, -1))
    assertEquals(Int.MinValue, Math.floorDiv(Int.MinValue, 1))
    assertEquals(Int.MinValue, Math.floorDiv(Int.MinValue, -1))

    assertThrows(classOf[ArithmeticException], Math.floorDiv(5, 0))

    assertEquals(0L, Math.floorDiv(0L, 1L))
    assertEquals(0L, Math.floorDiv(0L, -1L))
    assertEquals(1L, Math.floorDiv(1L, 1L))
    assertEquals(-1L, Math.floorDiv(1L, -1L))
    assertEquals(0L, Math.floorDiv(1L, 2L))
    assertEquals(-1L, Math.floorDiv(1L, -2L))
    assertEquals(-1L, Math.floorDiv(-1L, 2L))
    assertEquals(0L, Math.floorDiv(-1L, -2L))
    assertEquals(0L, Math.floorDiv(1L, Long.MaxValue))
    assertEquals(-1L, Math.floorDiv(1L, Long.MinValue))
    assertEquals(-1L, Math.floorDiv(-1L, Long.MaxValue))
    assertEquals(0L, Math.floorDiv(-1L, Long.MinValue))
    assertEquals(Long.MaxValue, Math.floorDiv(Long.MaxValue, 1))
    assertEquals(-Long.MaxValue, Math.floorDiv(Long.MaxValue, -1))
    assertEquals(Long.MinValue, Math.floorDiv(Long.MinValue, 1))
    assertEquals(Long.MinValue, Math.floorDiv(Long.MinValue, -1))

    assertThrows(classOf[ArithmeticException], Math.floorDiv(5L, 0L))
  }

  @Test def floorMod(): Unit = {
    assertEquals(0, Math.floorMod(0, 1))
    assertEquals(0, Math.floorMod(0, -1))
    assertEquals(0, Math.floorMod(1, 1))
    assertEquals(0, Math.floorMod(1, -1))
    assertEquals(1, Math.floorMod(1, 3))
    assertEquals(-2, Math.floorMod(1, -3))
    assertEquals(2, Math.floorMod(-1, 3))
    assertEquals(-1, Math.floorMod(-1, -3))
    assertEquals(1, Math.floorMod(1, Int.MaxValue))
    assertEquals(-2147483647, Math.floorMod(1, Int.MinValue))
    assertEquals(2147483646, Math.floorMod(-1, Int.MaxValue))
    assertEquals(-1, Math.floorMod(-1, Int.MinValue))
    assertEquals(0, Math.floorMod(Int.MaxValue, 1))
    assertEquals(0, Math.floorMod(Int.MaxValue, -1))
    assertEquals(0, Math.floorMod(Int.MinValue, 1))
    assertEquals(0, Math.floorMod(Int.MinValue, -1))

    assertThrows(classOf[ArithmeticException], Math.floorMod(5, 0))

    assertEquals(0L, Math.floorMod(0L, 1L))
    assertEquals(0L, Math.floorMod(0L, -1L))
    assertEquals(0L, Math.floorMod(1L, 1L))
    assertEquals(0L, Math.floorMod(1L, -1L))
    assertEquals(1L, Math.floorMod(1L, 3L))
    assertEquals(-2L, Math.floorMod(1L, -3L))
    assertEquals(2L, Math.floorMod(-1L, 3L))
    assertEquals(-1L, Math.floorMod(-1L, -3L))
    assertEquals(1L, Math.floorMod(1L, Long.MaxValue))
    assertEquals(-9223372036854775807L, Math.floorMod(1L, Long.MinValue))
    assertEquals(9223372036854775806L, Math.floorMod(-1L, Long.MaxValue))
    assertEquals(-1L, Math.floorMod(-1, Long.MinValue))
    assertEquals(0L, Math.floorMod(Long.MaxValue, 1L))
    assertEquals(0L, Math.floorMod(Long.MaxValue, -1L))
    assertEquals(0L, Math.floorMod(Long.MinValue, 1L))
    assertEquals(0L, Math.floorMod(Long.MinValue, -1L))

    assertThrows(classOf[ArithmeticException], Math.floorMod(5L, 0L))
  }

  @Test def nextDownForDouble(): Unit = {
    // Specials
    assertSameDouble(-Double.MinPositiveValue, Math.nextDown(0.0))
    assertSameDouble(-Double.MinPositiveValue, Math.nextDown(-0.0))
    assertSameDouble(Double.MaxValue, Math.nextDown(Double.PositiveInfinity))
    assertSameDouble(Double.NegativeInfinity, Math.nextDown(Double.NegativeInfinity))
    assertSameDouble(Double.NaN, Math.nextDown(Double.NaN))

    // Corner cases
    val MinNormal = java.lang.Double.MIN_NORMAL
    val MaxSubnormal = 2.225073858507201e-308
    assertSameDouble(1.7976931348623155e+308, Math.nextDown(Double.MaxValue))
    assertSameDouble(Double.NegativeInfinity, Math.nextDown(Double.MinValue))
    assertSameDouble(0.0, Math.nextDown(Double.MinPositiveValue))
    assertSameDouble(MaxSubnormal, Math.nextDown(MinNormal))
    assertSameDouble(-MinNormal, Math.nextDown(-MaxSubnormal))

    // Try very hard to produce non-canonical NaN's that are corner cases
    @noinline def fromBits(bits: Long): Double = java.lang.Double.longBitsToDouble(bits)

    for (bits <- List(Long.MaxValue, -1L, 0x7ff0000000000001L, 0xfff0000000000001L))
      assertSameDouble(Double.NaN, Math.nextDown(fromBits(bits)))

    // Random values
    assertSameDouble(9007199254740991.0, Math.nextDown(9007199254740992.0))
    assertSameDouble(9007199254740992.0, Math.nextDown(9007199254740994.0))
    assertSameDouble(0.9999999999999999, Math.nextDown(1.0))
  }

  @Test def nextDownForFloat(): Unit = {
    // Specials
    assertSameFloat(-Float.MinPositiveValue, Math.nextDown(0.0f))
    assertSameFloat(-Float.MinPositiveValue, Math.nextDown(-0.0f))
    assertSameFloat(Float.MaxValue, Math.nextDown(Float.PositiveInfinity))
    assertSameFloat(Float.NegativeInfinity, Math.nextDown(Float.NegativeInfinity))
    assertSameFloat(Float.NaN, Math.nextDown(Float.NaN))

    // Corner cases
    val MinNormal = java.lang.Float.MIN_NORMAL
    val MaxSubnormal = 1.1754942e-38f
    assertSameFloat(3.4028233e38f, Math.nextDown(Float.MaxValue))
    assertSameFloat(Float.NegativeInfinity, Math.nextDown(Float.MinValue))
    assertSameFloat(0.0f, Math.nextDown(Float.MinPositiveValue))
    assertSameFloat(MaxSubnormal, Math.nextDown(MinNormal))
    assertSameFloat(-MinNormal, Math.nextDown(-MaxSubnormal))

    // Try very hard to produce non-canonical NaN's that are corner cases
    @noinline def fromBits(bits: Int): Float = java.lang.Float.intBitsToFloat(bits)

    for (bits <- List(Int.MaxValue, -1, 0x7f800001, 0xff800001))
      assertSameFloat(Float.NaN, Math.nextDown(fromBits(bits)))

    // Random values
    assertSameFloat(9007198700000000.0f, Math.nextDown(9007199300000000.0f))
    assertSameFloat(0.99999994f, Math.nextDown(1.0f))
  }

  @Test def scalbDouble(): Unit = {
    import java.lang.Double.{MIN_NORMAL => MinNormal}
    import Double.{PositiveInfinity, NegativeInfinity, MinPositiveValue, MaxValue, NaN}

    // Specials
    for {
      special <- List(+0.0, -0.0, PositiveInfinity, NegativeInfinity, NaN)
      scaleFactor <- List(0, 1, -1, 50, -50, 10000, -10000, Int.MinValue, Int.MaxValue)
    } {
      assertSameDouble(s"scalb($special, $scaleFactor)", special, Math.scalb(special, scaleFactor))
    }

    // Normal-to-normal
    assertSameDouble(10.5, Math.scalb(5.25, 1))
    assertSameDouble(-10.5, Math.scalb(-5.25, 1))
    assertSameDouble(5.25, Math.scalb(10.5, -1))
    assertSameDouble(20.0 * MinNormal, Math.scalb(5.0 * MinNormal, 2))
    assertSameDouble(5.0 * MinNormal, Math.scalb(20.0 * MinNormal, -2))

    // Subnormal-to-subnormal
    assertSameDouble(20 * MinPositiveValue, Math.scalb(5 * MinPositiveValue, 2))
    assertSameDouble(-20 * MinPositiveValue, Math.scalb(-5 * MinPositiveValue, 2))
    assertSameDouble(5 * MinPositiveValue, Math.scalb(20 * MinPositiveValue, -2))

    // Subnormal-to-subnormal with rounding
    assertSameDouble(6 * MinPositiveValue, Math.scalb(25 * MinPositiveValue, -2))
    assertSameDouble(6 * MinPositiveValue, Math.scalb(26 * MinPositiveValue, -2)) // even down
    assertSameDouble(7 * MinPositiveValue, Math.scalb(27 * MinPositiveValue, -2))
    assertSameDouble(7 * MinPositiveValue, Math.scalb(28 * MinPositiveValue, -2)) // exact
    assertSameDouble(7 * MinPositiveValue, Math.scalb(29 * MinPositiveValue, -2))
    assertSameDouble(8 * MinPositiveValue, Math.scalb(30 * MinPositiveValue, -2)) // even up
    assertSameDouble(8 * MinPositiveValue, Math.scalb(31 * MinPositiveValue, -2))
    assertSameDouble(-6 * MinPositiveValue, Math.scalb(-25 * MinPositiveValue, -2))
    assertSameDouble(-6 * MinPositiveValue, Math.scalb(-26 * MinPositiveValue, -2)) // even up
    assertSameDouble(-7 * MinPositiveValue, Math.scalb(-27 * MinPositiveValue, -2))
    assertSameDouble(-7 * MinPositiveValue, Math.scalb(-28 * MinPositiveValue, -2)) // exact
    assertSameDouble(-7 * MinPositiveValue, Math.scalb(-29 * MinPositiveValue, -2))
    assertSameDouble(-8 * MinPositiveValue, Math.scalb(-30 * MinPositiveValue, -2)) // even down
    assertSameDouble(-8 * MinPositiveValue, Math.scalb(-31 * MinPositiveValue, -2))

    // Subnormal-to-normal
    assertSameDouble(40 * MinNormal, Math.scalb(0.625 * MinNormal, 6))
    assertSameDouble(2.5 * MinNormal, Math.scalb(0.625 * MinNormal, 2))
    assertSameDouble(-40 * MinNormal, Math.scalb(-0.625 * MinNormal, 6))
    assertSameDouble(-2.5 * MinNormal, Math.scalb(-0.625 * MinNormal, 2))

    // Normal-to-subnormal
    assertSameDouble(0.625 * MinNormal, Math.scalb(40 * MinNormal, -6))
    assertSameDouble(0.625 * MinNormal, Math.scalb(2.5 * MinNormal, -2))
    assertSameDouble(-0.625 * MinNormal, Math.scalb(-40 * MinNormal, -6))
    assertSameDouble(-0.625 * MinNormal, Math.scalb(-2.5 * MinNormal, -2))

    // Normal-to-subnormal with rounding
    assertSameDouble(6 * MinPositiveValue, Math.scalb(25 * MinNormal, -54))
    assertSameDouble(6 * MinPositiveValue, Math.scalb(26 * MinNormal, -54)) // even down
    assertSameDouble(7 * MinPositiveValue, Math.scalb(27 * MinNormal, -54))
    assertSameDouble(7 * MinPositiveValue, Math.scalb(28 * MinNormal, -54)) // exact
    assertSameDouble(7 * MinPositiveValue, Math.scalb(29 * MinNormal, -54))
    assertSameDouble(8 * MinPositiveValue, Math.scalb(30 * MinNormal, -54)) // even up
    assertSameDouble(8 * MinPositiveValue, Math.scalb(31 * MinNormal, -54))
    assertSameDouble(-6 * MinPositiveValue, Math.scalb(-25 * MinNormal, -54))
    assertSameDouble(-6 * MinPositiveValue, Math.scalb(-26 * MinNormal, -54)) // even up
    assertSameDouble(-7 * MinPositiveValue, Math.scalb(-27 * MinNormal, -54))
    assertSameDouble(-7 * MinPositiveValue, Math.scalb(-28 * MinNormal, -54)) // exact
    assertSameDouble(-7 * MinPositiveValue, Math.scalb(-29 * MinNormal, -54))
    assertSameDouble(-8 * MinPositiveValue, Math.scalb(-30 * MinNormal, -54)) // even down
    assertSameDouble(-8 * MinPositiveValue, Math.scalb(-31 * MinNormal, -54))

    // Overflow
    assertSameDouble(PositiveInfinity, Math.scalb(25.0, 2000))
    assertSameDouble(PositiveInfinity, Math.scalb(25.0, Int.MaxValue))
    assertSameDouble(NegativeInfinity, Math.scalb(-25.0, 2000))
    assertSameDouble(NegativeInfinity, Math.scalb(-25.0, Int.MaxValue))

    // Underflow
    assertSameDouble(+0.0, Math.scalb(25.0, -2000))
    assertSameDouble(+0.0, Math.scalb(25.0, Int.MinValue))
    assertSameDouble(-0.0, Math.scalb(-25.0, -2000))
    assertSameDouble(-0.0, Math.scalb(-25.0, Int.MinValue))

    // Limits at the overflow boundary
    assertSameDouble(MaxValue, Math.scalb(7.999999999999999, 1021))
    assertSameDouble(PositiveInfinity, Math.scalb(8.0, 1021))

    // Limits at the underflow boundary
    assertSameDouble(MinPositiveValue, Math.scalb(3.0, -1076)) // mantissa pattern is 1.1000...
    assertSameDouble(MinPositiveValue, Math.scalb(2.0000000000000004, -1076)) // mantissa pattern is 1.00...001
    assertSameDouble(+0.0, Math.scalb(2.0, -1076))
  }

  @Test def scalbFloat(): Unit = {
    import java.lang.Float.{MIN_NORMAL => MinNormal}
    import Float.{PositiveInfinity, NegativeInfinity, MinPositiveValue, MaxValue, NaN}

    // Specials
    for {
      special <- List(+0.0f, -0.0f, PositiveInfinity, NegativeInfinity, NaN)
      scaleFactor <- List(0, 1, -1, 50, -50, 10000, -10000, Int.MinValue, Int.MaxValue)
    } {
      assertSameFloat(s"scalb($special, $scaleFactor)", special, Math.scalb(special, scaleFactor))
    }

    // Normal-to-normal
    assertSameFloat(10.5f, Math.scalb(5.25f, 1))
    assertSameFloat(-10.5f, Math.scalb(-5.25f, 1))
    assertSameFloat(5.25f, Math.scalb(10.5f, -1))
    assertSameFloat(20.0f * MinNormal, Math.scalb(5.0f * MinNormal, 2))
    assertSameFloat(5.0f * MinNormal, Math.scalb(20.0f * MinNormal, -2))

    // Subnormal-to-subnormal
    assertSameFloat(20 * MinPositiveValue, Math.scalb(5 * MinPositiveValue, 2))
    assertSameFloat(-20 * MinPositiveValue, Math.scalb(-5 * MinPositiveValue, 2))
    assertSameFloat(5 * MinPositiveValue, Math.scalb(20 * MinPositiveValue, -2))

    // Subnormal-to-subnormal with rounding
    assertSameFloat(6 * MinPositiveValue, Math.scalb(25 * MinPositiveValue, -2))
    assertSameFloat(6 * MinPositiveValue, Math.scalb(26 * MinPositiveValue, -2)) // even down
    assertSameFloat(7 * MinPositiveValue, Math.scalb(27 * MinPositiveValue, -2))
    assertSameFloat(7 * MinPositiveValue, Math.scalb(28 * MinPositiveValue, -2)) // exact
    assertSameFloat(7 * MinPositiveValue, Math.scalb(29 * MinPositiveValue, -2))
    assertSameFloat(8 * MinPositiveValue, Math.scalb(30 * MinPositiveValue, -2)) // even up
    assertSameFloat(8 * MinPositiveValue, Math.scalb(31 * MinPositiveValue, -2))
    assertSameFloat(-6 * MinPositiveValue, Math.scalb(-25 * MinPositiveValue, -2))
    assertSameFloat(-6 * MinPositiveValue, Math.scalb(-26 * MinPositiveValue, -2)) // even up
    assertSameFloat(-7 * MinPositiveValue, Math.scalb(-27 * MinPositiveValue, -2))
    assertSameFloat(-7 * MinPositiveValue, Math.scalb(-28 * MinPositiveValue, -2)) // exact
    assertSameFloat(-7 * MinPositiveValue, Math.scalb(-29 * MinPositiveValue, -2))
    assertSameFloat(-8 * MinPositiveValue, Math.scalb(-30 * MinPositiveValue, -2)) // even down
    assertSameFloat(-8 * MinPositiveValue, Math.scalb(-31 * MinPositiveValue, -2))

    // Subnormal-to-normal
    assertSameFloat(40 * MinNormal, Math.scalb(0.625f * MinNormal, 6))
    assertSameFloat(2.5f * MinNormal, Math.scalb(0.625f * MinNormal, 2))
    assertSameFloat(-40 * MinNormal, Math.scalb(-0.625f * MinNormal, 6))
    assertSameFloat(-2.5f * MinNormal, Math.scalb(-0.625f * MinNormal, 2))

    // Normal-to-subnormal
    assertSameFloat(0.625f * MinNormal, Math.scalb(40 * MinNormal, -6))
    assertSameFloat(0.625f * MinNormal, Math.scalb(2.5f * MinNormal, -2))
    assertSameFloat(-0.625f * MinNormal, Math.scalb(-40 * MinNormal, -6))
    assertSameFloat(-0.625f * MinNormal, Math.scalb(-2.5f * MinNormal, -2))

    // Normal-to-subnormal with rounding
    assertSameFloat(6 * MinPositiveValue, Math.scalb(25 * MinNormal, -25))
    assertSameFloat(6 * MinPositiveValue, Math.scalb(26 * MinNormal, -25)) // even down
    assertSameFloat(7 * MinPositiveValue, Math.scalb(27 * MinNormal, -25))
    assertSameFloat(7 * MinPositiveValue, Math.scalb(28 * MinNormal, -25)) // exact
    assertSameFloat(7 * MinPositiveValue, Math.scalb(29 * MinNormal, -25))
    assertSameFloat(8 * MinPositiveValue, Math.scalb(30 * MinNormal, -25)) // even up
    assertSameFloat(8 * MinPositiveValue, Math.scalb(31 * MinNormal, -25))
    assertSameFloat(-6 * MinPositiveValue, Math.scalb(-25 * MinNormal, -25))
    assertSameFloat(-6 * MinPositiveValue, Math.scalb(-26 * MinNormal, -25)) // even up
    assertSameFloat(-7 * MinPositiveValue, Math.scalb(-27 * MinNormal, -25))
    assertSameFloat(-7 * MinPositiveValue, Math.scalb(-28 * MinNormal, -25)) // exact
    assertSameFloat(-7 * MinPositiveValue, Math.scalb(-29 * MinNormal, -25))
    assertSameFloat(-8 * MinPositiveValue, Math.scalb(-30 * MinNormal, -25)) // even down
    assertSameFloat(-8 * MinPositiveValue, Math.scalb(-31 * MinNormal, -25))

    // Overflow
    assertSameFloat(PositiveInfinity, Math.scalb(25.0f, 300))
    assertSameFloat(PositiveInfinity, Math.scalb(25.0f, Int.MaxValue))
    assertSameFloat(NegativeInfinity, Math.scalb(-25.0f, 300))
    assertSameFloat(NegativeInfinity, Math.scalb(-25.0f, Int.MaxValue))

    // Underflow
    assertSameFloat(+0.0f, Math.scalb(25.0f, -300))
    assertSameFloat(+0.0f, Math.scalb(25.0f, Int.MinValue))
    assertSameFloat(-0.0f, Math.scalb(-25.0f, -300))
    assertSameFloat(-0.0f, Math.scalb(-25.0f, Int.MinValue))

    // Limits at the overflow boundary
    assertSameFloat(MaxValue, Math.scalb(7.9999995f, 125))
    assertSameFloat(PositiveInfinity, Math.scalb(8.0f, 125))

    // Limits at the underflow boundary
    assertSameFloat(MinPositiveValue, Math.scalb(3.0f, -151)) // mantissa pattern is 1.1000...
    assertSameFloat(MinPositiveValue, Math.scalb(2.0000002f, -151)) // mantissa pattern is 1.00...001
    assertSameFloat(+0.0f, Math.scalb(2.0f, -151))
  }
}
