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

import java.lang.Math

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform._

class MathTest {

  /** Like `assertEquals` with `delta = 0.0`, but positive and negative zeros
   *  compare not equal.
   */
  private def assertSameDouble(expected: Double, actual: Double): Unit =
    assertTrue(s"expected: $expected but was: $actual", expected.equals(actual))

  /** Like `assertEquals` with `delta = 0.0`, but positive and negative zeros
   *  compare not equal.
   */
  private def assertSameDouble(msg: String, expected: Double, actual: Double): Unit =
    assertTrue(s"$msg; expected: $expected but was: $actual", expected.equals(actual))

  /** Like `assertEquals` with `delta = 0.0f`, but positive and negative zeros
   *  compare not equal.
   */
  private def assertSameFloat(expected: Float, actual: Float): Unit =
    assertTrue(s"expected: $expected but was: $actual", expected.equals(actual))

  /** Like `assertEquals` with `delta = 0.0f`, but positive and negative zeros
   *  compare not equal.
   */
  private def assertSameFloat(msg: String, expected: Float, actual: Float): Unit =
    assertTrue(s"$msg; expected: $expected but was: $actual", expected.equals(actual))

  @Test def abs(): Unit = {
    assertSameDouble(0, Math.abs(0))
    assertSameDouble(0.0, Math.abs(-0.0))
    assertEquals(42, Math.abs(42))
    assertEquals(42, Math.abs(-42))
    assertTrue(Math.abs(0.0).equals(0.0))
    assertTrue(Math.abs(-0.0).equals(0.0))
    assertEquals(42.0, Math.abs(42.0), 0.0)
    assertEquals(42.0, Math.abs(-42.0), 0.0)
    assertEquals(Double.PositiveInfinity, Math.abs(Double.PositiveInfinity), 0.0)
    assertEquals(Double.PositiveInfinity, Math.abs(Double.NegativeInfinity), 0.0)
    assertTrue(Math.abs(Double.NaN).isNaN)
    assertEquals(Long.MaxValue, Math.abs(Long.MaxValue))
    assertEquals(Long.MinValue, Math.abs(Long.MinValue))
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

  @Test def cbrt(): Unit = {
    assertSameDouble(-0.0, Math.cbrt(-0.0))
    assertSameDouble(0.0, Math.cbrt(0.0))
    assertEquals(3.0, Math.cbrt(27.0), 0.0)
    assertEquals(100.0, Math.cbrt(1000000.0), 0.0)
    assertEquals(1000.0, Math.cbrt(1000000000.0), 0.0)
    assertEquals(-100000000.0, Math.cbrt(-1.0E24), 0.0)
    assertEquals(-4039.0E8, Math.cbrt(-65890311319.0E24), 0.0)
    assertTrue(Math.cbrt(Double.NaN).isNaN)
    assertSameDouble(Double.PositiveInfinity, Math.cbrt(Double.PositiveInfinity))
    assertSameDouble(Double.NegativeInfinity, Math.cbrt(Double.NegativeInfinity))
  }

  @Test def log1p(): Unit = {
    assertTrue(Math.log1p(-2.0).isNaN)
    assertTrue(Math.log1p(Double.NaN).isNaN)
    assertSameDouble(0.0, Math.log1p(0.0))
    assertSameDouble(-0.0, Math.log1p(-0.0))
    assertTrue(Math.log1p(Double.NaN).isNaN)
    assertSameDouble(Double.PositiveInfinity, Math.log1p(Double.PositiveInfinity))
    assertTrue(Math.log1p(Double.NegativeInfinity).isNaN)
    assertSameDouble(Double.NegativeInfinity, Math.log1p(-1))
  }

  @Test def log10(): Unit = {
    assertTrue(Math.log10(-230.0).isNaN)
    assertTrue(Math.log10(Double.NaN).isNaN)
    assertSameDouble(Double.NegativeInfinity, Math.log10(0.0))
    assertSameDouble(Double.NegativeInfinity, Math.log10(-0.0))
    assertTrue(Math.log10(Double.NaN).isNaN)
    assertSameDouble(Double.PositiveInfinity, Math.log10(Double.PositiveInfinity))
    assertTrue(Math.log10(Double.NegativeInfinity).isNaN)
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

  @Test def hypot(): Unit = {
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

  @Test def expm1(): Unit = {
    assertTrue(1 / Math.expm1(-0.0) < 0)
    assertTrue(1 / Math.expm1(0.0) > 0)
    assertSameDouble(-0.0, Math.expm1(-0.0))
    assertSameDouble(0.0, Math.expm1(0.0))
    assertEquals(19.085536923187668, Math.expm1(3.0), 0.01)
    assertEquals(3269016.3724721107, Math.expm1(15.0), 0.01)
    assertEquals(Double.PositiveInfinity, Math.expm1(1.8E10), 0.0)
    assertEquals(Double.PositiveInfinity, Math.expm1(Double.PositiveInfinity), 0.0)
    assertEquals(-1.0, Math.expm1(Double.NegativeInfinity), 0.01)
    assertEquals(4.9E-324, Math.expm1(4.9E-324), 0.01)
    assertTrue(Math.expm1(Double.NaN).isNaN)
  }

  @Test def sinh(): Unit = {
    assertEquals(Double.NegativeInfinity, Math.sinh(-1234.56), 0.0)
    assertEquals(Double.PositiveInfinity, Math.sinh(1234.56), 0.0)
    assertSameDouble(0.0, Math.sinh(0.0))
    assertSameDouble(-0.0, Math.sinh(-0.0))
    assertEquals(Double.PositiveInfinity, Math.sinh(Double.PositiveInfinity), 0.0)
    assertEquals(Double.NegativeInfinity, Math.sinh(Double.NegativeInfinity), 0.0)
    assertTrue(Math.sinh(Double.NaN).isNaN)
  }

  @Test def cosh(): Unit = {
    assertEquals(Double.PositiveInfinity, Math.cosh(-1234.56), 0.0)
    assertEquals(Double.PositiveInfinity, Math.cosh(1234.56), 0.0)
    assertEquals(1.0, Math.cosh(-0.0), 0.01)
    assertEquals(1.0, Math.cosh(0.0), 0.01)
    assertEquals(Double.PositiveInfinity, Math.cosh(Double.PositiveInfinity), 0.0)
    assertEquals(Double.PositiveInfinity, Math.cosh(Double.NegativeInfinity), 0.0)
    assertTrue(Math.cosh(Double.NaN).isNaN)
  }

  @Test def tanh(): Unit = {
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

    def isPosZero(x: Double): Boolean =
      x == 0.0 && (1.0 / x) == Double.PositiveInfinity

    def isNegZero(x: Double): Boolean =
      x == 0.0 && (1.0 / x) == Double.NegativeInfinity

    // Specials
    assertTrue(isPosZero(rint(+0.0)))
    assertTrue(isNegZero(rint(-0.0)))
    assertEquals(Double.PositiveInfinity, rint(Double.PositiveInfinity), 0.0)
    assertEquals(Double.NegativeInfinity, rint(Double.NegativeInfinity), 0.0)
    assertTrue(rint(Double.NaN).isNaN)

    // Positive values
    assertTrue(isPosZero(rint(0.1)))
    assertTrue(isPosZero(rint(0.5)))
    assertEquals(1.0, rint(0.5000000000000001), 0.0)
    assertEquals(1.0, rint(0.999), 0.0)
    assertEquals(1.0, rint(1.4999999999999998), 0.0)
    assertEquals(2.0, rint(1.5), 0.0)
    assertEquals(2.0, rint(2.0), 0.0)
    assertEquals(2.0, rint(2.1), 0.0)
    assertEquals(2.0, rint(2.5), 0.0)
    assertEquals(Double.MaxValue, rint(Double.MaxValue), 0.0)
    assertEquals(4503599627370496.0, rint(4503599627370495.5), 0.0) // MaxSafeInt / 2

    // Negative values
    assertTrue(isNegZero(rint(-0.1)))
    assertTrue(isNegZero(rint(-0.5)))
    assertEquals(-1.0, rint(-0.5000000000000001), 0.0)
    assertEquals(-1.0, rint(-0.999), 0.0)
    assertEquals(-1.0, rint(-1.4999999999999998), 0.0)
    assertEquals(-2.0, rint(-1.5), 0.0)
    assertEquals(-2.0, rint(-2.0), 0.0)
    assertEquals(-2.0, rint(-2.1), 0.0)
    assertEquals(-2.0, rint(-2.5), 0.0)
    assertEquals(Double.MinValue, rint(Double.MinValue), 0.0)
    assertEquals(-4503599627370496.0, rint(-4503599627370495.5), 0.0) // -MaxSafeInt / 2
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

    expectThrows(classOf[ArithmeticException], Math.addExact(Int.MinValue, -1))
    expectThrows(classOf[ArithmeticException], Math.addExact(-1, Int.MinValue))
    expectThrows(classOf[ArithmeticException], Math.addExact(Int.MinValue, Int.MinValue))
    expectThrows(classOf[ArithmeticException], Math.addExact(Int.MaxValue, 1))
    expectThrows(classOf[ArithmeticException], Math.addExact(1, Int.MaxValue))
    expectThrows(classOf[ArithmeticException], Math.addExact(Int.MaxValue, Int.MaxValue))
    expectThrows(classOf[ArithmeticException], Math.addExact(1073741824, 1073741824))

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

    expectThrows(classOf[ArithmeticException], Math.addExact(Long.MinValue, -1))
    expectThrows(classOf[ArithmeticException], Math.addExact(-1, Long.MinValue))
    expectThrows(classOf[ArithmeticException], Math.addExact(Long.MinValue, Long.MinValue))
    expectThrows(classOf[ArithmeticException], Math.addExact(Long.MaxValue, 1))
    expectThrows(classOf[ArithmeticException], Math.addExact(1, Long.MaxValue))
    expectThrows(classOf[ArithmeticException], Math.addExact(Long.MaxValue, Long.MaxValue))
    expectThrows(classOf[ArithmeticException], Math.addExact(4611686018427387904L, 4611686018427387904L))
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
    expectThrows(classOf[ArithmeticException], Math.subtractExact(0, Int.MinValue))
    expectThrows(classOf[ArithmeticException], Math.subtractExact(Int.MinValue, 1))
    expectThrows(classOf[ArithmeticException], Math.subtractExact(Int.MinValue, Int.MaxValue))
    expectThrows(classOf[ArithmeticException], Math.subtractExact(-2, Int.MaxValue))
    expectThrows(classOf[ArithmeticException], Math.subtractExact(Int.MaxValue, -1))
    expectThrows(classOf[ArithmeticException], Math.subtractExact(Int.MaxValue, Int.MinValue))
    expectThrows(classOf[ArithmeticException], Math.subtractExact(1073741824, -1073741824))

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

    expectThrows(classOf[ArithmeticException], Math.subtractExact(0, Long.MinValue))
    expectThrows(classOf[ArithmeticException], Math.subtractExact(Long.MinValue, 1))
    expectThrows(classOf[ArithmeticException], Math.subtractExact(Long.MinValue, Long.MaxValue))
    expectThrows(classOf[ArithmeticException], Math.subtractExact(Long.MinValue, 1))
    expectThrows(classOf[ArithmeticException], Math.subtractExact(-2, Long.MaxValue))
    expectThrows(classOf[ArithmeticException], Math.subtractExact(Long.MaxValue, -1))
    expectThrows(classOf[ArithmeticException], Math.subtractExact(Long.MaxValue, Long.MinValue))
    expectThrows(classOf[ArithmeticException], Math.subtractExact(4611686018427387904L, -4611686018427387904L))
  }

  @Test def multiplyExact(): Unit = {
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

    expectThrows(classOf[ArithmeticException], Math.multiplyExact(Int.MinValue, -1))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(-1, Int.MinValue))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(Int.MinValue, Int.MinValue))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(Int.MaxValue, Int.MaxValue))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(Int.MinValue, Int.MaxValue))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(Int.MaxValue, Int.MinValue))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(1073741824, 2))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(2, 1073741824))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(1073741825, -2))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(-2, 1073741825))

    for (n <- Seq(Long.MinValue, -1L, 0L, 1L, Long.MaxValue)) {
      assertEquals(0L, Math.multiplyExact(n, 0))
      assertEquals(0L, Math.multiplyExact(0, n))
      assertEquals(n, Math.multiplyExact(n, 1))
      assertEquals(n, Math.multiplyExact(1, n))
    }
    assertEquals(0L, Math.multiplyExact(Long.MinValue, 0))
    assertEquals(0L, Math.multiplyExact(0, Long.MinValue))
    assertEquals(Long.MaxValue, Math.multiplyExact(-9223372036854775807L, -1))
    assertEquals(Long.MaxValue, Math.multiplyExact(-1, -9223372036854775807L))
    assertEquals(9223372036854775806L, Math.multiplyExact(4611686018427387903L, 2))
    assertEquals(9223372036854775806L, Math.multiplyExact(2, 4611686018427387903L))
    assertEquals(Long.MinValue, Math.multiplyExact(4611686018427387904L, -2))
    assertEquals(Long.MinValue, Math.multiplyExact(-2, 4611686018427387904L))

    expectThrows(classOf[ArithmeticException], Math.multiplyExact(Long.MinValue, -1))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(-1, Long.MinValue))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(Long.MinValue, Long.MinValue))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(Long.MaxValue, Long.MaxValue))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(Long.MinValue, Long.MaxValue))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(Long.MaxValue, Long.MinValue))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(4611686018427387904L, 2))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(2, 4611686018427387904L))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(4611686018427387905L, -2))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(-2, 4611686018427387905L))
  }

  @Test def incrementExact(): Unit = {
    assertEquals(Int.MaxValue, Math.incrementExact(Int.MaxValue - 1))
    assertEquals(Long.MaxValue, Math.incrementExact(Long.MaxValue - 1))

    expectThrows(classOf[ArithmeticException], Math.incrementExact(Int.MaxValue))
    expectThrows(classOf[ArithmeticException], Math.incrementExact(Long.MaxValue))
  }

  @Test def decrementExact(): Unit = {
    assertEquals(Int.MinValue, Math.decrementExact(Int.MinValue + 1))
    assertEquals(Long.MinValue, Math.decrementExact(Long.MinValue + 1))

    expectThrows(classOf[ArithmeticException], Math.decrementExact(Int.MinValue))
    expectThrows(classOf[ArithmeticException], Math.decrementExact(Long.MinValue))
  }

  @Test def negateExact(): Unit = {
    assertEquals(Int.MaxValue, Math.negateExact(Int.MinValue + 1))
    assertEquals(Int.MinValue + 1, Math.negateExact(Int.MaxValue))
    assertEquals(Long.MaxValue, Math.negateExact(Long.MinValue + 1))
    assertEquals(Long.MinValue + 1, Math.negateExact(Long.MaxValue))

    expectThrows(classOf[ArithmeticException], Math.negateExact(Int.MinValue))
    expectThrows(classOf[ArithmeticException], Math.negateExact(Long.MinValue))
  }

  @Test def toIntExact(): Unit = {
    assertEquals(Int.MinValue, Math.toIntExact(-2147483648L))
    assertEquals(Int.MaxValue, Math.toIntExact(2147483647L))

    expectThrows(classOf[ArithmeticException], Math.toIntExact(-2147483649L))
    expectThrows(classOf[ArithmeticException], Math.toIntExact(2147483648L))
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

    // Random values
    assertSameFloat(9007198700000000.0f, Math.nextDown(9007199300000000.0f))
    assertSameFloat(0.99999994f, Math.nextDown(1.0f))
  }
}
