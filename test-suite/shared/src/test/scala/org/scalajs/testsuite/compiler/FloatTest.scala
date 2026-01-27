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

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertExtensions.assertExactEquals

class FloatTest {
  @noinline def froundNotInlined(x: Double): Float =
    x.toFloat

  @Test
  def `toInt`(): Unit = {
    @inline
    def test(x: Float, expected: Int): Unit =
      assertEquals(x.toInt, expected)

    // Specials
    test(+0.0f, 0)
    test(-0.0f, 0)
    test(Float.PositiveInfinity, Int.MaxValue)
    test(Float.NegativeInfinity, Int.MinValue)
    test(Float.NaN, 0)

    // Positive numbers
    test(0.3f, 0)
    test(0.7f, 0)
    test(1.2f, 1)
    test(5e12f, Int.MaxValue)
    test(2147483646f, 2147483647)
    test(2147483500f, 2147483520)
    test(65.67f, 65)

    // Negative numbers
    test(-0.3f, 0)
    test(-0.7f, 0)
    test(-1.2f, -1)
    test(-5e12f, Int.MinValue)
    test(-2147483646f, -2147483648)
    test(-2147483500f, -2147483520)
    test(-65.67f, -65)
  }

  @Test
  def differentialTestDivide(): Unit = {
    /* The optimizer rewrites divisions by powers of 2 to multiplications.
     * Test that it does not do anything wrong with them with some sort of
     * differntial testing.
     */

    import Float.{NaN, MinPositiveValue, PositiveInfinity, NegativeInfinity}
    import java.lang.Float.{MIN_NORMAL => MinNormal}

    val numerators: List[Float] = List(
      NaN,
      PositiveInfinity,
      NegativeInfinity,
      +0.0f,
      -0.0f,
      1.0f,
      -1.0f,
      1.2564f,
      -6.54321f,
      MinPositiveValue,
      -MinPositiveValue,
      MinNormal,
      -MinNormal,
      1.1754942e-38f, // largest subnormal
      -1.1754942e-38f,
      1.7299e-41f, // some subnormal
      -1.7299e-41f,
      Float.MaxValue,
      Float.MinValue,
      2.2265356e38f, // value close to the max value
      -2.2265356e38f
    )

    @noinline def hide(x: Float): Float = x

    @inline
    def test(n: Float, d: Float): Unit =
      assertExactEquals(n / hide(d), n / d)

    for (n <- numerators) {
      // Specials
      test(n, NaN)
      test(n, PositiveInfinity)
      test(n, NegativeInfinity)
      test(n, +0.0f)
      test(n, -0.0f)

      // Powers of 2 whose inverse is representable
      test(n, 1.0f)
      test(n, -1.0f)
      test(n, 2.0f)
      test(n, -2.0f)
      test(n, 0.5f)
      test(n, -0.5f)
      test(n, MinNormal)
      test(n, -MinNormal)
      test(n, 256 * MinNormal)
      test(n, -256 * MinNormal)
      test(n, 0.5f * MinNormal) // smallest power of 2 whose inverse is representable
      test(n, -0.5f * MinNormal)
      test(n, 1.7014118e38f) // largest power of 2
      test(n, -1.7014118e38f)
      test(n, 1.329228e36f)
      test(n, -1.329228e36f)

      // Powers of 2 whose inverse is not representable
      test(n, MinPositiveValue)
      test(n, -MinPositiveValue)
      test(n, 256 * MinPositiveValue)
      test(n, -256 * MinPositiveValue)
      test(n, 0.25f * MinNormal) // largest power of 2 whose inverse is not representable
      test(n, -0.25f * MinNormal)

      // Non-powers of 2
      test(n, 1.2564f)
      test(n, -6.54321f)
      test(n, 1.1754942e-38f) // largest subnormal
      test(n, -1.1754942e-38f)
      test(n, 1.7299e-41f) // some subnormal
      test(n, -1.7299e-41f)
      test(n, Float.MaxValue)
      test(n, -Float.MaxValue)
      test(n, 2.2265356e38f) // value close to the max value
      test(n, -2.2265356e38f)
    }
  }

  @Test
  def testRemainder(): Unit = {
    /* Float `%` is atypical. It does not correspond to the IEEE-754 notion
     * of remainder/modulo. Instead, it correspond to the common math function
     * `fmod`. Therefore, we have dedicated tests for it, to make sure that
     * our platforms agree on the semantics. They are not much, but they are
     * enough to rule out the naive formula that can sometimes be found on the
     * Web, namely `x - trunc(x / y) * y`.
     */

    def test(expected: Float, x: Float, y: Float): Unit =
      assertExactEquals(expected, x % y)

    // If n is NaN, return NaN
    test(Float.NaN, Float.NaN, Float.NaN)
    test(Float.NaN, Float.NaN, Float.PositiveInfinity)
    test(Float.NaN, Float.NaN, Float.NegativeInfinity)
    test(Float.NaN, Float.NaN, +0.0f)
    test(Float.NaN, Float.NaN, -0.0f)
    test(Float.NaN, Float.NaN, 2.1f)
    test(Float.NaN, Float.NaN, 5.5f)
    test(Float.NaN, Float.NaN, -151.189f)
    test(Float.NaN, Float.NaN, 8.858e-42f)
    test(Float.NaN, Float.NaN, 6.39164e-40f)

    // If d is NaN, return NaN
    test(Float.NaN, Float.NaN, Float.NaN)
    test(Float.NaN, Float.PositiveInfinity, Float.NaN)
    test(Float.NaN, Float.NegativeInfinity, Float.NaN)
    test(Float.NaN, +0.0f, Float.NaN)
    test(Float.NaN, -0.0f, Float.NaN)
    test(Float.NaN, 2.1f, Float.NaN)
    test(Float.NaN, 5.5f, Float.NaN)
    test(Float.NaN, -151.189f, Float.NaN)
    test(Float.NaN, 8.858e-42f, Float.NaN)
    test(Float.NaN, 6.39164e-40f, Float.NaN)

    // If n is PositiveInfinity, return NaN
    test(Float.NaN, Float.PositiveInfinity, Float.PositiveInfinity)
    test(Float.NaN, Float.PositiveInfinity, Float.NegativeInfinity)
    test(Float.NaN, Float.PositiveInfinity, +0.0f)
    test(Float.NaN, Float.PositiveInfinity, -0.0f)
    test(Float.NaN, Float.PositiveInfinity, 2.1f)
    test(Float.NaN, Float.PositiveInfinity, 5.5f)
    test(Float.NaN, Float.PositiveInfinity, -151.189f)
    test(Float.NaN, Float.PositiveInfinity, 8.858e-42f)
    test(Float.NaN, Float.PositiveInfinity, 6.39164e-40f)

    // If n is NegativeInfinity, return NaN
    test(Float.NaN, Float.NegativeInfinity, Float.PositiveInfinity)
    test(Float.NaN, Float.NegativeInfinity, Float.NegativeInfinity)
    test(Float.NaN, Float.NegativeInfinity, +0.0f)
    test(Float.NaN, Float.NegativeInfinity, -0.0f)
    test(Float.NaN, Float.NegativeInfinity, 2.1f)
    test(Float.NaN, Float.NegativeInfinity, 5.5f)
    test(Float.NaN, Float.NegativeInfinity, -151.189f)
    test(Float.NaN, Float.NegativeInfinity, 8.858e-42f)
    test(Float.NaN, Float.NegativeInfinity, 6.39164e-40f)

    // If d is PositiveInfinity, return n
    test(+0.0f, +0.0f, Float.PositiveInfinity)
    test(-0.0f, -0.0f, Float.PositiveInfinity)
    test(2.1f, 2.1f, Float.PositiveInfinity)
    test(5.5f, 5.5f, Float.PositiveInfinity)
    test(-151.189f, -151.189f, Float.PositiveInfinity)
    test(8.858e-42f, 8.858e-42f, Float.PositiveInfinity)
    test(6.39164e-40f, 6.39164e-40f, Float.PositiveInfinity)

    // If d is NegativeInfinity, return n
    test(+0.0f, +0.0f, Float.NegativeInfinity)
    test(-0.0f, -0.0f, Float.NegativeInfinity)
    test(2.1f, 2.1f, Float.NegativeInfinity)
    test(5.5f, 5.5f, Float.NegativeInfinity)
    test(-151.189f, -151.189f, Float.NegativeInfinity)
    test(8.858e-42f, 8.858e-42f, Float.NegativeInfinity)
    test(6.39164e-40f, 6.39164e-40f, Float.NegativeInfinity)

    // If d is +0.0, return NaN
    test(Float.NaN, +0.0f, +0.0f)
    test(Float.NaN, -0.0f, +0.0f)
    test(Float.NaN, 2.1f, +0.0f)
    test(Float.NaN, 5.5f, +0.0f)
    test(Float.NaN, -151.189f, +0.0f)
    test(Float.NaN, 8.858e-42f, +0.0f)
    test(Float.NaN, 6.39164e-40f, +0.0f)

    // If d is -0.0, return NaN
    test(Float.NaN, +0.0f, -0.0f)
    test(Float.NaN, -0.0f, -0.0f)
    test(Float.NaN, 2.1f, -0.0f)
    test(Float.NaN, 5.5f, -0.0f)
    test(Float.NaN, -151.189f, -0.0f)
    test(Float.NaN, 8.858e-42f, -0.0f)
    test(Float.NaN, 6.39164e-40f, -0.0f)

    // If n is +0.0, return n
    test(+0.0f, +0.0f, 2.1f)
    test(+0.0f, +0.0f, 5.5f)
    test(+0.0f, +0.0f, -151.189f)
    test(+0.0f, +0.0f, 8.858e-42f)
    test(+0.0f, +0.0f, 6.39164e-40f)

    // If n is -0.0, return n
    test(-0.0f, -0.0f, 2.1f)
    test(-0.0f, -0.0f, 5.5f)
    test(-0.0f, -0.0f, -151.189f)
    test(-0.0f, -0.0f, 8.858e-42f)
    test(-0.0f, -0.0f, 6.39164e-40f)

    // Non-special values
    // { val l = List(2.1f, 5.5f, -151.189f, 8.858e-42f, 6.39164e-40f);
    //   for (n <- l; d <- l) println(s"    test(${n % d}f, ${n}f, ${d}f)".toLowerCase()) }
    test(0.0f, 2.1f, 2.1f)
    test(2.1f, 2.1f, 5.5f)
    test(2.1f, 2.1f, -151.189f)
    test(8.085e-42f, 2.1f, 8.858e-42f)
    test(6.1636e-40f, 2.1f, 6.39164e-40f)
    test(1.3000002f, 5.5f, 2.1f)
    test(0.0f, 5.5f, 5.5f)
    test(5.5f, 5.5f, -151.189f)
    test(5.11e-43f, 5.5f, 8.858e-42f)
    test(4.77036e-40f, 5.5f, 6.39164e-40f)
    test(-2.0890021f, -151.189f, 2.1f)
    test(-2.6889954f, -151.189f, 5.5f)
    test(-0.0f, -151.189f, -151.189f)
    test(-1.139e-42f, -151.189f, 8.858e-42f)
    test(-5.64734e-40f, -151.189f, 6.39164e-40f)
    test(8.858e-42f, 8.858e-42f, 2.1f)
    test(8.858e-42f, 8.858e-42f, 5.5f)
    test(8.858e-42f, 8.858e-42f, -151.189f)
    test(0.0f, 8.858e-42f, 8.858e-42f)
    test(8.858e-42f, 8.858e-42f, 6.39164e-40f)
    test(6.39164e-40f, 6.39164e-40f, 2.1f)
    test(6.39164e-40f, 6.39164e-40f, 5.5f)
    test(6.39164e-40f, 6.39164e-40f, -151.189f)
    test(1.417e-42f, 6.39164e-40f, 8.858e-42f)
    test(0.0f, 6.39164e-40f, 6.39164e-40f)
  }

  @Test
  def noReverseComparisons_Issue3575(): Unit = {
    import Float.NaN

    @noinline def test_not_==(x: Float, y: Float): Boolean = !(x == y)
    @noinline def test_not_!=(x: Float, y: Float): Boolean = !(x != y)
    @noinline def test_not_<(x: Float, y: Float): Boolean = !(x < y)
    @noinline def test_not_<=(x: Float, y: Float): Boolean = !(x <= y)
    @noinline def test_not_>(x: Float, y: Float): Boolean = !(x > y)
    @noinline def test_not_>=(x: Float, y: Float): Boolean = !(x >= y)

    assertFalse(test_not_==(5, 5))
    assertTrue(test_not_==(5, 10))
    assertTrue(test_not_==(10, 5))
    assertTrue(test_not_==(5, NaN))
    assertTrue(test_not_==(NaN, NaN))
    assertFalse(test_not_==(0.0f, -0.0f))

    assertTrue(test_not_!=(5, 5))
    assertFalse(test_not_!=(5, 10))
    assertFalse(test_not_!=(10, 5))
    assertFalse(test_not_!=(5, NaN))
    assertFalse(test_not_!=(NaN, NaN))
    assertTrue(test_not_!=(0.0f, -0.0f))

    assertTrue(test_not_<(5, 5))
    assertFalse(test_not_<(5, 10))
    assertTrue(test_not_<(10, 5))
    assertTrue(test_not_<(5, NaN))
    assertTrue(test_not_<(NaN, NaN))
    assertTrue(test_not_<(0.0f, -0.0f))

    assertFalse(test_not_<=(5, 5))
    assertFalse(test_not_<=(5, 10))
    assertTrue(test_not_<=(10, 5))
    assertTrue(test_not_<=(5, NaN))
    assertTrue(test_not_<=(NaN, NaN))
    assertFalse(test_not_<=(0.0f, -0.0f))

    assertTrue(test_not_>(5, 5))
    assertTrue(test_not_>(5, 10))
    assertFalse(test_not_>(10, 5))
    assertTrue(test_not_>(5, NaN))
    assertTrue(test_not_>(NaN, NaN))
    assertTrue(test_not_>(0.0f, -0.0f))

    assertFalse(test_not_>=(5, 5))
    assertTrue(test_not_>=(5, 10))
    assertFalse(test_not_>=(10, 5))
    assertTrue(test_not_>=(5, NaN))
    assertTrue(test_not_>=(NaN, NaN))
    assertFalse(test_not_>=(0.0f, -0.0f))
  }

  @Test
  def negate_Issue4034(): Unit = {
    @noinline
    def testNoInline(expected: Float, x: Float): Unit = {
      assertExactEquals(expected, -x)
      assertExactEquals(expected, -1.0f * x)
    }

    @inline
    def test(expected: Float, x: Float): Unit = {
      testNoInline(expected, x)
      assertExactEquals(expected, -x)
      assertExactEquals(expected, -1f * x)
    }

    test(-0.0f, 0.0f)
    test(0.0f, -0.0f)
    test(Float.NaN, Float.NaN)
    test(Float.NegativeInfinity, Float.PositiveInfinity)
    test(Float.PositiveInfinity, Float.NegativeInfinity)

    test(-1.5f, 1.5f)
    test(567.89f, -567.89f)
  }

  @Test
  def noWrongSimplifications(): Unit = {
    @noinline
    def hide(x: Float): Float = x

    @inline
    def negate(x: Float): Float = -x

    assertExactEquals(0.8f, (hide(0.1f) + 0.3f) + 0.4f)
    assertExactEquals(0.8000001f, 0.1f + (0.3f + hide(0.4f)))

    assertExactEquals(0.0f, 0.0f + hide(-0.0f))
    assertExactEquals(0.0f, 0.0f - hide(0.0f))

    assertExactEquals(0.0f, negate(negate(hide(0.0f))))
    assertExactEquals(-0.0f, negate(negate(hide(-0.0f))))
  }

  @Test def froundForSpecialValues(): Unit = {
    assertTrue(froundNotInlined(Double.NaN).isNaN)
    assertEquals(Double.PositiveInfinity, 1 / froundNotInlined(0.0).toDouble, 0.0)
    assertEquals(Double.NegativeInfinity, 1 / froundNotInlined(-0.0).toDouble, 0.0)
    assertEquals(Float.PositiveInfinity, froundNotInlined(Double.PositiveInfinity), 0.0)
    assertEquals(Float.NegativeInfinity, froundNotInlined(Double.NegativeInfinity), 0.0)
  }

  @Test def froundOverflows(): Unit = {
    assertEquals(Double.PositiveInfinity, froundNotInlined(1e200), 0.0)
    assertEquals(Double.NegativeInfinity, froundNotInlined(-1e200), 0.0)
  }

  @Test def froundUnderflows(): Unit = {
    assertEquals(Double.PositiveInfinity, 1 / froundNotInlined(1e-300).toDouble, 0.0)
    assertEquals(Double.NegativeInfinity, 1 / froundNotInlined(-1e-300).toDouble, 0.0)
  }

  @Test def froundNormalCases(): Unit = {
    @inline
    def test(input: Double, expected: Double): Unit = {
      assertExactEquals(expected, input.toFloat.toDouble)
      assertExactEquals(expected, froundNotInlined(input).toDouble)
    }

    // From MDN documentation
    test(0.0, 0.0)
    test(1.0, 1.0)
    test(1.5, 1.5)
    test(1.337, 1.3370000123977661)
    test(-4.3, -4.300000190734863)

    // Some bounds
    test(Float.MinPositiveValue.toDouble, Float.MinPositiveValue.toDouble)
    test(Float.MaxValue.toDouble, Float.MaxValue.toDouble)
    test(Float.MinValue.toDouble, Float.MinValue.toDouble)

    // Randomly generated Doubles
    test(2.705609035558863e20, 2.7056090763400262e20)
    test(-1.447710531503027e15, -1.447710532042752e15)
    test(-5.1970024617732836e13, -5.1970022834176e13)
    test(1.627661085098256e31, 1.6276610930768024e31)
    test(-3.7731947682593834e-32, -3.7731946313230934e-32)
    test(34.48229849163326, 34.4822998046875)
    test(26.62034396181652, 26.620344161987305)
    test(8.198435190113375e-24, 8.198434961596576e-24)
    test(-6.079928908440255e-23, -6.079928963558556e-23)
    test(3.3756949828462674e-13, 3.37569490589662e-13)
    test(-1.2599049874324274e33, -1.2599049641449257e33)
    test(6.08574575776438e-10, 6.085745796191588e-10)
    test(1.973497969450596e-21, 1.973498047135062e-21)
  }

  @Test def intWidenedToFloatWhenComparingToFloat_Issue1878(): Unit = {
    val intMax: Int = Int.MaxValue
    val float: Float = (Int.MaxValue - 1).toFloat

    assertTrue(intMax == float)
    assertFalse(intMax != float)
    assertFalse(intMax < float)
    assertTrue(intMax <= float)
    assertFalse(intMax > float)
    assertTrue(intMax >= float)

    assertTrue(float == intMax)
    assertFalse(float != intMax)
    assertFalse(float < intMax)
    assertTrue(float <= intMax)
    assertFalse(float > intMax)
    assertTrue(float >= intMax)
  }
}
