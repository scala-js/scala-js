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

class FloatTest {
  final def assertExactEquals(expected: Float, actual: Float): Unit =
    assertTrue(s"expected: $expected; actual: $actual", expected.equals(actual))

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

    // If d is NaN, return NaN
    test(Float.NaN, Float.NaN, Float.NaN)
    test(Float.NaN, Float.PositiveInfinity, Float.NaN)
    test(Float.NaN, Float.NegativeInfinity, Float.NaN)
    test(Float.NaN, +0.0f, Float.NaN)
    test(Float.NaN, -0.0f, Float.NaN)
    test(Float.NaN, 2.1f, Float.NaN)
    test(Float.NaN, 5.5f, Float.NaN)
    test(Float.NaN, -151.189f, Float.NaN)

    // If n is PositiveInfinity, return NaN
    test(Float.NaN, Float.PositiveInfinity, Float.PositiveInfinity)
    test(Float.NaN, Float.PositiveInfinity, Float.NegativeInfinity)
    test(Float.NaN, Float.PositiveInfinity, +0.0f)
    test(Float.NaN, Float.PositiveInfinity, -0.0f)
    test(Float.NaN, Float.PositiveInfinity, 2.1f)
    test(Float.NaN, Float.PositiveInfinity, 5.5f)
    test(Float.NaN, Float.PositiveInfinity, -151.189f)

    // If n is NegativeInfinity, return NaN
    test(Float.NaN, Float.NegativeInfinity, Float.PositiveInfinity)
    test(Float.NaN, Float.NegativeInfinity, Float.NegativeInfinity)
    test(Float.NaN, Float.NegativeInfinity, +0.0f)
    test(Float.NaN, Float.NegativeInfinity, -0.0f)
    test(Float.NaN, Float.NegativeInfinity, 2.1f)
    test(Float.NaN, Float.NegativeInfinity, 5.5f)
    test(Float.NaN, Float.NegativeInfinity, -151.189f)

    // If d is PositiveInfinity, return n
    test(+0.0f, +0.0f, Float.PositiveInfinity)
    test(-0.0f, -0.0f, Float.PositiveInfinity)
    test(2.1f, 2.1f, Float.PositiveInfinity)
    test(5.5f, 5.5f, Float.PositiveInfinity)
    test(-151.189f, -151.189f, Float.PositiveInfinity)

    // If d is NegativeInfinity, return n
    test(+0.0f, +0.0f, Float.NegativeInfinity)
    test(-0.0f, -0.0f, Float.NegativeInfinity)
    test(2.1f, 2.1f, Float.NegativeInfinity)
    test(5.5f, 5.5f, Float.NegativeInfinity)
    test(-151.189f, -151.189f, Float.NegativeInfinity)

    // If d is +0.0, return NaN
    test(Float.NaN, +0.0f, +0.0f)
    test(Float.NaN, -0.0f, +0.0f)
    test(Float.NaN, 2.1f, +0.0f)
    test(Float.NaN, 5.5f, +0.0f)
    test(Float.NaN, -151.189f, +0.0f)

    // If d is -0.0, return NaN
    test(Float.NaN, +0.0f, -0.0f)
    test(Float.NaN, -0.0f, -0.0f)
    test(Float.NaN, 2.1f, -0.0f)
    test(Float.NaN, 5.5f, -0.0f)
    test(Float.NaN, -151.189f, -0.0f)

    // If n is +0.0, return n
    test(+0.0f, +0.0f, 2.1f)
    test(+0.0f, +0.0f, 5.5f)
    test(+0.0f, +0.0f, -151.189f)

    // If n is -0.0, return n
    test(-0.0f, -0.0f, 2.1f)
    test(-0.0f, -0.0f, 5.5f)
    test(-0.0f, -0.0f, -151.189f)

    // Non-special values
    // { val l = List(2.1f, 5.5f, -151.189f); for (n <- l; d <- l) println(s"      test(${n % d}f, ${n}f, ${d}f)") }
    test(0.0f, 2.1f, 2.1f)
    test(2.1f, 2.1f, 5.5f)
    test(2.1f, 2.1f, -151.189f)
    test(1.3000002f, 5.5f, 2.1f)
    test(0.0f, 5.5f, 5.5f)
    test(5.5f, 5.5f, -151.189f)
    test(-2.0890021f, -151.189f, 2.1f)
    test(-2.6889954f, -151.189f, 5.5f)
    test(-0.0f, -151.189f, -151.189f)
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
}
