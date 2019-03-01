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
  def noReverseComparisons_issue3575(): Unit = {
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
}
