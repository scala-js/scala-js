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

class DoubleTest {
  @Test
  def `toInt`(): Unit = {
    @inline
    def test(x: Double, expected: Int): Unit =
      assertEquals(expected, x.toInt)

    // Specials
    test(+0.0, 0)
    test(-0.0, 0)
    test(Double.PositiveInfinity, Int.MaxValue)
    test(Double.NegativeInfinity, Int.MinValue)
    test(Double.NaN, 0)

    // Positive numbers
    test(0.3, 0)
    test(0.7, 0)
    test(1.2, 1)
    test(5e12, Int.MaxValue)
    test(2147483646, 2147483646)
    test(2147483646.999, 2147483646)
    test(2147483512.546, 2147483512)
    test(65.67, 65)

    // Negative numbers
    test(-0.3, 0)
    test(-0.7, 0)
    test(-1.2, -1)
    test(-5e12, Int.MinValue)
    test(-2147483647.9999, -2147483647)
    test(-2147483565.123, -2147483565)
    test(-65.67, -65)
  }

  @Test
  def noReverseComparisons_issue3575(): Unit = {
    import Double.NaN

    @noinline def test_not_==(x: Double, y: Double): Boolean = !(x == y)
    @noinline def test_not_!=(x: Double, y: Double): Boolean = !(x != y)
    @noinline def test_not_<(x: Double, y: Double): Boolean = !(x < y)
    @noinline def test_not_<=(x: Double, y: Double): Boolean = !(x <= y)
    @noinline def test_not_>(x: Double, y: Double): Boolean = !(x > y)
    @noinline def test_not_>=(x: Double, y: Double): Boolean = !(x >= y)

    assertFalse(test_not_==(5, 5))
    assertTrue(test_not_==(5, 10))
    assertTrue(test_not_==(10, 5))
    assertTrue(test_not_==(5, NaN))
    assertTrue(test_not_==(NaN, NaN))
    assertFalse(test_not_==(0.0, -0.0))

    assertTrue(test_not_!=(5, 5))
    assertFalse(test_not_!=(5, 10))
    assertFalse(test_not_!=(10, 5))
    assertFalse(test_not_!=(5, NaN))
    assertFalse(test_not_!=(NaN, NaN))
    assertTrue(test_not_!=(0.0, -0.0))

    assertTrue(test_not_<(5, 5))
    assertFalse(test_not_<(5, 10))
    assertTrue(test_not_<(10, 5))
    assertTrue(test_not_<(5, NaN))
    assertTrue(test_not_<(NaN, NaN))
    assertTrue(test_not_<(0.0, -0.0))

    assertFalse(test_not_<=(5, 5))
    assertFalse(test_not_<=(5, 10))
    assertTrue(test_not_<=(10, 5))
    assertTrue(test_not_<=(5, NaN))
    assertTrue(test_not_<=(NaN, NaN))
    assertFalse(test_not_<=(0.0, -0.0))

    assertTrue(test_not_>(5, 5))
    assertTrue(test_not_>(5, 10))
    assertFalse(test_not_>(10, 5))
    assertTrue(test_not_>(5, NaN))
    assertTrue(test_not_>(NaN, NaN))
    assertTrue(test_not_>(0.0, -0.0))

    assertFalse(test_not_>=(5, 5))
    assertTrue(test_not_>=(5, 10))
    assertFalse(test_not_>=(10, 5))
    assertTrue(test_not_>=(5, NaN))
    assertTrue(test_not_>=(NaN, NaN))
    assertFalse(test_not_>=(0.0, -0.0))
  }
}
