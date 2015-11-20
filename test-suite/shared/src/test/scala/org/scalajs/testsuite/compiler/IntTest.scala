/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.compiler

import org.junit.Test
import org.junit.Assert._

/* General note on the way these tests are written:
 * We leverage the constant folding applied by the Scala compiler to write
 * sound tests. We always perform the same operation, on the same operands,
 * once in a way constant folding understands, and once in a way it doesn't.
 * Since constant folding is performed on the JVM, we know it has the right
 * semantics.
 */

class IntTest {
  import IntTest._

  @Test def `should_support_unary_minus`(): Unit = {
    def test(a: Int, expected: Int): Unit =
      assertEquals(expected, -a)

    test(56, -56)
    test(0, 0)
    test(-36, 36)

    test(MaxVal, -MaxVal)
    test(MinVal, -MinVal)
    test(-MaxVal, MaxVal)
    test(AlmostMinVal, -AlmostMinVal)
    test(AlmostMaxVal, -AlmostMaxVal)
  }

  @Test def `should_support_plus`(): Unit = {
    def test(a: Int, b: Int, expected: Int): Unit =
      assertEquals(expected, a + b)

    test(56, 654, 56 + 654)
    test(0, 25, 0 + 25)
    test(-36, 13, -36 + 13)

    test(MaxVal, 1, MaxVal + 1)
    test(MinVal, -1, MinVal - 1)
    test(MaxVal, MinVal, MaxVal + MinVal)
    test(AlmostMinVal, -100, AlmostMinVal - 100)
    test(AlmostMaxVal, 123, AlmostMaxVal + 123)
  }

  @Test def `should_support_minus`(): Unit = {
    def test(a: Int, b: Int, expected: Int): Unit =
      assertEquals(expected, a - b)

    test(56, 654, 56 - 654)
    test(0, 25, 0 - 25)
    test(-36, 13, -36 - 13)

    test(MaxVal, -1, MaxVal + 1)
    test(MinVal, 1, MinVal - 1)
    test(MaxVal, MinVal, MaxVal - MinVal)
    test(AlmostMinVal, 100, AlmostMinVal - 100)
    test(AlmostMaxVal, -123, AlmostMaxVal + 123)
  }

  @Test def `should_support_times`(): Unit = {
    def test(a: Int, b: Int, expected: Int): Unit =
      assertEquals(expected, a * b)

    test(56, 654, 56 * 654)
    test(0, 25, 0 * 25)
    test(-36, 13, -36 * 13)
    test(-5, -6, -5 * -6)

    test(MinVal, 1, MinVal * 1)
    test(MinVal, -1, MinVal * -1)
    test(MaxVal, 1, MaxVal * 1)
    test(MaxVal, -1, MaxVal * -1)

    test(MaxVal, MinVal, MaxVal * MinVal)
    test(MaxVal, MaxVal, MaxVal * MaxVal)
    test(MinVal, MaxVal, MinVal * MaxVal)
    test(MinVal, MinVal, MinVal * MinVal)

    test(AlmostMaxVal, 2, AlmostMaxVal * 2)
    test(AlmostMaxVal, 5, AlmostMaxVal * 5)
    test(AlmostMaxVal, -7, AlmostMaxVal * -7)
    test(AlmostMaxVal, -14, AlmostMaxVal * -14)
    test(AlmostMinVal, 100, AlmostMinVal * 100)
    test(AlmostMaxVal, -123, AlmostMaxVal * -123)
  }

  @Test def `should_support_division`(): Unit = {
    def test(a: Int, b: Int, expected: Int): Unit =
      assertEquals(expected, a / b)

    test(654, 56, 654 / 56)
    test(0, 25, 0 / 25)
    test(-36, 13, -36 / 13)
    test(-55, -6, -55 / -6)

    test(MinVal, 1, MinVal / 1)
    test(MinVal, -1, MinVal / -1)
    test(MaxVal, 1, MaxVal / 1)
    test(MaxVal, -1, MaxVal / -1)

    test(MaxVal, MinVal, MaxVal / MinVal)
    test(MaxVal, MaxVal, MaxVal / MaxVal)
    test(MinVal, MaxVal, MinVal / MaxVal)
    test(MinVal, MinVal, MinVal / MinVal)

    test(AlmostMaxVal, 2, AlmostMaxVal / 2)
    test(AlmostMaxVal, 5, AlmostMaxVal / 5)
    test(AlmostMaxVal, -7, AlmostMaxVal / -7)
    test(AlmostMaxVal, -14, AlmostMaxVal / -14)
    test(AlmostMinVal, 100, AlmostMinVal / 100)
    test(AlmostMaxVal, -123, AlmostMaxVal / -123)
  }

  @Test def `percent_should_never_produce_a_negative_0_#1984`(): Unit = {
    @noinline def value: Int = -8
    assertTrue((value % 8).asInstanceOf[java.lang.Integer].equals(0))
  }

  @Test def `should_support_shift_left`(): Unit = {
    def test(a: Int, b: Int, expected: Int): Unit =
      assertEquals(expected, a << b)

    test(0, 5, 0 << 5)
    test(1, 5, 1 << 5)
    test(13, 4, 13 << 4)
    test(-35, 5, -35 << 5)
    test(345, 0, 345 << 0)

    test(MinVal, 0, MinVal << 0)
    test(MaxVal, 0, MaxVal << 0)
    test(MinVal, 1, MinVal << 1)
    test(MaxVal, 1, MaxVal << 1)
  }

  @Test def `should_support_shift_right`(): Unit = {
    def test(a: Int, b: Int, expected: Int): Unit =
      assertEquals(expected, a >> b)

    test(0, 5, 0 >> 5)
    test(32, 5, 32 >> 5)
    test(31, 4, 31 >> 4)
    test(-355, 5, -355 >> 5)
    test(345, 0, 345 >> 0)

    test(MinVal, 0, MinVal >> 0)
    test(MaxVal, 0, MaxVal >> 0)
    test(MinVal, 1, MinVal >> 1)
    test(MaxVal, 1, MaxVal >> 1)
  }

  @Test def `should_support_shift_right_sign_extend`(): Unit = {
    def test(a: Int, b: Int, expected: Int): Unit =
      assertEquals(expected, a >>> b)

    test(0, 5, 0 >>> 5)
    test(32, 5, 32 >>> 5)
    test(31, 4, 31 >>> 4)
    test(-355, 5, -355 >>> 5)
    test(345, 0, 345 >>> 0)

    test(MinVal, 0, MinVal >>> 0)
    test(MaxVal, 0, MaxVal >>> 0)
    test(MinVal, 1, MinVal >>> 1)
    test(MaxVal, 1, MaxVal >>> 1)
  }
}

object IntTest {

  // final val without type ascription to make sure these are constant-folded
  final val MinVal = Int.MinValue
  final val MaxVal = Int.MaxValue
  final val AlmostMinVal = Int.MinValue + 43
  final val AlmostMaxVal = Int.MaxValue - 36
}
