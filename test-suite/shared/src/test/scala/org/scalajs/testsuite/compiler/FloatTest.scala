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
}
