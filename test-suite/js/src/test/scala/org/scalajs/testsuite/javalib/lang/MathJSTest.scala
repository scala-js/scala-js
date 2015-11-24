/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.lang

import org.scalajs.jasminetest.JasmineTest

import org.junit.Assert._

object MathJSTest extends JasmineTest {

  describe("java.lang.Math") {
    unless("rhino").// js.Math.round() is buggy on Rhino
    it("should_respond_to_rint(Double)") {
      import Math.rint

      def isPosZero(x: Double): Boolean =
        x == 0.0 && (1.0 / x) == Double.PositiveInfinity

      def isNegZero(x: Double): Boolean =
        x == 0.0 && (1.0 / x) == Double.NegativeInfinity

      // Specials
      assertTrue(isPosZero(rint(+0.0)))
      assertTrue(isNegZero(rint(-0.0)))
      assertEquals(Double.PositiveInfinity, rint(Double.PositiveInfinity))
      assertEquals(Double.NegativeInfinity, rint(Double.NegativeInfinity))
      assertTrue(rint(Double.NaN).isNaN)

      // Positive values
      assertTrue(isPosZero(rint(0.1)))
      assertTrue(isPosZero(rint(0.5)))
      assertEquals(1.0, rint(0.5000000000000001))
      assertEquals(1.0, rint(0.999))
      assertEquals(1.0, rint(1.4999999999999998))
      assertEquals(2.0, rint(1.5))
      assertEquals(2.0, rint(2.0))
      assertEquals(2.0, rint(2.1))
      assertEquals(2.0, rint(2.5))
      assertEquals(Double.MaxValue, rint(Double.MaxValue))
      assertEquals(4503599627370496.0, rint(4503599627370495.5)) // MaxSafeInt / 2

      // Negative values
      assertTrue(isNegZero(rint(-0.1)))
      assertTrue(isNegZero(rint(-0.5)))
      assertEquals(-1.0, rint(-0.5000000000000001))
      assertEquals(-1.0, rint(-0.999))
      assertEquals(-1.0, rint(-1.4999999999999998))
      assertEquals(-2.0, rint(-1.5))
      assertEquals(-2.0, rint(-2.0))
      assertEquals(-2.0, rint(-2.1))
      assertEquals(-2.0, rint(-2.5))
      assertEquals(Double.MinValue, rint(Double.MinValue))
      assertEquals(-4503599627370496.0, rint(-4503599627370495.5)) // -MaxSafeInt / 2
    }
  }
}
