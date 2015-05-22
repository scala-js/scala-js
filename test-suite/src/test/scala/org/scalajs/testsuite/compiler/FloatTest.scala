/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.compiler

import org.scalajs.jasminetest.JasmineTest

object FloatTest extends JasmineTest {

  def froundNotInlined(x: Double): Float = {
    val y = x // so you don't inline me
    y.toFloat
  }

  describe("Float") {

    it("toInt") {
      @inline
      def test(x: Float, expected: Int): Unit =
        expect(x.toInt).toEqual(expected)

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

  when("strict-floats").
  describe("Strict floats") {

    it("fround for special values") {
      expect(froundNotInlined(Double.NaN).isNaN).toBeTruthy
      expect(1 / froundNotInlined(0.0).toDouble).toBe(Double.PositiveInfinity)
      expect(1 / froundNotInlined(-0.0).toDouble).toBe(Double.NegativeInfinity)
      expect(froundNotInlined(Double.PositiveInfinity)).toBe(Float.PositiveInfinity)
      expect(froundNotInlined(Double.NegativeInfinity)).toBe(Float.NegativeInfinity)
    }

    it("fround overflows") {
      expect(froundNotInlined(1e200)).toBe(Double.PositiveInfinity)
      expect(froundNotInlined(-1e200)).toBe(Double.NegativeInfinity)
    }

    it("fround underflows") {
      expect(1 / froundNotInlined(1e-300).toDouble).toBe(Double.PositiveInfinity)
      expect(1 / froundNotInlined(-1e-300).toDouble).toBe(Double.NegativeInfinity)
    }

    it("fround normal cases") {
      def test(input: Double, expected: Double): Unit =
        expect(input.toFloat.toDouble).toBe(expected)

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
      test(2.705609035558863E20, 2.7056090763400262E20)
      test(-1.447710531503027E15, -1.447710532042752E15)
      test(-5.1970024617732836E13, -5.1970022834176E13)
      test(1.627661085098256E31, 1.6276610930768024E31)
      test(-3.7731947682593834E-32, -3.7731946313230934E-32)
      test(34.48229849163326, 34.4822998046875)
      test(26.62034396181652, 26.620344161987305)
      test(8.198435190113375E-24, 8.198434961596576E-24)
      test(-6.079928908440255E-23, -6.079928963558556E-23)
      test(3.3756949828462674E-13, 3.37569490589662E-13)
      test(-1.2599049874324274E33, -1.2599049641449257E33)
      test(6.08574575776438E-10, 6.085745796191588E-10)
      test(1.973497969450596E-21, 1.973498047135062E-21)
    }

  }
}
