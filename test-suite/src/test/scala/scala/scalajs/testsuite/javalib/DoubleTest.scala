/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.testsuite.javalib

import org.scalajs.jasminetest.JasmineTest
import scala.scalajs.js

import java.lang.{Double => JDouble}

import scala.util.Try

object DoubleTest extends JasmineTest {

  describe("java.lang.Double") {

    it("should provide proper `equals`") {
      expect(Double.box(0.0) == Double.box(-0.0)).toBeTruthy
      expect(Double.box(Double.NaN) == Double.box(Double.NaN)).toBeTruthy
    }

    it("hashCode") {
      def hashCodeNotInlined(x: Any): Int = {
        var y = x // do not inline
        y.hashCode
      }

      def test(x: Double, expected: Int): Unit = {
        expect(x.hashCode).toEqual(expected)
        expect(hashCodeNotInlined(x)).toEqual(expected)
      }

      test(0.0, 0)
      test(-0.0, 0)
      test(1234.0, 1234)
      test(1.5, 1073217536)
      test(Math.PI, 340593891)
      test(-54.0, -54)

      test(Double.MinPositiveValue, 1)
      test(Double.MinValue, 1048576)
      test(Double.MaxValue, -2146435072)

      test(Double.NaN, 2146959360)
      test(Double.PositiveInfinity, 2146435072)
      test(Double.NegativeInfinity, -1048576)
    }

    it("should provide `toString` with integer values when an integer") {
      expect(0.0.toString).toEqual("0")
      expect(-0.0.toString).toEqual("0")
      expect(Double.NaN.toString).toEqual("NaN")
      expect(Double.PositiveInfinity.toString).toEqual("Infinity")
      expect(Double.NegativeInfinity.toString).toEqual("-Infinity")
      expect(5.0.toString).toEqual("5")
      expect(-5.0.toString).toEqual("-5")
      expect(1.2.toString).toEqual("1.2")
    }

    it("should parse strings") {
      expect("0.0".toDouble).toEqual(0.0f)
      expect("NaN".toDouble.isNaN).toBeTruthy
      expect(Try("asdf".toDouble).isFailure).toBeTruthy

      def test(s: String, v: Double): Unit = {
        expect(JDouble.parseDouble(s)).toBeCloseTo(v)
        expect(JDouble.valueOf(s).doubleValue()).toBeCloseTo(v)
        expect(new JDouble(s).doubleValue()).toBeCloseTo(v)
      }

      test("0", 0.0)
      test("5.3", 5.3)
      test("127e2", 12700.0)
      test("127E-2", 1.27)
      test("1E+1", 10)
      test("-123.4", -123.4)
      test("65432.1", 65432.10)
      test("-87654.321", -87654.321)
      test("+.3f", 0.3)
    }

    it("should reject invalid strings when parsing") {
      def test(s: String): Unit =
        expect(() => JDouble.parseDouble(s)).toThrow

      test("4.3.5")
      test("4e3.5")
      test("hello world")
      test("--4")
      test("4E-3.2")
    }

    it("should provide `compareTo`") {
      def compare(x: Double, y: Double): Int =
        new JDouble(x).compareTo(new JDouble(y))

      expect(compare(0.0, 5.5)).toBeLessThan(0)
      expect(compare(10.5, 10.2)).toBeGreaterThan(0)
      expect(compare(-2.1, -1.0)).toBeLessThan(0)
      expect(compare(3.14, 3.14)).toEqual(0)

      // From compareTo's point of view, NaN is equal to NaN
      expect(compare(Double.NaN, Double.NaN)).toEqual(0)
    }

    it("should be a Comparable") {
      def compare(x: Any, y: Any): Int =
        x.asInstanceOf[Comparable[Any]].compareTo(y)

      expect(compare(0.0, 5.5)).toBeLessThan(0)
      expect(compare(10.5, 10.2)).toBeGreaterThan(0)
      expect(compare(-2.1, -1.0)).toBeLessThan(0)
      expect(compare(3.14, 3.14)).toEqual(0)

      // From compareTo's point of view, NaN is equal to NaN
      expect(compare(Double.NaN, Double.NaN)).toEqual(0)
    }

    it("should provide isInfinite - #515") {
      expect(Double.PositiveInfinity.isInfinite).toBeTruthy
      expect(Double.NegativeInfinity.isInfinite).toBeTruthy
      expect((1.0/0).isInfinite).toBeTruthy
      expect((-1.0/0).isInfinite).toBeTruthy
      expect((0.0).isInfinite).toBeFalsy
    }

    it("isNaN") {
      def f(v: Double): Boolean = {
        var v2 = v // do not inline
        v2.isNaN
      }

      expect(f(Double.NaN)).toBeTruthy

      expect(f(Double.PositiveInfinity)).toBeFalsy
      expect(f(Double.NegativeInfinity)).toBeFalsy
      expect(f(1.0 / 0)).toBeFalsy
      expect(f(-1.0 / 0)).toBeFalsy
      expect(f(0.0)).toBeFalsy
      expect(f(3.0)).toBeFalsy
      expect(f(-1.5)).toBeFalsy
    }

    it("longBitsToDouble") {
      def isZero(v: Double, neg: Boolean): Boolean = {
        (v == 0.0) && (1 / v == (
            if (neg) Double.NegativeInfinity
            else Double.PositiveInfinity))
      }

      import JDouble.{longBitsToDouble => f}

      // Specials
      expect(f(0x7ff0000000000000L)).toEqual(Double.PositiveInfinity)
      expect(f(0xfff0000000000000L)).toEqual(Double.NegativeInfinity)
      expect(isZero(f(0x0000000000000000L), false)).toBeTruthy
      expect(isZero(f(0x8000000000000000L), true)).toBeTruthy
      expect(f(0x7ff8000000000000L).isNaN).toBeTruthy // canonical NaN

      // Non-canonical NaNs
      expect(f(0x7ff0000000000001L).isNaN).toBeTruthy // smallest positive NaN
      expect(f(0x7ff15ab515d3cca1L).isNaN).toBeTruthy // an arbitrary positive NaN
      expect(f(0x7fffffffffffffffL).isNaN).toBeTruthy // largest positive NaN
      expect(f(0xfff0000000000001L).isNaN).toBeTruthy // smallest negative NaN
      expect(f(0xfff15ab515d3cca1L).isNaN).toBeTruthy // an arbitrary negative NaN
      expect(f(0xffffffffffffffffL).isNaN).toBeTruthy // largest negative NaN

      // Normal forms
      expect(f(0x0010000000000000L)).toEqual(2.2250738585072014e-308)  // smallest pos normal form
      expect(f(0x7fefffffffffffffL)).toEqual(1.7976931348623157e308)   // largest pos normal form
      expect(f(0x4d124568bc6584caL)).toEqual(1.8790766677624813e63)    // an arbitrary pos normal form
      expect(f(0x8010000000000000L)).toEqual(-2.2250738585072014e-308) // smallest neg normal form
      expect(f(0xffefffffffffffffL)).toEqual(-1.7976931348623157e308)  // largest neg normal form
      expect(f(0xcd124568bc6584caL)).toEqual(-1.8790766677624813e63)   // an arbitrary neg normal form

      // Subnormal forms
      expect(f(0x0000000000000001L)).toEqual(Double.MinPositiveValue)  // smallest pos subnormal form
      expect(f(0x000fffffffffffffL)).toEqual(2.225073858507201e-308)   // largest pos subnormal form
      expect(f(0x000c5d44ae45cb60L)).toEqual(1.719471609939382e-308)   // an arbitrary pos subnormal form
      expect(f(0x8000000000000001L)).toEqual(-Double.MinPositiveValue) // smallest neg subnormal form
      expect(f(0x800fffffffffffffL)).toEqual(-2.225073858507201e-308)  // largest neg subnormal form
      expect(f(0x800c5d44ae45cb60L)).toEqual(-1.719471609939382e-308)  // an arbitrary neg subnormal form
    }

    it("doubleToLongBits") {
      import JDouble.{doubleToLongBits => f}

      // Specials
      expect(f(Double.PositiveInfinity) == 0x7ff0000000000000L).toBeTruthy
      expect(f(Double.NegativeInfinity) == 0xfff0000000000000L)
      expect(f(0.0) == 0x0000000000000000L).toBeTruthy
      expect(f(-0.0) == 0x8000000000000000L).toBeTruthy
      expect(f(Double.NaN) == 0x7ff8000000000000L).toBeTruthy // canonical NaN

      // Normal forms
      expect(f(2.2250738585072014e-308) == 0x0010000000000000L).toBeTruthy  // smallest pos normal form
      expect(f(1.7976931348623157e308) == 0x7fefffffffffffffL).toBeTruthy   // largest pos normal form
      expect(f(1.8790766677624813e63) == 0x4d124568bc6584caL).toBeTruthy    // an arbitrary pos normal form
      expect(f(-2.2250738585072014e-308) == 0x8010000000000000L).toBeTruthy // smallest neg normal form
      expect(f(-1.7976931348623157e308) == 0xffefffffffffffffL).toBeTruthy  // largest neg normal form
      expect(f(-1.8790766677624813e63) == 0xcd124568bc6584caL).toBeTruthy   // an arbitrary neg normal form

      // Subnormal forms
      expect(f(Double.MinPositiveValue) == 0x0000000000000001L).toBeTruthy  // smallest pos subnormal form
      expect(f(2.225073858507201e-308) == 0x000fffffffffffffL).toBeTruthy   // largest pos subnormal form
      expect(f(1.719471609939382e-308) == 0x000c5d44ae45cb60L).toBeTruthy   // an arbitrary pos subnormal form
      expect(f(-Double.MinPositiveValue) == 0x8000000000000001L).toBeTruthy // smallest neg subnormal form
      expect(f(-2.225073858507201e-308) == 0x800fffffffffffffL).toBeTruthy  // largest neg subnormal form
      expect(f(-1.719471609939382e-308) == 0x800c5d44ae45cb60L).toBeTruthy  // an arbitrary neg subnormal form
    }

  }
}
