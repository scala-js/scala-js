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

import java.lang.{Float => JFloat}

import scala.util.Try

object FloatTest extends JasmineTest {

  describe("java.lang.Float") {

    it("should provide proper `equals`") {
      expect(Float.box(0.0f) == Float.box(-0.0f)).toBeTruthy
      expect(Float.box(Float.NaN) == Float.box(Float.NaN)).toBeTruthy
    }

    it("hashCode") {
      def hashCodeNotInlined(x: Any): Int = {
        var y = x // do not inline
        y.hashCode
      }

      def test(x: Float, expected: Int): Unit = {
        expect(x.hashCode).toEqual(expected)
        expect(hashCodeNotInlined(x)).toEqual(expected)
      }

      test(0.0f, 0)
      test(-0.0f, 0)
      test(1234.0f, 1234)
      test(1.5f, 1073217536)
      test(-54f, -54)

      test(Float.MinPositiveValue, 916455424)
      test(Float.MinValue, 670040063)
      test(Float.MaxValue, -1477443585)

      test(Float.NaN, 2146959360)
      test(Float.PositiveInfinity, 2146435072)
      test(Float.NegativeInfinity, -1048576)
    }

    it("should provide `toString` with integer values when an integer") {
      expect(0.0f.toString).toEqual("0")
      expect(-0.0f.toString).toEqual("0")
      expect(Float.NaN.toString).toEqual("NaN")
      expect(Float.PositiveInfinity.toString).toEqual("Infinity")
      expect(Float.NegativeInfinity.toString).toEqual("-Infinity")
      expect(5.0f.toString).toEqual("5")
      expect(-5.0f.toString).toEqual("-5")

      // We need to explicitly cut the string here, since floats are
      // represented by doubles (but the literal is emitted as
      // float). Therefore there may be some imprecision. This is
      // documented as semantic difference.
      expect(1.2f.toString.substring(0,3)).toEqual("1.2")
    }

    it("should parse strings") {
      expect("0.0".toFloat).toEqual(0.0f)
      expect("NaN".toFloat.isNaN).toBeTruthy
      expect(Try("asdf".toFloat).isFailure).toBeTruthy

      def test(s: String, v: Float): Unit = {
        expect(JFloat.parseFloat(s)).toBeCloseTo(v)
        expect(JFloat.valueOf(s).floatValue()).toBeCloseTo(v)
        expect(new JFloat(s).floatValue()).toBeCloseTo(v)
      }

      test("0", 0.0f)
      test("5.3", 5.3f)
      test("127e2", 12700.0f)
      test("127E-2", 1.27f)
      test("1E+1", 10f)
      test("-123.4", -123.4f)
      test("65432.1", 65432.10f)
      test("-87654.321", -87654.321f)
      test("+.3f", 0.3f)
    }

    it("should reject invalid strings when parsing") {
      def test(s: String): Unit =
        expect(() => JFloat.parseFloat(s)).toThrow

      test("4.3.5")
      test("4e3.5")
      test("hello world")
      test("--4")
      test("4E-3.2")
    }

    it("should provide `compareTo`") {
      def compare(x: Float, y: Float): Int =
        new JFloat(x).compareTo(new JFloat(y))

      expect(compare(0.0f, 5.5f)).toBeLessThan(0)
      expect(compare(10.5f, 10.2f)).toBeGreaterThan(0)
      expect(compare(-2.1f, -1.0f)).toBeLessThan(0)
      expect(compare(3.14f, 3.14f)).toEqual(0)

      // From compareTo's point of view, NaN is equal to NaN
      expect(compare(Float.NaN, Float.NaN)).toEqual(0)
    }

    it("should be a Comparable") {
      def compare(x: Any, y: Any): Int =
        x.asInstanceOf[Comparable[Any]].compareTo(y)

      expect(compare(0.0f, 5.5f)).toBeLessThan(0)
      expect(compare(10.5f, 10.2f)).toBeGreaterThan(0)
      expect(compare(-2.1f, -1.0f)).toBeLessThan(0)
      expect(compare(3.14f, 3.14f)).toEqual(0)

      // From compareTo's point of view, NaN is equal to NaN
      expect(compare(Float.NaN, Float.NaN)).toEqual(0)
    }

    it("should provide isInfinite - #515") {
      expect(Float.PositiveInfinity.isInfinite).toBeTruthy
      expect(Float.NegativeInfinity.isInfinite).toBeTruthy
      expect((1f/0).isInfinite).toBeTruthy
      expect((-1f/0).isInfinite).toBeTruthy
      expect(0f.isInfinite).toBeFalsy
    }

    it("isNaN") {
      def f(v: Float): Boolean = {
        var v2 = v // do not inline
        v2.isNaN
      }

      expect(f(Float.NaN)).toBeTruthy

      expect(f(Float.PositiveInfinity)).toBeFalsy
      expect(f(Float.NegativeInfinity)).toBeFalsy
      expect(f(1f / 0)).toBeFalsy
      expect(f(-1f / 0)).toBeFalsy
      expect(f(0f)).toBeFalsy
      expect(f(3f)).toBeFalsy
      expect(f(-1.5f)).toBeFalsy
    }

    it("intBitsToFloat") {
      def isZero(v: Float, neg: Boolean): Boolean = {
        (v == 0.0f) && (1 / v == (
            if (neg) Float.NegativeInfinity
            else Float.PositiveInfinity))
      }

      import JFloat.{intBitsToFloat => f}

      // Specials
      expect(f(0x7f800000)).toEqual(Float.PositiveInfinity)
      expect(f(0xff800000)).toEqual(Float.NegativeInfinity)
      expect(isZero(f(0x00000000), false)).toBeTruthy
      expect(isZero(f(0x80000000), true)).toBeTruthy
      expect(f(0x7fc00000).isNaN).toBeTruthy // canonical NaN

      // Non-canonical NaNs
      expect(f(0x7f800001).isNaN).toBeTruthy // smallest positive NaN
      expect(f(0x7f915ab5).isNaN).toBeTruthy // an arbitrary positive NaN
      expect(f(0x7fffffff).isNaN).toBeTruthy // largest positive NaN
      expect(f(0xff800001).isNaN).toBeTruthy // smallest negative NaN
      expect(f(0xff915ab5).isNaN).toBeTruthy // an arbitrary negative NaN
      expect(f(0xffffffff).isNaN).toBeTruthy // largest negative NaN

      // Normal forms
      expect(f(0x00800000)).toEqual(1.17549435e-38f)  // smallest pos normal form
      expect(f(0x7f7fffff)).toEqual(3.4028234e38f)    // largest pos normal form
      expect(f(0x4d124568)).toEqual(1.53376384e8f)    // an arbitrary pos normal form
      expect(f(0x80800000)).toEqual(-1.17549435e-38f) // smallest neg normal form
      expect(f(0xff7fffff)).toEqual(-3.4028234e38f)   // largest neg normal form
      expect(f(0xcd124568)).toEqual(-1.53376384e8f)   // an arbitrary neg normal form

      // Subnormal forms
      expect(f(0x00000001)).toEqual(Float.MinPositiveValue)  // smallest pos subnormal form
      expect(f(0x007fffff)).toEqual(1.1754942e-38f)          // largest pos subnormal form
      expect(f(0x007c5d44)).toEqual(1.1421059e-38f)          // an arbitrary pos subnormal form
      expect(f(0x80000001)).toEqual(-Float.MinPositiveValue) // smallest neg subnormal form
      expect(f(0x807fffff)).toEqual(-1.1754942e-38f)         // largest neg subnormal form
      expect(f(0x807c5d44)).toEqual(-1.1421059e-38f)         // an arbitrary neg subnormal form
    }

    it("floatToIntBits") {
      import JFloat.{floatToIntBits => f}

      // Specials
      expect(f(Float.PositiveInfinity)).toEqual(0x7f800000)
      expect(f(Float.NegativeInfinity)).toEqual(0xff800000)
      expect(f(0.0f)).toEqual(0x00000000)
      expect(f(-0.0f)).toEqual(0x80000000)
      expect(f(Float.NaN)).toEqual(0x7fc00000) // canonical NaN

      // Normal forms
      expect(f(1.17549435e-38f)).toEqual(0x00800000)  // smallest pos normal form
      expect(f(3.4028234e38f)).toEqual(0x7f7fffff)    // largest pos normal form
      expect(f(1.53376384e8f)).toEqual(0x4d124568)    // an arbitrary pos normal form
      expect(f(-1.17549435e-38f)).toEqual(0x80800000) // smallest neg normal form
      expect(f(-3.4028234e38f)).toEqual(0xff7fffff)   // largest neg normal form
      expect(f(-1.53376384e8f)).toEqual(0xcd124568)   // an arbitrary neg normal form

      // Subnormal forms
      expect(f(Float.MinPositiveValue)).toEqual(0x00000001)  // smallest pos subnormal form
      expect(f(1.1754942e-38f)).toEqual(0x007fffff)          // largest pos subnormal form
      expect(f(1.1421059e-38f)).toEqual(0x007c5d44)          // an arbitrary pos subnormal form
      expect(f(-Float.MinPositiveValue)).toEqual(0x80000001) // smallest neg subnormal form
      expect(f(-1.1754942e-38f)).toEqual(0x807fffff)         // largest neg subnormal form
      expect(f(-1.1421059e-38f)).toEqual(0x807c5d44)         // an arbitrary neg subnormal form
    }

  }
}
