/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package javalib

import scala.scalajs.test.JasmineTest
import scala.scalajs.js

import java.lang.{Float => JFloat}

import scala.util.Try

object FloatTest extends JasmineTest {

  describe("java.lang.Float") {

    it("should provide proper `equals`") {
      expect(Float.box(0.0f) == Float.box(-0.0f)).toBeFalsy
      expect(Float.box(Float.NaN) == Float.box(Float.NaN)).toBeTruthy
    }

    it("should provide `toString` with integer values when an integer") {
      expect(0.0f.toString).toEqual("0")
      expect(-0.0f.toString).toEqual("0")
      expect(Float.NaN.toString).toEqual("NaN")
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
      test("-123.4", -123.4f)
      test("65432.1", 65432.10f)
      test("-87654.321", -87654.321f)
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

  }
}
