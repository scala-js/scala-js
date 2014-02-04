/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package javalib

import scala.scalajs.js
import scala.scalajs.test.JasmineTest
import java.lang.Math

object MathTest extends JasmineTest {

  describe("java.lang.Math") {

    it("should respond to `cbrt`") {
      expect(1 / Math.cbrt(-0.0) < 0).toBeTruthy
      expect(Math.cbrt(27.0)).toEqual(3.0)
      expect(Math.cbrt(1000000.0)).toEqual(100.0)
      expect(Math.cbrt(1000000000.0)).toEqual(1000.0)
      expect(Math.cbrt(-1.0E24)).toEqual(-100000000.0)
      expect(Math.cbrt(-65890311319.0E24)).toEqual(-4039.0E8)
    }

    it("should respond to `log1p`") {
      expect(Math.log1p(-2.0).isNaN).toBeTruthy
      expect(Math.log1p(js.Number.NaN.toDouble).isNaN).toBeTruthy
      expect(Math.log1p(0.0)).toEqual(0.0)
    }

    it("should respond to `log10`") {
      expect(Math.log10(-230.0).isNaN).toBeTruthy
      expect(Math.log10(js.Number.NaN.toDouble).isNaN).toBeTruthy
    }

    it("should respond to `signum` for Double") {
      expect(Math.signum(234394.2198273)).toEqual(1.0)
      expect(Math.signum(-124937498.58)).toEqual(-1.0)

      expect(Math.signum(+0.0)).toEqual(0.0)
      expect(1 / Math.signum(+0.0) > 0).toBeTruthy

      expect(Math.signum(-0.0)).toEqual(-0.0)
      expect(1 / Math.signum(-0.0) < 0).toBeTruthy

      expect(Math.signum(js.Number.NaN.toDouble).isNaN).toBeTruthy
    }

    it("should respond to `signum` for Float") {
      expect(Math.signum(234394.2198273f)).toEqual(1.0f)
      expect(Math.signum(-124937498.58f)).toEqual(-1.0f)

      expect(Math.signum(+0.0f)).toEqual(0.0f)
      expect(1 / Math.signum(+0.0f) > 0).toBeTruthy

      expect(Math.signum(-0.0f)).toEqual(-0.0f)
      expect(1 / Math.signum(-0.0f) < 0).toBeTruthy

      expect(Math.signum(js.Number.NaN.toFloat).isNaN).toBeTruthy
    }

    it("should respond to `nextUp` for Double") {
      expect(Math.nextUp(Double.PositiveInfinity)).toEqual(Double.PositiveInfinity)
      expect(Math.nextUp(Double.NegativeInfinity)).toEqual(-Double.MaxValue)
      expect(Math.nextUp(Double.MaxValue)).toEqual(Double.PositiveInfinity)
      expect(Math.nextUp(-Double.MaxValue)).toEqual(-1.7976931348623155e+308)
      expect(Math.nextUp(-Double.MinValue)).toEqual(Double.PositiveInfinity)
      expect(Math.nextUp(0.0)).toEqual(Double.MinValue)
      expect(Math.nextUp(-0.0)).toEqual(Double.MinValue)
      expect(Math.nextUp(9007199254740991.0)).toEqual(9007199254740992.0)
      expect(Math.nextUp(9007199254740992.0)).toEqual(9007199254740994.0)
      expect(Math.nextUp(1.0)).toEqual(1 + 2.2204460492503130808472633361816E-16)
    }

    it("should respond to `nextAfter` for Double") {
      expect(Math.nextAfter(1.0, js.Number.NaN.toDouble).isNaN).toBeTruthy
      expect(Math.nextAfter(js.Number.NaN.toDouble, 1.0).isNaN).toBeTruthy
      expect(Math.nextAfter(0.0, 0.0)).toEqual(0.0)
      expect(Math.nextAfter(0.0, -0.0)).toEqual(-0.0)
      expect(Math.nextAfter(-0.0, 0.0)).toEqual(0.0)
      expect(Math.nextAfter(-0.0, -0.0)).toEqual(-0.0)
      expect(Math.nextAfter(Double.MinValue, Double.NegativeInfinity)).toEqual(Double.NegativeInfinity)
      expect(Math.nextAfter(-Double.MinValue, Double.PositiveInfinity)).toEqual(Double.PositiveInfinity)
      expect(Math.nextAfter(Double.PositiveInfinity, Double.NegativeInfinity)).toEqual(Double.MaxValue)
      expect(Math.nextAfter(Double.NegativeInfinity, Double.PositiveInfinity)).toEqual(-Double.MaxValue)
      expect(Math.nextAfter(Double.MaxValue, Double.PositiveInfinity)).toEqual(Double.PositiveInfinity)
      expect(Math.nextAfter(-Double.MaxValue, Double.NegativeInfinity)).toEqual(Double.NegativeInfinity)
      expect(Math.nextAfter(1.0, 1.0)).toEqual(1.0)
    }

    it("should respond to `ulp` for Double") {
      expect(Math.ulp(3.4)).toEqual(4.440892098500626E-16)
      expect(Math.ulp(3.423E109)).toEqual(4.1718496795330275E93)
      expect(Math.ulp(0.0)).toEqual(Double.MinValue)
    }

  }

}
