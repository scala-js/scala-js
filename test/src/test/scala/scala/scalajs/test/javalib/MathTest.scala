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

  }

}
