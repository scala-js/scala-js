/*
 *  Ported by Alistair Johnson from https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/MathContextTest.java
 */

package org.scalajs.testsuite.javalib.math

import java.math.MathContext
import java.math.RoundingMode
import org.scalajs.jasminetest.JasmineTest

object MathContextTest extends JasmineTest {

  describe("MathContextTest") {

    it("testMathContextSingleArgConstructor") {
      val mc1 = new MathContext("precision=16 roundingMode=CEILING")
      expect(mc1.getPrecision == 16).toBeTruthy
      expect(mc1.getRoundingMode == RoundingMode.CEILING).toBeTruthy
      val mc2 = new MathContext("precision=17 roundingMode=DOWN")
      expect(mc2.getPrecision == 17).toBeTruthy
      expect(mc2.getRoundingMode == RoundingMode.DOWN).toBeTruthy
      val mc3 = new MathContext("precision=18 roundingMode=FLOOR")
      expect(mc3.getPrecision == 18).toBeTruthy
      expect(mc3.getRoundingMode == RoundingMode.FLOOR).toBeTruthy
      val mc4 = new MathContext("precision=19 roundingMode=HALF_DOWN")
      expect(mc4.getPrecision == 19).toBeTruthy
      expect(mc4.getRoundingMode == RoundingMode.HALF_DOWN).toBeTruthy
      val mc5 = new MathContext("precision=20 roundingMode=HALF_EVEN")
      expect(mc5.getPrecision == 20).toBeTruthy
      expect(mc5.getRoundingMode == RoundingMode.HALF_EVEN).toBeTruthy
      val mc6 = new MathContext("precision=21 roundingMode=HALF_UP")
      expect(mc6.getPrecision == 21).toBeTruthy
      expect(mc6.getRoundingMode == RoundingMode.HALF_UP).toBeTruthy
      val mc7 = new MathContext("precision=22 roundingMode=UNNECESSARY")
      expect(mc7.getPrecision == 22).toBeTruthy
      expect(mc7.getRoundingMode == RoundingMode.UNNECESSARY).toBeTruthy
      val mc8 = new MathContext("precision=23 roundingMode=UP")
      expect(mc8.getPrecision == 23).toBeTruthy
      expect(mc8.getRoundingMode == RoundingMode.UP).toBeTruthy

      expect(() => new MathContext("prcision=27 roundingMode=CEILING")).toThrow
      expect(() => new MathContext("precision=26 roundingMoe=CEILING")).toThrow
      expect(() => new MathContext("precision=25 roundingMode=CEILINGFAN")).toThrow
      expect(() => new MathContext("precision=24 roundingMode=HALF")).toThrow
      expect(() => new MathContext("precision=23 roundingMode=UPSIDEDOWN")).toThrow
      expect(() => new MathContext("precision=22roundingMode=UP")).toThrow
      expect(() => new MathContext("")).toThrow
      expect(() => new MathContext(null)).toThrow

    }

    it("testMathContextConstructorEquality") {
      val mc1 = new MathContext(16, RoundingMode.CEILING)
      val mc1a = new MathContext("precision=16 roundingMode=CEILING")
      expect(mc1 == mc1a).toBeTruthy
      val mc2 = new MathContext(17, RoundingMode.DOWN)
      val mc2a = new MathContext("precision=17 roundingMode=DOWN")
      expect(mc2 == mc2a).toBeTruthy
      val mc3 = new MathContext(18, RoundingMode.FLOOR)
      val mc3a = new MathContext("precision=18 roundingMode=FLOOR")
      expect(mc3 == mc3a).toBeTruthy
      val mc4 = new MathContext(19, RoundingMode.HALF_DOWN)
      val mc4a = new MathContext("precision=19 roundingMode=HALF_DOWN")
      expect(mc4 == mc4a).toBeTruthy
      val mc5 = new MathContext(20, RoundingMode.HALF_EVEN)
      val mc5a = new MathContext("precision=20 roundingMode=HALF_EVEN")
      expect(mc5 == mc5a).toBeTruthy
      val mc6 = new MathContext(21, RoundingMode.HALF_UP)
      val mc6a = new MathContext("precision=21 roundingMode=HALF_UP")
      expect(mc6 == mc6a).toBeTruthy
      val mc7 = new MathContext(22, RoundingMode.UNNECESSARY)
      val mc7a = new MathContext("precision=22 roundingMode=UNNECESSARY")
      expect(mc7 == mc7a).toBeTruthy
      val mc8 = new MathContext(23, RoundingMode.UP)
      val mc8a = new MathContext("precision=23 roundingMode=UP")
      expect(mc8 == mc8a).toBeTruthy
    }
  }
}
