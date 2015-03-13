/*
 *  Ported by Alistair Johnson from https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigDecimalConstructorsTest.java
 */

package org.scalajs.testsuite.javalib.math

import java.math._

import org.scalajs.jasminetest.JasmineTest

object BigDecimalConstructorsTest extends JasmineTest {

  describe("BigDecimalConstructorsTest should") {

    it("testConstrBI") {
      val a = "1231212478987482988429808779810457634781384756794987"
      val bA = new BigInteger(a)
      val aNumber = new BigDecimal(bA)
      expect(aNumber.unscaledValue() == bA).toBeTruthy
      expect(aNumber.scale()).toEqual(0)
      expect(() => new BigDecimal(null.asInstanceOf[BigInteger])).toThrow()
    }

    it("testConstrBigIntegerMathContext") {
      val a = "1231212478987482988429808779810457634781384756794987"
      val bA = new BigInteger(a)
      val precision = 46
      val rm = RoundingMode.UP
      val mc = new MathContext(precision, rm)
      val res = "1231212478987482988429808779810457634781384757"
      val resScale = -6
      val result = new BigDecimal(bA, mc)
      expect(result.unscaledValue().toString).toEqual(res)
      expect(result.scale()).toEqual(resScale)
    }

    it("testConstrBigIntegerScaleMathContext") {
      val a = "1231212478987482988429808779810457634781384756794987"
      val bA = new BigInteger(a)
      val aScale = 10
      val precision = 46
      val rm = RoundingMode.CEILING
      val mc = new MathContext(precision, rm)
      val res = "1231212478987482988429808779810457634781384757"
      val resScale = 4
      val result = new BigDecimal(bA, aScale, mc)
      expect(result.unscaledValue().toString).toEqual(res)
      expect(result.scale()).toEqual(resScale)
    }

    it("testConstrBIScale") {
      val a = "1231212478987482988429808779810457634781384756794987"
      val bA = new BigInteger(a)
      val aScale = 10
      val aNumber = new BigDecimal(bA, aScale)
      expect(aNumber.unscaledValue() == bA).toBeTruthy
      expect(aNumber.scale()).toEqual(aScale)
    }

    it("testConstrChar") {
      val value = Array('-', '1', '2', '3', '8', '0', '.', '4', '7', '3', '8', 'E', '-', '4', '2', '3')
      val result = new BigDecimal(value)
      val res = "-1.23804738E-419"
      val resScale = 427
      expect(res).toEqual(result.toString)
      expect(resScale).toEqual(result.scale())
      expect(() => new BigDecimal(Array[Char]())).toThrow()
    }

    it("testConstrCharIntInt") {
      val value = Array('-', '1', '2', '3', '8', '0', '.', '4', '7', '3', '8', 'E', '-', '4', '2', '3')
      val offset = 3
      val len = 12
      val result = new BigDecimal(value, offset, len)
      val res = "3.804738E-40"
      val resScale = 46
      expect(res).toEqual(result.toString)
      expect(resScale).toEqual(result.scale())
      expect(() => new BigDecimal(Array[Char](), 0, 0)).toThrow()
    }

    it("testConstrCharIntIntMathContext") {
      val value = Array('-', '1', '2', '3', '8', '0', '.', '4', '7', '3', '8', 'E', '-', '4', '2', '3')
      val offset = 3
      val len = 12
      val precision = 4
      val rm = RoundingMode.CEILING
      val mc = new MathContext(precision, rm)
      val result = new BigDecimal(value, offset, len, mc)
      val res = "3.805E-40"
      val resScale = 43
      expect(res).toEqual(result.toString)
      expect(resScale).toEqual(result.scale())
      expect(() =>  new BigDecimal(Array(), 0, 0, MathContext.DECIMAL32)).toThrow()
    }

    it("testConstrCharIntIntMathContextException1") {
      val value = Array('-', '1', '2', '3', '8', '0', '.', '4', '7', '3', '8', 'E', '-', '4', '2', '3')
      val offset = 3
      val len = 120
      val precision = 4
      val rm = RoundingMode.CEILING
      val mc = new MathContext(precision, rm)
      expect(() => new BigDecimal(value, offset, len, mc)).toThrow()
    }

    it("testConstrCharIntIntMathContextException2") {
      val value = Array('-', '1', '2', '3', '8', '0', ',', '4', '7', '3', '8', 'E', '-', '4', '2', '3')
      val offset = 3
      val len = 120
      val precision = 4
      val rm = RoundingMode.CEILING
      val mc = new MathContext(precision, rm)
      expect(() => new BigDecimal(value, offset, len, mc)).toThrow()
    }

    it("testConstrCharMathContext") {
      val value = Array('3', '8', '0', '.', '4', '7', '3', '8', 'E', '-', '4', '2')
      val precision = 4
      val rm = RoundingMode.CEILING
      val mc = new MathContext(precision, rm)
      val result = new BigDecimal(value, mc)
      val res = "3.805E-40"
      val resScale = 43
      expect(res).toEqual(result.toString)
      expect(resScale).toEqual(result.scale())
      expect(() => new BigDecimal(Array[Char](), MathContext.DECIMAL32)).toThrow()
    }

    it("testConstrDouble") {
      val a = 732546982374982347892379283571094797.287346782359284756
      val aNumber = new BigDecimal(a)
      val expected = new BigDecimal("732546982374982347892379283571094797.287346782359284756")
      expect(aNumber.minus(expected)).toBeLessThan(1E21)
    }

    it("testConstrDouble01") {
      val a: Double = 1.0e-1
      val aNumber = new BigDecimal(a)
      val expected = new BigDecimal(".1000000000000000055511151231257827021181583404541015625")
      expect(aNumber.minus(expected)).toBeLessThan(1e-9)
    }

    it("testConstrDouble02") {
      val a: Double = 0.555
      val aNumber = new BigDecimal(a)
      val expected = new BigDecimal(".55500000000000004884981308350688777863979339599609375")
      expect(aNumber.minus(expected)).toBeLessThan(1e-8)
    }

    it("testConstrDoubleDenormalized") {
      //INF
      val a: Double = 2.274341322658976E-304
      val aNumber = new BigDecimal(a)
      val expected = new BigDecimal("2.274341322658976E-304")
      expect(aNumber.minus(expected)).toBeLessThan(1e-305)
    }

    it("testConstrDoubleMathContext") {
      val a: Double = 732546982374982347892379283571094797.287346782359284756
      val precision = 21
      val rm = RoundingMode.CEILING
      val mc = new MathContext(precision, rm)
      val result = new BigDecimal(a, mc)
      val expected = new BigDecimal("732546982374982e21")
      expect(result.minus(expected)).toBeLessThan(1e21)
      expect(() => new BigDecimal(Double.NaN)).toThrow()
      expect(() => new BigDecimal(Double.PositiveInfinity)).toThrow()
      expect(() => new BigDecimal(Double.NegativeInfinity)).toThrow()
    }

    it("testConstrDoubleMinus01") {
      val a: Double = -1.0e-1
      val aNumber = new BigDecimal(a)
      val expected = new BigDecimal("-.1000000000000000055511151231257827021181583404541015625")
      expect(aNumber.minus(expected)).toBeLessThan(1e-9)
    }

    it("testConstrDoubleNaN") {
      val a: Double = Double.NaN
      expect(() => new BigDecimal(a)).toThrow()
    }

    it("testConstrDoubleNegInfinity") {
      val a: Double = Double.NegativeInfinity
      expect(() => new BigDecimal(a) ).toThrow()
    }

    it("testConstrDoublePosInfinity") {
      val a: Double = Double.PositiveInfinity
      expect(() => new BigDecimal(a)).toThrow()
    }

    it("testConstrInt") {
      val a = 732546982
      val res = "732546982"
      val resScale = 0
      val result = new BigDecimal(a)
      expect(result.unscaledValue().toString).toEqual(res)
      expect(result.scale()).toEqual(resScale)
    }

    it("testConstrIntMathContext") {
      val a = 732546982
      val precision = 21
      val rm = RoundingMode.CEILING
      val mc = new MathContext(precision, rm)
      val res = "732546982"
      val resScale = 0
      val result = new BigDecimal(a, mc)
      expect(result.unscaledValue().toString).toEqual(res)
      expect(result.scale()).toEqual(resScale)
    }

    it("testConstrLong") {
      val a: Long = 4576578677732546982L
      val res = "4576578677732546982"
      val resScale = 0
      val result = new BigDecimal(a)
      expect(result.unscaledValue().toString).toEqual(res)
      expect(result.scale()).toEqual(resScale)
    }

    it("testConstrLongMathContext") {
      val a: Long = 4576578677732546982L
      val precision = 5
      val rm = RoundingMode.CEILING
      val mc = new MathContext(precision, rm)
      val res = "45766"
      val resScale = -14
      val result = new BigDecimal(a, mc)
      expect(result.unscaledValue().toString).toEqual(res)
      expect(result.scale()).toEqual(resScale)
    }

    it("testConstrStringException") {
      val a = "-238768.787678287a+10"
      expect(() => new BigDecimal(a)).toThrow()
    }

    it("testConstrStringExceptionEmptyExponent1") {
      val a = "-238768.787678287e"
      expect(() => new BigDecimal(a)).toThrow()
    }

    it("testConstrStringExceptionEmptyExponent2") {
      val a = "-238768.787678287e-"
      expect(() => new BigDecimal(a)).toThrow()
    }

    it("testConstrStringExceptionExponentGreaterIntegerMax") {
      val a = "-238768.787678287e214748364767876"
      expect(() => new BigDecimal(a)).toThrow()
    }

    it("testConstrStringExceptionExponentLessIntegerMin") {
      val a = "-238768.787678287e-214748364767876"
      expect(() => new BigDecimal(a)).toThrow()
    }

    it("testConstrStringExponentIntegerMax") {
      val a = "-238768.787678287e2147483647"
      val aScale = -2147483638
      val bA = new BigInteger("-238768787678287")
      val aNumber = new BigDecimal(a)
      expect(aNumber.unscaledValue() == bA).toBeTruthy
      expect(aNumber.scale()).toEqual(aScale)
    }

    it("testConstrStringExponentIntegerMin") {
      val a = ".238768e-2147483648"
      expect(() => new BigDecimal(a)).toThrow()
    }

    it("testConstrStringMultipleSignsStartWithPlus") {
      val a = "+-3"
      expect(() => new BigDecimal(a)).toThrow()
    }

    it("testConstrStringMultipleSignsStartWithMinus") {
      val a = "-+3"
      expect(() => new BigDecimal(a)).toThrow()
    }

    it("testConstrStringMathContext") {
      val a = "-238768787678287e214"
      val precision = 5
      val rm = RoundingMode.CEILING
      val mc = new MathContext(precision, rm)
      val res = "-23876"
      val resScale = -224
      val result = new BigDecimal(a, mc)
      expect(res).toEqual(result.unscaledValue().toString)
      expect(result.scale()).toEqual(resScale)
    }

    it("testConstrStringWithExponentWithoutPoint1") {
      val a = "-238768787678287e214"
      val aScale = -214
      val bA = new BigInteger("-238768787678287")
      val aNumber = new BigDecimal(a)
      expect(aNumber.unscaledValue() == bA).toBeTruthy
      expect(aNumber.scale()).toEqual(aScale)
    }

    it("testConstrStringWithExponentWithoutPoint2") {
      val a = "-238768787678287e-214"
      val aScale = 214
      val bA = new BigInteger("-238768787678287")
      val aNumber = new BigDecimal(a)
      expect(aNumber.unscaledValue() == bA).toBeTruthy
      expect(aNumber.scale()).toEqual(aScale)
    }

    it("testConstrStringWithExponentWithoutPoint3") {
      val a = "238768787678287e-214"
      val aScale = 214
      val bA = new BigInteger("238768787678287")
      val aNumber = new BigDecimal(a)
      expect(aNumber.unscaledValue() == bA).toBeTruthy
      expect(aNumber.scale()).toEqual(aScale)
    }

    it("testConstrStringWithExponentWithoutPoint4") {
      val a = "238768787678287e+214"
      val aScale = -214
      val bA = new BigInteger("238768787678287")
      val aNumber = new BigDecimal(a)
      expect(aNumber.unscaledValue() == bA).toBeTruthy
      expect(aNumber.scale()).toEqual(aScale)
    }

    it("testConstrStringWithExponentWithoutPoint5") {
      val a = "238768787678287E214"
      val aScale = -214
      val bA = new BigInteger("238768787678287")
      val aNumber = new BigDecimal(a)
      expect(aNumber.unscaledValue() == bA).toBeTruthy
      expect(aNumber.scale()).toEqual(aScale)
    }

    it("testConstrStringWithExponentWithPoint1") {
      val a = "23985439837984782435652424523876878.7678287e+214"
      val aScale = -207
      val bA = new BigInteger("239854398379847824356524245238768787678287")
      val aNumber = new BigDecimal(a)
      expect(aNumber.unscaledValue() == bA).toBeTruthy
      expect(aNumber.scale()).toEqual(aScale)
    }

    it("testConstrStringWithExponentWithPoint2") {
      val a = "238096483923847545735673567457356356789029578490276878.7678287e-214"
      val aScale = 221
      val bA = new BigInteger("2380964839238475457356735674573563567890295784902768787678287")
      val aNumber = new BigDecimal(a)
      expect(aNumber.unscaledValue() == bA).toBeTruthy
      expect(aNumber.scale()).toEqual(aScale)
    }

    it("testConstrStringWithExponentWithPoint3") {
      val a = "2380964839238475457356735674573563567890.295784902768787678287E+21"
      val aScale = 0
      val bA = new BigInteger("2380964839238475457356735674573563567890295784902768787678287")
      val aNumber = new BigDecimal(a)
      expect(aNumber.unscaledValue() == bA).toBeTruthy
      expect(aNumber.scale()).toEqual(aScale)
    }

    it("testConstrStringWithExponentWithPoint4") {
      val a = "23809648392384754573567356745735635678.90295784902768787678287E+21"
      val aScale = 2
      val bA = new BigInteger("2380964839238475457356735674573563567890295784902768787678287")
      val aNumber = new BigDecimal(a)
      expect(aNumber.unscaledValue() == bA).toBeTruthy
      expect(aNumber.scale()).toEqual(aScale)
    }

    it("testConstrStringWithExponentWithPoint5") {
      val a = "238096483923847545735673567457356356789029.5784902768787678287E+21"
      val aScale = -2
      val bA = new BigInteger("2380964839238475457356735674573563567890295784902768787678287")
      val aNumber = new BigDecimal(a)
      expect(aNumber.unscaledValue() == bA).toBeTruthy
      expect(aNumber.scale()).toEqual(aScale)
    }

    it("testConstrStringWithLeadingZeros") {
      expect(new BigDecimal("-000.1").precision()).toEqual(1)
      expect(new BigDecimal("001234").precision()).toEqual(4)
      expect(new BigDecimal("-5555").precision()).toEqual(4)
      expect(new BigDecimal("0").precision()).toEqual(1)
      expect(new BigDecimal("-0").precision()).toEqual(1)
      expect(new BigDecimal("001234e3").precision()).toEqual(4)
      expect(new BigDecimal("00056789e+17").precision()).toEqual(5)
      expect(new BigDecimal("0900000e-42").precision()).toEqual(6)
    }

    it("testConstrStringWithoutExpNeg") {
      val a = "-732546982374982347892379283571094797.287346782359284756"
      val aScale = 18
      val bA = new BigInteger("-732546982374982347892379283571094797287346782359284756")
      val aNumber = new BigDecimal(a)
      expect(bA == aNumber.unscaledValue()).toBeTruthy
      expect(aNumber.scale()).toEqual(aScale)
    }

    it("testConstrStringWithoutExpPos1") {
      val a = "732546982374982347892379283571094797.287346782359284756"
      val aScale = 18
      val bA = new BigInteger("732546982374982347892379283571094797287346782359284756")
      val aNumber = new BigDecimal(a)
      expect(aNumber.unscaledValue() == bA).toBeTruthy
      expect(aScale).toEqual(aNumber.scale())
    }

    it("testConstrStringWithoutExpPos2") {
      val a = "+732546982374982347892379283571094797.287346782359284756"
      val aScale = 18
      val bA = new BigInteger("732546982374982347892379283571094797287346782359284756")
      val aNumber = new BigDecimal(a)
      expect(aNumber.unscaledValue() == bA).toBeTruthy
      expect(aScale).toEqual(aNumber.scale())
    }

    it("testConstrStringWithoutExpWithoutPoint") {
      val a = "-732546982374982347892379283571094797287346782359284756"
      val aScale = 0
      val bA = new BigInteger("-732546982374982347892379283571094797287346782359284756")
      val aNumber = new BigDecimal(a)
      expect(bA == aNumber.unscaledValue()).toBeTruthy
      expect(aScale).toEqual(aNumber.scale())
    }

    it("testConstrZero") {
      var bd = new BigDecimal("0")
      expect(bd.intValueExact()).toEqual(0)
      expect(bd.precision()).toEqual(1)
      expect(bd.scale()).toEqual(0)
      bd = new BigDecimal("0.0")
      expect(bd.intValueExact()).toEqual(0)
      expect(bd.precision()).toEqual(1)
      expect(bd.scale()).toEqual(1)
      bd = new BigDecimal("0.00")
      expect(bd.intValueExact()).toEqual(0)
      expect(bd.precision()).toEqual(1)
      expect(bd.scale()).toEqual(2)
    }

    it("testFieldONE") {
      val oneS = "1"
      val oneD = 1.0
      expect(BigDecimal.ONE.toString).toEqual(oneS)
      expect(BigDecimal.ONE.doubleValue()).toEqual(oneD)
    }

    it("testFieldTEN") {
      val oneS = "10"
      val oneD = 10.0
      expect(BigDecimal.TEN.toString).toEqual(oneS)
      expect(BigDecimal.TEN.doubleValue()).toEqual(oneD)
    }

    it("testFieldZERO") {
      val oneS = "0"
      val oneD = 0.0
      expect(BigDecimal.ZERO.toString).toEqual(oneS)
      expect(BigDecimal.ZERO.doubleValue()).toEqual(oneD)
    }
  }

  // Helper for comparing values within a certain +/- delta
  implicit class BigDecimalOps(val actual: BigDecimal) extends AnyVal {
    def minus(expected: BigDecimal): Double = {
      val actualDeltaDecimal:BigDecimal = actual.subtract(expected)
      val actualDelta = actualDeltaDecimal.abs().doubleValue()
      actualDelta
    }
  }
}
