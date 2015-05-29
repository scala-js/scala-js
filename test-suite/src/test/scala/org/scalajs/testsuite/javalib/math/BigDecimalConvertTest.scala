// scalastyle:off line.size.limit
/*
 * Ported by Alistair Johnson from
 * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigDecimalConvertTest.java
 */
// scalastyle:on line.size.limit

package org.scalajs.testsuite.javalib.math

import java.math._

import org.scalajs.jasminetest.JasmineTest

object BigDecimalConvertTest extends JasmineTest {

  describe("BigDecimalConvertTest") {

    it("testByteValue") {
      expect(BigDecimal.ONE.byteValue()).toEqual(1.toByte)
      expect(-1.toByte).toEqual(BigDecimal.valueOf(255).byteValue())
      expect(1.toByte).toEqual(BigDecimal.ONE.byteValueExact())
      expect(() => BigDecimal.valueOf(255).byteValueExact()).toThrow()
    }

    it("testDoubleValueNeg") {
      val a = "-123809648392384754573567356745735.63567890295784902768787678287E+21"
      val aNumber = new BigDecimal(a)
      val result = -1.2380964839238476E53
      expect(result).toEqual(aNumber.doubleValue())
    }

    it("testDoubleValueNegInfinity") {
      val a = "-123809648392384754573567356745735.63567890295784902768787678287E+400"
      val aNumber = new BigDecimal(a)
      val result = Double.NegativeInfinity
      expect(result == aNumber.doubleValue()).toBeTruthy
    }

    it("testDoubleValuePos") {
      val a = "123809648392384754573567356745735.63567890295784902768787678287E+21"
      val aNumber = new BigDecimal(a)
      val result = 1.2380964839238476E53
      expect(result).toEqual(aNumber.doubleValue())
    }

    it("testDoubleValuePosInfinity") {
      val a = "123809648392384754573567356745735.63567890295784902768787678287E+400"
      val aNumber = new BigDecimal(a)
      val result = Double.PositiveInfinity
      expect(result == aNumber.doubleValue()).toBeTruthy
    }

    it("testFloatValueNeg") {
      val a = "-1238096483923847.6356789029578E+21"
      val aNumber = new BigDecimal(a)
      val result = -1.2380965E36f
      expect(Math.abs(aNumber.floatValue() - result)).toBeLessThan(1E29)
    }

    it("testFloatValueNegInfinity") {
      val a = "-123809648392384755735.63567887678287E+200"
      val aNumber = new BigDecimal(a)
      val result =  Float.NegativeInfinity
      expect(aNumber.floatValue() == result).toBeTruthy
    }

    it("testFloatValuePos") {
      val a = "1238096483923847.6356789029578E+21"
      val aNumber = new BigDecimal(a)
      val result = 1.2380965E36f
      expect(Math.abs(aNumber.floatValue() - result)).toBeLessThan(1E29)
    }

    it("testFloatValuePosInfinity") {
      val a = "123809648373567356745735.6356789787678287E+200"
      val aNumber = new BigDecimal(a)
      val result =  Float.PositiveInfinity
      expect(aNumber.floatValue() == result).toBeTruthy
    }

    it("testIntValueNeg") {
      val a = "-123809648392384754573567356745735.63567890295784902768787678287E+21"
      val aNumber = new BigDecimal(a)
      val result = 218520473
      expect(result).toEqual(aNumber.intValue())
      expect(() => aNumber.intValueExact()).toThrow()
    }

    it("testIntValuePos") {
      val a = "123809648392384754573567356745735.63567890295784902768787678287E+21"
      val aNumber = new BigDecimal(a)
      val result = -218520473
      expect(result).toEqual(aNumber.intValue())
      expect(() => aNumber.intValueExact()).toThrow()
    }

    it("testLongValueNeg") {
      val a = "-123809648392384754573567356745735.63567890295784902768787678287E+21"
      val aNumber = new BigDecimal(a)
      val result = -1246043477766677607L
      expect(aNumber.longValue() == result).toBeTruthy
      expect(() => aNumber.longValueExact()).toThrow()
    }

    it("testLongValuePos") {
      val a = "123809648392384754573567356745735.63567890295784902768787678287E+21"
      val aNumber = new BigDecimal(a)
      val result = 1246043477766677607L
      expect(aNumber.longValue() == result).toBeTruthy
      expect(() => aNumber.longValueExact()).toThrow()
    }

    it("testScaleByPowerOfTen1") {
      val a = "1231212478987482988429808779810457634781384756794987"
      val aScale = 13
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val result = aNumber.scaleByPowerOfTen(10)
      val res = "1231212478987482988429808779810457634781384756794.987"
      val resScale = 3
      expect(res).toEqual(result.toString)
      expect(resScale).toEqual(result.scale())
    }

    it("testScaleByPowerOfTen2") {
      val a = "1231212478987482988429808779810457634781384756794987"
      val aScale = -13
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val result = aNumber.scaleByPowerOfTen(10)
      val res = "1.231212478987482988429808779810457634781384756794987E+74"
      val resScale = -23
      expect(res).toEqual(result.toString)
      expect(resScale).toEqual(result.scale())
    }

    it("testShortValue") {
      val value = BigDecimal.valueOf(0x13fff)
      expect(0x3fff).toEqual(value.shortValue())
      expect(() => value.shortValueExact()).toThrow()
    }

    it("testToBigIntegerExact1") {
      val a = "-123809648392384754573567356745735.63567890295784902768787678287E+45"
      val aNumber = new BigDecimal(a)
      val res = "-123809648392384754573567356745735635678902957849027687876782870000000000000000"
      val result = aNumber.toBigIntegerExact()
      expect(res).toEqual(result.toString)
    }

    it("testToBigIntegerExactException") {
      val a = "-123809648392384754573567356745735.63567890295784902768787678287E-10"
      val aNumber = new BigDecimal(a)
      expect(() =>  aNumber.toBigIntegerExact()).toThrow()
    }

    it("testToBigIntegerNeg1") {
      val a = "-123809648392384754573567356745735.63567890295784902768787678287E+21"
      val bNumber = new BigInteger("-123809648392384754573567356745735635678902957849027687")
      val aNumber = new BigDecimal(a)
      val result = aNumber.toBigInteger()
      expect(result == bNumber).toBeTruthy
    }

    it("testToBigIntegerNeg2") {
      val a = "-123809648392384754573567356745735.63567890295784902768787678287E+15"
      val bNumber = new BigInteger("-123809648392384754573567356745735635678902957849")
      val aNumber = new BigDecimal(a)
      val result = aNumber.toBigInteger()
      expect(result == bNumber).toBeTruthy
    }

    it("testToBigIntegerNeg3") {
      val a = "-123809648392384754573567356745735.63567890295784902768787678287E+45"
      val bNumber = new BigInteger("-123809648392384754573567356745735635678902957849027687876782870000000000000000")
      val aNumber = new BigDecimal(a)
      val result = aNumber.toBigInteger()
      expect(result == bNumber).toBeTruthy
    }

    it("testToBigIntegerPos1") {
      val a = "123809648392384754573567356745735.63567890295784902768787678287E+21"
      val bNumber = new BigInteger("123809648392384754573567356745735635678902957849027687")
      val aNumber = new BigDecimal(a)
      val result = aNumber.toBigInteger()
      expect(result == bNumber).toBeTruthy
    }

    it("testToBigIntegerPos2") {
      val a = "123809648392384754573567356745735.63567890295784902768787678287E+15"
      val bNumber = new BigInteger("123809648392384754573567356745735635678902957849")
      val aNumber = new BigDecimal(a)
      val result = aNumber.toBigInteger()
      expect(result == bNumber).toBeTruthy
    }

    it("testToBigIntegerPos3") {
      val a = "123809648392384754573567356745735.63567890295784902768787678287E+45"
      val bNumber = new BigInteger("123809648392384754573567356745735635678902957849027687876782870000000000000000")
      val aNumber = new BigDecimal(a)
      val result = aNumber.toBigInteger()
      expect(result == bNumber).toBeTruthy
    }

    it("testToBigIntegerZero") {
      val a = "-123809648392384754573567356745735.63567890295784902768787678287E-500"
      val bNumber = new BigInteger("0")
      val aNumber = new BigDecimal(a)
      val result = aNumber.toBigInteger()
      expect(result == bNumber).toBeTruthy
    }

    it("testToEngineeringStringNeg") {
      val a = "-123809648392384754573567356745735.63567890295784902768787678287E-501"
      val aNumber = new BigDecimal(a)
      val result = "-123.80964839238475457356735674573563567890295784902768787678287E-471"
      expect(result).toEqual(aNumber.toEngineeringString())
    }

    it("testToEngineeringStringPos") {
      val a = "123809648392384754573567356745735.63567890295784902768787678287E-501"
      val aNumber = new BigDecimal(a)
      val result = "123.80964839238475457356735674573563567890295784902768787678287E-471"
      expect(result).toEqual(aNumber.toEngineeringString())
    }

    it("testToEngineeringStringZeroNegExponent") {
      val a = "0.0E-16"
      val aNumber = new BigDecimal(a)
      val result = "0.00E-15"
      expect(result).toEqual(aNumber.toEngineeringString())
    }

    it("testToEngineeringStringZeroPosExponent") {
      val a = "0.0E+16"
      val aNumber = new BigDecimal(a)
      val result = "0E+15"
      expect(result).toEqual(aNumber.toEngineeringString())
    }

    it("testToPlainStringNegNegExp") {
      val a = "-123809648392384754573567356745735.63567890295784902768787678287E-100"
      val aNumber = new BigDecimal(a)
      val result = "-0.00000000000000000000000000000000000000000000000000000" +
        "0000000000000012380964839238475457356735674573563567890295784902768" +
        "787678287"
      expect(aNumber.toPlainString() == result).toBeTruthy
    }

    it("testToPlainStringNegPosExp") {
      val a = "-123809648392384754573567356745735.63567890295784902768787678287E100"
      val aNumber = new BigDecimal(a)
      val result = "-1238096483923847545735673567457356356789029578490276878" +
        "7678287000000000000000000000000000000000000000000000000000000000000" +
        "00000000000"
      expect(aNumber.toPlainString() == result).toBeTruthy
    }

    it("testToPlainStringPosNegExp") {
      val a = "123809648392384754573567356745735.63567890295784902768787678287E-100"
      val aNumber = new BigDecimal(a)
      val result = "0.000000000000000000000000000000000000000000000000000000" +
        "0000000000000123809648392384754573567356745735635678902957849027687" +
        "87678287"
      expect(aNumber.toPlainString() == result).toBeTruthy
    }

    it("testToPlainStringPosPosExp") {
      val a = "123809648392384754573567356745735.63567890295784902768787678287E+100"
      val aNumber = new BigDecimal(a)
      val result = "12380964839238475457356735674573563567890295784902768787" +
        "6782870000000000000000000000000000000000000000000000000000000000000" +
        "0000000000"
      expect(aNumber.toPlainString() == result).toBeTruthy
    }

    it("testToStringNeg") {
      val a = "-123.4564563673567380964839238475457356735674573563567890295784902768787678287E-5"
      val aNumber = new BigDecimal(a)
      val result = "-0.001234564563673567380964839238475457356735674573563567890295784902768787678287"
      expect(aNumber.toString == result).toBeTruthy
    }

    it("testToStringPos") {
      val a = "123809648392384754573567356745735.63567890295784902768787678287E-500"
      val aNumber = new BigDecimal(a)
      val result = "1.2380964839238475457356735674573563567890295784902768787678287E-468"
      expect(aNumber.toString == result).toBeTruthy
    }

    it("testToStringZeroScale") {
      val a = "-123809648392384754573567356745735635678902957849027687876782870"
      val aNumber = new BigDecimal(new BigInteger(a))
      val result = "-123809648392384754573567356745735635678902957849027687876782870"
      expect(aNumber.toString == result).toBeTruthy
    }

    it("testValueOfDoubleNaN") {
      val a = Double.NaN
      expect(() =>  BigDecimal.valueOf(a)).toThrow()
    }

    it("testValueOfDoubleNeg") {
      val a = -65678765876567576.98788767
      val result = BigDecimal.valueOf(a)
      val res = "-65678765876567576"
      val resScale = 0
      expect(res).toEqual(result.toString)
      expect(resScale).toEqual(result.scale())
    }

    it("testValueOfDoublePos1") {
      val a = 65678765876567576.98788767
      val result = BigDecimal.valueOf(a)
      val res = "65678765876567576"
      val resScale = 0
      expect(res).toEqual(result.toString)
      expect(resScale).toEqual(result.scale())
    }

    it("testValueOfDoublePos2") {
      val a = 12321237576.98788767
      val result = BigDecimal.valueOf(a)
      val res = "12321237576.987888"
      val resScale = 6
      expect(res).toEqual(result.toString)
      expect(resScale).toEqual(result.scale())
    }

    it("testValueOfDoublePos3") {
      val a = 12321237576.9878838
      val result = BigDecimal.valueOf(a)
      val res = "12321237576.98788"
      val resScale = 6
      expect(result.toString.startsWith(res)).toBeTruthy
      expect(resScale).toEqual(result.scale())
    }

    it("testValueOfNegScaleNeg") {
      val a = -98374823947823578L
      val scale = -12
      val aNumber = BigDecimal.valueOf(a, scale)
      val result = "-9.8374823947823578E+28"
      expect(aNumber.toString == result).toBeTruthy
    }

    it("testValueOfNegScalePos") {
      val a = -98374823947823578L
      val scale = 12
      val aNumber = BigDecimal.valueOf(a, scale)
      val result = "-98374.823947823578"
      expect(aNumber.toString == result).toBeTruthy
    }

    it("testValueOfNegZeroScale") {
      val a = -98374823947823578L
      val aNumber = BigDecimal.valueOf(a)
      val result = "-98374823947823578"
      expect(aNumber.toString == result).toBeTruthy
    }

    it("testValueOfPosScaleNeg") {
      val a = 98374823947823578L
      val scale = -12
      val aNumber = BigDecimal.valueOf(a, scale)
      val result = "9.8374823947823578E+28"
      expect(aNumber.toString == result).toBeTruthy
    }

    it("testValueOfPosScalePos") {
      val a = 98374823947823578L
      val scale = 12
      val aNumber = BigDecimal.valueOf(a, scale)
      val result = "98374.823947823578"
      expect(aNumber.toString == result).toBeTruthy
    }

    it("testValueOfPosZeroScale") {
      val a = 98374823947823578L
      val aNumber = BigDecimal.valueOf(a)
      val result = "98374823947823578"
      expect(aNumber.toString == result).toBeTruthy
    }

    it("testValueOfZeroScaleNeg") {
      val scale = -2
      val number = BigDecimal.valueOf(0L, scale)
      expect("0E+2").toEqual(number.toString)
      expect(scale).toEqual(number.scale())
    }

    it("testValueOfZeroScalePos") {
      val scale = 1
      val number = BigDecimal.valueOf(0L, scale)
      expect("0.0").toEqual(number.toString)
      expect(scale).toEqual(number.scale())
    }
  }
}
