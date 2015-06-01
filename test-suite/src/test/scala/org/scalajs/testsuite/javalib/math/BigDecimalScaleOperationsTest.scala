// scalastyle:off line.size.limit
/*
 * Ported by Alistair Johnson from
 * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigDecimalScaleOperationsTest.java
 */
// scalastyle:on line.size.limit

package org.scalajs.testsuite.javalib.math

import java.math._

import org.scalajs.jasminetest.JasmineTest

object  BigDecimalScaleOperationsTest extends JasmineTest {

  describe("BigDecimalScaleOperationsTest") {

    it("testScaleByPowerOfTen") {
      val bd = BigDecimal.ONE.scaleByPowerOfTen(1)
      expect(10).toEqual(bd.intValue())
    }

    it("testScaleDefault") {
      val a = "1231212478987482988429808779810457634781384756794987"
      val cScale = 0
      val aNumber = new BigDecimal(new BigInteger(a))
      expect(aNumber.scale() == cScale).toBeTruthy
    }

    it("testScaleNeg") {
      val a = "1231212478987482988429808779810457634781384756794987"
      val aScale = -10
      val cScale = -10
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      expect(aNumber.scale() == cScale).toBeTruthy
    }

    it("testScalePos") {
      val a = "1231212478987482988429808779810457634781384756794987"
      val aScale = 10
      val cScale = 10
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      expect(aNumber.scale() == cScale).toBeTruthy
    }

    it("testScaleZero") {
      val a = "1231212478987482988429808779810457634781384756794987"
      val aScale = 0
      val cScale = 0
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      expect(aNumber.scale() == cScale).toBeTruthy
    }

    it("testUnscaledValue") {
      val a = "1231212478987482988429808779810457634781384756794987"
      val aScale = 100
      val bNumber = new BigInteger(a)
      val aNumber = new BigDecimal(bNumber, aScale)
      val aNumberUnscaledValue:BigInteger = aNumber.unscaledValue()
      expect(aNumberUnscaledValue == bNumber).toBeTruthy
      expect(aNumber.unscaledValue() == bNumber).toBeTruthy
    }

    it("testUnscaledValue") {
      val a = "1231212478987482988429808779810457634781384756794987"
      val aScale = 100
      val bNumber = new BigInteger(a)
      val aNumber = new BigDecimal(bNumber, aScale)
      expect(aNumber.unscaledValue() == bNumber).toBeTruthy
    }

    it("testSetScaleGreater") {
      val a = "1231212478987482988429808779810457634781384756794987"
      val aScale = 18
      val newScale = 28
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = aNumber.setScale(newScale)
      expect(bNumber.scale() == newScale).toBeTruthy
      expect(0).toEqual(bNumber.compareTo(aNumber))
    }

    it("testSetScaleLess") {
      val a = "2.345726458768760000E+10"
      val newScale = 5
      val aNumber = new BigDecimal(a)
      val bNumber = aNumber.setScale(newScale)
      expect(bNumber.scale() == newScale).toBeTruthy
      expect(0).toEqual(bNumber.compareTo(aNumber))
    }

    it("testSetScaleException") {
      val a = "1231212478987482988429808779810457634781384756794987"
      val aScale = 28
      val newScale = 18
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      expect(() => aNumber.setScale(newScale)).toThrow()
    }

    it("testSetScaleSame") {
      val a = "1231212478987482988429808779810457634781384756794987"
      val aScale = 18
      val newScale = 18
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = aNumber.setScale(newScale)
      expect(bNumber.scale() == newScale).toBeTruthy
      expect(bNumber == aNumber).toBeTruthy
    }

    it("testSetScaleRoundUp") {
      val a = "1231212478987482988429808779810457634781384756794987"
      val b = "123121247898748298842980877981045763478139"
      val aScale = 28
      val newScale = 18
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = aNumber.setScale(newScale, BigDecimal.ROUND_UP)
      expect(bNumber.scale()).toEqual(newScale)
      expect(bNumber.unscaledValue().toString).toEqual(b)
    }

    it("testSetScaleRoundDown") {
      val a = "1231212478987482988429808779810457634781384756794987"
      val b = "123121247898748298842980877981045763478138"
      val aScale = 28
      val newScale = 18
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = aNumber.setScale(newScale, BigDecimal.ROUND_DOWN)
      expect(bNumber.scale() == newScale).toBeTruthy
      expect(bNumber.unscaledValue().toString == b).toBeTruthy
    }

    it("testSetScaleRoundCeiling") {
      val a = "1231212478987482988429808779810457634781384756794987"
      val b = "123121247898748298842980877981045763478139"
      val aScale = 28
      val newScale = 18
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = aNumber.setScale(newScale, BigDecimal.ROUND_CEILING)
      expect(bNumber.scale() == newScale).toBeTruthy
      expect(bNumber.unscaledValue().toString == b).toBeTruthy
    }

    it("testSetScaleRoundFloor") {
      val a = "1231212478987482988429808779810457634781384756794987"
      val b = "123121247898748298842980877981045763478138"
      val aScale = 28
      val newScale = 18
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = aNumber.setScale(newScale, BigDecimal.ROUND_FLOOR)
      expect(bNumber.scale() == newScale).toBeTruthy
      expect(bNumber.unscaledValue().toString == b).toBeTruthy
    }

    it("testSetScaleRoundHalfUp") {
      val a = "1231212478987482988429808779810457634781384756794987"
      val b = "123121247898748298842980877981045763478138"
      val aScale = 28
      val newScale = 18
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = aNumber.setScale(newScale, BigDecimal.ROUND_HALF_UP)
      expect(bNumber.scale() == newScale).toBeTruthy
      expect(bNumber.unscaledValue().toString == b).toBeTruthy
    }

    it("testSetScaleRoundHalfDown") {
      val a = "1231212478987482988429808779810457634781384756794987"
      val b = "123121247898748298842980877981045763478138"
      val aScale = 28
      val newScale = 18
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = aNumber.setScale(newScale, BigDecimal.ROUND_HALF_DOWN)
      expect(bNumber.scale() == newScale).toBeTruthy
      expect(bNumber.unscaledValue().toString == b).toBeTruthy
    }

    it("testSetScaleRoundHalfEven") {
      val a = "1231212478987482988429808779810457634781384756794987"
      val b = "123121247898748298842980877981045763478138"
      val aScale = 28
      val newScale = 18
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = aNumber.setScale(newScale, BigDecimal.ROUND_HALF_EVEN)
      expect(bNumber.scale() == newScale).toBeTruthy
      expect(bNumber.unscaledValue().toString == b).toBeTruthy
    }

    it("testSetScaleIntRoundingMode") {
      val a = "1231212478987482988429808779810457634781384756794987"
      val aScale = 28
      val newScale = 18
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val result = aNumber.setScale(newScale, RoundingMode.HALF_EVEN)
      val res = "123121247898748298842980.877981045763478138"
      val resScale = 18
      expect(res).toEqual(result.toString)
      expect(resScale).toEqual(result.scale())
    }

    it("testMovePointLeftPos") {
      val a = "1231212478987482988429808779810457634781384756794987"
      val aScale = 28
      val shift = 18
      val resScale = 46
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = aNumber.movePointLeft(shift)
      expect(bNumber.scale() == resScale).toBeTruthy
      expect(bNumber.unscaledValue().toString == a).toBeTruthy
    }

    it("testMovePointLeftNeg") {
      val a = "1231212478987482988429808779810457634781384756794987"
      val aScale = 28
      val shift = -18
      val resScale = 10
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = aNumber.movePointLeft(shift)
      expect(bNumber.scale() == resScale).toBeTruthy
      expect(bNumber.unscaledValue().toString == a).toBeTruthy
    }

    it("testMovePointRightPosGreater") {
      val a = "1231212478987482988429808779810457634781384756794987"
      val aScale = 28
      val shift = 18
      val resScale = 10
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = aNumber.movePointRight(shift)
      expect(bNumber.scale() == resScale).toBeTruthy
      expect(bNumber.unscaledValue().toString == a).toBeTruthy
    }

    it("testMovePointRightPosLess") {
      val a = "1231212478987482988429808779810457634781384756794987"
      val b = "123121247898748298842980877981045763478138475679498700"
      val aScale = 28
      val shift = 30
      val resScale = 0
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = aNumber.movePointRight(shift)
      expect(bNumber.scale()).toEqual(resScale)
      expect(bNumber.unscaledValue().toString).toEqual(b)
    }

    it("testMovePointRightNeg") {
      val a = "1231212478987482988429808779810457634781384756794987"
      val aScale = 28
      val shift = -18
      val resScale = 46
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = aNumber.movePointRight(shift)
      expect(bNumber.scale()).toEqual(resScale)
      expect(bNumber.unscaledValue().toString).toEqual(a)
    }

    it("testMovePointRightException") {
      val a = "12312124789874829887348723648726347429808779810457634781384756794987"
      val aScale = Int.MaxValue
      val shift = -18
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      expect(() => aNumber.movePointRight(shift)).toThrow()
    }

    it("testPrecision") {
      val a = "12312124789874829887348723648726347429808779810457634781384756794987"
      val aScale = 14
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val prec = aNumber.precision()
      expect(68).toEqual(prec)
    }
  }
}
