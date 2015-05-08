/*
 *  Ported by Alistair Johnson from https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigDecimalCompareTest.java
 */

package org.scalajs.testsuite.javalib.math

import java.math._

import org.scalajs.jasminetest.JasmineTest

object BigDecimalCompareTest extends JasmineTest {

  describe("BigDecimalCompareTest") {

    it("testAbsMathContextNeg") {
      val a = "-123809648392384754573567356745735.63567890295784902768787678287E+21"
      val aNumber = new BigDecimal(a)
      val precision = 15
      val rm = RoundingMode.HALF_DOWN
      val mc = new MathContext(precision, rm)
      val result = "1.23809648392385E+53"
      val resScale = -39
      val res = aNumber.abs(mc)
      expect(result).toEqual(res.toString)
      expect(resScale).toEqual(res.scale())
    }

    it("testAbsMathContextPos") {
      val a = "123809648392384754573567356745735.63567890295784902768787678287E+21"
      val aNumber = new BigDecimal(a)
      val precision = 41
      val rm = RoundingMode.HALF_EVEN
      val mc = new MathContext(precision, rm)
      val result = "1.2380964839238475457356735674573563567890E+53"
      val resScale = -13
      val res = aNumber.abs(mc)
      expect(result).toEqual(res.toString)
      expect(resScale).toEqual(res.scale())
    }

    it("testAbsNeg") {
      val a = "-123809648392384754573567356745735.63567890295784902768787678287E+21"
      val aNumber = new BigDecimal(a)
      val result = "123809648392384754573567356745735635678902957849027687.87678287"
      expect(aNumber.abs().toString).toEqual(result)
    }

    it("testAbsPos") {
      val a = "123809648392384754573567356745735.63567890295784902768787678287E+21"
      val aNumber = new BigDecimal(a)
      val result = "123809648392384754573567356745735635678902957849027687.87678287"
      expect(aNumber.abs().toString).toEqual(result)
    }

    it("testCompareEqualScale1") {
      val a = "12380964839238475457356735674573563567890295784902768787678287"
      val aScale = 18
      val b = "4573563567890295784902768787678287"
      val bScale = 18
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = new BigDecimal(new BigInteger(b), bScale)
      val result = 1
      expect(aNumber.compareTo(bNumber)).toEqual(result)
    }

    it("testCompareEqualScale2") {
      val a = "12380964839238475457356735674573563567890295784902768787678287"
      val aScale = 18
      val b = "4573563923487289357829759278282992758247567890295784902768787678287"
      val bScale = 18
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = new BigDecimal(new BigInteger(b), bScale)
      val result = -1
      expect(aNumber.compareTo(bNumber)).toEqual(result)
    }

    it("testCompareGreaterScale1") {
      val a = "12380964839238475457356735674573563567890295784902768787678287"
      val aScale = 28
      val b = "4573563567890295784902768787678287"
      val bScale = 18
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = new BigDecimal(new BigInteger(b), bScale)
      val result = 1
      expect(aNumber.compareTo(bNumber)).toEqual(result)
    }

    it("testCompareGreaterScale2") {
      val a = "12380964839238475457356735674573563567890295784902768787678287"
      val aScale = 48
      val b = "4573563567890295784902768787678287"
      val bScale = 2
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = new BigDecimal(new BigInteger(b), bScale)
      val result = -1
      expect(aNumber.compareTo(bNumber)).toEqual(result)
    }

    it("testCompareLessScale1") {
      val a = "12380964839238475457356735674573563567890295784902768787678287"
      val aScale = 18
      val b = "4573563567890295784902768787678287"
      val bScale = 28
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = new BigDecimal(new BigInteger(b), bScale)
      val result = 1
      expect(aNumber.compareTo(bNumber)).toEqual(result)
    }

    it("testCompareLessScale2") {
      val a = "12380964839238475457356735674573"
      val aScale = 36
      val b = "45735635948573894578349572001798379183767890295784902768787678287"
      val bScale = 48
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = new BigDecimal(new BigInteger(b), bScale)
      val result = -1
      expect(aNumber.compareTo(bNumber)).toEqual(result)
    }

    it("testEqualsEqual") {
      val a = "92948782094488478231212478987482988429808779810457634781384756794987"
      val aScale = -24
      val b = "92948782094488478231212478987482988429808779810457634781384756794987"
      val bScale = -24
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = new BigDecimal(new BigInteger(b), bScale)
      expect(aNumber == bNumber).toBeTruthy
    }

    it("testEqualsNull") {
      val a = "92948782094488478231212478987482988429808779810457634781384756794987"
      val aScale = -24
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      expect(aNumber == null).toBeFalsy
    }

    it("testEqualsUnequal1") {
      val a = "92948782094488478231212478987482988429808779810457634781384756794987"
      val aScale = -24
      val b = "7472334223847623782375469293018787918347987234564568"
      val bScale = 13
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = new BigDecimal(new BigInteger(b), bScale)
      expect(aNumber == bNumber).toBeFalsy
    }

    it("testEqualsUnequal2") {
      val a = "92948782094488478231212478987482988429808779810457634781384756794987"
      val aScale = -24
      val b = "92948782094488478231212478987482988429808779810457634781384756794987"
      val bScale = 13
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = new BigDecimal(new BigInteger(b), bScale)
      expect(aNumber == bNumber).toBeFalsy
    }

    it("testEqualsUnequal3") {
      val a = "92948782094488478231212478987482988429808779810457634781384756794987"
      val aScale = -24
      val b = "92948782094488478231212478987482988429808779810457634781384756794987"
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      // changed '==' to 'equals' to remove compiler warning
      expect(aNumber.equals(b)).toBeFalsy
    }

    it("testFractionScale") {
      var a = new BigDecimal("0.02")
      var b = new BigDecimal("0.02000")
      expect(0).toEqual(a.compareTo(b))
      val a1 = new BigDecimal("0.029900000000000003")
      val a2 = new BigDecimal("0.0001")
      a = a1.add(a2)
      b = new BigDecimal("0.03990")
      expect(-1).toEqual(a.compareTo(b))
    }

    it("testHashCodeEqual") {
      val a = "92948782094488478231212478987482988429808779810457634781384756794987"
      val aScale = -24
      val b = "92948782094488478231212478987482988429808779810457634781384756794987"
      val bScale = -24
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = new BigDecimal(new BigInteger(b), bScale)
      expect(aNumber.hashCode).toEqual(bNumber.hashCode)
    }

    it("testHashCodeUnequal") {
      val a = "8478231212478987482988429808779810457634781384756794987"
      val aScale = 41
      val b = "92948782094488478231212478987482988429808779810457634781384756794987"
      val bScale = -24
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = new BigDecimal(new BigInteger(b), bScale)
      expect(aNumber.hashCode != bNumber.hashCode).toBeTruthy
    }

    it("testMaxEqual") {
      val a = "8478231212478987482988429808779810457634781384756794987"
      val aScale = 41
      val b = "8478231212478987482988429808779810457634781384756794987"
      val bScale = 41
      val c = "8478231212478987482988429808779810457634781384756794987"
      val cScale = 41
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = new BigDecimal(new BigInteger(b), bScale)
      val cNumber = new BigDecimal(new BigInteger(c), cScale)
      expect(cNumber == aNumber.max(bNumber)).toBeTruthy
    }

    it("testMaxUnequal1") {
      val a = "92948782094488478231212478987482988429808779810457634781384756794987"
      val aScale = 24
      val b = "92948782094488478231212478987482988429808779810457634781384756794987"
      val bScale = 41
      val c = "92948782094488478231212478987482988429808779810457634781384756794987"
      val cScale = 24
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = new BigDecimal(new BigInteger(b), bScale)
      val cNumber = new BigDecimal(new BigInteger(c), cScale)
      expect(cNumber == aNumber.max(bNumber)).toBeTruthy
    }

    it("testMaxUnequal2") {
      val a = "92948782094488478231212478987482988429808779810457634781384756794987"
      val aScale = 41
      val b = "94488478231212478987482988429808779810457634781384756794987"
      val bScale = 41
      val c = "92948782094488478231212478987482988429808779810457634781384756794987"
      val cScale = 41
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = new BigDecimal(new BigInteger(b), bScale)
      val cNumber = new BigDecimal(new BigInteger(c), cScale)
      expect(cNumber == aNumber.max(bNumber)).toBeTruthy
    }

    it("testMinEqual") {
      val a = "8478231212478987482988429808779810457634781384756794987"
      val aScale = 41
      val b = "8478231212478987482988429808779810457634781384756794987"
      val bScale = 41
      val c = "8478231212478987482988429808779810457634781384756794987"
      val cScale = 41
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = new BigDecimal(new BigInteger(b), bScale)
      val cNumber = new BigDecimal(new BigInteger(c), cScale)
      expect(cNumber == aNumber.min(bNumber)).toBeTruthy
    }

    it("testMinUnequal1") {
      val a = "92948782094488478231212478987482988429808779810457634781384756794987"
      val aScale = 24
      val b = "92948782094488478231212478987482988429808779810457634781384756794987"
      val bScale = 41
      val c = "92948782094488478231212478987482988429808779810457634781384756794987"
      val cScale = 41
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = new BigDecimal(new BigInteger(b), bScale)
      val cNumber = new BigDecimal(new BigInteger(c), cScale)
      expect(cNumber == aNumber.min(bNumber)).toBeTruthy
    }

    it("testMinUnequal2") {
      val a = "92948782094488478231212478987482988429808779810457634781384756794987"
      val aScale = 41
      val b = "94488478231212478987482988429808779810457634781384756794987"
      val bScale = 41
      val c = "94488478231212478987482988429808779810457634781384756794987"
      val cScale = 41
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val bNumber = new BigDecimal(new BigInteger(b), bScale)
      val cNumber = new BigDecimal(new BigInteger(c), cScale)
      expect(cNumber == aNumber.min(bNumber)).toBeTruthy
    }

    it("testNegateMathContextNegative") {
      val a = "-92948782094488478231212478987482988429808779810457634781384756794987"
      val aScale = 49
      val precision = 46
      val rm = RoundingMode.CEILING
      val mc = new MathContext(precision, rm)
      val c = "9294878209448847823.121247898748298842980877982"
      val cScale = 27
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val res = aNumber.negate(mc)
      expect(c).toEqual(res.toString)
      expect(cScale).toEqual(res.scale())
    }

    it("testNegateMathContextPositive") {
      val a = "92948782094488478231212478987482988429808779810457634781384756794987"
      val aScale = 41
      val precision = 37
      val rm = RoundingMode.FLOOR
      val mc = new MathContext(precision, rm)
      val c = "-929487820944884782312124789.8748298843"
      val cScale = 10
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val res = aNumber.negate(mc)
      expect(c).toEqual(res.toString)
      expect(cScale).toEqual(res.scale())
    }

    it("testNegateNegative") {
      val a = "-92948782094488478231212478987482988429808779810457634781384756794987"
      val aScale = 41
      val c = "92948782094488478231212478987482988429808779810457634781384756794987"
      val cScale = 41
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val cNumber = new BigDecimal(new BigInteger(c), cScale)
      expect(cNumber == aNumber.negate()).toBeTruthy
    }

    it("testNegatePositive") {
      val a = "92948782094488478231212478987482988429808779810457634781384756794987"
      val aScale = 41
      val c = "-92948782094488478231212478987482988429808779810457634781384756794987"
      val cScale = 41
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val cNumber = new BigDecimal(new BigInteger(c), cScale)
      expect(cNumber == aNumber.negate()).toBeTruthy
    }

    it("testPlusMathContextNegative") {
      val a = "-92948782094488478231212478987482988429808779810457634781384756794987"
      val aScale = 49
      val precision = 46
      val rm = RoundingMode.CEILING
      val mc = new MathContext(precision, rm)
      val c = "-9294878209448847823.121247898748298842980877981"
      val cScale = 27
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val res = aNumber.plus(mc)
      expect(c).toEqual(res.toString)
      expect(cScale).toEqual(res.scale())
    }

    it("testPlusMathContextPositive") {
      val a = "92948782094488478231212478987482988429808779810457634781384756794987"
      val aScale = 41
      val precision = 37
      val rm = RoundingMode.FLOOR
      val mc = new MathContext(precision, rm)
      val c = "929487820944884782312124789.8748298842"
      val cScale = 10
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val res = aNumber.plus(mc)
      expect(c).toEqual(res.toString)
      expect(cScale).toEqual(res.scale())
    }

    it("testPlusNegative") {
      val a = "-92948782094488478231212478987482988429808779810457634781384756794987"
      val aScale = 41
      val c = "-92948782094488478231212478987482988429808779810457634781384756794987"
      val cScale = 41
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val cNumber = new BigDecimal(new BigInteger(c), cScale)
      expect(cNumber == aNumber.plus()).toBeTruthy
    }

    it("testPlusPositive") {
      val a = "92948782094488478231212478987482988429808779810457634781384756794987"
      val aScale = 41
      val c = "92948782094488478231212478987482988429808779810457634781384756794987"
      val cScale = 41
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      val cNumber = new BigDecimal(new BigInteger(c), cScale)
      expect(cNumber == aNumber.plus()).toBeTruthy
    }

    it("testSignumNegative") {
      val a = "-92948782094488478231212478987482988429808779810457634781384756794987"
      val aScale = 41
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      expect(aNumber.signum()).toEqual(-1)
    }

    it("testSignumPositive") {
      val a = "92948782094488478231212478987482988429808779810457634781384756794987"
      val aScale = 41
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      expect(aNumber.signum()).toEqual(1)
    }

    it("testSignumZero") {
      val a = "0"
      val aScale = 41
      val aNumber = new BigDecimal(new BigInteger(a), aScale)
      expect(aNumber.signum()).toEqual(0)
    }
  }
}
