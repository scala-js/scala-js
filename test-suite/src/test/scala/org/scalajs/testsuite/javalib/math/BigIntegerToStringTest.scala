/*
 * Ported by Alistair Johnson from
 * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigIntegerToStringTest.java
 */

package org.scalajs.testsuite.javalib.math

import java.math.BigInteger
import org.scalajs.jasminetest.JasmineTest

object BigIntegerToStringTest extends JasmineTest {

  describe("BigIntegerToStringTest") {

    it("tes10PosVerySmall") {
      val value = "2"
      val aNumber = new BigInteger(value)
      val result = aNumber.toString()
      expect(aNumber.intValue()).toEqual(2)
      expect(result).toEqual(value)
    }

    it("tes10NegVerySmall") {
      val value = "-2"
      val aNumber = new BigInteger(value)
      val result = aNumber.toString()
      expect(aNumber.intValue()).toEqual(-2)
      expect(result).toEqual(value)
    }

    it("tes10PosSmall") {
      val value = "24"
      val aNumber = new BigInteger(value)
      val result = aNumber.toString()
      expect(aNumber.intValue()).toEqual(24)
      expect(result).toEqual(value)
    }

    it("tes10NegSmall") {
      val value = "-24"
      val aNumber = new BigInteger(value)
      val result = aNumber.toString()
      expect(aNumber.intValue()).toEqual(-24)
      expect(result).toEqual(value)
    }

    it("tes10PosLong") {
      val value = "2489756308572"
      val aNumber = new BigInteger(value)
      val result = aNumber.toString()
      expect(aNumber.longValue()).toEqual(2489756308572L)
      expect(result).toEqual(value)
    }

    it("tes10NegLong") {
      val value = "-2489756308572"
      val aNumber = new BigInteger(value)
      val result = aNumber.toString()
      expect(aNumber.longValue()).toEqual(-2489756308572L)
      expect(result).toEqual(value)
    }

    it("tes10Neg") {
      val value = "-2489756308572364789878394872984"
      val aNumber = new BigInteger(value)
      val result = aNumber.toString()
      expect(result).toEqual(value)
    }

    it("testRadix10Neg") {
      val value = "-2489756308572364789878394872984"
      val radix = 10
      val aNumber = new BigInteger(value, radix)
      val result = aNumber.toString(radix)
      expect(result).toEqual(value)
    }

    it("testRadix10Pos") {
      val value = "2387627892347567398736473476"
      val radix = 10
      val aNumber = new BigInteger(value, radix)
      val result = aNumber.toString(radix)
      expect(result).toEqual(value)
    }

    it("testRadix1610Neg") {
      val value = "-2489756308572364789878394872984"
      val radix = 16
      val aNumber = new BigInteger(value, radix)
      val result = aNumber.toString(radix)
      expect(result).toEqual(value)
    }

    it("testRadix1610Pos") {
      val value = "2387627892347567398736473476"
      val radix = 16
      val aNumber = new BigInteger(value, radix)
      val result = aNumber.toString(radix)
      expect(result).toEqual(value)
    }

    it("testRadix16Neg") {
      val value = "-287628a883451b800865c67e8d7ff20"
      val radix = 16
      val aNumber = new BigInteger(value, radix)
      val result = aNumber.toString(radix)
      expect(result).toEqual(value)
    }

    it("testRadix16Pos") {
      val value = "287628a883451b800865c67e8d7ff20"
      val radix = 16
      val aNumber = new BigInteger(value, radix)
      val result = aNumber.toString(radix)
      expect(result).toEqual(value)
    }

    it("testRadix24Neg") {
      val value = "-287628a88gmn3451b8ijk00865c67e8d7ff20"
      val radix = 24
      val aNumber = new BigInteger(value, radix)
      val result = aNumber.toString(radix)
      expect(result).toEqual(value)
    }

    it("testRadix24Pos") {
      val value = "287628a883451bg80ijhk0865c67e8d7ff20"
      val radix = 24
      val aNumber = new BigInteger(value, radix)
      val result = aNumber.toString(radix)
      expect(result).toEqual(value)
    }

    it("testRadix2Neg") {
      val value = "-101001100010010001001010101110000101010110001010010101010101010101010101010101010101010101010010101"
      val radix = 2
      val aNumber = new BigInteger(value, radix)
      val result = aNumber.toString(radix)
      expect(result).toEqual(value)
    }

    it("testRadix2Pos") {
      val value = "101000011111000000110101010101010101010001001010101010101010010101010101010000100010010"
      val radix = 2
      val aNumber = new BigInteger(value, radix)
      val result = aNumber.toString(radix)
      expect(result).toEqual(value)
    }

    it("testRadix36Neg") {
      val value = "-uhguweut98iu4h3478tq3985pq98yeiuth33485yq4aiuhalai485yiaehasdkr8tywi5uhslei8"
      val radix = 36
      val aNumber = new BigInteger(value, radix)
      val result = aNumber.toString(radix)
      expect(result).toEqual(value)
    }

    it("testRadix36Pos") {
      val value = "23895lt45y6vhgliuwhgi45y845htsuerhsi4586ysuerhtsio5y68peruhgsil4568ypeorihtse48y6"
      val radix = 36
      val aNumber = new BigInteger(value, radix)
      val result = aNumber.toString(radix)
      expect(result).toEqual(value)
    }

    it("testRadixOutOfRange") {
      val value = "442429234853876401"
      val radix = 10
      val aNumber = new BigInteger(value, radix)
      val result = aNumber.toString(45)
      expect(result).toEqual(value)
    }
  }
}
