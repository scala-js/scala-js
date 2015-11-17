/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.math

import java.math.BigInteger
import java.util.Arrays
import org.scalajs.jasminetest.JasmineTest

object BigIntegerTest extends JasmineTest {

  describe("java.lang.Math.BigInteger Constructors") {

    it("should accept 3 as a Byte Array") {
      val bi = new BigInteger(Array[Byte](3))
      expect(bi.intValue()).toEqual(3)
    }

    it("should accept 127 as a Byte Array") {
      val bi = new BigInteger(Array[Byte](127))
      expect(bi.intValue()).toEqual(127)
    }

    it("should accept 3 as aLong") {
      val bi = BigInteger.valueOf(3L)
      expect(bi.intValue()).toEqual(3)
      expect(bi.longValue()).toEqual(3L)
    }

    it("should accept 999999999 as aLong") {
      val bi = BigInteger.valueOf(999999999L)
      expect(bi.intValue()).toEqual(999999999)
      expect(bi.longValue()).toEqual(999999999L)
    }

    it("should accept 9999999999 as aLong") {
      val bi = BigInteger.valueOf(9999999999L)
      expect(bi.longValue()).toEqual(9999999999L)
    }

    it("should accept -999999999 as aLong") {
      val bi = BigInteger.valueOf(-999999999L)
      expect(bi.intValue()).toEqual(-999999999)
      expect(bi.longValue()).toEqual(-999999999L)
    }

    it("should accept -9999999999 as aLong") {
      val bi = BigInteger.valueOf(-9999999999L)
      expect(bi.longValue()).toEqual(-9999999999L)
    }

    it("should accept 99 as a string") {
      val bi = new BigInteger("99")
      expect(bi.intValue()).toEqual(99)
      expect(bi.longValue()).toEqual(99L)
    }

    it("should accept 999999999 as sting") {
      val bi = new BigInteger("999999999")
      expect(bi.intValue()).toEqual(999999999)
      expect(bi.longValue()).toEqual(999999999L)
    }

    it("should accept 9999999999 as a string") {
      val bi = new BigInteger("9999999999")
      expect(bi.longValue()).toEqual(9999999999L)
    }

    it("should accept -99 as a string") {
      val bi = new BigInteger("-99")
      expect(bi.intValue()).toEqual(-99)
      expect(bi.longValue()).toEqual(-99L)
    }

    it("should accept -999999999 as sting") {
      val bi = new BigInteger("-999999999")
      expect(bi.intValue()).toEqual(-999999999)
      expect(bi.longValue()).toEqual(-999999999L)
    }

    it("should accept -9999999999 as a string") {
      val bi = new BigInteger("-9999999999")
      expect(bi.longValue()).toEqual(-9999999999L)
    }

    it("should intialise from byte array of Pos two's complement") {
      val eBytesSignum = Array[Byte](27, -15, 65, 39)
      val eBytes = Array[Byte](27, -15, 65, 39)
      val expSignum = new BigInteger(eBytesSignum)
      expect(Arrays.equals(eBytes, expSignum.toByteArray)).toBeTruthy
    }

    it("should intialise from byte array of Neg two's complement") {
      val eBytesSignum = Array[Byte](-27, -15, 65, 39)
      val eBytes = Array[Byte](-27, -15, 65, 39)
      val expSignum = new BigInteger(eBytesSignum)
      expect(Arrays.equals(eBytes, expSignum.toByteArray)).toBeTruthy
    }

    it("should intialise from Pos byte array with explicit sign") {
      val eBytes = Array[Byte](27, -15, 65, 39)
      val eSign = 1
      val exp = new BigInteger(eSign, eBytes)
      expect(Arrays.equals(eBytes, exp.toByteArray)).toBeTruthy
    }

    it("should intialise from Zero byte array with explicit sign") {
      val eBytes = Array[Byte](0, 0, 0, 0)
      val eSign = 0
      val exp = new BigInteger(eSign, eBytes)
      expect(Arrays.equals(Array[Byte](0), exp.toByteArray)).toBeTruthy
    }

    it("should intialise from Neg small byte array with explicit sign") {
      val eBytes = Array[Byte](27)
      val eSign = -1
      val eRes = Array[Byte](-27)
      val exp = new BigInteger(eSign, eBytes)
      expect(Arrays.equals(eRes, exp.toByteArray)).toBeTruthy
    }

    it("should intialise from Neg byte array with explicit sign") {
      val eBytes = Array[Byte](27, -15, 65, 39)
      val eSign = -1
      val eRes = Array[Byte](-28, 14, -66, -39)
      val exp = new BigInteger(eSign, eBytes)
      expect(Arrays.equals(eRes, exp.toByteArray)).toBeTruthy
    }

    it("should intialise both Pos byte arrays arrays the same") {
      val eBytes = Array[Byte](27, -15, 65, 39)
      val eSign = 1
      val exp = new BigInteger(eSign, eBytes)
      val eBytesSignum = Array[Byte](27, -15, 65, 39)
      val expSignum = new BigInteger(eBytesSignum)

      expect(expSignum.compareTo(exp)).toEqual(0)
      expect(Arrays.equals(eBytes, exp.toByteArray)).toBeTruthy
      expect(Arrays.equals(eBytes, expSignum.toByteArray)).toBeTruthy
      expect(Arrays.equals(exp.toByteArray, expSignum.toByteArray)).toBeTruthy
    }

    it("should intialise both Neg byte arrays arrays the same") {
      val eBytes = Array[Byte](27, -15, 65, 39)
      val eSign = -1
      val eRes = Array[Byte](-28, 14, -66, -39)
      val exp = new BigInteger(eSign, eBytes)
      val eBytesSignum = Array[Byte](-28, 14, -66, -39)
      val expSignum = new BigInteger(eBytesSignum)

      expect(expSignum.toString).toEqual(exp.toString)
      expect(Arrays.equals(eRes, exp.toByteArray)).toBeTruthy
      expect(Arrays.equals(eBytesSignum, expSignum.toByteArray)).toBeTruthy
      expect(Arrays.equals(exp.toByteArray, expSignum.toByteArray)).toBeTruthy
    }
  }
}
