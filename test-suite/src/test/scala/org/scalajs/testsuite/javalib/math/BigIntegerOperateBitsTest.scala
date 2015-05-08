/*
 *  Ported by Alistair Johnson from https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigIntegerOperateBitsTest.java
 */

package org.scalajs.testsuite.javalib.math

import java.math.BigInteger
import org.scalajs.jasminetest.JasmineTest

object BigIntegerOperateBitsTest extends JasmineTest {

  describe("BigIntegerOperateBitsTest") {

    it("testBitCountNeg") {
      val aNumber = new BigInteger("-12378634756382937873487638746283767238657872368748726875")
      expect(aNumber.bitCount()).toEqual(87)
    }

    it("testBitCountPos") {
      val aNumber = new BigInteger("12378634756343564757582937873487638746283767238657872368748726875")
      expect(aNumber.bitCount()).toEqual(107)
    }

    it("testBitCountZero") {
      val aNumber = new BigInteger("0")
      expect(aNumber.bitCount()).toEqual(0)
    }

    it("testBitLengthNegative1") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = -1
      val aNumber = new BigInteger(aSign, aBytes)
      expect(aNumber.bitLength()).toEqual(108)
    }

    it("testBitLengthNegative2") {
      val aBytes = Array[Byte](-128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = -1
      val aNumber = new BigInteger(aSign, aBytes)
      expect(aNumber.bitLength()).toEqual(96)
    }

    it("testBitLengthNegative3") {
      val aBytes = Array[Byte](1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      val aSign = -1
      val aNumber = new BigInteger(aSign, aBytes)
      expect(aNumber.bitLength()).toEqual(80)
    }

    it("testBitLengthPositive1") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = 1
      val aNumber = new BigInteger(aSign, aBytes)
      expect(aNumber.bitLength()).toEqual(108)
    }

    it("testBitLengthPositive2") {
      val aBytes = Array[Byte](-128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val aNumber = new BigInteger(aSign, aBytes)
      expect(aNumber.bitLength()).toEqual(96)
    }

    it("testBitLengthPositive3") {
      val aBytes = Array[Byte](1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      val aSign = 1
      val aNumber = new BigInteger(aSign, aBytes)
      expect(aNumber.bitLength()).toEqual(81)
    }

    it("testBitLengthZero") {
      val aNumber = new BigInteger("0")
      expect(aNumber.bitLength()).toEqual(0)
    }

    it("testClearBitException") {
      val aBytes = Array[Byte](-1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = -7
      val aNumber = new BigInteger(aSign, aBytes)
      expect(() => aNumber.clearBit(number)).toThrow()
    }

    it("testClearBitNegativeInside1") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = -1
      val number = 15
      val rBytes = Array[Byte](-2, 127, -57, -101, 1, 75, -90, -46, -92, -4, 14, 92, -26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.clearBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testClearBitNegativeInside2") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = -1
      val number = 44
      val rBytes = Array[Byte](-2, 127, -57, -101, 1, 75, -90, -62, -92, -4, 14, -36, -26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.clearBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testClearBitNegativeInside3") {
      val as = "-18446744073709551615"
      val number = 2
      val aNumber = new BigInteger(as)
      val result = aNumber.clearBit(number)
      expect(as).toEqual(result.toString)
    }

    it("testClearBitNegativeInside4") {
      val as = "-4294967295"
      val res = "-4294967296"
      val number = 0
      val aNumber = new BigInteger(as)
      val result = aNumber.clearBit(number)
      expect(res).toEqual(result.toString)
    }

    it("testClearBitNegativeInside5") {
      val as = "-18446744073709551615"
      val res = "-18446744073709551616"
      val number = 0
      val aNumber = new BigInteger(as)
      val result = aNumber.clearBit(number)
      expect(res).toEqual(result.toString)
    }

    it("testClearBitNegativeOutside1") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = -1
      val number = 150
      val rBytes = Array[Byte](-65, -1, -1, -1, -1, -1, -2, 127, -57, -101, 1, 75, -90, -46, -92, -4, 14, -36, -26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.clearBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testClearBitNegativeOutside2") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = -1
      val number = 165
      val rBytes = Array[Byte](-33, -1, -1, -1, -1, -1, -1, -1, -2, 127, -57, -101, 1, 75, -90, -46, -92, -4, 14, -36, -26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.clearBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testClearBitPositiveInside1") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = 20
      val rBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -31, 35, 26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.clearBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testClearBitPositiveInside2") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = 17
      val rBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.clearBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testClearBitPositiveInside3") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = 45
      val rBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 13, 91, 3, -15, 35, 26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.clearBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testClearBitPositiveInside4") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = 50
      val rBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.clearBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testClearBitPositiveInside5") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = 63
      val rBytes = Array[Byte](1, -128, 56, 100, -2, 52, 89, 45, 91, 3, -15, 35, 26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.clearBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testClearBitPositiveOutside1") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = 150
      val rBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.clearBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testClearBitPositiveOutside2") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = 191
      val rBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.clearBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testClearBitTopNegative") {
      val aBytes = Array[Byte](1, -128, 56, 100, -15, 35, 26)
      val aSign = -1
      val number = 63
      val rBytes = Array[Byte](-1, 127, -2, 127, -57, -101, 14, -36, -26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.clearBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testClearBitZero") {
      val aBytes = Array[Byte](0)
      val aSign = 0
      val number = 0
      val rBytes = Array[Byte](0)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.clearBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(0)
    }

    it("testClearBitZeroOutside1") {
      val aBytes = Array[Byte](0)
      val aSign = 0
      val number = 95
      val rBytes = Array[Byte](0)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.clearBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(0)
    }

    it("testFlipBitException") {
      val aBytes = Array[Byte](-1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = -7
      val aNumber = new BigInteger(aSign, aBytes)
      expect(() => aNumber.flipBit(number)).toThrow()
    }

    it("testFlipBitLeftmostNegative") {
      val aBytes = Array[Byte](1, -128, 56, 100, -15, 35, 26)
      val aSign = -1
      val number = 48
      val rBytes = Array[Byte](-1, 127, -57, -101, 14, -36, -26, 49)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.flipBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testFlipBitLeftmostPositive") {
      val aBytes = Array[Byte](1, -128, 56, 100, -15, 35, 26)
      val aSign = 1
      val number = 48
      val rBytes = Array[Byte](0, -128, 56, 100, -15, 35, 26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.flipBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testFlipBitNegativeInside1") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = -1
      val number = 15
      val rBytes = Array[Byte](-2, 127, -57, -101, 1, 75, -90, -46, -92, -4, 14, 92, -26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.flipBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testFlipBitNegativeInside2") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = -1
      val number = 45
      val rBytes = Array[Byte](-2, 127, -57, -101, 1, 75, -90, -14, -92, -4, 14, -36, -26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.flipBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testFlipBitNegativeInside3") {
      val as = "-18446744073709551615"
      val res = "-18446744073709551611"
      val number = 2
      val aNumber = new BigInteger(as)
      val result = aNumber.flipBit(number)
      expect(res).toEqual(result.toString)
    }

    it("testFlipBitNegativeInside4") {
      val as = "-4294967295"
      val res = "-4294967296"
      val number = 0
      val aNumber = new BigInteger(as)
      val result = aNumber.flipBit(number)
      expect(res).toEqual(result.toString)
    }

    it("testFlipBitNegativeInside5") {
      val as = "-18446744073709551615"
      val res = "-18446744073709551616"
      val number = 0
      val aNumber = new BigInteger(as)
      val result = aNumber.flipBit(number)
      expect(res).toEqual(result.toString)
    }

    it("testFlipBitNegativeOutside1") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = -1
      val number = 150
      val rBytes = Array[Byte](-65, -1, -1, -1, -1, -1, -2, 127, -57, -101, 1, 75, -90, -46, -92, -4, 14, -36, -26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.flipBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testFlipBitNegativeOutside2") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = -1
      val number = 191
      val rBytes = Array[Byte](-1, 127, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -2, 127, -57, -101, 1, 75, -90, -46, -92, -4, 14, -36, -26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.flipBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testFlipBitPositiveInside1") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = 15
      val rBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, -93, 26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.flipBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testFlipBitPositiveInside2") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = 45
      val rBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 13, 91, 3, -15, 35, 26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.flipBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testFlipBitPositiveOutside1") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = 150
      val rBytes = Array[Byte](64, 0, 0, 0, 0, 0, 1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.flipBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testFlipBitPositiveOutside2") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = 191
      val rBytes = Array[Byte](0, -128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.flipBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testFlipBitZero") {
      val aBytes = Array[Byte](0)
      val aSign = 0
      val number = 0
      val rBytes = Array[Byte](1)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.flipBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testFlipBitZeroOutside1") {
      val aBytes = Array[Byte](0)
      val aSign = 0
      val number = 62
      val rBytes = Array[Byte](64, 0, 0, 0, 0, 0, 0, 0)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.flipBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testFlipBitZeroOutside2") {
      val aBytes = Array[Byte](0)
      val aSign = 0
      val number = 63
      val rBytes = Array[Byte](0, -128, 0, 0, 0, 0, 0, 0, 0)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.flipBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testSetBitBug1331") {
      val result = BigInteger.valueOf(0L).setBit(191)
      expect(result.toString).toEqual("3138550867693340381917894711603833208051177722232017256448")
      expect(result.signum()).toEqual(1)
    }

    it("testSetBitException") {
      val aBytes = Array[Byte](-1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = -7
      var aNumber = new BigInteger(aSign, aBytes)
      expect(() => aNumber.setBit(number)).toThrow()
    }

    it("testSetBitLeftmostNegative") {
      val aBytes = Array[Byte](1, -128, 56, 100, -15, 35, 26)
      val aSign = -1
      val number = 48
      val rBytes = Array[Byte](-1, 127, -57, -101, 14, -36, -26, 49)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.setBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testSetBitNegativeInside1") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = -1
      val number = 15
      val rBytes = Array[Byte](-2, 127, -57, -101, 1, 75, -90, -46, -92, -4, 14, -36, -26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.setBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testSetBitNegativeInside2") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = -1
      val number = 44
      val rBytes = Array[Byte](-2, 127, -57, -101, 1, 75, -90, -46, -92, -4, 14, -36, -26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.setBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testSetBitNegativeInside3") {
      val as = "-18446744073709551615"
      val res = "-18446744073709551611"
      val number = 2
      val aNumber = new BigInteger(as)
      val result = aNumber.setBit(number)
      expect(res).toEqual(result.toString)
    }

    it("testSetBitNegativeInside4") {
      val as = "-4294967295"
      val number = 0
      val aNumber = new BigInteger(as)
      val result = aNumber.setBit(number)
      expect(result.toString).toEqual(as)
    }

    it("testSetBitNegativeInside5") {
      val as = "-18446744073709551615"
      val number = 0
      val aNumber = new BigInteger(as)
      val result = aNumber.setBit(number)
      expect(result.toString).toEqual(as)
    }

    it("testSetBitNegativeOutside1") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = -1
      val number = 150
      val rBytes = Array[Byte](-2, 127, -57, -101, 1, 75, -90, -46, -92, -4, 14, -36, -26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.setBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testSetBitNegativeOutside2") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = -1
      val number = 191
      val rBytes = Array[Byte](-2, 127, -57, -101, 1, 75, -90, -46, -92, -4, 14, -36, -26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.setBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testSetBitPositiveInside1") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = 20
      val rBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.setBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testSetBitPositiveInside2") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = 17
      val rBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -13, 35, 26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.setBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testSetBitPositiveInside3") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = 45
      val rBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.setBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testSetBitPositiveInside4") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = 50
      val rBytes = Array[Byte](1, -128, 56, 100, -2, -76, 93, 45, 91, 3, -15, 35, 26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.setBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testSetBitPositiveOutside1") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = 150
      val rBytes = Array[Byte](64, 0, 0, 0, 0, 0, 1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.setBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testSetBitPositiveOutside2") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = 223
      val rBytes = Array[Byte](0, -128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.setBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testSetBitTopPositive") {
      val aBytes = Array[Byte](1, -128, 56, 100, -15, 35, 26)
      val aSign = 1
      val number = 63
      val rBytes = Array[Byte](0, -128, 1, -128, 56, 100, -15, 35, 26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.setBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testSetBitZero") {
      val aBytes = Array[Byte](0)
      val aSign = 0
      val number = 0
      val rBytes = Array[Byte](1)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.setBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testSetBitZeroOutside1") {
      val aBytes = Array[Byte](0)
      val aSign = 0
      val number = 95
      val rBytes = Array[Byte](0, -128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.setBit(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testShiftLeft1") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = 0
      val rBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.shiftLeft(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testShiftLeft2") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = -27
      val rBytes = Array[Byte](48, 7, 12, -97, -42, -117, 37, -85, 96)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.shiftLeft(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testShiftLeft3") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = 27
      val rBytes = Array[Byte](12, 1, -61, 39, -11, -94, -55, 106, -40, 31, -119, 24, -48, 0, 0, 0)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.shiftLeft(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testShiftLeft4") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = 45
      val rBytes = Array[Byte](48, 7, 12, -97, -42, -117, 37, -85, 96, 126, 36, 99, 64, 0, 0, 0, 0, 0)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.shiftLeft(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testShiftLeft5") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = -1
      val number = 45
      val rBytes = Array[Byte](-49, -8, -13, 96, 41, 116, -38, 84, -97, -127, -37, -100, -64, 0, 0, 0, 0, 0)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.shiftLeft(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testShiftRight1") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = 0
      val rBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.shiftRight(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testShiftRight2") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = -27
      val rBytes = Array[Byte](12, 1, -61, 39, -11, -94, -55, 106, -40, 31, -119, 24, -48, 0, 0, 0)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.shiftRight(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testShiftRight3") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = 27
      val rBytes = Array[Byte](48, 7, 12, -97, -42, -117, 37, -85, 96)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.shiftRight(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testShiftRight4") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = 45
      val rBytes = Array[Byte](12, 1, -61, 39, -11, -94, -55)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.shiftRight(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testShiftRight5") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = 300
      val rBytes = Array[Byte](0)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.shiftRight(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(0)
    }

    it("testShiftRightNegNonZeroes") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 0, 0, 0, 0, 0, 0, 0, 0)
      val aSign = -1
      val number = 68
      val rBytes = Array[Byte](-25, -4, 121, -80, 20, -70, 109, 42)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.shiftRight(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testShiftRightNegNonZeroesMul32") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 1, 0, 0, 0, 0, 0, 0, 0)
      val aSign = -1
      val number = 64
      val rBytes = Array[Byte](-2, 127, -57, -101, 1, 75, -90, -46, -92)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.shiftRight(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testShiftRightNegZeroes") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      val aSign = -1
      val number = 68
      val rBytes = Array[Byte](-25, -4, 121, -80, 20, -70, 109, 48)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.shiftRight(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testShiftRightNegZeroesMul32") {
      val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 0, 0, 0, 0, 0, 0, 0, 0)
      val aSign = -1
      val number = 64
      val rBytes = Array[Byte](-2, 127, -57, -101, 1, 75, -90, -46, -91)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.shiftRight(number)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testTestBitException") {
      val aBytes = Array[Byte](-1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = -7
      val aNumber = new BigInteger(aSign, aBytes)
      expect(() =>  aNumber.testBit(number)).toThrow()
    }

    it("testTestBitNegative1") {
      val aBytes = Array[Byte](-1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = -1
      val number = 7
      val aNumber = new BigInteger(aSign, aBytes)
      expect(aNumber.testBit(number)).toBeTruthy
    }

    it("testTestBitNegative2") {
      val aBytes = Array[Byte](-1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = -1
      val number = 45
      val aNumber = new BigInteger(aSign, aBytes)
      expect(!aNumber.testBit(number)).toBeTruthy
    }

    it("testTestBitNegative3") {
      val aBytes = Array[Byte](-1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = -1
      val number = 300
      val aNumber = new BigInteger(aSign, aBytes)
      expect(aNumber.testBit(number)).toBeTruthy
    }

    it("testTestBitPositive1") {
      val aBytes = Array[Byte](-1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = 7
      val aNumber = new BigInteger(aSign, aBytes)
      expect(!aNumber.testBit(number)).toBeTruthy
    }

    it("testTestBitPositive2") {
      val aBytes = Array[Byte](-1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = 45
      val aNumber = new BigInteger(aSign, aBytes)
      expect(aNumber.testBit(number)).toBeTruthy
    }

    it("testTestBitPositive3") {
      val aBytes = Array[Byte](-1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
      val aSign = 1
      val number = 300
      val aNumber = new BigInteger(aSign, aBytes)
      expect(!aNumber.testBit(number)).toBeTruthy
    }
  }
}
