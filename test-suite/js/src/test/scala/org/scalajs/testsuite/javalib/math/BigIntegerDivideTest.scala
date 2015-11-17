// scalastyle:off line.size.limit
/*
 * Ported by Alistair Johnson from
 * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigIntegerDivideTest.java
 */
// scalastyle:on line.size.limit

package org.scalajs.testsuite.javalib.math

import java.math.BigInteger
import org.scalajs.jasminetest.JasmineTest

object BigIntegerDivideTest extends JasmineTest {

  describe("BigIntegerDivideTest") {

    it("testCase1") {
      val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
      val bBytes = Array[Byte](0)
      val aSign = 1
      val bSign = 0
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      expect(() => aNumber.divide(bNumber)).toThrow()
    }

    it("testCase10") {
      val aBytes = Array[Byte](1, 100, 56, 7, 98, -1, 39, -128, 127, 5, 6, 7, 8, 9)
      val bBytes = Array[Byte](15, 48, -29, 7, 98, -1, 39, -128)
      val aSign = -1
      val bSign = -1
      val rBytes = Array[Byte](23, 115, 11, 78, 35, -11)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.divide(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testCase11") {
      val aBytes = Array[Byte](0)
      val bBytes = Array[Byte](15, 48, -29, 7, 98, -1, 39, -128)
      val aSign = 0
      val bSign = -1
      val rBytes = Array[Byte](0)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.divide(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(0)
    }

    it("testCase12") {
      val bBytes = Array[Byte](15, 48, -29, 7, 98, -1, 39, -128)
      val bSign = -1
      val rBytes = Array[Byte](0)
      val aNumber = BigInteger.ZERO
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.divide(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(0)
    }

    it("testCase13") {
      val aBytes = Array[Byte](15, 48, -29, 7, 98, -1, 39, -128)
      val aSign = 1
      val rBytes = Array[Byte](15, 48, -29, 7, 98, -1, 39, -128)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = BigInteger.ONE
      val result = aNumber.divide(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testCase14") {
      val rBytes = Array[Byte](1)
      val aNumber = BigInteger.ONE
      val bNumber = BigInteger.ONE
      val result = aNumber.divide(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testCase15") {
      val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
      val bBytes = Array[Byte](0)
      val aSign = 1
      val bSign = 0
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      expect(() => aNumber.remainder(bNumber)).toThrow()
    }

    it("testCase16") {
      val aBytes = Array[Byte](-127, 100, 56, 7, 98, -1, 39, -128, 127)
      val bBytes = Array[Byte](-127, 100, 56, 7, 98, -1, 39, -128, 127)
      val aSign = 1
      val bSign = 1
      val rBytes = Array[Byte](0)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.remainder(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(0)
    }

    it("testCase17") {
      val aBytes = Array[Byte](-127, 100, 56, 7, 98, -1, 39, -128, 127, 75)
      val bBytes = Array[Byte](27, -15, 65, 39, 100)
      val aSign = 1
      val bSign = 1
      val rBytes = Array[Byte](12, -21, 73, 56, 27)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.remainder(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testCase18") {
      val aBytes = Array[Byte](-127, 100, 56, 7, 98, -1, 39, -128, 127, 75)
      val bBytes = Array[Byte](27, -15, 65, 39, 100)
      val aSign = -1
      val bSign = -1
      val rBytes = Array[Byte](-13, 20, -74, -57, -27)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.remainder(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testCase19") {
      val aBytes = Array[Byte](-127, 100, 56, 7, 98, -1, 39, -128, 127, 75)
      val bBytes = Array[Byte](27, -15, 65, 39, 100)
      val aSign = 1
      val bSign = -1
      val rBytes = Array[Byte](12, -21, 73, 56, 27)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.remainder(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testCase2") {
      val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
      val aSign = 1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = BigInteger.ZERO
      expect(() => aNumber.divide(bNumber)).toThrow()
    }

    it("testCase20") {
      val aBytes = Array[Byte](-127, 100, 56, 7, 98, -1, 39, -128, 127, 75)
      val bBytes = Array[Byte](27, -15, 65, 39, 100)
      val aSign = -1
      val bSign = 1
      val rBytes = Array[Byte](-13, 20, -74, -57, -27)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.remainder(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testCase21") {
      val aBytes = Array[Byte](-127, 100, 56, 7, 98, -1, 39, -128, 127, 75)
      val bBytes = Array[Byte](27, -15, 65, 39, 100)
      val aSign = -1
      val bSign = 1
      val rBytes = Array[Array[Byte]](Array[Byte](-5, 94, -115, -74, -85, 84), Array[Byte](-13, 20, -74, -57, -27))
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.divideAndRemainder(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result(0).toByteArray()
      for (i <- 0 until resBytes.length){
        expect(resBytes(i)).toEqual(rBytes(0)(i))
      }
      expect(result(0).signum()).toEqual(-1)
      resBytes = result(1).toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(1)(i))
        expect(result(1).signum()).toEqual(-1)
      }
    }

    it("testCase22") {
      val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
      val bBytes = Array[Byte](1, 30, 40, 56, -1, 45)
      val aSign = 1
      val bSign = -1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      expect(() => aNumber.mod(bNumber)).toThrow()
    }

    it("testCase23") {
      val aBytes = Array[Byte](-127, 100, 56, 7, 98, -1, 39, -128, 127, 75)
      val bBytes = Array[Byte](27, -15, 65, 39, 100)
      val aSign = 1
      val bSign = 1
      val rBytes = Array[Byte](12, -21, 73, 56, 27)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.mod(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testCase24") {
      val aBytes = Array[Byte](-127, 100, 56, 7, 98, -1, 39, -128, 127, 75)
      val bBytes = Array[Byte](27, -15, 65, 39, 100)
      val aSign = -1
      val bSign = 1
      val rBytes = Array[Byte](15, 5, -9, -17, 73)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.mod(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testCase3") {
      val aBytes = Array[Byte](-127, 100, 56, 7, 98, -1, 39, -128, 127)
      val bBytes = Array[Byte](-127, 100, 56, 7, 98, -1, 39, -128, 127)
      val aSign = 1
      val bSign = 1
      val rBytes = Array[Byte](1)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.divide(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testCase4") {
      val aBytes = Array[Byte](-127, 100, 56, 7, 98, -1, 39, -128, 127)
      val bBytes = Array[Byte](-127, 100, 56, 7, 98, -1, 39, -128, 127)
      val aSign = -1
      val bSign = 1
      val rBytes = Array[Byte](-1)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.divide(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testCase5") {
      val aBytes = Array[Byte](-127, 100, 56, 7, 98, -1, 39, -128, 127)
      val bBytes = Array[Byte](-127, 100, 56, 7, 98, -1, 39, -128, 127, 1, 2, 3, 4, 5)
      val aSign = -1
      val bSign = 1
      val rBytes = Array[Byte](0)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.divide(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(0)
    }

    it("testCase6") {
      val aBytes = Array[Byte](1, 100, 56, 7, 98, -1, 39, -128, 127)
      val bBytes = Array[Byte](15, 100, 56, 7, 98, -1, 39, -128, 127)
      val aSign = 1
      val bSign = 1
      val rBytes = Array[Byte](0)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.divide(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(0)
    }

    it("testCase7") {
      val aBytes = Array[Byte](1, 100, 56, 7, 98, -1, 39, -128, 127, 5, 6, 7, 8, 9)
      val bBytes = Array[Byte](15, 48, -29, 7, 98, -1, 39, -128)
      val aSign = 1
      val bSign = 1
      val rBytes = Array[Byte](23, 115, 11, 78, 35, -11)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.divide(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testCase8") {
      val aBytes = Array[Byte](1, 100, 56, 7, 98, -1, 39, -128, 127, 5, 6, 7, 8, 9)
      val bBytes = Array[Byte](15, 48, -29, 7, 98, -1, 39, -128)
      val aSign = 1
      val bSign = -1
      val rBytes = Array[Byte](-24, -116, -12, -79, -36, 11)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.divide(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testCase9") {
      val aBytes = Array[Byte](1, 100, 56, 7, 98, -1, 39, -128, 127, 5, 6, 7, 8, 9)
      val bBytes = Array[Byte](15, 48, -29, 7, 98, -1, 39, -128)
      val aSign = -1
      val bSign = 1
      val rBytes = Array[Byte](-24, -116, -12, -79, -36, 11)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.divide(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testDivisionKnuth1") {
      val aBytes = Array[Byte](-7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7)
      val bBytes = Array[Byte](-3, -3, -3, -3)
      val aSign = 1
      val bSign = 1
      val rBytes = Array[Byte](0, -5, -12, -33, -96, -36, -105, -56, 92, 15, 48, -109)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.divide(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testDivisionKnuthFirstDigitsEqual") {
      val aBytes = Array[Byte](2, -3, -4, -5, -1, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5)
      val bBytes = Array[Byte](2, -3, -4, -5, -1, -1, -1, -1)
      val aSign = -1
      val bSign = -1
      val rBytes = Array[Byte](0, -1, -1, -1, -1, -2, -88, -60, 41)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.divide(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testDivisionKnuthIsNormalized") {
      val aBytes = Array[Byte](-9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5)
      val bBytes = Array[Byte](-1, -1, -1, -1, -1, -1, -1, -1)
      val aSign = -1
      val bSign = -1
      val rBytes = Array[Byte](0, -9, -8, -7, -6, -5, -4, -3)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.divide(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testDivisionKnuthMultiDigitsByOneDigit") {
      val aBytes = Array[Byte](113, -83, 123, -5, 18, -34, 67, 39, -29)
      val bBytes = Array[Byte](2, -3, -4, -5)
      val aSign = 1
      val bSign = -1
      val rBytes = Array[Byte](-38, 2, 7, 30, 109, -43)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.divide(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testDivisionKnuthOneDigitByOneDigit") {
      val aBytes = Array[Byte](113, -83, 123, -5)
      val bBytes = Array[Byte](2, -3, -4, -5)
      val aSign = 1
      val bSign = -1
      val rBytes = Array[Byte](-37)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.divide(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testRemainderKnuth1") {
      val aBytes = Array[Byte](-9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1)
      val bBytes = Array[Byte](0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      val aSign = 1
      val bSign = 1
      val rBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 7, 18, -89)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.remainder(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testRemainderKnuthMultiDigitsByOneDigit") {
      val aBytes = Array[Byte](113, -83, 123, -5, 18, -34, 67, 39, -29)
      val bBytes = Array[Byte](2, -3, -4, -50)
      val aSign = 1
      val bSign = -1
      val rBytes = Array[Byte](2, -37, -60, 59)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.remainder(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testRemainderKnuthOneDigitByOneDigit") {
      val aBytes = Array[Byte](113, -83, 123, -5)
      val bBytes = Array[Byte](2, -3, -4, -50)
      val aSign = 1
      val bSign = -1
      val rBytes = Array[Byte](2, -9, -14, 53)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.remainder(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }
  }
}
