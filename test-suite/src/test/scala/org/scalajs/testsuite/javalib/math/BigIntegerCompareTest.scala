/*
 *  Ported by Alistair Johnson from https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigIntegerCompareTest.java
 */
package org.scalajs.testsuite.javalib.math

import java.math.BigInteger
import org.scalajs.jasminetest.JasmineTest

object BigIntegerCompareTest extends JasmineTest {

  describe("BigIntegerCompareTest") {

    it("testAbsNegative") {
      val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
      val aSign = -1
      val rBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.abs()
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testAbsPositive") {
      val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
      val aSign = 1
      val rBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.abs()
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testCompareNegNeg2") {
      val aBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
      val bBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = -1
      val bSign = -1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      expect(aNumber.compareTo(bNumber)).toEqual(1)
      expect(aNumber.compareTo(bNumber)).toEqual(1)
    }

    it("testCompareToDiffSigns1") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val bBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
      val aSign = 1
      val bSign = -1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      expect(aNumber.compareTo(bNumber)).toEqual(1)
    }

    it("testCompareToDiffSigns2") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val bBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
      val aSign = -1
      val bSign = 1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      expect(aNumber.compareTo(bNumber)).toEqual(-1)
    }

    it("testCompareToEqualNeg") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val bBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = -1
      val bSign = -1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      expect(aNumber.compareTo(bNumber)).toEqual(0)
    }

    it("testCompareToEqualPos") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val bBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = 1
      val bSign = 1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      expect(aNumber.compareTo(bNumber)).toEqual(0)
    }

    it("testCompareToNegNeg1") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val bBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
      val aSign = -1
      val bSign = -1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      expect(aNumber.compareTo(bNumber)).toEqual(-1)
    }

    it("testCompareToNegZero") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = -1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = BigInteger.ZERO
      expect(aNumber.compareTo(bNumber)).toEqual(-1)
    }

    it("testCompareToPosPos1") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val bBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
      val aSign = 1
      val bSign = 1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      expect(aNumber.compareTo(bNumber)).toEqual(1)
    }

    it("testCompareToPosPos2") {
      val aBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
      val bBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = 1
      val bSign = 1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      expect(aNumber.compareTo(bNumber)).toEqual(-1)
    }

    it("testCompareToPosZero") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = 1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = BigInteger.ZERO
      expect(aNumber.compareTo(bNumber)).toEqual(1)
    }

    it("testCompareToZeroNeg") {
      val bBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val bSign = -1
      val aNumber = BigInteger.ZERO
      val bNumber = new BigInteger(bSign, bBytes)
      expect(aNumber.compareTo(bNumber)).toEqual(1)
    }

    it("testCompareToZeroPos") {
      val bBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val bSign = 1
      val aNumber = BigInteger.ZERO
      val bNumber = new BigInteger(bSign, bBytes)
      expect(aNumber.compareTo(bNumber)).toEqual(-1)
    }

    it("testCompareToZeroZero") {
      val aNumber = BigInteger.ZERO
      val bNumber = BigInteger.ZERO
      expect(aNumber.compareTo(bNumber)).toEqual(0)
    }

    it("testEqualsBigIntegerFalse") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val bBytes = Array[Byte](45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = 1
      val bSign = 1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      expect(aNumber == bNumber).toBeFalsy()
    }

    it("testEqualsBigIntegerTrue") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val bBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = 1
      val bSign = 1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      expect(aNumber == bNumber).toBeTruthy()
    }

    it("testEqualsNull") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = 1
      val aNumber = new BigInteger(aSign, aBytes)
      expect(aNumber == null).toBeFalsy()
    }

    it("testEqualsObject") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = 1
      val aNumber = new BigInteger(aSign, aBytes)
      val obj = new AnyRef()
      expect(aNumber == obj).toBeFalsy()
    }

    it("testMaxEqual") {
      val aBytes = Array[Byte](45, 91, 3, -15, 35, 26, 3, 91)
      val bBytes = Array[Byte](45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = 1
      val bSign = 1
      val rBytes = Array[Byte](45, 91, 3, -15, 35, 26, 3, 91)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.max(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testMaxGreater") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val bBytes = Array[Byte](45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = 1
      val bSign = 1
      val rBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      var result = aNumber.max(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
      result = bNumber.max(aNumber)
      expect(aNumber == result).toBeTruthy()
    }

    it("testMaxLess") {
      val aBytes = Array[Byte](45, 91, 3, -15, 35, 26, 3, 91)
      val bBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = 1
      val bSign = 1
      val rBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.max(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testMaxNegZero") {
      val aBytes = Array[Byte](45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = -1
      val rBytes = Array[Byte](0)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = BigInteger.ZERO
      val result = aNumber.max(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(0)
    }

    it("testMinEqual") {
      val aBytes = Array[Byte](45, 91, 3, -15, 35, 26, 3, 91)
      val bBytes = Array[Byte](45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = 1
      val bSign = 1
      val rBytes = Array[Byte](45, 91, 3, -15, 35, 26, 3, 91)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.min(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testMinGreater") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val bBytes = Array[Byte](45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = 1
      val bSign = 1
      val rBytes = Array[Byte](45, 91, 3, -15, 35, 26, 3, 91)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.min(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testMinLess") {
      val aBytes = Array[Byte](45, 91, 3, -15, 35, 26, 3, 91)
      val bBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = 1
      val bSign = 1
      val rBytes = Array(45, 91, 3, -15, 35, 26, 3, 91)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.min(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testMinPosZero") {
      val aBytes = Array[Byte](45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = 1
      val rBytes = Array[Byte](0)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = BigInteger.ZERO
      val result = aNumber.min(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(0)
    }

    it("testNegateNegative") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = -1
      val rBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.negate()
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testNegatePositive") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = 1
      val rBytes = Array[Byte](-13, -57, -101, 1, 75, -90, -46, -92, -4, 14, -36, -27, -4, -91)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.negate()
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testNegateZero") {
      val rBytes = Array[Byte](0)
      val aNumber = BigInteger.ZERO
      val result = aNumber.negate()
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
     expect(result.signum()).toEqual(0)
    }

    it("tassestSignumNegative") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = -1
      val aNumber = new BigInteger(aSign, aBytes)
      expect(aNumber.signum()).toEqual(-1)
    }

    it("testSignumPositive") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = 1
      val aNumber = new BigInteger(aSign, aBytes)
      expect(aNumber.signum()).toEqual(1)
    }

    it("testSignumZero") {
      val aNumber = BigInteger.ZERO
      expect(aNumber.signum()).toEqual(0)
    }
  }
}
