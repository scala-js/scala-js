/*
 *  Ported by Alistair Johnson from https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigIntegerSubtractTest.java
 */

package org.scalajs.testsuite.javalib.math

import java.math.BigInteger
import org.scalajs.jasminetest.JasmineTest

object BigIntegerSubtractTest extends JasmineTest {

  describe("BigIntegerSubtractTest") {

    it("testCase1") {
      val aBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
      val bBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3)
      val aSign = 1
      val bSign = 1
      val rBytes = Array[Byte](9, 18, 27, 36, 45, 54, 63, 9, 18, 27)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.subtract(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testCase10") {
      val aBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
      val bBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7)
      val aSign = 1
      val bSign = 1
      val rBytes = Array[Byte](-2, -3, -4, -4, 5, 14, 23, 39, 48, 57, 66, 5, 14, 23)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.subtract(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testCase11") {
      val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7)
      val bBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
      val aSign = 1
      val bSign = -1
      val rBytes = Array[Byte](1, 2, 3, 4, 15, 26, 37, 41, 52, 63, 74, 15, 26, 37)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.subtract(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testCase12") {
      val aBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
      val bBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7)
      val aSign = 1
      val bSign = -1
      val rBytes = Array[Byte](1, 2, 3, 4, 15, 26, 37, 41, 52, 63, 74, 15, 26, 37)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.subtract(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testCase13") {
      val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7)
      val bBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
      val aSign = -1
      val bSign = 1
      val rBytes = Array[Byte](-2, -3, -4, -5, -16, -27, -38, -42, -53, -64, -75, -16, -27, -37)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.subtract(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testCase14") {
      val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7)
      val bBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
      val aSign = -1
      val bSign = 1
      val rBytes = Array[Byte](-2, -3, -4, -5, -16, -27, -38, -42, -53, -64, -75, -16, -27, -37)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.subtract(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testCase15") {
      val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7)
      val bBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
      val aSign = -1
      val bSign = -1
      val rBytes = Array[Byte](-2, -3, -4, -4, 5, 14, 23, 39, 48, 57, 66, 5, 14, 23)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.subtract(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testCase16") {
      val aBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
      val bBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7)
      val aSign = -1
      val bSign = -1
      val rBytes = Array[Byte](1, 2, 3, 3, -6, -15, -24, -40, -49, -58, -67, -6, -15, -23)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.subtract(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testCase17") {
      val aBytes = Array[Byte](-120, 34, 78, -23, -111, 45, 127, 23, 45, -3)
      val bBytes = Array[Byte](-120, 34, 78, -23, -111, 45, 127, 23, 45, -3)
      val rBytes = Array[Byte](0)
      val aSign = 1
      val bSign = 1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.subtract(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(0)
    }

    it("testCase18") {
      val aBytes = Array[Byte](120, 34, 78, -23, -111, 45, 127, 23, 45, -3)
      val bBytes = Array[Byte](0)
      val rBytes = Array[Byte](120, 34, 78, -23, -111, 45, 127, 23, 45, -3)
      val aSign = 1
      val bSign = 0
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.subtract(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testCase19") {
      val aBytes = Array[Byte](0)
      val bBytes = Array[Byte](120, 34, 78, -23, -111, 45, 127, 23, 45, -3)
      val rBytes = Array[Byte](120, 34, 78, -23, -111, 45, 127, 23, 45, -3)
      val aSign = 0
      val bSign = -1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.subtract(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testCase2") {
      val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3)
      val bBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
      val aSign = 1
      val bSign = 1
      val rBytes = Array[Byte](-10, -19, -28, -37, -46, -55, -64, -10, -19, -27)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.subtract(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testCase20") {
      val aBytes = Array[Byte](0)
      val bBytes = Array[Byte](0)
      val rBytes = Array[Byte](0)
      val aSign = 0
      val bSign = 0
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.subtract(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(0)
    }

    it("testCase21") {
      val aBytes = Array[Byte](120, 34, 78, -23, -111, 45, 127, 23, 45, -3)
      val rBytes = Array[Byte](120, 34, 78, -23, -111, 45, 127, 23, 45, -3)
      val aSign = 1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = BigInteger.ZERO
      val result = aNumber.subtract(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testCase22") {
      val bBytes = Array[Byte](120, 34, 78, -23, -111, 45, 127, 23, 45, -3)
      val rBytes = Array[Byte](120, 34, 78, -23, -111, 45, 127, 23, 45, -3)
      val bSign = -1
      val aNumber = BigInteger.ZERO
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.subtract(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testCase23") {
      val rBytes = Array[Byte](0)
      val aNumber = BigInteger.ZERO
      val bNumber = BigInteger.ZERO
      val result = aNumber.subtract(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(0)
    }

    it("testCase24") {
      val rBytes = Array[Byte](0)
      val aNumber = BigInteger.ONE
      val bNumber = BigInteger.ONE
      val result = aNumber.subtract(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(0)
    }

    it("testCase25") {
      val aBytes = Array[Byte](-1, -1, -1, -1, -1, -1, -1, -1)
      val bBytes = Array[Byte](-128, -128, -128, -128, -128, -128, -128, -128, -128)
      val aSign = 1
      val bSign = 1
      val rBytes = Array[Byte](-128, 127, 127, 127, 127, 127, 127, 127, 127)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.subtract(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testCase3") {
      val aBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
      val bBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3)
      val aSign = 1
      val bSign = -1
      val rBytes = Array[Byte](11, 22, 33, 44, 55, 66, 77, 11, 22, 33)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.subtract(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testCase4") {
      val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3)
      val bBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
      val aSign = 1
      val bSign = -1
      val rBytes = Array[Byte](11, 22, 33, 44, 55, 66, 77, 11, 22, 33)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.subtract(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testCase5") {
      val aBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
      val bBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3)
      val aSign = -1
      val bSign = -1
      val rBytes = Array[Byte](-10, -19, -28, -37, -46, -55, -64, -10, -19, -27)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.subtract(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testCase6") {
      val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3)
      val bBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
      val aSign = -1
      val bSign = -1
      val rBytes = Array[Byte](9, 18, 27, 36, 45, 54, 63, 9, 18, 27)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.subtract(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testCase7") {
      val aBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
      val bBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3)
      val aSign = -1
      val bSign = 1
      val rBytes = Array[Byte](-12, -23, -34, -45, -56, -67, -78, -12, -23, -33)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.subtract(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testCase8") {
      val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3)
      val bBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
      val aSign = -1
      val bSign = 1
      val rBytes = Array[Byte](-12, -23, -34, -45, -56, -67, -78, -12, -23, -33)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.subtract(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testCase9") {
      val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7)
      val bBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
      val aSign = 1
      val bSign = 1
      val rBytes = Array[Byte](1, 2, 3, 3, -6, -15, -24, -40, -49, -58, -67, -6, -15, -23)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.subtract(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }
  }
}
