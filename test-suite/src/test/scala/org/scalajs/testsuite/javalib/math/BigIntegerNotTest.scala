/*
 *  Ported by Alistair Johnson from https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigIntegerNotTest.java
 */

package org.scalajs.testsuite.javalib.math

import java.math.BigInteger
import org.scalajs.jasminetest.JasmineTest

object BigIntegerNotTest extends JasmineTest {

  describe("BigIntegerNotTest") {

    it("testAndNotNegNegFirstLonger") {
      val aBytes = Array[Byte](-128, 9, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, -117, 23, 87, -25, -75)
      val bBytes = Array[Byte](-2, -3, -4, -4, 5, 14, 23, 39, 48, 57, 66, 5, 14, 23)
      val aSign = -1
      val bSign = -1
      val rBytes = Array[Byte](73, -92, -48, 4, 12, 6, 4, 32, 48, 64, 0, 8, 2)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.andNot(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testAndNotPosPosFirstLonger") {
      val aBytes = Array[Byte](-128, 9, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, -117, 23, 87, -25, -75)
      val bBytes = Array[Byte](-2, -3, -4, -4, 5, 14, 23, 39, 48, 57, 66, 5, 14, 23)
      val aSign = 1
      val bSign = 1
      val rBytes = Array[Byte](0, -128, 9, 56, 100, 0, 0, 1, 1, 90, 1, -32, 0, 10, -126, 21, 82, -31, -96)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.andNot(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testAndNotPosPosFirstShorter") {
      val aBytes = Array[Byte](-2, -3, -4, -4, 5, 14, 23, 39, 48, 57, 66, 5, 14, 23)
      val bBytes = Array[Byte](-128, 9, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, -117, 23, 87, -25, -75)
      val aSign = 1
      val bSign = 1
      val rBytes = Array[Byte](73, -92, -48, 4, 12, 6, 4, 32, 48, 64, 0, 8, 2)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.andNot(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testNegPosFirstLonger") {
      val aBytes = Array[Byte](-128, 9, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, -117, 23, 87, -25, -75)
      val bBytes = Array[Byte](-2, -3, -4, -4, 5, 14, 23, 39, 48, 57, 66, 5, 14, 23)
      val aSign = -1
      val bSign = 1
      val rBytes = Array[Byte](-1, 127, -10, -57, -101, 1, 2, 2, 2, -96, -16, 8, -40, -59, 68, -88, -88, 16, 72)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.andNot(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testNotNeg") {
      val aBytes = Array[Byte](-128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, -117)
      val aSign = -1
      val rBytes = Array[Byte](0, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, -118)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.not()
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testNotOne") {
      val rBytes = Array[Byte](-2)
      val aNumber = BigInteger.ONE
      val result = aNumber.not()
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testNotPos") {
      val aBytes = Array[Byte](-128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, -117)
      val aSign = 1
      val rBytes = Array[Byte](-1, 127, -57, -101, 1, 75, -90, -46, -92, -4, 14, -36, -27, 116)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.not()
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testNotSpecialCase") {
      val aBytes = Array[Byte](-1, -1, -1, -1)
      val aSign = 1
      val rBytes = Array[Byte](-1, 0, 0, 0, 0)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.not()
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }

    it("testNotZero") {
      val rBytes = Array[Byte](-1)
      val aNumber = BigInteger.ZERO
      val result = aNumber.not()
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(-1)
    }
  }
}
