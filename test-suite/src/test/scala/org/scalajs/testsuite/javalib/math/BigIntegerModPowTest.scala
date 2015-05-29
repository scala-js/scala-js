// scalastyle:off line.size.limit
/*
 * Ported by Alistair Johnson from
 * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigIntegerModPowTest.java
 */
// scalastyle:on line.size.limit

package org.scalajs.testsuite.javalib.math

import java.math.BigInteger
import java.util.Arrays
import org.scalajs.jasminetest.JasmineTest

object BigIntegerModPowTest extends JasmineTest {

  describe("BigIntegerModPowTest") {

    it("testGcdBothZeros") {
      val rBytes = Array[Byte](0)
      val aNumber = new BigInteger("0")
      val bNumber = BigInteger.valueOf(0L)
      val result = aNumber.gcd(bNumber)
      val resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(0)
    }

    it("testGcdFirstLonger") {
      val aBytes = Array[Byte](-15, 24, 123, 56, -11, -112, -34, -98, 8, 10, 12, 14, 25, 125, -15, 28, -127)
      val bBytes = Array[Byte](-12, 1, 0, 0, 0, 23, 44, 55, 66)
      val aSign = 1
      val bSign = 1
      val rBytes = Array[Byte](7)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.gcd(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testGcdFirstZero") {
      val aBytes = Array[Byte](0)
      val bBytes = Array[Byte](15, 24, 123, 57, -15, 24, 123, 57, -15, 24, 123, 57)
      val aSign = 1
      val bSign = 1
      val rBytes = Array[Byte](15, 24, 123, 57, -15, 24, 123, 57, -15, 24, 123, 57)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.gcd(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testGcdFirstZERO") {
      val bBytes = Array[Byte](15, 24, 123, 57, -15, 24, 123, 57, -15, 24, 123, 57)
      val bSign = 1
      val rBytes = Array[Byte](15, 24, 123, 57, -15, 24, 123, 57, -15, 24, 123, 57)
      val aNumber = BigInteger.ZERO
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.gcd(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testGcdSecondLonger") {
      val aBytes = Array[Byte](-12, 1, 0, 0, 0, 23, 44, 55, 66)
      val bBytes = Array[Byte](-15, 24, 123, 56, -11, -112, -34, -98, 8, 10, 12, 14, 25, 125, -15, 28, -127)
      val aSign = 1
      val bSign = 1
      val rBytes = Array[Byte](7)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.gcd(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testGcdSecondZero") {
      val aBytes = Array[Byte](15, 24, 123, 57, -15, 24, 123, 57, -15, 24, 123, 57)
      val bBytes = Array[Byte](0)
      val aSign = 1
      val bSign = 1
      val rBytes = Array[Byte](15, 24, 123, 57, -15, 24, 123, 57, -15, 24, 123, 57)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      val result = aNumber.gcd(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testmodInverseException") {
      val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
      val mBytes = Array[Byte](1, 2, 3)
      val aSign = 1
      val mSign = -1
      val aNumber = new BigInteger(aSign, aBytes)
      val modulus = new BigInteger(mSign, mBytes)
      expect(() => aNumber.modInverse(modulus)).toThrow()
    }

    it("testmodInverseNeg1") {
      val aBytes = Array[Byte](15, 24, 123, 56, -11, -112, -34, -98, 8, 10, 12, 14, 25, 125, -15, 28, -127)
      val mBytes = Array[Byte](2, 122, 45, 36, 100)
      val aSign = -1
      val mSign = 1
      val rBytes = Array[Byte](0, -41, 4, -91, 27)
      val aNumber = new BigInteger(aSign, aBytes)
      val modulus = new BigInteger(mSign, mBytes)
      val result = aNumber.modInverse(modulus)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testmodInverseNeg2") {
      val aBytes = Array[Byte](-15, 24, 123, 57, -15, 24, 123, 57, -15, 24, 123, 57)
      val mBytes = Array[Byte](122, 2, 4, 122, 2, 4)
      val rBytes = Array[Byte](85, 47, 127, 4, -128, 45)
      val aNumber = new BigInteger(aBytes)
      val modulus = new BigInteger(mBytes)
      val result = aNumber.modInverse(modulus)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testmodInverseNonInvertible") {
      val aBytes = Array[Byte](-15, 24, 123, 56, -11, -112, -34, -98, 8, 10, 12, 14, 25, 125, -15, 28, -127)
      val mBytes = Array[Byte](-12, 1, 0, 0, 0, 23, 44, 55, 66)
      val aSign = 1
      val mSign = 1
      val aNumber = new BigInteger(aSign, aBytes)
      val modulus = new BigInteger(mSign, mBytes)
      expect(() => aNumber.modInverse(modulus)).toThrow()
    }

    it("testmodInversePos1") {
      val aBytes = Array[Byte](24, 123, 56, -11, -112, -34, -98, 8, 10, 12, 14, 25, 125, -15, 28, -127)
      val mBytes = Array[Byte](122, 45, 36, 100, 122, 45)
      val aSign = 1
      val mSign = 1
      val rBytes = Array[Byte](47, 3, 96, 62, 87, 19)
      val aNumber = new BigInteger(aSign, aBytes)
      val modulus = new BigInteger(mSign, mBytes)
      val result = aNumber.modInverse(modulus)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testmodInversePos2") {
      val aBytes = Array[Byte](15, 24, 123, 56, -11, -112, -34, -98, 8, 10, 12, 14, 25, 125, -15, 28, -127)
      val mBytes = Array[Byte](2, 122, 45, 36, 100)
      val aSign = 1
      val mSign = 1
      val rBytes = Array[Byte](1, -93, 40, 127, 73)
      val aNumber = new BigInteger(aSign, aBytes)
      val modulus = new BigInteger(mSign, mBytes)
      val result = aNumber.modInverse(modulus)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testModPowException") {
      val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
      val eBytes = Array[Byte](1, 2, 3, 4, 5)
      val mBytes = Array[Byte](1, 2, 3)
      val aSign = 1
      val eSign = 1
      val mSign = -1
      val aNumber = new BigInteger(aSign, aBytes)
      expect(Arrays.equals(aBytes, aNumber.toByteArray)).toBeTruthy()
      val exp = new BigInteger(eSign, eBytes)
      val modulus = new BigInteger(mSign, mBytes)
      expect(() => aNumber.modPow(exp, modulus)).toThrow()
    }

    it("testModPowNegExp") {
      val aBytes = Array[Byte](-127, 100, 56, 7, 98, -1, 39, -128, 127, 75, 48, -7)
      val eBytes = Array[Byte](27, -15, 65, 39)
      val mBytes = Array[Byte](-128, 2, 3, 4, 5)
      val aSign = 1
      val eSign = -1
      val mSign = 1
      val rBytes = Array[Byte](12, 118, 46, 86, 92)
      val aNumber = new BigInteger(aSign, aBytes)
      val exp = new BigInteger(eSign, eBytes)
      val modulus = new BigInteger(mSign, mBytes)
      val result = aNumber.modPow(exp, modulus)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }

    it("testModPowPosExp") {
      val aBytes = Array[Byte](-127, 100, 56, 7, 98, -1, 39, -128, 127, 75, 48, -7)
      val eBytes = Array[Byte](27, -15, 65, 39)
      val mBytes = Array[Byte](-128, 2, 3, 4, 5)
      val aSign = 1
      val eSign = 1
      val mSign = 1
      val rBytes = Array[Byte](113, 100, -84, -28, -85)
      val aNumber = new BigInteger(aSign, aBytes)
      val exp = new BigInteger(eSign, eBytes)
      val modulus = new BigInteger(mSign, mBytes)
      val result = aNumber.modPow(exp, modulus)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(result.signum()).toEqual(1)
    }
  }
}
