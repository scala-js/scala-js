/*
 *  Ported by Alistair Johnson from https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigIntegerConstructorsTest.java
 */
package org.scalajs.testsuite.javalib.math

import java.math.BigInteger
import java.util.Random
import org.scalajs.jasminetest.JasmineTest

object BigIntegerConstructorsTest extends JasmineTest {

  describe("BigIntegerConstructorsTest") {

    it("testConstructorBytesException") {
      val aBytes = Array[Byte]()
      expect(() => new BigInteger(aBytes)).toThrow()
    }

    it("testConstructorBytesNegative1") {
      val aBytes = Array[Byte](-12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val rBytes = Array[Byte](-12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aNumber = new BigInteger(aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(-1)
    }

    it("testConstructorBytesNegative2") {
      val aBytes = Array[Byte](-12, 56, 100)
      val rBytes = Array[Byte](-12, 56, 100)
      val aNumber = new BigInteger(aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(-1)
    }

    it("testConstructorBytesNegative3") {
      val aBytes = Array[Byte](-128, -12, 56, 100)
      val rBytes = Array[Byte](-128, -12, 56, 100)
      val aNumber = new BigInteger(aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(-1)
    }

    it("testConstructorBytesNegative4") {
      val aBytes = Array[Byte](-128, -12, 56, 100, -13, 56, 93, -78)
      val rBytes = Array[Byte](-128, -12, 56, 100, -13, 56, 93, -78)
      val aNumber = new BigInteger(aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(-1)
    }

    it("testConstructorBytesPositive") {
      val aBytes = Array[Byte](127, 56, 100, -1, 14, 75, -24, -100)
      val rBytes = Array[Byte](127, 56, 100, -1, 14, 75, -24, -100)
      val aNumber = new BigInteger(aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(1)
    }

    it("testConstructorBytesPositive1") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val rBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aNumber = new BigInteger(aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(1)
    }

    it("testConstructorBytesPositive2") {
      val aBytes = Array[Byte](12, 56, 100)
      val rBytes = Array[Byte](12, 56, 100)
      val aNumber = new BigInteger(aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(1)
    }

    it("testConstructorBytesPositive3") {
      val aBytes = Array[Byte](127, 56, 100, -1)
      val rBytes = Array[Byte](127, 56, 100, -1)
      val aNumber = new BigInteger(aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(1)
    }

    it("testConstructorBytesZero") {
      val aBytes = Array[Byte](0, 0, 0, -0, +0, 0, -0)
      val rBytes = Array[Byte](0)
      val aNumber = new BigInteger(aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(0)
    }

    it("testConstructorPrime") {
      val bitLen = 25
      val rnd = new Random()
      val aNumber = new BigInteger(bitLen, 80, rnd)
      expect(aNumber.bitLength()).toEqual(bitLen)
    }

    it("testConstructorPrime2") {
      val bitLen = 2
      val rnd = new Random()
      val aNumber = new BigInteger(bitLen, 80, rnd)
      expect(aNumber.bitLength()).toEqual(bitLen)
      val num = aNumber.intValue()
      expect(num == 2 || num == 3).toBeTruthy()
    }

    it("testConstructorRandom") {
      val bitLen = 75
      val rnd: Random = new Random()
      val aNumber = new BigInteger(bitLen, rnd)
      expect(aNumber.bitLength() <= bitLen).toBeTruthy()
    }

    it("testConstructorSignBytesException1") {
      val aBytes = Array[Byte](123, 45, -3, -76)
      val aSign = 3
      expect(() => new BigInteger(aSign, aBytes)).toThrow()
    }

    it("testConstructorSignBytesException2") {
      val aBytes = Array[Byte](123, 45, -3, -76)
      val aSign = 0
      expect(() => new BigInteger(aSign, aBytes)).toThrow()
    }

    it("testConstructorSignBytesNegative1") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15)
      val aSign = -1
      val rBytes = Array[Byte](-13, -57, -101, 1, 75, -90, -46, -92, -4, 15)
      val aNumber = new BigInteger(aSign, aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(-1)
    }

    it("testConstructorSignBytesNegative2") {
      val aBytes = Array[Byte](-12, 56, 100, -2, -76, 89, 45, 91, 3, -15)
      val aSign = -1
      val rBytes = Array[Byte](-1, 11, -57, -101, 1, 75, -90, -46, -92, -4, 15)
      val aNumber = new BigInteger(aSign, aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(-1)
    }

    it("testConstructorSignBytesNegative3") {
      val aBytes = Array[Byte](-12, 56, 100)
      val aSign = -1
      val rBytes = Array[Byte](-1, 11, -57, -100)
      val aNumber = new BigInteger(aSign, aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(-1)
    }

    it("testConstructorSignBytesNegative4") {
      val aBytes = Array[Byte](127, 56, 100, -2)
      val aSign = -1
      val rBytes = Array[Byte](-128, -57, -101, 2)
      val aNumber = new BigInteger(aSign, aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(-1)
    }

    it("testConstructorSignBytesNegative5") {
      val aBytes = Array[Byte](-127, 56, 100, -2)
      val aSign = -1
      val rBytes = Array[Byte](-1, 126, -57, -101, 2)
      val aNumber = new BigInteger(aSign, aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(-1)
    }

    it("testConstructorSignBytesNegative6") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 23, -101)
      val aSign = -1
      val rBytes = Array[Byte](-13, -57, -101, 1, 75, -90, -46, -92, -4, 14, -24, 101)
      val aNumber = new BigInteger(aSign, aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(-1)
    }

    it("testConstructorSignBytesNegative7") {
      val aBytes = Array[Byte](-12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 23, -101)
      val aSign = -1
      val rBytes = Array[Byte](-1, 11, -57, -101, 1, 75, -90, -46, -92, -4, 14, -24, 101)
      val aNumber = new BigInteger(aSign, aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(-1)
    }

    it("testConstructorSignBytesPositive1") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15)
      val aSign = 1
      val rBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15)
      val aNumber = new BigInteger(aSign, aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(1)
    }

    it("testConstructorSignBytesPositive2") {
      val aBytes = Array[Byte](-12, 56, 100, -2, -76, 89, 45, 91, 3, -15)
      val aSign = 1
      val rBytes = Array[Byte](0, -12, 56, 100, -2, -76, 89, 45, 91, 3, -15)
      val aNumber = new BigInteger(aSign, aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(1)
    }

    it("testConstructorSignBytesPositive3") {
      val aBytes = Array[Byte](-12, 56, 100)
      val aSign = 1
      val rBytes = Array[Byte](0, -12, 56, 100)
      val aNumber = new BigInteger(aSign, aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(1)
    }

    it("testConstructorSignBytesPositive4") {
      val aBytes = Array[Byte](127, 56, 100, -2)
      val aSign = 1
      val rBytes = Array[Byte](127, 56, 100, -2)
      val aNumber = new BigInteger(aSign, aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(1)
    }

    it("testConstructorSignBytesPositive5") {
      val aBytes = Array[Byte](-127, 56, 100, -2)
      val aSign = 1
      val rBytes = Array[Byte](0, -127, 56, 100, -2)
      val aNumber = new BigInteger(aSign, aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(1)
    }

    it("testConstructorSignBytesPositive6") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 23, -101)
      val aSign = 1
      val rBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 23, -101)
      val aNumber = new BigInteger(aSign, aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(1)
    }

    it("testConstructorSignBytesPositive7") {
      val aBytes = Array[Byte](-12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 23, -101)
      val aSign = 1
      val rBytes = Array[Byte](0, -12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 23, -101)
      val aNumber = new BigInteger(aSign, aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(1)
    }

    it("testConstructorSignBytesZero1") {
      val aBytes = Array[Byte](-0, 0, +0, 0, 0, 0, 0)
      val aSign = -1
      val rBytes = Array[Byte](0)
      val aNumber = new BigInteger(aSign, aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(0)
    }

    it("testConstructorSignBytesZero2") {
      val aBytes = Array[Byte](-0, 0, +0, 0, 0, 0, 0)
      val aSign = 0
      val rBytes = Array[Byte](0)
      val aNumber = new BigInteger(aSign, aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(0)
    }

    it("testConstructorSignBytesZero3") {
      val aBytes = Array[Byte](-0, 0, +0, 0, 0, 0, 0)
      val aSign = 1
      val rBytes = Array[Byte](0)
      val aNumber = new BigInteger(aSign, aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(0)
    }

    it("testConstructorSignBytesZeroNull1") {
      val aBytes = Array[Byte]()
      val aSign = -1
      val rBytes = Array[Byte](0)
      val aNumber = new BigInteger(aSign, aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(0)
    }

    it("testConstructorSignBytesZeroNull2") {
      val aBytes = Array[Byte]()
      val aSign = 0
      val rBytes = Array[Byte](0)
      val aNumber = new BigInteger(aSign, aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(0)
    }

    it("testConstructorSignBytesZeroNull3") {
      val aBytes = Array[Byte]()
      val aSign = 1
      val rBytes = Array[Byte](0)
      val aNumber = new BigInteger(aSign, aBytes)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(0)
    }

    it("testConstructorStringException1") {
      val value = "9234853876401"
      val radix = 45
      expect(() => new BigInteger(value, radix)).toThrow()
    }

    it("testConstructorStringException2") {
      val value = "   9234853876401"
      val radix = 10
      expect(() => new BigInteger(value, radix)).toThrow()
    }

    it("testConstructorStringException3") {
      val value = "92348$*#78987"
      val radix = 34
      expect(() => new BigInteger(value, radix)).toThrow()
    }

    it("testConstructorStringException4") {
      val value = "98zv765hdsaiy"
      val radix = 20
      expect(() => new BigInteger(value, radix)).toThrow()
    }

    it("testConstructorStringRadix10") {
      val value = "987328901348934898"
      val radix = 10
      val rBytes = Array(13, -77, -78, 103, -103, 97, 68, -14)
      val aNumber = new BigInteger(value, radix)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(1)
    }

    it("testConstructorStringRadix10Negative") {
      val value = "-234871376037"
      val radix = 36
      val rBytes = Array(-4, 48, 71, 62, -76, 93, -105, 13)
      val aNumber = new BigInteger(value, radix)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(-1)
    }

    it("testConstructorStringRadix10Zero") {
      val value = "-00000000000000"
      val radix = 10
      val rBytes = Array(0)
      val aNumber = new BigInteger(value, radix)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(0)
    }

    it("testConstructorStringRadix16") {
      val value = "fe2340a8b5ce790"
      val radix = 16
      val rBytes = Array(15, -30, 52, 10, -117, 92, -25, -112)
      val aNumber = new BigInteger(value, radix)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(1)
    }

    it("testConstructorStringRadix2") {
      val value = "10101010101010101"
      val radix = 2
      val rBytes = Array(1, 85, 85)
      val aNumber = new BigInteger(value, radix)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(1)
    }

    it("testConstructorStringRadix36") {
      val value = "skdjgocvhdjfkl20jndjkf347ejg457"
      val radix = 36
      val rBytes = Array(0, -12, -116, 112, -105, 12, -36, 66, 108, 66, -20,
          -37, -15, 108, -7, 52, -99, -109, -8, -45, -5)
      val aNumber = new BigInteger(value, radix)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(1)
    }

    it("testConstructorStringRadix8") {
      val value = "76356237071623450"
      val radix = 8
      val rBytes = Array(7, -50, -28, -8, -25, 39, 40)
      val aNumber = new BigInteger(value, radix)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
        expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(1)
    }
  }
}
