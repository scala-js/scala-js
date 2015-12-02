// scalastyle:off line.size.limit
/*
 * Ported by Alistair Johnson from
 * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigIntegerModPowTest.java
 */
// scalastyle:on line.size.limit

package org.scalajs.testsuite.javalib.math

import java.math.BigInteger
import java.util.Arrays

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._

class BigIntegerModPowTest {

  @Test def testsGcdFirstOne(): Unit = {
    val aBytes = Array[Byte](1, 0, 0, 0, 0)
    val big = new BigInteger(1, aBytes)
    assertEquals(1, BigInteger.ONE.gcd(big).intValue)
    assertEquals(1, BigInteger.ONE.gcd(BigInteger.ZERO).intValue)
  }

  @Test def testsGcdSecondOne(): Unit = {
    val aBytes = Array[Byte](1, 0, 0, 0, 0)
    val big = new BigInteger(1, aBytes)
    assertEquals(1, big.gcd(BigInteger.ONE).intValue)
    assertEquals(1, BigInteger.ZERO.gcd(BigInteger.ONE).intValue)
  }

  @Test def testsGcdBothOne(): Unit = {
    assertEquals(1, BigInteger.ONE.gcd(BigInteger.ONE).intValue)
  }

  @Test def testGcdBothZeros(): Unit = {
    val rBytes = Array[Byte](0)
    val aNumber = new BigInteger("0")
    val bNumber = BigInteger.valueOf(0L)
    val result = aNumber.gcd(bNumber)
    val resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(0, result.signum())
  }

  @Test def testGcdFirstLonger(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testGcdFirstZero(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testGcdFirstZero2(): Unit = {
    val bBytes = Array[Byte](15, 24, 123, 57, -15, 24, 123, 57, -15, 24, 123, 57)
    val bSign = 1
    val rBytes = Array[Byte](15, 24, 123, 57, -15, 24, 123, 57, -15, 24, 123, 57)
    val aNumber = BigInteger.ZERO
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.gcd(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testGcdSecondLonger(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testGcdSecondZero(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testmodInverseException(): Unit = {
    val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
    val mBytes = Array[Byte](1, 2, 3)
    val aSign = 1
    val mSign = -1
    val aNumber = new BigInteger(aSign, aBytes)
    val modulus = new BigInteger(mSign, mBytes)
    expectThrows(classOf[ArithmeticException], aNumber.modInverse(modulus))
  }

  @Test def testmodInverseNeg1(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testmodInverseNeg2(): Unit = {
    val aBytes = Array[Byte](-15, 24, 123, 57, -15, 24, 123, 57, -15, 24, 123, 57)
    val mBytes = Array[Byte](122, 2, 4, 122, 2, 4)
    val rBytes = Array[Byte](85, 47, 127, 4, -128, 45)
    val aNumber = new BigInteger(aBytes)
    val modulus = new BigInteger(mBytes)
    val result = aNumber.modInverse(modulus)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testmodInverseNonInvertible(): Unit = {
    val aBytes = Array[Byte](-15, 24, 123, 56, -11, -112, -34, -98, 8, 10, 12, 14, 25, 125, -15, 28, -127)
    val mBytes = Array[Byte](-12, 1, 0, 0, 0, 23, 44, 55, 66)
    val aSign = 1
    val mSign = 1
    val aNumber = new BigInteger(aSign, aBytes)
    val modulus = new BigInteger(mSign, mBytes)
    expectThrows(classOf[ArithmeticException], aNumber.modInverse(modulus))
  }

  @Test def testmodInversePos1(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testmodInversePos2(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def `testmodInverse - #1764`(): Unit = {
    def test(a: BigInt, b: BigInt, expexted: BigInt): Unit =
      assertTrue(a.modInverse(b) == expexted)

    // Cases that failed due to the bug
    test(BigInt(1795804389L), BigInt(2957870813L), BigInt(2849476504L))
    test(BigInt(53389L), BigInt(29578713L), BigInt(4631629L))
    test(BigInt(175389L), BigInt(2954378713L), BigInt(2628921865L))
  }

  @Test def testModPowException(): Unit = {
    val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
    val eBytes = Array[Byte](1, 2, 3, 4, 5)
    val mBytes = Array[Byte](1, 2, 3)
    val aSign = 1
    val eSign = 1
    val mSign = -1
    val aNumber = new BigInteger(aSign, aBytes)
    assertTrue(Arrays.equals(aBytes, aNumber.toByteArray))
    val exp = new BigInteger(eSign, eBytes)
    val modulus = new BigInteger(mSign, mBytes)
    expectThrows(classOf[ArithmeticException], aNumber.modPow(exp, modulus))
  }

  @Test def testModPowNegExp(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testModPowPosExp(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }
}
