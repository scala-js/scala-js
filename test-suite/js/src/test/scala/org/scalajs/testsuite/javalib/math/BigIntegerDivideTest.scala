// scalastyle:off line.size.limit
/*
 * Ported by Alistair Johnson from
 * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigIntegerDivideTest.java
 */
// scalastyle:on line.size.limit

package org.scalajs.testsuite.javalib.math

import java.math.BigInteger

import org.junit.Test
import org.junit.Assert._

class BigIntegerDivideTest {

  @Test def testCase1(): Unit = {
    val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
    val bBytes = Array[Byte](0)
    val aSign = 1
    val bSign = 0
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    try {
    aNumber.divide(bNumber)
    fail()
  } catch {
     case _: Throwable => // As expected
  }
  }

  @Test def testCase10(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testCase11(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(0, result.signum())
  }

  @Test def testCase12(): Unit = {
    val bBytes = Array[Byte](15, 48, -29, 7, 98, -1, 39, -128)
    val bSign = -1
    val rBytes = Array[Byte](0)
    val aNumber = BigInteger.ZERO
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.divide(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(0, result.signum())
  }

  @Test def testCase13(): Unit = {
    val aBytes = Array[Byte](15, 48, -29, 7, 98, -1, 39, -128)
    val aSign = 1
    val rBytes = Array[Byte](15, 48, -29, 7, 98, -1, 39, -128)
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = BigInteger.ONE
    val result = aNumber.divide(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testCase14(): Unit = {
    val rBytes = Array[Byte](1)
    val aNumber = BigInteger.ONE
    val bNumber = BigInteger.ONE
    val result = aNumber.divide(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testCase15(): Unit = {
    val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
    val bBytes = Array[Byte](0)
    val aSign = 1
    val bSign = 0
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    try {
    aNumber.remainder(bNumber)
    fail()
  } catch {
     case _: Throwable => // As expected
  }
  }

  @Test def testCase16(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(0, result.signum())
  }

  @Test def testCase17(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testCase18(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testCase19(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testCase2(): Unit = {
    val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
    val aSign = 1
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = BigInteger.ZERO
    try {
    aNumber.divide(bNumber)
    fail()
  } catch {
     case _: Throwable => // As expected
  }
  }

  @Test def testCase20(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testCase21(): Unit = {
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
      assertEquals(rBytes(0)(i), resBytes(i))
    }
    assertEquals(-1, result(0).signum())
    resBytes = result(1).toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(1)(i), resBytes(i))
      assertEquals(-1, result(1).signum())
    }
  }

  @Test def testCase22(): Unit = {
    val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
    val bBytes = Array[Byte](1, 30, 40, 56, -1, 45)
    val aSign = 1
    val bSign = -1
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    try {
    aNumber.mod(bNumber)
    fail()
  } catch {
     case _: Throwable => // As expected
  }
  }

  @Test def testCase23(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testCase24(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testCase3(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testCase4(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testCase5(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(0, result.signum())
  }

  @Test def testCase6(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(0, result.signum())
  }

  @Test def testCase7(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testCase8(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testCase9(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testDivisionKnuth1(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testDivisionKnuthFirstDigitsEqual(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testDivisionKnuthIsNormalized(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testDivisionKnuthMultiDigitsByOneDigit(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testDivisionKnuthOneDigitByOneDigit(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testRemainderKnuth1(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testRemainderKnuthMultiDigitsByOneDigit(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testRemainderKnuthOneDigitByOneDigit(): Unit = {
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
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }
}
