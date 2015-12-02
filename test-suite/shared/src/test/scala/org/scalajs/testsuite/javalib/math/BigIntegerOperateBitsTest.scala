// scalastyle:off line.size.limit
/*
 * Ported by Alistair Johnson from
 * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigIntegerOperateBitsTest.java
 */
// scalastyle:on line.size.limit

package org.scalajs.testsuite.javalib.math

import java.math.BigInteger

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._

class BigIntegerOperateBitsTest {

  @Test def testBitCountNeg(): Unit = {
    val aNumber = new BigInteger("-12378634756382937873487638746283767238657872368748726875")
    assertEquals(87, aNumber.bitCount())
  }

  @Test def testBitCountPos(): Unit = {
    val aNumber = new BigInteger("12378634756343564757582937873487638746283767238657872368748726875")
    assertEquals(107, aNumber.bitCount())
  }

  @Test def testBitCountZero(): Unit = {
    val aNumber = new BigInteger("0")
    assertEquals(0, aNumber.bitCount())
  }

  @Test def testBitLengthNegative1(): Unit = {
    val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
    val aSign = -1
    val aNumber = new BigInteger(aSign, aBytes)
    assertEquals(108, aNumber.bitLength())
  }

  @Test def testBitLengthNegative2(): Unit = {
    val aBytes = Array[Byte](-128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = -1
    val aNumber = new BigInteger(aSign, aBytes)
    assertEquals(96, aNumber.bitLength())
  }

  @Test def testBitLengthNegative3(): Unit = {
    val aBytes = Array[Byte](1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    val aSign = -1
    val aNumber = new BigInteger(aSign, aBytes)
    assertEquals(80, aNumber.bitLength())
  }

  @Test def testBitLengthPositive1(): Unit = {
    val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
    val aSign = 1
    val aNumber = new BigInteger(aSign, aBytes)
    assertEquals(108, aNumber.bitLength())
  }

  @Test def testBitLengthPositive2(): Unit = {
    val aBytes = Array[Byte](-128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val aNumber = new BigInteger(aSign, aBytes)
    assertEquals(96, aNumber.bitLength())
  }

  @Test def testBitLengthPositive3(): Unit = {
    val aBytes = Array[Byte](1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    val aSign = 1
    val aNumber = new BigInteger(aSign, aBytes)
    assertEquals(81, aNumber.bitLength())
  }

  @Test def testBitLengthZero(): Unit = {
    val aNumber = new BigInteger("0")
    assertEquals(0, aNumber.bitLength())
  }

  @Test def testClearBitException(): Unit = {
    val aBytes = Array[Byte](-1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = -7
    val aNumber = new BigInteger(aSign, aBytes)
    expectThrows(classOf[ArithmeticException], aNumber.clearBit(number))
  }

  @Test def testClearBitNegativeInside1(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = -1
    val number = 15
    val rBytes = Array[Byte](-2, 127, -57, -101, 1, 75, -90, -46, -92, -4, 14, 92, -26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.clearBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testClearBitNegativeInside2(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = -1
    val number = 44
    val rBytes = Array[Byte](-2, 127, -57, -101, 1, 75, -90, -62, -92, -4, 14, -36, -26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.clearBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testClearBitNegativeInside3(): Unit = {
    val as = "-18446744073709551615"
    val number = 2
    val aNumber = new BigInteger(as)
    val result = aNumber.clearBit(number)
    assertEquals(result.toString, as)
  }

  @Test def testClearBitNegativeInside4(): Unit = {
    val as = "-4294967295"
    val res = "-4294967296"
    val number = 0
    val aNumber = new BigInteger(as)
    val result = aNumber.clearBit(number)
    assertEquals(result.toString, res)
  }

  @Test def testClearBitNegativeInside5(): Unit = {
    val as = "-18446744073709551615"
    val res = "-18446744073709551616"
    val number = 0
    val aNumber = new BigInteger(as)
    val result = aNumber.clearBit(number)
    assertEquals(result.toString, res)
  }

  @Test def testClearBitNegativeOutside1(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = -1
    val number = 150
    val rBytes = Array[Byte](-65, -1, -1, -1, -1, -1, -2, 127, -57, -101, 1, 75, -90, -46, -92, -4, 14, -36, -26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.clearBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testClearBitNegativeOutside2(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = -1
    val number = 165
    val rBytes = Array[Byte](-33, -1, -1, -1, -1, -1, -1, -1, -2, 127, -57,
        -101, 1, 75, -90, -46, -92, -4, 14, -36, -26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.clearBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testClearBitPositiveInside1(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = 20
    val rBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -31, 35, 26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.clearBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testClearBitPositiveInside2(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = 17
    val rBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.clearBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testClearBitPositiveInside3(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = 45
    val rBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 13, 91, 3, -15, 35, 26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.clearBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testClearBitPositiveInside4(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = 50
    val rBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.clearBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testClearBitPositiveInside5(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = 63
    val rBytes = Array[Byte](1, -128, 56, 100, -2, 52, 89, 45, 91, 3, -15, 35, 26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.clearBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testClearBitPositiveOutside1(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = 150
    val rBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.clearBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testClearBitPositiveOutside2(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = 191
    val rBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.clearBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testClearBitTopNegative(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -15, 35, 26)
    val aSign = -1
    val number = 63
    val rBytes = Array[Byte](-1, 127, -2, 127, -57, -101, 14, -36, -26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.clearBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testClearBitZero(): Unit = {
    val aBytes = Array[Byte](0)
    val aSign = 0
    val number = 0
    val rBytes = Array[Byte](0)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.clearBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(0, result.signum())
  }

  @Test def testClearBitZeroOutside1(): Unit = {
    val aBytes = Array[Byte](0)
    val aSign = 0
    val number = 95
    val rBytes = Array[Byte](0)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.clearBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(0, result.signum())
  }

  @Test def testFlipBitException(): Unit = {
    val aBytes = Array[Byte](-1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = -7
    val aNumber = new BigInteger(aSign, aBytes)
    expectThrows(classOf[ArithmeticException], aNumber.flipBit(number))
  }

  @Test def testFlipBitLeftmostNegative(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -15, 35, 26)
    val aSign = -1
    val number = 48
    val rBytes = Array[Byte](-1, 127, -57, -101, 14, -36, -26, 49)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.flipBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testFlipBitLeftmostPositive(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -15, 35, 26)
    val aSign = 1
    val number = 48
    val rBytes = Array[Byte](0, -128, 56, 100, -15, 35, 26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.flipBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testFlipBitNegativeInside1(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = -1
    val number = 15
    val rBytes = Array[Byte](-2, 127, -57, -101, 1, 75, -90, -46, -92, -4, 14, 92, -26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.flipBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testFlipBitNegativeInside2(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = -1
    val number = 45
    val rBytes = Array[Byte](-2, 127, -57, -101, 1, 75, -90, -14, -92, -4, 14, -36, -26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.flipBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testFlipBitNegativeInside3(): Unit = {
    val as = "-18446744073709551615"
    val res = "-18446744073709551611"
    val number = 2
    val aNumber = new BigInteger(as)
    val result = aNumber.flipBit(number)
    assertEquals(result.toString, res)
  }

  @Test def testFlipBitNegativeInside4(): Unit = {
    val as = "-4294967295"
    val res = "-4294967296"
    val number = 0
    val aNumber = new BigInteger(as)
    val result = aNumber.flipBit(number)
    assertEquals(result.toString, res)
  }

  @Test def testFlipBitNegativeInside5(): Unit = {
    val as = "-18446744073709551615"
    val res = "-18446744073709551616"
    val number = 0
    val aNumber = new BigInteger(as)
    val result = aNumber.flipBit(number)
    assertEquals(result.toString, res)
  }

  @Test def testFlipBitNegativeOutside1(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = -1
    val number = 150
    val rBytes = Array[Byte](-65, -1, -1, -1, -1, -1, -2, 127, -57, -101, 1,
        75, -90, -46, -92, -4, 14, -36, -26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.flipBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testFlipBitNegativeOutside2(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = -1
    val number = 191
    val rBytes = Array[Byte](-1, 127, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -2, 127, -57, -101, 1, 75, -90, -46, -92, -4, 14, -36, -26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.flipBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testFlipBitPositiveInside1(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = 15
    val rBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, -93, 26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.flipBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testFlipBitPositiveInside2(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = 45
    val rBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 13, 91, 3, -15, 35, 26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.flipBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testFlipBitPositiveOutside1(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = 150
    val rBytes = Array[Byte](64, 0, 0, 0, 0, 0, 1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.flipBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testFlipBitPositiveOutside2(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = 191
    val rBytes = Array[Byte](0, -128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -128,
        56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.flipBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testFlipBitZero(): Unit = {
    val aBytes = Array[Byte](0)
    val aSign = 0
    val number = 0
    val rBytes = Array[Byte](1)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.flipBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testFlipBitZeroOutside1(): Unit = {
    val aBytes = Array[Byte](0)
    val aSign = 0
    val number = 62
    val rBytes = Array[Byte](64, 0, 0, 0, 0, 0, 0, 0)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.flipBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testFlipBitZeroOutside2(): Unit = {
    val aBytes = Array[Byte](0)
    val aSign = 0
    val number = 63
    val rBytes = Array[Byte](0, -128, 0, 0, 0, 0, 0, 0, 0)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.flipBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testSetBitBug1331(): Unit = {
    val result = BigInteger.valueOf(0L).setBit(191)
    assertEquals("3138550867693340381917894711603833208051177722232017256448", result.toString)
    assertEquals(1, result.signum())
  }

  @Test def testSetBitException(): Unit = {
    val aBytes = Array[Byte](-1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = -7
    var aNumber = new BigInteger(aSign, aBytes)
    expectThrows(classOf[ArithmeticException], aNumber.setBit(number))
  }

  @Test def testSetBitLeftmostNegative(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -15, 35, 26)
    val aSign = -1
    val number = 48
    val rBytes = Array[Byte](-1, 127, -57, -101, 14, -36, -26, 49)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.setBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testSetBitNegativeInside1(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = -1
    val number = 15
    val rBytes = Array[Byte](-2, 127, -57, -101, 1, 75, -90, -46, -92, -4, 14, -36, -26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.setBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testSetBitNegativeInside2(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = -1
    val number = 44
    val rBytes = Array[Byte](-2, 127, -57, -101, 1, 75, -90, -46, -92, -4, 14, -36, -26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.setBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testSetBitNegativeInside3(): Unit = {
    val as = "-18446744073709551615"
    val res = "-18446744073709551611"
    val number = 2
    val aNumber = new BigInteger(as)
    val result = aNumber.setBit(number)
    assertEquals(result.toString, res)
  }

  @Test def testSetBitNegativeInside4(): Unit = {
    val as = "-4294967295"
    val number = 0
    val aNumber = new BigInteger(as)
    val result = aNumber.setBit(number)
    assertEquals(as, result.toString)
  }

  @Test def testSetBitNegativeInside5(): Unit = {
    val as = "-18446744073709551615"
    val number = 0
    val aNumber = new BigInteger(as)
    val result = aNumber.setBit(number)
    assertEquals(as, result.toString)
  }

  @Test def testSetBitNegativeOutside1(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = -1
    val number = 150
    val rBytes = Array[Byte](-2, 127, -57, -101, 1, 75, -90, -46, -92, -4, 14, -36, -26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.setBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testSetBitNegativeOutside2(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = -1
    val number = 191
    val rBytes = Array[Byte](-2, 127, -57, -101, 1, 75, -90, -46, -92, -4, 14, -36, -26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.setBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testSetBitPositiveInside1(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = 20
    val rBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.setBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testSetBitPositiveInside2(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = 17
    val rBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -13, 35, 26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.setBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testSetBitPositiveInside3(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = 45
    val rBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.setBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testSetBitPositiveInside4(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = 50
    val rBytes = Array[Byte](1, -128, 56, 100, -2, -76, 93, 45, 91, 3, -15, 35, 26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.setBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testSetBitPositiveOutside1(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = 150
    val rBytes = Array[Byte](64, 0, 0, 0, 0, 0, 1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.setBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testSetBitPositiveOutside2(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = 223
    val rBytes = Array[Byte](0, -128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.setBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testSetBitTopPositive(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -15, 35, 26)
    val aSign = 1
    val number = 63
    val rBytes = Array[Byte](0, -128, 1, -128, 56, 100, -15, 35, 26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.setBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testSetBitZero(): Unit = {
    val aBytes = Array[Byte](0)
    val aSign = 0
    val number = 0
    val rBytes = Array[Byte](1)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.setBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testSetBitZeroOutside1(): Unit = {
    val aBytes = Array[Byte](0)
    val aSign = 0
    val number = 95
    val rBytes = Array[Byte](0, -128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.setBit(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testShiftLeft1(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = 0
    val rBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.shiftLeft(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testShiftLeft2(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = -27
    val rBytes = Array[Byte](48, 7, 12, -97, -42, -117, 37, -85, 96)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.shiftLeft(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testShiftLeft3(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = 27
    val rBytes = Array[Byte](12, 1, -61, 39, -11, -94, -55, 106, -40, 31, -119, 24, -48, 0, 0, 0)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.shiftLeft(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testShiftLeft4(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = 45
    val rBytes = Array[Byte](48, 7, 12, -97, -42, -117, 37, -85, 96, 126, 36, 99, 64, 0, 0, 0, 0, 0)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.shiftLeft(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testShiftLeft5(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = -1
    val number = 45
    val rBytes = Array[Byte](-49, -8, -13, 96, 41, 116, -38, 84, -97, -127,
        -37, -100, -64, 0, 0, 0, 0, 0)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.shiftLeft(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testShiftRight1(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = 0
    val rBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.shiftRight(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testShiftRight2(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = -27
    val rBytes = Array[Byte](12, 1, -61, 39, -11, -94, -55, 106, -40, 31, -119, 24, -48, 0, 0, 0)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.shiftRight(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testShiftRight3(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = 27
    val rBytes = Array[Byte](48, 7, 12, -97, -42, -117, 37, -85, 96)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.shiftRight(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testShiftRight4(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = 45
    val rBytes = Array[Byte](12, 1, -61, 39, -11, -94, -55)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.shiftRight(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testShiftRight5(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = 300
    val rBytes = Array[Byte](0)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.shiftRight(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(0, result.signum())
  }

  @Test def testShiftRightNegNonZeroes(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 0, 0, 0, 0, 0, 0, 0, 0)
    val aSign = -1
    val number = 68
    val rBytes = Array[Byte](-25, -4, 121, -80, 20, -70, 109, 42)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.shiftRight(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testShiftRightNegNonZeroesMul32(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 1, 0, 0, 0, 0, 0, 0, 0)
    val aSign = -1
    val number = 64
    val rBytes = Array[Byte](-2, 127, -57, -101, 1, 75, -90, -46, -92)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.shiftRight(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testShiftRightNegZeroes(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    val aSign = -1
    val number = 68
    val rBytes = Array[Byte](-25, -4, 121, -80, 20, -70, 109, 48)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.shiftRight(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testShiftRightNegZeroesMul32(): Unit = {
    val aBytes = Array[Byte](1, -128, 56, 100, -2, -76, 89, 45, 91, 0, 0, 0, 0, 0, 0, 0, 0)
    val aSign = -1
    val number = 64
    val rBytes = Array[Byte](-2, 127, -57, -101, 1, 75, -90, -46, -91)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.shiftRight(number)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testTestBitException(): Unit = {
    val aBytes = Array[Byte](-1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = -7
    val aNumber = new BigInteger(aSign, aBytes)
    expectThrows(classOf[ArithmeticException],  aNumber.testBit(number))
  }

  @Test def testTestBitNegative1(): Unit = {
    val aBytes = Array[Byte](-1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = -1
    val number = 7
    val aNumber = new BigInteger(aSign, aBytes)
    assertTrue(aNumber.testBit(number))
  }

  @Test def testTestBitNegative2(): Unit = {
    val aBytes = Array[Byte](-1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = -1
    val number = 45
    val aNumber = new BigInteger(aSign, aBytes)
    assertTrue(!aNumber.testBit(number))
  }

  @Test def testTestBitNegative3(): Unit = {
    val aBytes = Array[Byte](-1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = -1
    val number = 300
    val aNumber = new BigInteger(aSign, aBytes)
    assertTrue(aNumber.testBit(number))
  }

  @Test def testTestBitPositive1(): Unit = {
    val aBytes = Array[Byte](-1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = 7
    val aNumber = new BigInteger(aSign, aBytes)
    assertTrue(!aNumber.testBit(number))
  }

  @Test def testTestBitPositive2(): Unit = {
    val aBytes = Array[Byte](-1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = 45
    val aNumber = new BigInteger(aSign, aBytes)
    assertTrue(aNumber.testBit(number))
  }

  @Test def testTestBitPositive3(): Unit = {
    val aBytes = Array[Byte](-1, -128, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26)
    val aSign = 1
    val number = 300
    val aNumber = new BigInteger(aSign, aBytes)
    assertTrue(!aNumber.testBit(number))
  }
}
