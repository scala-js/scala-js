// scalastyle:off line.size.limit
/*
 * Ported by Alistair Johnson from
 * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigIntegerMultiplyTest.java
 */
// scalastyle:on line.size.limit

package org.scalajs.testsuite.javalib.math

import java.math.BigInteger

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._

class BigIntegerMultiplyTest {

  @Test def testCase1(): Unit = {
    val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3)
    val bBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
    val aSign = -1
    val bSign = -1
    val rBytes = Array[Byte](10, 40, 100, -55, 96, 51, 76, 40, -45, 85, 105, 4, 28, -86, -117, -52, 100, 120, 90)
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.multiply(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testCase10(): Unit = {
    val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 1, 2, 3, 4, 5)
    val aSign = -1
    val rBytes = Array[Byte](-2, -3, -4, -5, -6, -7, -8, -2, -3, -4, -2, -3, -4, -5, -5)
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = BigInteger.ONE
    val result = aNumber.multiply(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testCase2(): Unit = {
    val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3)
    val bBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
    val aSign = -1
    val bSign = 1
    val rBytes = Array[Byte](-11, -41, -101, 54, -97, -52, -77, -41, 44, -86,
        -106, -5, -29, 85, 116, 51, -101, -121, -90)
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.multiply(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testCase3(): Unit = {
    val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 1, 2, 3, 4, 5)
    val bBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
    val aSign = 1
    val bSign = 1
    val rBytes = Array[Byte](10, 40, 100, -55, 96, 51, 76, 40, -45, 85, 115,
        44, -127, 115, -21, -62, -15, 85, 64, -87, -2, -36, -36, -106)
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.multiply(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testCase4(): Unit = {
    val aBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
    val bBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 1, 2, 3, 4, 5)
    val aSign = 1
    val bSign = 1
    val rBytes = Array[Byte](10, 40, 100, -55, 96, 51, 76, 40, -45, 85, 115,
        44, -127, 115, -21, -62, -15, 85, 64, -87, -2, -36, -36, -106)
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.multiply(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testCase5(): Unit = {
    val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 1, 2, 3, 4, 5)
    val bBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
    val aSign = 1
    val bSign = -1
    val rBytes = Array[Byte](-11, -41, -101, 54, -97, -52, -77, -41, 44, -86,
        -116, -45, 126, -116, 20, 61, 14, -86, -65, 86, 1, 35, 35, 106)
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.multiply(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testCase6(): Unit = {
    val aBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
    val bBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 1, 2, 3, 4, 5)
    val aSign = 1
    val bSign = -1
    val rBytes = Array[Byte](-11, -41, -101, 54, -97, -52, -77, -41, 44, -86,
        -116, -45, 126, -116, 20, 61, 14, -86, -65, 86, 1, 35, 35, 106)
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.multiply(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testCase7(): Unit = {
    val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 1, 2, 3, 4, 5)
    val bBytes = Array[Byte](0)
    val aSign = 1
    val bSign = 0
    val rBytes = Array[Byte](0)
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.multiply(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(0, result.signum())
  }

  @Test def testCase8(): Unit = {
    val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 1, 2, 3, 4, 5)
    val aSign = 1
    val rBytes = Array[Byte](0)
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = BigInteger.ZERO
    val result = aNumber.multiply(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(0, result.signum())
  }

  @Test def testCase9(): Unit = {
    val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 1, 2, 3, 4, 5)
    val aSign = 1
    val rBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 1, 2, 3, 4, 5)
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = BigInteger.ONE
    val result = aNumber.multiply(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testIntbyInt1(): Unit = {
    val aBytes = Array[Byte](10, 20, 30, 40)
    val bBytes = Array[Byte](1, 2, 3, 4)
    val aSign = 1
    val bSign = -1
    val rBytes = Array[Byte](-11, -41, -101, 55, 5, 15, 96)
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.multiply(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testIntbyInt2(): Unit = {
    val aBytes = Array[Byte](-1, -1, -1, -1)
    val bBytes = Array[Byte](-1, -1, -1, -1)
    val aSign = 1
    val bSign = 1
    val rBytes = Array[Byte](0, -1, -1, -1, -2, 0, 0, 0, 1)
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.multiply(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testPowException(): Unit = {
    val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
    val aSign = 1
    val exp = -5
    val aNumber = new BigInteger(aSign, aBytes)
    expectThrows(classOf[ArithmeticException], aNumber.pow(exp))
  }

  @Test def testPowNegativeNumToEvenExp(): Unit = {
    val aBytes = Array[Byte](50, -26, 90, 69, 120, 32, 63, -103, -14, 35)
    val aSign = -1
    val exp = 4
    val rBytes = Array[Byte](102, 107, -122, -43, -52, -20, -27, 25, -9, 88,
        -13, 75, 78, 81, -33, -77, 39, 27, -37, 106, 121, -73, 108, -47, -101,
        80, -25, 71, 13, 94, -7, -33, 1, -17, -65, -70, -61, -3, -47)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.pow(exp)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testPowNegativeNumToOddExp(): Unit = {
    val aBytes = Array[Byte](50, -26, 90, 69, 120, 32, 63, -103, -14, 35)
    val aSign = -1
    val exp = 5
    val rBytes = Array[Byte](-21, -94, -42, -15, -127, 113, -50, -88, 115,
        -35, 3, 59, -92, 111, -75, 103, -42, 41, 34, -114, 99, -32, 105, -59,
        127, 45, 108, 74, -93, 105, 33, 12, -5, -20, 17, -21, -119, -127,
        -115, 27, -122, 26, -67, 109, -125, 16, 91, -70, 109)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.pow(exp)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testPowNegativeNumToZeroExp(): Unit = {
    val aBytes = Array[Byte](50, -26, 90, 69, 120, 32, 63, -103, -14, 35)
    val aSign = -1
    val exp = 0
    val rBytes = Array[Byte](1)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.pow(exp)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testPowPositiveNum(): Unit = {
    val aBytes = Array[Byte](50, -26, 90, 69, 120, 32, 63, -103, -14, 35)
    val aSign = 1
    val exp = 5
    val rBytes = Array[Byte](20, 93, 41, 14, 126, -114, 49, 87, -116, 34, -4,
        -60, 91, -112, 74, -104, 41, -42, -35, 113, -100, 31, -106, 58, -128,
        -46, -109, -75, 92, -106, -34, -13, 4, 19, -18, 20, 118, 126, 114,
        -28, 121, -27, 66, -110, 124, -17, -92, 69, -109)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.pow(exp)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testPowPositiveNumToZeroExp(): Unit = {
    val aBytes = Array[Byte](50, -26, 90, 69, 120, 32, 63, -103, -14, 35)
    val aSign = 1
    val exp = 0
    val rBytes = Array[Byte](1)
    val aNumber = new BigInteger(aSign, aBytes)
    val result = aNumber.pow(exp)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testPow31_issue_2045(): Unit = {
    assertEquals(BigInt("2147483648"), BigInt(2).pow(31))
    assertEquals(BigInt("1326443518324400147398656"), BigInt(6).pow(31))
    assertEquals(BigInt("10000000000000000000000000000000"), BigInt(10).pow(31))
    assertEquals(BigInt("338820052976784335907174521413566464"),
        BigInt(14).pow(31))
    assertEquals(BigInt("819308872942260126404286866009182175232"),
        BigInt(18).pow(31))
    assertEquals(BigInt("412195366437884247746798137865015318806528"),
        BigInt(22).pow(31))
    assertEquals(BigInt("73143171433403393900724146770015259539275776"),
        BigInt(26).pow(31))
    assertEquals(BigInt("6176733962839470000000000000000000000000000000"),
        BigInt(30).pow(31))
    assertEquals(BigInt("299120672332806228664106719451209941853702979584"),
        BigInt(34).pow(31))
    assertEquals(BigInt("9403542573537726518107430106299028013391242330112"),
        BigInt(38).pow(31))
    assertEquals(BigInt("209280132851277227501988209234026802122409700753408"),
        BigInt(42).pow(31))
    assertEquals(BigInt("3511500884366740741319133039278240178576180323024896"),
        BigInt(46).pow(31))
  }
}
