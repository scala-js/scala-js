// scalastyle:off line.size.limit
/*
 * Ported by Alistair Johnson from
 * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigIntegerAddTest.java
 */
// scalastyle:on line.size.limit

package org.scalajs.testsuite.javalib.math

import java.math.BigInteger

import org.junit.Test
import org.junit.Assert._

class BigIntegerAddTest {

  @Test def testCase1(): Unit = {
    val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3)
    val bBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
    val aSign = 1
    val bSign = 1
    val rBytes = Array[Byte](11, 22, 33, 44, 55, 66, 77, 11, 22, 33)
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.add(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testCase2(): Unit = {
    val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3)
    val bBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
    val aSign = -1
    val bSign = -1
    val rBytes = Array[Byte](-12, -23, -34, -45, -56, -67, -78, -12, -23, -33)
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.add(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testCase3(): Unit = {
    val aBytes = Array[Byte](3, 4, 5, 6, 7, 8, 9)
    val bBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
    val rBytes = Array[Byte](2, 2, 2, 2, 2, 2, 2)
    val aSign = 1
    val bSign = -1
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.add(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testCase4(): Unit = {
    val aBytes = Array[Byte](3, 4, 5, 6, 7, 8, 9)
    val bBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
    val rBytes = Array[Byte](-3, -3, -3, -3, -3, -3, -2)
    val aSign = -1
    val bSign = 1
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.add(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testCase5(): Unit = {
    val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
    val bBytes = Array[Byte](3, 4, 5, 6, 7, 8, 9)
    val rBytes = Array[Byte](-3, -3, -3, -3, -3, -3, -2)
    val aSign = 1
    val bSign = -1
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.add(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testCase6(): Unit = {
    val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
    val bBytes = Array[Byte](3, 4, 5, 6, 7, 8, 9)
    val rBytes = Array[Byte](2, 2, 2, 2, 2, 2, 2)
    val aSign = -1
    val bSign = 1
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.add(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testCase7(): Unit = {
    val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7)
    val bBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
    val aSign = 1
    val bSign = 1
    val rBytes = Array[Byte](1, 2, 3, 4, 15, 26, 37, 41, 52, 63, 74, 15, 26, 37)
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.add(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testCase8(): Unit = {
    val aBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
    val bBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7)
    val rBytes = Array[Byte](1, 2, 3, 4, 15, 26, 37, 41, 52, 63, 74, 15, 26, 37)
    val aNumber = new BigInteger(aBytes)
    val bNumber = new BigInteger(bBytes)
    val result = aNumber.add(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testCase9(): Unit = {
    val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7)
    val bBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
    val aSign = -1
    val bSign = -1
    val rBytes = Array[Byte](-2, -3, -4, -5, -16, -27, -38, -42, -53, -64, -75, -16, -27, -37)
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.add(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testCase10(): Unit = {
    val aBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
    val bBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7)
    val aSign = -1
    val bSign = -1
    val rBytes = Array(-2, -3, -4, -5, -16, -27, -38, -42, -53, -64, -75, -16, -27, -37)
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.add(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testCase11(): Unit = {
    val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7)
    val bBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
    val aSign = 1
    val bSign = -1
    val rBytes = Array[Byte](1, 2, 3, 3, -6, -15, -24, -40, -49, -58, -67, -6, -15, -23)
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.add(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testCase12(): Unit = {
    val aBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
    val bBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7)
    val aSign = 1
    val bSign = -1
    val rBytes = Array[Byte](-2, -3, -4, -4, 5, 14, 23, 39, 48, 57, 66, 5, 14, 23)
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.add(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testCase13(): Unit = {
    val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7)
    val bBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
    val aSign = -1
    val bSign = 1
    val rBytes = Array[Byte](-2, -3, -4, -4, 5, 14, 23, 39, 48, 57, 66, 5, 14, 23)
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.add(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, result.signum())
  }

  @Test def testCase14(): Unit = {
    val aBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
    val bBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7)
    val aSign = -1
    val bSign = 1
    val rBytes = Array[Byte](1, 2, 3, 3, -6, -15, -24, -40, -49, -58, -67, -6, -15, -23)
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.add(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testCase15(): Unit = {
    val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
    val bBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
    val rBytes = Array[Byte](0)
    val aSign = -1
    val bSign = 1
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.add(bNumber)
    val resBytes = Array.ofDim[Byte](rBytes.length)
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(0, result.signum())
  }

  @Test def testCase16(): Unit = {
    val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
    val bBytes = Array[Byte](0)
    val rBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
    val aSign = 1
    val bSign = 1
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.add(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testCase17(): Unit = {
    val aBytes = Array[Byte](0)
    val bBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
    val rBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
    val aSign = 1
    val bSign = 1
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.add(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testCase18(): Unit = {
    val aBytes = Array[Byte](0)
    val bBytes = Array[Byte](0)
    val rBytes = Array[Byte](0)
    val aSign = 1
    val bSign = 1
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.add(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(0, result.signum())
  }

  @Test def testCase19(): Unit = {
    val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
    val rBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
    val aSign = 1
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = BigInteger.ZERO
    val result = aNumber.add(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testCase20(): Unit = {
    val bBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
    val rBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
    val bSign = 1
    val aNumber = BigInteger.ZERO
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.add(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testCase21(): Unit = {
    val rBytes = Array[Byte](0)
    val aNumber = BigInteger.ZERO
    val bNumber = BigInteger.ZERO
    val result = aNumber.add(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(0, result.signum())
  }

  @Test def testCase22(): Unit = {
    val rBytes = Array[Byte](2)
    val aNumber = BigInteger.ONE
    val bNumber = BigInteger.ONE
    val result = aNumber.add(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }

  @Test def testCase23(): Unit = {
    val aBytes = Array[Byte](-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1)
    val bBytes = Array[Byte](-1, -1, -1, -1, -1, -1, -1, -1)
    val aSign = 1
    val bSign = 1
    val rBytes = Array[Byte](1, 0, 0, 0, 0, 0, 0, -1, -1, -1, -1, -1, -1, -1, -2)
    val aNumber = new BigInteger(aSign, aBytes)
    val bNumber = new BigInteger(bSign, bBytes)
    val result = aNumber.add(bNumber)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = result.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, result.signum())
  }
}
