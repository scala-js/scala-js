// scalastyle:off line.size.limit
/*
 * Ported by Alistair Johnson from
 * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigIntegerCompareTest.java
 */
// scalastyle:on line.size.limit

package org.scalajs.testsuite.javalib.math

import java.math.BigInteger

import org.junit.Test
import org.junit.Assert._

class BigIntegerCompareTest {



    @Test def testAbsNegative(): Unit = {
      val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
      val aSign = -1
      val rBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.abs()
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        assertEquals(rBytes(i), resBytes(i))
      }
      assertEquals(1, result.signum())
    }

    @Test def testAbsPositive(): Unit = {
      val aBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
      val aSign = 1
      val rBytes = Array[Byte](1, 2, 3, 4, 5, 6, 7)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.abs()
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        assertEquals(rBytes(i), resBytes(i))
      }
      assertEquals(1, result.signum())
    }

    @Test def testCompareNegNeg2(): Unit = {
      val aBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
      val bBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = -1
      val bSign = -1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      assertEquals(1, aNumber.compareTo(bNumber))
      assertEquals(1, aNumber.compareTo(bNumber))
    }

    @Test def testCompareToDiffSigns1(): Unit = {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val bBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
      val aSign = 1
      val bSign = -1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      assertEquals(1, aNumber.compareTo(bNumber))
    }

    @Test def testCompareToDiffSigns2(): Unit = {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val bBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
      val aSign = -1
      val bSign = 1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      assertEquals(-1, aNumber.compareTo(bNumber))
    }

    @Test def testCompareToEqualNeg(): Unit = {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val bBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = -1
      val bSign = -1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      assertEquals(0, aNumber.compareTo(bNumber))
    }

    @Test def testCompareToEqualPos(): Unit = {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val bBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = 1
      val bSign = 1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      assertEquals(0, aNumber.compareTo(bNumber))
    }

    @Test def testCompareToNegNeg1(): Unit = {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val bBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
      val aSign = -1
      val bSign = -1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      assertEquals(-1, aNumber.compareTo(bNumber))
    }

    @Test def testCompareToNegZero(): Unit = {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = -1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = BigInteger.ZERO
      assertEquals(-1, aNumber.compareTo(bNumber))
    }

    @Test def testCompareToPosPos1(): Unit = {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val bBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
      val aSign = 1
      val bSign = 1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      assertEquals(1, aNumber.compareTo(bNumber))
    }

    @Test def testCompareToPosPos2(): Unit = {
      val aBytes = Array[Byte](10, 20, 30, 40, 50, 60, 70, 10, 20, 30)
      val bBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = 1
      val bSign = 1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      assertEquals(-1, aNumber.compareTo(bNumber))
    }

    @Test def testCompareToPosZero(): Unit = {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = 1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = BigInteger.ZERO
      assertEquals(1, aNumber.compareTo(bNumber))
    }

    @Test def testCompareToZeroNeg(): Unit = {
      val bBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val bSign = -1
      val aNumber = BigInteger.ZERO
      val bNumber = new BigInteger(bSign, bBytes)
      assertEquals(1, aNumber.compareTo(bNumber))
    }

    @Test def testCompareToZeroPos(): Unit = {
      val bBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val bSign = 1
      val aNumber = BigInteger.ZERO
      val bNumber = new BigInteger(bSign, bBytes)
      assertEquals(-1, aNumber.compareTo(bNumber))
    }

    @Test def testCompareToZeroZero(): Unit = {
      val aNumber = BigInteger.ZERO
      val bNumber = BigInteger.ZERO
      assertEquals(0, aNumber.compareTo(bNumber))
    }

    @Test def testEqualsBigIntegerFalse(): Unit = {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val bBytes = Array[Byte](45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = 1
      val bSign = 1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      assertFalse(aNumber == bNumber)
    }

    @Test def testEqualsBigIntegerTrue(): Unit = {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val bBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = 1
      val bSign = 1
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = new BigInteger(bSign, bBytes)
      assertTrue(aNumber == bNumber)
    }

    @Test def testEqualsNull(): Unit = {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = 1
      val aNumber = new BigInteger(aSign, aBytes)
      assertFalse(aNumber == null)
    }

    @Test def testEqualsObject(): Unit = {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = 1
      val aNumber = new BigInteger(aSign, aBytes)
      val obj = new AnyRef()
      assertFalse(aNumber == obj)
    }

    @Test def testMaxEqual(): Unit = {
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
        assertEquals(rBytes(i), resBytes(i))
      }
      assertEquals(1, result.signum())
    }

    @Test def testMaxGreater(): Unit = {
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
        assertEquals(rBytes(i), resBytes(i))
      }
      assertEquals(1, result.signum())
      result = bNumber.max(aNumber)
      assertTrue(aNumber == result)
    }

    @Test def testMaxLess(): Unit = {
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
        assertEquals(rBytes(i), resBytes(i))
      }
      assertEquals(1, result.signum())
    }

    @Test def testMaxNegZero(): Unit = {
      val aBytes = Array[Byte](45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = -1
      val rBytes = Array[Byte](0)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = BigInteger.ZERO
      val result = aNumber.max(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        assertEquals(rBytes(i), resBytes(i))
      }
      assertEquals(0, result.signum())
    }

    @Test def testMinEqual(): Unit = {
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
        assertEquals(rBytes(i), resBytes(i))
      }
      assertEquals(1, result.signum())
    }

    @Test def testMinGreater(): Unit = {
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
        assertEquals(rBytes(i), resBytes(i))
      }
      assertEquals(1, result.signum())
    }

    @Test def testMinLess(): Unit = {
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
        assertEquals(rBytes(i), resBytes(i))
      }
      assertEquals(1, result.signum())
    }

    @Test def testMinPosZero(): Unit = {
      val aBytes = Array[Byte](45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = 1
      val rBytes = Array[Byte](0)
      val aNumber = new BigInteger(aSign, aBytes)
      val bNumber = BigInteger.ZERO
      val result = aNumber.min(bNumber)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        assertEquals(rBytes(i), resBytes(i))
      }
      assertEquals(0, result.signum())
    }

    @Test def testNegateNegative(): Unit = {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = -1
      val rBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.negate()
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        assertEquals(rBytes(i), resBytes(i))
      }
      for (i <- 0 until resBytes.length) {
        assertEquals(rBytes(i), resBytes(i))
      }
      assertEquals(1, result.signum())
    }

    @Test def testNegatePositive(): Unit = {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = 1
      val rBytes = Array[Byte](-13, -57, -101, 1, 75, -90, -46, -92, -4, 14, -36, -27, -4, -91)
      val aNumber = new BigInteger(aSign, aBytes)
      val result = aNumber.negate()
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        assertEquals(rBytes(i), resBytes(i))
      }
      assertEquals(-1, result.signum())
    }

    @Test def testNegateZero(): Unit = {
      val rBytes = Array[Byte](0)
      val aNumber = BigInteger.ZERO
      val result = aNumber.negate()
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = result.toByteArray()
      for (i <- 0 until resBytes.length) {
        assertEquals(rBytes(i), resBytes(i))
      }
     assertEquals(0, result.signum())
    }

    @Test def tassestSignumNegative(): Unit = {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = -1
      val aNumber = new BigInteger(aSign, aBytes)
      assertEquals(-1, aNumber.signum())
    }

    @Test def testSignumPositive(): Unit = {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
      val aSign = 1
      val aNumber = new BigInteger(aSign, aBytes)
      assertEquals(1, aNumber.signum())
    }

    @Test def testSignumZero(): Unit = {
      val aNumber = BigInteger.ZERO
      assertEquals(0, aNumber.signum())
    }
}
