/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

// scalastyle:off line.size.limit
/*
 * Ported by Alistair Johnson from
 * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigIntegerConstructorsTest.java
 */
// scalastyle:on line.size.limit

package org.scalajs.testsuite.javalib.math

import java.math.BigInteger
import java.util.Random

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.Platform
import org.scalajs.testsuite.utils.AssertThrows._

class BigIntegerConstructorsTest {

  @Test def testConstructorBytesException(): Unit = {
    val aBytes = Array[Byte]()
    expectThrows(classOf[NumberFormatException], new BigInteger(aBytes))
  }

  @Test def testConstructorBytesNegative1(): Unit = {
    val aBytes = Array[Byte](-12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
    val rBytes = Array[Byte](-12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
    val aNumber = new BigInteger(aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, aNumber.signum())
  }

  @Test def testConstructorBytesNegative2(): Unit = {
    val aBytes = Array[Byte](-12, 56, 100)
    val rBytes = Array[Byte](-12, 56, 100)
    val aNumber = new BigInteger(aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, aNumber.signum())
  }

  @Test def testConstructorBytesNegative3(): Unit = {
    val aBytes = Array[Byte](-128, -12, 56, 100)
    val rBytes = Array[Byte](-128, -12, 56, 100)
    val aNumber = new BigInteger(aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, aNumber.signum())
  }

  @Test def testConstructorBytesNegative4(): Unit = {
    val aBytes = Array[Byte](-128, -12, 56, 100, -13, 56, 93, -78)
    val rBytes = Array[Byte](-128, -12, 56, 100, -13, 56, 93, -78)
    val aNumber = new BigInteger(aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, aNumber.signum())
  }

  @Test def testConstructorBytesPositive(): Unit = {
    val aBytes = Array[Byte](127, 56, 100, -1, 14, 75, -24, -100)
    val rBytes = Array[Byte](127, 56, 100, -1, 14, 75, -24, -100)
    val aNumber = new BigInteger(aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, aNumber.signum())
  }

  @Test def testConstructorBytesPositive1(): Unit = {
    val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
    val rBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 35, 26, 3, 91)
    val aNumber = new BigInteger(aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, aNumber.signum())
  }

  @Test def testConstructorBytesPositive2(): Unit = {
    val aBytes = Array[Byte](12, 56, 100)
    val rBytes = Array[Byte](12, 56, 100)
    val aNumber = new BigInteger(aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, aNumber.signum())
  }

  @Test def testConstructorBytesPositive3(): Unit = {
    val aBytes = Array[Byte](127, 56, 100, -1)
    val rBytes = Array[Byte](127, 56, 100, -1)
    val aNumber = new BigInteger(aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, aNumber.signum())
  }

  @Test def testConstructorBytesZero(): Unit = {
    val aBytes = Array[Byte](0, 0, 0, -0, 0, 0, -0)
    val rBytes = Array[Byte](0)
    val aNumber = new BigInteger(aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(0, aNumber.signum())
  }

  @Test def testConstructorPrime(): Unit = {
    val bitLen = 25
    val rnd = new Random()
    val aNumber = new BigInteger(bitLen, 80, rnd)
    assertEquals(bitLen, aNumber.bitLength())
  }

  @Test def testConstructorPrime2(): Unit = {
    val bitLen = 2
    val rnd = new Random()
    val aNumber = new BigInteger(bitLen, 80, rnd)
    assertEquals(bitLen, aNumber.bitLength())
    val num = aNumber.intValue()
    assertTrue(num == 2 || num == 3)
  }

  @Test def testConstructorRandom(): Unit = {
    val bitLen = 75
    val rnd: Random = new Random()
    val aNumber = new BigInteger(bitLen, rnd)
    assertTrue(aNumber.bitLength() <= bitLen)
  }

  @Test def testConstructorSignBytesException1(): Unit = {
    val aBytes = Array[Byte](123, 45, -3, -76)
    val aSign = 3
    expectThrows(classOf[NumberFormatException], new BigInteger(aSign, aBytes))
  }

  @Test def testConstructorSignBytesException2(): Unit = {
    val aBytes = Array[Byte](123, 45, -3, -76)
    val aSign = 0
    expectThrows(classOf[NumberFormatException], new BigInteger(aSign, aBytes))
  }

  @Test def testConstructorSignBytesNegative1(): Unit = {
    val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15)
    val aSign = -1
    val rBytes = Array[Byte](-13, -57, -101, 1, 75, -90, -46, -92, -4, 15)
    val aNumber = new BigInteger(aSign, aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, aNumber.signum())
  }

  @Test def testConstructorSignBytesNegative2(): Unit = {
    val aBytes = Array[Byte](-12, 56, 100, -2, -76, 89, 45, 91, 3, -15)
    val aSign = -1
    val rBytes = Array[Byte](-1, 11, -57, -101, 1, 75, -90, -46, -92, -4, 15)
    val aNumber = new BigInteger(aSign, aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, aNumber.signum())
  }

  @Test def testConstructorSignBytesNegative3(): Unit = {
    val aBytes = Array[Byte](-12, 56, 100)
    val aSign = -1
    val rBytes = Array[Byte](-1, 11, -57, -100)
    val aNumber = new BigInteger(aSign, aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, aNumber.signum())
  }

  @Test def testConstructorSignBytesNegative4(): Unit = {
    val aBytes = Array[Byte](127, 56, 100, -2)
    val aSign = -1
    val rBytes = Array[Byte](-128, -57, -101, 2)
    val aNumber = new BigInteger(aSign, aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, aNumber.signum())
  }

  @Test def testConstructorSignBytesNegative5(): Unit = {
    val aBytes = Array[Byte](-127, 56, 100, -2)
    val aSign = -1
    val rBytes = Array[Byte](-1, 126, -57, -101, 2)
    val aNumber = new BigInteger(aSign, aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, aNumber.signum())
  }

  @Test def testConstructorSignBytesNegative6(): Unit = {
    val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 23, -101)
    val aSign = -1
    val rBytes = Array[Byte](-13, -57, -101, 1, 75, -90, -46, -92, -4, 14, -24, 101)
    val aNumber = new BigInteger(aSign, aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, aNumber.signum())
  }

  @Test def testConstructorSignBytesNegative7(): Unit = {
    val aBytes = Array[Byte](-12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 23, -101)
    val aSign = -1
    val rBytes = Array[Byte](-1, 11, -57, -101, 1, 75, -90, -46, -92, -4, 14, -24, 101)
    val aNumber = new BigInteger(aSign, aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, aNumber.signum())
  }

  @Test def testConstructorSignBytesPositive1(): Unit = {
    val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15)
    val aSign = 1
    val rBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15)
    val aNumber = new BigInteger(aSign, aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, aNumber.signum())
  }

  @Test def testConstructorSignBytesPositive2(): Unit = {
    val aBytes = Array[Byte](-12, 56, 100, -2, -76, 89, 45, 91, 3, -15)
    val aSign = 1
    val rBytes = Array[Byte](0, -12, 56, 100, -2, -76, 89, 45, 91, 3, -15)
    val aNumber = new BigInteger(aSign, aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, aNumber.signum())
  }

  @Test def testConstructorSignBytesPositive3(): Unit = {
    val aBytes = Array[Byte](-12, 56, 100)
    val aSign = 1
    val rBytes = Array[Byte](0, -12, 56, 100)
    val aNumber = new BigInteger(aSign, aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, aNumber.signum())
  }

  @Test def testConstructorSignBytesPositive4(): Unit = {
    val aBytes = Array[Byte](127, 56, 100, -2)
    val aSign = 1
    val rBytes = Array[Byte](127, 56, 100, -2)
    val aNumber = new BigInteger(aSign, aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, aNumber.signum())
  }

  @Test def testConstructorSignBytesPositive5(): Unit = {
    val aBytes = Array[Byte](-127, 56, 100, -2)
    val aSign = 1
    val rBytes = Array[Byte](0, -127, 56, 100, -2)
    val aNumber = new BigInteger(aSign, aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, aNumber.signum())
  }

  @Test def testConstructorSignBytesPositive6(): Unit = {
    val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 23, -101)
    val aSign = 1
    val rBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 23, -101)
    val aNumber = new BigInteger(aSign, aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, aNumber.signum())
  }

  @Test def testConstructorSignBytesPositive7(): Unit = {
    val aBytes = Array[Byte](-12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 23, -101)
    val aSign = 1
    val rBytes = Array[Byte](0, -12, 56, 100, -2, -76, 89, 45, 91, 3, -15, 23, -101)
    val aNumber = new BigInteger(aSign, aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, aNumber.signum())
  }

  @Test def testConstructorSignBytesZero1(): Unit = {
    val aBytes = Array[Byte](-0, 0, 0, 0, 0, 0, 0)
    val aSign = -1
    val rBytes = Array[Byte](0)
    val aNumber = new BigInteger(aSign, aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(0, aNumber.signum())
  }

  @Test def testConstructorSignBytesZero2(): Unit = {
    val aBytes = Array[Byte](-0, 0, 0, 0, 0, 0, 0)
    val aSign = 0
    val rBytes = Array[Byte](0)
    val aNumber = new BigInteger(aSign, aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(0, aNumber.signum())
  }

  @Test def testConstructorSignBytesZero3(): Unit = {
    val aBytes = Array[Byte](-0, 0, 0, 0, 0, 0, 0)
    val aSign = 1
    val rBytes = Array[Byte](0)
    val aNumber = new BigInteger(aSign, aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(0, aNumber.signum())
  }

  @Test def testConstructorSignBytesZeroNull1(): Unit = {
    val aBytes = Array[Byte]()
    val aSign = -1
    val rBytes = Array[Byte](0)
    val aNumber = new BigInteger(aSign, aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(0, aNumber.signum())
  }

  @Test def testConstructorSignBytesZeroNull2(): Unit = {
    val aBytes = Array[Byte]()
    val aSign = 0
    val rBytes = Array[Byte](0)
    val aNumber = new BigInteger(aSign, aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(0, aNumber.signum())
  }

  @Test def testConstructorSignBytesZeroNull3(): Unit = {
    val aBytes = Array[Byte]()
    val aSign = 1
    val rBytes = Array[Byte](0)
    val aNumber = new BigInteger(aSign, aBytes)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(0, aNumber.signum())
  }

  @Test def testConstructorStringException(): Unit = {
    def test(s: String, radix: Int): Unit =
      expectThrows(classOf[NumberFormatException], new BigInteger(s, radix))

    test("9234853876401", 45)
    test("   9234853876401", 10)
    test("92348$*#78987", 34)
    test("98zv765hdsaiy", 20)

    test("", 10)
    test("+", 10)
    test("-", 10)
    test("100000000+10000000", 10) // embedded sign character
  }

  @Test def testConstructorStringRadix10(): Unit = {
    val value = "987328901348934898"
    val radix = 10
    val rBytes = Array(13, -77, -78, 103, -103, 97, 68, -14)
    val aNumber = new BigInteger(value, radix)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, aNumber.signum())
  }

  @Test def testConstructorStringRadix10Negative(): Unit = {
    val value = "-234871376037"
    val radix = 36
    val rBytes = Array(-4, 48, 71, 62, -76, 93, -105, 13)
    val aNumber = new BigInteger(value, radix)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, aNumber.signum())
  }

  @Test def testConstructorStringRadix10Zero(): Unit = {
    val value = "-00000000000000"
    val radix = 10
    val rBytes = Array(0)
    val aNumber = new BigInteger(value, radix)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(0, aNumber.signum())
  }

  @Test def testConstructorStringRadix10Issue2228(): Unit = {
    assumeFalse("Assumed not executing on JDK6", Platform.executingInJVMOnJDK6)

    val value = "+100000000"
    val radix = 10
    val rBytes = Array[Byte](5, -11, -31, 0)
    val aNumber = new BigInteger(value, radix)
    assertArrayEquals(rBytes, aNumber.toByteArray())
    assertEquals(1, aNumber.signum())
  }

  @Test def testConstructorStringRadix16(): Unit = {
    val value = "fe2340a8b5ce790"
    val radix = 16
    val rBytes = Array(15, -30, 52, 10, -117, 92, -25, -112)
    val aNumber = new BigInteger(value, radix)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, aNumber.signum())
  }

  @Test def testConstructorStringRadix2(): Unit = {
    val value = "10101010101010101"
    val radix = 2
    val rBytes = Array(1, 85, 85)
    val aNumber = new BigInteger(value, radix)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, aNumber.signum())
  }

  @Test def testConstructorStringRadix36(): Unit = {
    val value = "skdjgocvhdjfkl20jndjkf347ejg457"
    val radix = 36
    val rBytes = Array(0, -12, -116, 112, -105, 12, -36, 66, 108, 66, -20,
        -37, -15, 108, -7, 52, -99, -109, -8, -45, -5)
    val aNumber = new BigInteger(value, radix)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, aNumber.signum())
  }

  @Test def testConstructorStringRadix8(): Unit = {
    val value = "76356237071623450"
    val radix = 8
    val rBytes = Array(7, -50, -28, -8, -25, 39, 40)
    val aNumber = new BigInteger(value, radix)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
      assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, aNumber.signum())
  }
}
