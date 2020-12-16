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

package org.scalajs.testsuite.javalib.math

import java.math.BigInteger

import java.util.Arrays

import org.junit.Test
import org.junit.Assert._

class BigIntegerTest {

  @Test def ctorArrayByte3(): Unit = {
    val bi = new BigInteger(Array[Byte](3))
    assertEquals(3, bi.intValue())
  }

  @Test def ctorArrayByte127(): Unit = {
    val bi = new BigInteger(Array[Byte](127))
    assertEquals(127, bi.intValue())
  }

  @Test def valueOfLong3(): Unit = {
    val bi = BigInteger.valueOf(3L)
    assertEquals(3, bi.intValue())
    assertEquals(3L, bi.longValue())
  }

  @Test def valueOfLong999999999(): Unit = {
    val bi = BigInteger.valueOf(999999999L)
    assertEquals(999999999, bi.intValue())
    assertEquals(999999999L, bi.longValue())
  }

  @Test def valueOfLong9999999999(): Unit = {
    val bi = BigInteger.valueOf(9999999999L)
    assertEquals(9999999999L, bi.longValue())
  }

  @Test def valueOfLongNegative999999999(): Unit = {
    val bi = BigInteger.valueOf(-999999999L)
    assertEquals(-999999999, bi.intValue())
    assertEquals(-999999999L, bi.longValue())
  }

  @Test def valueOfLongNegative9999999999(): Unit = {
    val bi = BigInteger.valueOf(-9999999999L)
    assertEquals(-9999999999L, bi.longValue())
  }

  @Test def ctorString99(): Unit = {
    val bi = new BigInteger("99")
    assertEquals(99, bi.intValue())
    assertEquals(99L, bi.longValue())
  }

  @Test def ctorString999999999(): Unit = {
    val bi = new BigInteger("999999999")
    assertEquals(999999999, bi.intValue())
    assertEquals(999999999L, bi.longValue())
  }

  @Test def ctorString9999999999(): Unit = {
    val bi = new BigInteger("9999999999")
    assertEquals(9999999999L, bi.longValue())
  }

  @Test def ctorStringNegative99(): Unit = {
    val bi = new BigInteger("-99")
    assertEquals(-99, bi.intValue())
    assertEquals(-99L, bi.longValue())
  }

  @Test def ctorStringNegative999999999(): Unit = {
    val bi = new BigInteger("-999999999")
    assertEquals(-999999999, bi.intValue())
    assertEquals(-999999999L, bi.longValue())
  }

  @Test def ctorStringNegative9999999999(): Unit = {
    val bi = new BigInteger("-9999999999")
    assertEquals(-9999999999L, bi.longValue())
  }

  @Test def ctorArrayBytePosTwosComplement(): Unit = {
    val eBytesSignum = Array[Byte](27, -15, 65, 39)
    val eBytes = Array[Byte](27, -15, 65, 39)
    val expSignum = new BigInteger(eBytesSignum)
    assertTrue(Arrays.equals(eBytes, expSignum.toByteArray))
  }

  @Test def ctorArrayByteNegTwosComplement(): Unit = {
    val eBytesSignum = Array[Byte](-27, -15, 65, 39)
    val eBytes = Array[Byte](-27, -15, 65, 39)
    val expSignum = new BigInteger(eBytesSignum)
    assertTrue(Arrays.equals(eBytes, expSignum.toByteArray))
  }

  @Test def ctorArrayByteSign1PosTwosComplement(): Unit = {
    val eBytes = Array[Byte](27, -15, 65, 39)
    val eSign = 1
    val exp = new BigInteger(eSign, eBytes)
    assertTrue(Arrays.equals(eBytes, exp.toByteArray))
  }

  @Test def ctorIntArrayByteSign0Zeros(): Unit = {
    val eBytes = Array[Byte](0, 0, 0, 0)
    val eSign = 0
    val exp = new BigInteger(eSign, eBytes)
    assertTrue(Arrays.equals(Array[Byte](0), exp.toByteArray))
  }

  @Test def ctorIntArrayByteSignNeg1(): Unit = {
    val eBytes = Array[Byte](27)
    val eSign = -1
    val eRes = Array[Byte](-27)
    val exp = new BigInteger(eSign, eBytes)
    assertTrue(Arrays.equals(eRes, exp.toByteArray))
  }

  @Test def ctorIntArrayByteSignNeg1PosTwosComplement(): Unit = {
    val eBytes = Array[Byte](27, -15, 65, 39)
    val eSign = -1
    val eRes = Array[Byte](-28, 14, -66, -39)
    val exp = new BigInteger(eSign, eBytes)
    assertTrue(Arrays.equals(eRes, exp.toByteArray))
  }

  @Test def ctorArrayByteSign1CompareNoSignTwosComplement(): Unit = {
    val eBytes = Array[Byte](27, -15, 65, 39)
    val eSign = 1
    val exp = new BigInteger(eSign, eBytes)
    val eBytesSignum = Array[Byte](27, -15, 65, 39)
    val expSignum = new BigInteger(eBytesSignum)

    assertEquals(0, expSignum.compareTo(exp))
    assertTrue(Arrays.equals(eBytes, exp.toByteArray))
    assertTrue(Arrays.equals(eBytes, expSignum.toByteArray))
    assertTrue(Arrays.equals(exp.toByteArray, expSignum.toByteArray))
  }

  @Test def ctorIntArrayByteCompareCtorArrayByte(): Unit = {
    val eBytes = Array[Byte](27, -15, 65, 39)
    val eSign = -1
    val eRes = Array[Byte](-28, 14, -66, -39)
    val exp = new BigInteger(eSign, eBytes)
    val eBytesSignum = Array[Byte](-28, 14, -66, -39)
    val expSignum = new BigInteger(eBytesSignum)

    assertEquals(exp.toString, expSignum.toString)
    assertTrue(Arrays.equals(eRes, exp.toByteArray))
    assertTrue(Arrays.equals(eBytesSignum, expSignum.toByteArray))
    assertTrue(Arrays.equals(exp.toByteArray, expSignum.toByteArray))
  }
}
