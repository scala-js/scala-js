/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.math

import java.math.BigInteger

import java.util.Arrays

import org.junit.Test
import org.junit.Assert._

class BigIntegerTest {

  @Test def `should accept 3 as a Byte Array`(): Unit = {
    val bi = new BigInteger(Array[Byte](3))
    assertEquals(3, bi.intValue())
  }

  @Test def `should accept 127 as a Byte Array`(): Unit = {
    val bi = new BigInteger(Array[Byte](127))
    assertEquals(127, bi.intValue())
  }

  @Test def `should accept 3 as aLong`(): Unit = {
    val bi = BigInteger.valueOf(3L)
    assertEquals(3, bi.intValue())
    assertEquals(3L, bi.longValue())
  }

  @Test def `should accept 999999999 as aLong`(): Unit = {
    val bi = BigInteger.valueOf(999999999L)
    assertEquals(999999999, bi.intValue())
    assertEquals(999999999L, bi.longValue())
  }

  @Test def `should accept 9999999999 as aLong`(): Unit = {
    val bi = BigInteger.valueOf(9999999999L)
    assertEquals(9999999999L, bi.longValue())
  }

  @Test def `should accept -999999999 as aLong`(): Unit = {
    val bi = BigInteger.valueOf(-999999999L)
    assertEquals(-999999999, bi.intValue())
    assertEquals(-999999999L, bi.longValue())
  }

  @Test def `should accept -9999999999 as aLong`(): Unit = {
    val bi = BigInteger.valueOf(-9999999999L)
    assertEquals(-9999999999L, bi.longValue())
  }

  @Test def `should accept 99 as a string`(): Unit = {
    val bi = new BigInteger("99")
    assertEquals(99, bi.intValue())
    assertEquals(99L, bi.longValue())
  }

  @Test def `should accept 999999999 as sting`(): Unit = {
    val bi = new BigInteger("999999999")
    assertEquals(999999999, bi.intValue())
    assertEquals(999999999L, bi.longValue())
  }

  @Test def `should accept 9999999999 as a string`(): Unit = {
    val bi = new BigInteger("9999999999")
    assertEquals(9999999999L, bi.longValue())
  }

  @Test def `should accept -99 as a string`(): Unit = {
    val bi = new BigInteger("-99")
    assertEquals(-99, bi.intValue())
    assertEquals(-99L, bi.longValue())
  }

  @Test def `should accept -999999999 as sting`(): Unit = {
    val bi = new BigInteger("-999999999")
    assertEquals(-999999999, bi.intValue())
    assertEquals(-999999999L, bi.longValue())
  }

  @Test def `should accept -9999999999 as a string`(): Unit = {
    val bi = new BigInteger("-9999999999")
    assertEquals(-9999999999L, bi.longValue())
  }

  @Test def `should intialise from byte array of Pos two's complement`(): Unit = {
    val eBytesSignum = Array[Byte](27, -15, 65, 39)
    val eBytes = Array[Byte](27, -15, 65, 39)
    val expSignum = new BigInteger(eBytesSignum)
    assertTrue(Arrays.equals(eBytes, expSignum.toByteArray))
  }

  @Test def `should intialise from byte array of Neg two's complement`(): Unit = {
    val eBytesSignum = Array[Byte](-27, -15, 65, 39)
    val eBytes = Array[Byte](-27, -15, 65, 39)
    val expSignum = new BigInteger(eBytesSignum)
    assertTrue(Arrays.equals(eBytes, expSignum.toByteArray))
  }

  @Test def `should intialise from Pos byte array with explicit sign`(): Unit = {
    val eBytes = Array[Byte](27, -15, 65, 39)
    val eSign = 1
    val exp = new BigInteger(eSign, eBytes)
    assertTrue(Arrays.equals(eBytes, exp.toByteArray))
  }

  @Test def `should intialise from Zero byte array with explicit sign`(): Unit = {
    val eBytes = Array[Byte](0, 0, 0, 0)
    val eSign = 0
    val exp = new BigInteger(eSign, eBytes)
    assertTrue(Arrays.equals(Array[Byte](0), exp.toByteArray))
  }

  @Test def `should intialise from Neg small byte array with explicit sign`(): Unit = {
    val eBytes = Array[Byte](27)
    val eSign = -1
    val eRes = Array[Byte](-27)
    val exp = new BigInteger(eSign, eBytes)
    assertTrue(Arrays.equals(eRes, exp.toByteArray))
  }

  @Test def `should intialise from Neg byte array with explicit sign`(): Unit = {
    val eBytes = Array[Byte](27, -15, 65, 39)
    val eSign = -1
    val eRes = Array[Byte](-28, 14, -66, -39)
    val exp = new BigInteger(eSign, eBytes)
    assertTrue(Arrays.equals(eRes, exp.toByteArray))
  }

  @Test def `should intialise both Pos byte arrays arrays the same`(): Unit = {
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

  @Test def `should intialise both Neg byte arrays arrays the same`(): Unit = {
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
