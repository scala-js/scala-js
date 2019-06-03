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

package org.scalajs.testsuite.javalib.lang

import java.lang.{Byte => JByte}

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._

/** Tests the implementation of the java standard library Byte
 */
class ByteTest {

  @Test def compareTo(): Unit = {
    def compare(x: Byte, y: Byte): Int =
      new JByte(x).compareTo(new JByte(y))

    assertTrue(compare(0.toByte, 5.toByte) < 0)
    assertTrue(compare(10.toByte, 9.toByte) > 0)
    assertTrue(compare(-2.toByte, -1.toByte) < 0)
    assertEquals(0, compare(3.toByte, 3.toByte))
  }

  @Test def should_be_a_Comparable(): Unit = {
    def compare(x: Any, y: Any): Int =
      x.asInstanceOf[Comparable[Any]].compareTo(y)

    assertTrue(compare(0.toByte, 5.toByte) < 0)
    assertTrue(compare(10.toByte, 9.toByte) > 0)
    assertTrue(compare(-2.toByte, -1.toByte) < 0)
    assertEquals(0, compare(3.toByte, 3.toByte))
  }

  @Test def should_parse_strings(): Unit = {
    def test(s: String, v: Byte): Unit = {
      assertEquals(v, JByte.parseByte(s))
      assertEquals(v, JByte.valueOf(s).byteValue())
      assertEquals(v, new JByte(s).byteValue())
    }

    test("0", 0)
    test("5", 5)
    test("127", 127)
    test("-100", -100)
  }

  @Test def should_reject_invalid_strings_when_parsing(): Unit = {
    def test(s: String): Unit =
      expectThrows(classOf[NumberFormatException], JByte.parseByte(s))

    test("abc")
    test("")
    test("200") // out of range
  }

  @Test def should_decode_byte_strings(): Unit = {
    def check(v: String, expected: Byte): Unit ={
      assertEquals(expected, JByte.decode(v))
    }

    check(s"${JByte.MIN_VALUE}", JByte.MIN_VALUE)
    check(s"${JByte.MAX_VALUE}", JByte.MAX_VALUE)

    check("10", 10.toByte)
    check("0x10", 16.toByte)
    check("0X10", 16.toByte)
    check("010", 8.toByte)
    check("#10", 16.toByte)

    check("+10", 10.toByte)
    check("+0x10", 16.toByte)
    check("+0X10", 16.toByte)
    check("+010", 8.toByte)
    check("+#10", 16.toByte)

    check("-10", -10.toByte)
    check("-0x10", -16.toByte)
    check("-0X10", -16.toByte)
    check("-010", -8.toByte)
    check("-#10", -16.toByte)

    check(Integer.toString(JByte.MIN_VALUE.toInt), JByte.MIN_VALUE)
    check(Integer.toString(JByte.MAX_VALUE.toInt), JByte.MAX_VALUE)
  }

  @Test def should_reject_invalid_byte_strings_when_decoding(): Unit = {
    def checkFailure(v: String): Unit = {
      assertThrows(classOf[NumberFormatException], JByte.decode(v))
    }

    checkFailure("0x-10")
    checkFailure("0x+10")
    checkFailure("+")
    checkFailure("-")
    checkFailure(Integer.toString(JByte.MIN_VALUE.toInt - 1))
    checkFailure(Integer.toString(JByte.MAX_VALUE.toInt + 1))
    checkFailure("")
  }
}
