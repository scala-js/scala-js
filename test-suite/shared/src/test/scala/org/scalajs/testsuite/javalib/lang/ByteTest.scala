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

import org.scalajs.testsuite.utils.AssertThrows.assertThrows

/** Tests the implementation of the java standard library Byte
 */
class ByteTest {

  @Test def compareToJavaByte(): Unit = {
    def compare(x: Byte, y: Byte): Int =
      new JByte(x).compareTo(new JByte(y))

    assertTrue(compare(0.toByte, 5.toByte) < 0)
    assertTrue(compare(10.toByte, 9.toByte) > 0)
    assertTrue(compare(-2.toByte, -1.toByte) < 0)
    assertEquals(0, compare(3.toByte, 3.toByte))
  }

  @Test def compareTo(): Unit = {
    def compare(x: Any, y: Any): Int =
      x.asInstanceOf[Comparable[Any]].compareTo(y)

    assertTrue(compare(0.toByte, 5.toByte) < 0)
    assertTrue(compare(10.toByte, 9.toByte) > 0)
    assertTrue(compare(-2.toByte, -1.toByte) < 0)
    assertEquals(0, compare(3.toByte, 3.toByte))
  }

  @Test def parseString(): Unit = {
    def test(s: String, v: Byte): Unit = {
      assertEquals(v, JByte.parseByte(s))
      assertEquals(v, JByte.valueOf(s).byteValue())
      assertEquals(v, new JByte(s).byteValue())
      assertEquals(v, JByte.decode(s))
    }

    test("0", 0)
    test("5", 5)
    test("127", 127)
    test("-100", -100)
  }

  @Test def parseStringInvalidThrows(): Unit = {
    def test(s: String): Unit = {
      assertThrows(classOf[NumberFormatException], JByte.parseByte(s))
      assertThrows(classOf[NumberFormatException], JByte.decode(s))
    }

    test("abc")
    test("")
    test("200") // out of range
  }

  @Test def parseStringBase16(): Unit = {
    def test(s: String, v: Byte): Unit = {
      assertEquals(v, JByte.parseByte(s, 16))
      assertEquals(v, JByte.valueOf(s, 16).intValue())
      assertEquals(v, JByte.decode(IntegerTest.insertAfterSign("0x", s)))
      assertEquals(v, JByte.decode(IntegerTest.insertAfterSign("0X", s)))
      assertEquals(v, JByte.decode(IntegerTest.insertAfterSign("#", s)))
    }

    test("0", 0x0)
    test("5", 0x5)
    test("7f", 0x7f)
    test("-24", -0x24)
    test("30", 0x30)
    test("-9", -0x9)
  }

  @Test def decodeStringBase8(): Unit = {
    def test(s: String, v: Byte): Unit = {
      assertEquals(v, JByte.decode(s))
    }

    test("00", 0)
    test("0123", 83)
    test("-012", -10)
  }

  @Test def decodeInvalidThrows(): Unit = {
    def test(s: String): Unit =
      assertThrows(classOf[NumberFormatException], JByte.decode(s))

    // sign after another sign or after a base prefix
    test("++0")
    test("--0")
    test("0x+1")
    test("0X-1")
    test("#-1")
    test("0-1")

    // empty string after sign or after base prefix
    test("")
    test("+")
    test("-")
    test("-0x")
    test("+0X")
    test("#")

    // integer too large
    test("0x80")
    test("-0x81")
    test("0200")
    test("-0201")
  }
}
