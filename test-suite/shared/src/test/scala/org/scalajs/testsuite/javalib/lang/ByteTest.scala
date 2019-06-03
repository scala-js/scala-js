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

  @Test def should_decode_decimal_string(): Unit = {
    assertEquals(10.toByte, JByte.decode("10"))
    assertEquals(20.toByte, JByte.decode("+20"))
    assertEquals(-30.toByte, JByte.decode("-30"))
  }

  @Test def should_decode_octal_string_with_leading_0(): Unit = {
    assertEquals(8.toByte, JByte.decode("010"))
    assertEquals(16.toByte, JByte.decode("+020"))
    assertEquals(-24.toByte, JByte.decode("-030"))
  }

  @Test def should_decode_hexadeciaml_string_with_hexadecimal_specifier(): Unit = {
    assertEquals(16.toByte, JByte.decode("0x10"))
    assertEquals(32.toByte, JByte.decode("+0x20"))
    assertEquals(-48.toByte, JByte.decode("-0X30"))

    assertEquals(16.toByte, JByte.decode("0X10"))
    assertEquals(32.toByte, JByte.decode("+0X20"))
    assertEquals(-48.toByte, JByte.decode("-0X30"))

    assertEquals(16.toByte, JByte.decode("#10"))
    assertEquals(32.toByte, JByte.decode("+#20"))
    assertEquals(-48.toByte, JByte.decode("-#30"))
  }

  @Test def should_decode_min_and_max(): Unit = {
    assertEquals(JByte.MIN_VALUE, JByte.decode(s"-0200"))
    assertEquals(JByte.MIN_VALUE, JByte.decode(s"-0x80"))
    assertEquals(JByte.MIN_VALUE, JByte.decode(s"-128"))

    assertEquals(JByte.MAX_VALUE, JByte.decode(s"0177"))
    assertEquals(JByte.MAX_VALUE, JByte.decode(s"0x7f"))
    assertEquals(JByte.MAX_VALUE, JByte.decode(s"127"))
  }

  @Test def should_reject_strings_containing_other_than_numbers_when_decoding(): Unit = {
    // underscore delimitters
    assertThrows(classOf[NumberFormatException], JByte.decode("0_1_0"))
    assertThrows(classOf[NumberFormatException], JByte.decode("0x1_0"))

    // whitespaces
    assertThrows(classOf[NumberFormatException], JByte.decode(" 010"))
    assertThrows(classOf[NumberFormatException], JByte.decode("0x10 "))

    // signs after radix specifier
    assertThrows(classOf[NumberFormatException], JByte.decode("0+10"))
    assertThrows(classOf[NumberFormatException], JByte.decode("0x-10"))
  }

  @Test def should_reject_when_decoding_out_of_range(): Unit = {
    assertThrows(classOf[NumberFormatException], JByte.decode(s"128"))
    assertThrows(classOf[NumberFormatException], JByte.decode(s"-129"))
    assertThrows(classOf[NumberFormatException], JByte.decode(s"0x80"))
    assertThrows(classOf[NumberFormatException], JByte.decode(s"-0x81"))
  }
}
