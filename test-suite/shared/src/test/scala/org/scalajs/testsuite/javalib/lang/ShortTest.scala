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

import java.lang.{Short => JShort}

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._

/** Tests the implementation of the java standard library Short
 */
class ShortTest {

  @Test def compareTo(): Unit = {
    def compare(x: Short, y: Short): Int =
      new JShort(x).compareTo(new JShort(y))

    assertTrue(compare(0.toShort, 5.toShort) < 0)
    assertTrue(compare(10.toShort, 9.toShort) > 0)
    assertTrue(compare(-2.toShort, -1.toShort) < 0)
    assertEquals(0, compare(3.toShort, 3.toShort))
  }

  @Test def should_be_a_Comparable(): Unit = {
    def compare(x: Any, y: Any): Int =
      x.asInstanceOf[Comparable[Any]].compareTo(y)

    assertTrue(compare(0.toShort, 5.toShort) < 0)
    assertTrue(compare(10.toShort, 9.toShort) > 0)
    assertTrue(compare(-2.toShort, -1.toShort) < 0)
    assertEquals(0, compare(3.toShort, 3.toShort))
  }

  @Test def should_parse_strings(): Unit = {
    def test(s: String, v: Short): Unit = {
      assertEquals(v, JShort.parseShort(s))
      assertEquals(v, JShort.valueOf(s).shortValue())
      assertEquals(v, new JShort(s).shortValue())
      assertEquals(v, JShort.decode(s))
    }

    test("0", 0)
    test("5", 5)
    test("127", 127)
    test("-100", -100)
    test("30000", 30000)
  }

  @Test def should_reject_invalid_strings_when_parsing(): Unit = {
    def test(s: String): Unit = {
      expectThrows(classOf[NumberFormatException], JShort.parseShort(s))
      expectThrows(classOf[NumberFormatException], JShort.decode(s))
    }

    test("abc")
    test("")
    test("60000") // out of range
    test("-90000") // out of range
  }

  @Test def should_parse_strings_in_base_16(): Unit = {
    def test(s: String, v: Short): Unit = {
      assertEquals(v, JShort.parseShort(s, 16))
      assertEquals(v, JShort.valueOf(s, 16).intValue())
      assertEquals(v, JShort.decode(IntegerTest.insertAfterSign("0x", s)))
      assertEquals(v, JShort.decode(IntegerTest.insertAfterSign("0X", s)))
      assertEquals(v, JShort.decode(IntegerTest.insertAfterSign("#", s)))
    }

    test("0", 0x0)
    test("5", 0x5)
    test("ff", 0xff)
    test("-24", -0x24)
    test("3000", 0x3000)
    test("-900", -0x900)
  }

  @Test def testDecodeBase8(): Unit = {
    def test(s: String, v: Short): Unit = {
      assertEquals(v, JShort.decode(s))
    }

    test("00", 0)
    test("0123", 83)
    test("-012", -10)
  }

  @Test def testDecodeInvalid(): Unit = {
    def test(s: String): Unit =
      assertThrows(classOf[NumberFormatException], JShort.decode(s))

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
    test("0x8000")
    test("-0x8001")
    test("0100000")
    test("-0100001")
  }
}
