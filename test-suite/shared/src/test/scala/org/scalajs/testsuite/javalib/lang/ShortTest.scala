/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
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
    }

    test("0", 0)
    test("5", 5)
    test("127", 127)
    test("-100", -100)
    test("30000", 30000)
  }

  @Test def should_reject_invalid_strings_when_parsing(): Unit = {
    def test(s: String): Unit =
      expectThrows(classOf[NumberFormatException], JShort.parseShort(s))

    test("abc")
    test("")
    test("60000") // out of range
    test("-90000") // out of range
  }
}
