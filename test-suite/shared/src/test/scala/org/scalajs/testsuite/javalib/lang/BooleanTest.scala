/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.lang

import java.lang.{Boolean => JBoolean}

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._

/** Tests the implementation of the java standard library Boolean
 */
class BooleanTest {

  @Test def booleanValue(): Unit = {
    assertEquals(true, JBoolean.TRUE.booleanValue())
    assertEquals(false, JBoolean.FALSE.booleanValue())
    expectThrows(classOf[Exception], (null: JBoolean).booleanValue())
  }

  @Test def compareTo(): Unit = {
    def compare(x: Boolean, y: Boolean): Int =
      new JBoolean(x).compareTo(new JBoolean(y))

    assertEquals(0, compare(false, false))
    assertTrue(compare(false, true) < 0)
    assertTrue(compare(true, false) > 0)
    assertEquals(0, compare(true, true))
  }

  @Test def should_be_a_Comparable(): Unit = {
    def compare(x: Any, y: Any): Int =
      x.asInstanceOf[Comparable[Any]].compareTo(y)

    assertEquals(0, compare(false, false))
    assertTrue(compare(false, true) < 0)
    assertTrue(compare(true, false) > 0)
    assertEquals(0, compare(true, true))
  }

  @Test def should_parse_strings(): Unit = {
    def test(s: String, v: Boolean): Unit = {
      assertEquals(v, JBoolean.parseBoolean(s))
      assertEquals(v, JBoolean.valueOf(s).booleanValue())
      assertEquals(v, new JBoolean(s).booleanValue())
    }

    test("false", false)
    test("true", true)
    test("TrUe", true)
    test(null, false)
    test("truee", false)
  }
}
