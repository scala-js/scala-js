/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import java.util.Date

import org.junit.Assert._
import org.junit.Test

/**
  * tests the implementation of the java standard library Date
  */
class DateTest {

  @Test def compareTo(): Unit = {
    def compare(x: Date, y: Date): Int = {
      x.compareTo(y)
    }

    assertTrue(compare(new Date(97, 11, 5, 0, 0), new Date(98, 11, 5, 0, 0)) < 0)
    assertTrue(compare(new Date(98, 11, 5, 0, 0), new Date(97, 11, 5, 0, 0)) > 0)
    assertEquals(0, compare(new Date(97, 11, 5, 0, 0), new Date(97, 11, 5)))
    assertTrue(compare(new Date(97, 11, 5, 0, 0), new Date(97, 11, 5, 0, 1)) < 0)
    assertEquals(0, compare(new Date(97, 11, 5), new Date(97, 11, 5, 0, 0)))
  }

  @Test def comparable(): Unit = {
    def compare(x: Any, y: Any): Int =
      x.asInstanceOf[Comparable[Any]].compareTo(y)

    assertTrue(compare(new Date(97, 11, 5, 0, 0), new Date(98, 11, 5, 0, 0)) < 0)
    assertTrue(compare(new Date(98, 11, 5, 0, 0), new Date(97, 11, 5, 0, 0)) > 0)
    assertEquals(0, compare(new Date(97, 11, 5, 0, 0), new Date(97, 11, 5)))
    assertTrue(compare(new Date(97, 11, 5, 0, 0), new Date(97, 11, 5, 0, 1)) < 0)
    assertEquals(0, compare(new Date(97, 11, 5), new Date(97, 11, 5, 0, 0)))
  }

  @Test def parseStrings(): Unit = {
    def test(s: String, v: Date): Unit =
      assertEquals(0, new Date(s).compareTo(v))

    test("Nov 5 1997 5:23:27 GMT", new Date(Date.UTC(97, 10, 5, 5, 23, 27)))
    test("Nov 1 1997 GMT", new Date(Date.UTC(97,10,1, 0, 0, 0)))
    test("Jan 1 1970 18:11:01 GMT", new Date(Date.UTC(70,0,1,18,11,1)))
  }

  @Test def after(): Unit = {
    assertFalse(new Date(97, 11, 5, 0, 0).after(new Date(98, 11, 5, 0, 0)))
    assertTrue(new Date(99, 11, 5, 0, 0).after(new Date(98, 11, 5, 0, 0)))
    assertFalse(new Date(99, 11, 5, 0, 0).after(new Date(99, 11, 5, 0, 0)))
  }

  @Test def before(): Unit = {
    assertTrue(new Date(97, 11, 5, 0, 0).before(new Date(98, 11, 5, 0, 0)))
    assertFalse(new Date(99, 11, 5, 0, 0).before(new Date(98, 11, 5, 0, 0)))
    assertFalse(new Date(99, 11, 5, 0, 0).before(new Date(99, 11, 5, 0, 0)))
  }

  @Test def cloneTest(): Unit = {
    def testClone(date: Date): Boolean = {
      val cloned = date.clone()
      date == cloned
    }

    assertTrue(testClone(new Date(97, 11, 5, 0, 0)))
    assertTrue(testClone(new Date(92, 14, 6, 2, 1)))
    assertTrue(testClone(new Date(4, 1, 2, 3, 0, 0)))
  }

  @Test def getYear(): Unit = {
    def testYear(year: Int): Unit = {
      val date = new Date()
      date.setYear(year)
      assertEquals(year, date.getYear)
    }
    testYear(1940)
    testYear(1920)
    testYear(2030)
  }

  // #2392
  @Test def getTimezoneOffset(): Unit = {
    new Date().getTimezoneOffset // Test that it links.
  }
}
