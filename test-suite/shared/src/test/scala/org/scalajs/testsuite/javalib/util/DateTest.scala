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

package org.scalajs.testsuite.javalib.util

import java.util.Date

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform.executingInJVM

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
    def test(s: String, v: Date): Unit = {
      assertEquals(0, new Date(s).compareTo(v))
      assertEquals(0, Date.parse(s).compareTo(v.getTime))
    }

    test("Nov 5 1997 5:23:27 GMT", new Date(Date.UTC(97, 10, 5, 5, 23, 27)))
    test("Nov 1 1997 GMT", new Date(Date.UTC(97, 10, 1, 0, 0, 0)))
    test("Jan 1 1970 18:11:01 GMT", new Date(Date.UTC(70, 0, 1, 18, 11, 1)))

    assertThrows(classOf[IllegalArgumentException], new Date("not a date"))
    assertThrows(classOf[IllegalArgumentException], Date.parse("not a date"))
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

  @Test def toStringTest(): Unit = {
    def test(expectedRegex: String, actual: String): Unit =
      assertTrue(s"expected:<$expectedRegex> to match:<$actual>", actual.matches(expectedRegex))
    test("Mon Nov 03 05:23:27 .+ 1997", new Date(97, 10, 3, 5, 23, 27).toString)
    test("Sun Dec 31 00:00:00 .+ 1899", new Date(0, 0, 0, 0, 0, 0).toString)
    test("Sun Jan 05 08:01:09 .+ 1902", new Date(1, 12, 5, 8, 1, 9).toString)
    test("Sat Jan 09 05:03:04 .+ 2900", new Date(1000, 0, 9, 5, 3, 4).toString)
  }

  @Test def toGMTString(): Unit = {
    assertEquals("31 Dec 1899 00:00:00 GMT", new Date(Date.UTC(0, 0, 0, 0, 0, 0)).toGMTString)
    assertEquals("3 Nov 1997 05:23:27 GMT", new Date(Date.UTC(97, 10, 3, 5, 23, 27)).toGMTString)
    assertEquals("5 Jan 1902 08:01:09 GMT", new Date(Date.UTC(1, 12, 5, 8, 1, 9)).toGMTString)
    assertEquals("9 Jan 2900 05:03:04 GMT", new Date(Date.UTC(1000, 0, 9, 5, 3, 4)).toGMTString)
  }

  // #4131
  @Test def largeValues(): Unit = {
    val hi = new Date(8640000000000001L)
    assertEquals(8640000000000001L, hi.getTime())

    val lo = new Date(-8640000000000001L)
    assertEquals(-8640000000000001L, lo.getTime())
  }

  @Test def largeToString(): Unit = {
    assumeFalse(executingInJVM)
    assertEquals("java.util.Date(8640000000000001)", new Date(8640000000000001L).toString())
  }

  @Test def preventsUnsafeRead(): Unit = {
    assumeFalse(executingInJVM)
    assertThrows(classOf[IllegalArgumentException], new Date(8640000000000001L).getDate())
    assertThrows(classOf[IllegalArgumentException], new Date(-8640000000000001L).getDate())
  }

  @Test def preventsUnsafeWrite(): Unit = {
    assumeFalse(executingInJVM)
    val date = new Date(0L)
    assertThrows(classOf[IllegalArgumentException], date.setYear(300000))
    assertEquals(0L, date.getTime())
    assertThrows(classOf[IllegalArgumentException], date.setYear(-300000))
  }

  @Test def preventsUnsafeConstruct(): Unit = {
    assumeFalse(executingInJVM)
    assertThrows(classOf[IllegalArgumentException], new Date(3000000, 1, 1))
  }
}
