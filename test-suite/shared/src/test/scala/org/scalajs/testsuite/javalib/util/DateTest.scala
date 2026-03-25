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

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform.executingInJVM

/** tests the implementation of the java standard library Date */
class DateTest {
  private final val MSPerDay = 24 * 60 * 60 * 1000

  @Test def fromUTC(): Unit = {
    assertEquals(-2209075200000L, Date.UTC(1899 - 1900, 11, 31, 0, 0, 0))
    assertEquals(878534607000L, Date.UTC(97, 10, 3, 5, 23, 27))
    assertEquals(-2145542331000L, Date.UTC(2, 0, 5, 8, 1, 9))
    assertEquals(29348715784000L, Date.UTC(2900 - 1900, 0, 9, 5, 3, 4))
  }

  @Test def fromLocalFields(): Unit = {
    def test(expectedUTC: Long, date: Date): Unit = {
      // Whatever the local timezone, we should not be more than 1 day away from UTC
      assertTrue(Math.abs(date.getTime() - expectedUTC) <= MSPerDay)
    }

    test(-2209075200000L, new Date(1899 - 1900, 11, 31))
    test(878534580000L, new Date(97, 10, 3, 5, 23))
    test(-2145542331000L, new Date(2, 0, 5, 8, 1, 9))
    test(29348715784000L, new Date(2900 - 1900, 0, 9, 5, 3, 4))
  }

  @Test def comparisons(): Unit = {
    def assertCompare(expectedSign: Int, tx: Long, ty: Long): Unit = {
      val x = new Date(tx)
      val y = new Date(ty)
      assertEquals(expectedSign, Integer.signum(x.compareTo(y)))

      // also check that ju.Date implements Comparable
      val cx: Comparable[Date] = x
      assertEquals(expectedSign, Integer.signum(cx.compareTo(y)))

      assertEquals(expectedSign > 0, x.after(y))
      assertEquals(expectedSign < 0, x.before(y))
    }

    assertCompare(-1, 881280000000L, 912816000000L)
    assertCompare(1, 912816000000L, 881280000000L)
    assertCompare(0, 881280000000L, 881280000000L)
    assertCompare(-1, -912816000000L, 881280000000L)
    assertCompare(1, 881280000000L, -912816000000L)
    assertCompare(-1, Long.MinValue, 0L)
    assertCompare(1, Long.MaxValue, 0L)
  }

  @Test def parseStrings(): Unit = {
    def test(expected: Long, s: String): Unit = {
      assertEquals(s, expected, new Date(s).getTime())
      assertEquals(s, expected, Date.parse(s))
    }

    def testFailure(s: String): Unit = {
      assertThrows(classOf[IllegalArgumentException], new Date(s))
      assertThrows(classOf[IllegalArgumentException], Date.parse(s))
    }

    // Explicit GMT time zone
    test(878707407000L, "Nov 5 1997 5:23:27 GMT")
    test(878342400000L, "Nov 1 1997 GMT")
    test(65461000L, "Jan 1 1970 18:11:01 GMT")
    test(878707407000L, "Nov 5 1997 (ig(no)ré) 5:23:27 GMT")

    // Implicit time zone -> local time zone
    test(new Date(97, 10, 5, 5, 23, 27).getTime(), "Nov 5 1997 5:23:27")
    test(new Date(97, 10, 1).getTime(), "Nov 1 1997")
    test(new Date(70, 0, 1, 18, 11, 1).getTime(), "Jan 1 1970 18:11:01")

    testFailure("not a date")
  }

  @Test def cloneTest(): Unit = {
    def testClone(time: Long): Unit = {
      val date = new Date(time)
      val cloned = date.clone()
      assertNotSame(date, cloned)
      assertEquals(date, cloned)
    }

    testClone(881276400000L)
    testClone(731379660000L)
    testClone(-2080072800000L)
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
  @Test def getTimezoneOffset(): Unit =
    new Date().getTimezoneOffset // Test that it links.

  @Test def toStringTest(): Unit = {
    def test(expectedRegex: String, actual: String): Unit =
      assertTrue(s"expected:<$expectedRegex> to match:<$actual>", actual.matches(expectedRegex))
    test("Mon Nov 03 05:23:27 .+ 1997", new Date(97, 10, 3, 5, 23, 27).toString)
    test("Sun Dec 31 00:00:00 .+ 1899", new Date(0, 0, 0, 0, 0, 0).toString)
    test("Sun Jan 05 08:01:09 .+ 1902", new Date(1, 12, 5, 8, 1, 9).toString)
    test("Sat Jan 09 05:03:04 .+ 2900", new Date(1000, 0, 9, 5, 3, 4).toString)
  }

  @Test def toGMTString(): Unit = {
    assertEquals("3 Nov 1997 05:23:27 GMT", new Date(878534607567L).toGMTString())
    assertEquals("31 Dec 1899 00:00:00 GMT", new Date(-2209075200000L).toGMTString())
    assertEquals("5 Jan 1902 08:01:09 GMT", new Date(-2145542330500L).toGMTString())
    assertEquals("9 Jan 2900 05:03:04 GMT", new Date(29348715784999L).toGMTString())
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
