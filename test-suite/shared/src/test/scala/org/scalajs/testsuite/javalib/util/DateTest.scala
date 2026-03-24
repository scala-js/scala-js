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
import org.scalajs.testsuite.utils.Platform._

/** tests the implementation of the java standard library Date */
class DateTest {
  private final val MSPerHour = 60L * 60L * 1000L
  private final val MSPerDay = 24L * MSPerHour

  @Test def fromUTC(): Unit = {
    assertEquals(-2209075200000L, Date.UTC(1899 - 1900, 11, 31, 0, 0, 0))
    assertEquals(878534607000L, Date.UTC(97, 10, 3, 5, 23, 27))
    assertEquals(-2145542331000L, Date.UTC(2, 0, 5, 8, 1, 9))
    assertEquals(29348715784000L, Date.UTC(2900 - 1900, 0, 9, 5, 3, 4))

    // First Gregorian second and last Julian second
    assertEquals(-12219292800000L, Date.UTC(1582 - 1900, 9, 15, 0, 0, 0))
    assertEquals(-12219292801000L, Date.UTC(1582 - 1900, 9, 4, 23, 59, 59))

    // The last second interpreted as Julian (but wrapping into the Gregorian calendar)
    assertEquals(-12218428801000L, Date.UTC(1582 - 1900, 9, 14, 23, 59, 59))

    assertEquals(94668649689600000L, Date.UTC(3000000, 1, 1, 0, 0, 0))
    assertEquals(-94675005273600000L, Date.UTC(-3000000, 1, 1, 0, 0, 0))
  }

  @Test def fromLocalFields(): Unit = {
    // The locale timezone is always GMT
    assertEquals(-2209075200000L, new Date(1899 - 1900, 11, 31).getTime())
    assertEquals(878534580000L, new Date(97, 10, 3, 5, 23).getTime())
    assertEquals(-2145542331000L, new Date(2, 0, 5, 8, 1, 9).getTime())
    assertEquals(29348715784000L, new Date(2900 - 1900, 0, 9, 5, 3, 4).getTime())

    assertEquals(94668649689600000L, new Date(3000000, 1, 1).getTime())
    assertEquals(-94675005273600000L, new Date(-3000000, 1, 1).getTime())
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

    // Explicit time zone offsets
    test(878689407000L, "Nov 5 1997 5:23:27 +5")
    test(878414400000L, "Nov 1 1997 utc-20")
    test(-11939000L, "Jan 1 1970 18:11:01 +2130")
    test(878711907000L, "Nov 5 1997 (ig(no)ré) 5:23:27 GMT-0115")

    /* If there are several time zones, the last one wins
     * (which is why GMT-0115 works without dedicated code paths, btw).
     */
    test(878711907000L, "Nov 5 1997 GMT 5:23:27 -0115")
    test(878725407000L, "Nov 5 1997 +0 GMT -0115 EST 5:23:27")
    test(878725407000L, "Nov 5 1997 +0 GMT EST 5:23:27")
    test(878707407000L, "Nov 5 1997 +0 -0115 EST GMT 5:23:27")
    test(878689407000L, "Nov 5 1997 +0 EST GMT 5:23:27 +5")

    // But a time zone offset is rejected if the last time zone so far is non-UTC (not specified)
    testFailure("Nov 5 1997 +0 -0115 +5 5:23:27")
    testFailure("Nov 5 1997 +0 EST -0115 GMT 5:23:27")

    // Some other formats
    test(878707407000L, "5 Nov 1997 5:23:27 GMT")
    test(878707407000L, "11/5/1997 5:23:27 GMT")
    test(878707407000L, "Nov 5 Thu 1997 5:  23:  27 GMT") // It was a Wednesday, actually
    test(878707407000L, "Nov 5 1997 5:23:27 GMT")

    // Other separators
    test(878707407000L, "5\tNov\n1997 5:23:27 GMT")
    test(878707407000L, "1997, 5\rNov 5:23:27 GMT")
    test(878707407000L, "5\tNov\n1997 5:23,27 GMT")

    // Various prefixes
    test(878707407000L, "No 5 1997 5:23:27 GMT")
    test(876029007000L, "October 5 1997 sunday 5:23:27 GMT")
    test(876029007000L, "Oct5 1997 sunday5:23:27 GMT")

    // 1-letter prefix is not allowed (out of spec)
    testFailure("J 5 1997 5:23:27 GMT")

    /* The JavaDoc specifically calls out
     * > So is Ma, which is recognized as MARCH, not MAY.
     * but the JDK answers May ...
     */
    if (executingInJVM)
      test(862809807000L, "ma 5 1997 5:23:27 GMT") // oops, that's May
    else
      test(857539407000L, "ma 5 1997 5:23:27 GMT") // March, not May

    // We can omit time components
    test(878707380000L, "Nov 5 1997 5:23 GMT")
    test(878706000000L, "Nov 5 1997 5: GMT")
    test(878688000000L, "Nov 5 1997 GMT")

    // But not date components
    testFailure("Nov 5 GMT") // no year
    testFailure("5 1997 5:23:27 GMT") // no month
    testFailure("10/ 1997 5:23:27 GMT") // no day

    /* Also apparently the JVM rejects the following, but I don't know why.
     * For us, the '5:' gets interpreted as a year. If we disallow that
     * interpretation because it's followed by ':', then '27' gets interpreted
     * as a year anyway.
     */
    if (executingInJVM)
      testFailure("Nov 5 5:23:27 GMT")
    else
      test(1131233220000L, "Nov 5 5:23:27 GMT") // parsed as 5 Nov 2005 23:27:00

    // Year smaller than 100, time-dependent
    // These tests will start failing in 2078 (since 2078 - 80 > 1997)
    test(878707407000L, "5 Nov 97 5:23:27 GMT") // 1997
    test(1194240207000L, "noVemb 5/7 5:23:27 GMT") // 2007
    test(2298777807000L, "11/5/42 5:23:27 GMT") // 2042

    // Implicit time zone -> always GMT for us
    test(878707407000L, "Nov 5 1997 5:23:27")
    test(878342400000L, "Nov 1 1997")
    test(65461000L, "Jan 1 1970 18:11:01")

    // Some time zone abbreviations are explicitly supported (mix of cases)
    val supportedTimeZones = List(
      "GMT" -> 0,
      "UT" -> 0,
      "utc" -> 0,
      "Est" -> -5,
      "cSt" -> -6,
      "MsT" -> -7,
      "pst" -> -8,
      "eDT" -> -4,
      "CDt" -> -5,
      "MdT" -> -6,
      "PDT" -> -7
    )
    for ((tzName, offset) <- supportedTimeZones) {
      test(1774089000000L - offset * MSPerHour, s"Mar 21 2026 10:30 $tzName")
    }

    // But other abbreviations are not
    for (tzName <- List("CET", "JST", "cest"))
      testFailure(s"Mar 21 2026 10:30 $tzName")

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

  @Test def setYear(): Unit = {
    val date = new Date(0L)
    date.setYear(300000)
    assertEquals(9464876611200000L, date.getTime())
    date.setYear(-300000)
    assertEquals(-9469487952000000L, date.getTime())
  }

  @Test def getDate(): Unit = {
    assertEquals(13, new Date(8640000000000001L).getDate())
    assertEquals(19, new Date(-8640000000000001L).getDate())
  }

  // #2392
  @Test def getTimezoneOffset(): Unit =
    assertEquals(0, new Date().getTimezoneOffset())

  @Test def toStringTest(): Unit = {
    assertEquals("Mon Nov 03 05:23:27 GMT 1997", new Date(878534607567L).toString())
    assertEquals("Sun Dec 31 00:00:00 GMT 1899", new Date(-2209075200000L).toString())
    assertEquals("Sun Jan 05 08:01:09 GMT 1902", new Date(-2145542330500L).toString())
    assertEquals("Sat Jan 09 05:03:04 GMT 2900", new Date(29348715784999L).toString())

    assertEquals("Sat Sep 13 00:00:00 GMT 275760", new Date(8640000000000001L).toString())
    assertEquals("Sun Aug 17 07:12:55 GMT 292278994", new Date(Long.MaxValue).toString())
    assertEquals("Sun Dec 02 16:47:04 GMT 292269055", new Date(Long.MinValue).toString())
  }

  @Test def toGMTString(): Unit = {
    assertEquals("3 Nov 1997 05:23:27 GMT", new Date(878534607567L).toGMTString())
    assertEquals("31 Dec 1899 00:00:00 GMT", new Date(-2209075200000L).toGMTString())
    assertEquals("5 Jan 1902 08:01:09 GMT", new Date(-2145542330500L).toGMTString())
    assertEquals("9 Jan 2900 05:03:04 GMT", new Date(29348715784999L).toGMTString())

    assertEquals("13 Sep 275760 00:00:00 GMT", new Date(8640000000000001L).toGMTString())
    assertEquals("17 Aug 292278994 07:12:55 GMT", new Date(Long.MaxValue).toGMTString())
    assertEquals("2 Dec 292269055 16:47:04 GMT", new Date(Long.MinValue).toGMTString())
  }

  @Test def toLocaleString(): Unit = {
    /* On JDK 8, the results are of the form "Nov 3, 1997 5:23:27 AM". That
     * corresponds to the pattern of a DateFormat string obtained with
     *   DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.MEDIUM, Locale.ENGLISH)
     *
     * On JDK 11+, and in our implementation, we get the format below, which
     * corresponds to the pattern "y MMM d HH:mm:ss", which we get with
     *   DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.MEDIUM, Locale.ROOT)
     */
    assumeFalse(
        "JDK 8 did not abide by the default Locale rules yet",
        executingInJVMOnLowerThanJDK(11))

    assertEquals("1997 Nov 3 05:23:27", new Date(878534607567L).toLocaleString())
    assertEquals("1899 Dec 31 00:00:00", new Date(-2209075200000L).toLocaleString())
    assertEquals("1902 Jan 5 08:01:09", new Date(-2145542330500L).toLocaleString())
    assertEquals("2900 Jan 9 05:03:04", new Date(29348715784999L).toLocaleString())

    assertEquals("275760 Sep 13 00:00:00", new Date(8640000000000001L).toLocaleString())
    assertEquals("292278994 Aug 17 07:12:55", new Date(Long.MaxValue).toLocaleString())
    assertEquals("292269055 Dec 2 16:47:04", new Date(Long.MinValue).toLocaleString())
  }

  // #4131
  @Test def largeValues(): Unit = {
    val hi = new Date(8640000000000001L)
    assertEquals(8640000000000001L, hi.getTime())

    val lo = new Date(-8640000000000001L)
    assertEquals(-8640000000000001L, lo.getTime())
  }
}
