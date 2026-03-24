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

    /* To reject this, we must not interpret a number as a year if it follows
     * or precedes a ':'. It makes sense, but it's out of spec.
     */
    testFailure("Nov 5 5:23:27 GMT")

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

    // First Gregorian second
    test(-12219292800000L, "15 Oct 1582")

    // For some reason, the JVM gets these wrong
    if (!executingInJVM) {
      // Last Julian second
      test(-12219292801000L, "4 Oct 1582 23:59:59")

      // The last second interpreted as Julian (but wrapping into the Gregorian calendar)
      test(-12218428801000L, "14 Oct 1582 23:59:59")
    }

    // The earliest and latest dates we can parse
    test(-58980009600000L, "1 Jan 101 0:0:0") // but the JVM gets this one right!
    test(9223372036854775000L, "Aug 17 07:12:55 GMT 292278994")

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

  @Test def components(): Unit = {
    def testWith(d: Date, year: Int, month: Int, date: Int, day: Int,
        hours: Int, minutes: Int, seconds: Int): Unit = {
      val timeStr = d.getTime().toString()
      assertEquals(timeStr, year, d.getYear())
      assertEquals(timeStr, month, d.getMonth())
      assertEquals(timeStr, date, d.getDate())
      assertEquals(timeStr, day, d.getDay())
      assertEquals(timeStr, hours, d.getHours())
      assertEquals(timeStr, minutes, d.getMinutes())
      assertEquals(timeStr, seconds, d.getSeconds())
    }

    def test(time: Long, year: Int, month: Int, date: Int, day: Int,
        hours: Int, minutes: Int, seconds: Int): Unit = {
      testWith(new Date(time), year, month, date, day, hours, minutes, seconds)
    }

    // Some points of interest
    test(Long.MaxValue, 292277094, 7, 17, 0, 7, 12, 55)
    test(0L, 70, 0, 1, 4, 0, 0, 0)
    test(-1L, 69, 11, 31, 3, 23, 59, 59)
    test(-12219292800000L, -318, 9, 15, 5, 0, 0, 0) // first Gregorian
    test(-12219292800001L, -318, 9, 4, 4, 23, 59, 59) // last Julian
    test(Long.MinValue, 292267155, 11, 2, 0, 16, 47, 4)

    // Random values
    test(99340127223205L, 3217, 11, 18, 2, 23, 47, 3)
    test(89733015185664L, 2913, 6, 11, 4, 13, 33, 5)
    test(81953270636458L, 2666, 11, 30, 2, 5, 23, 56)
    test(69268172576093L, 2265, 0, 7, 1, 23, 2, 56)
    test(58819923690243L, 1933, 11, 6, 5, 3, 41, 30)
    test(58767886310098L, 1932, 3, 12, 4, 20, 51, 50)
    test(52888209773058L, 1745, 11, 17, 0, 1, 22, 53)
    test(44645843127687L, 1484, 9, 8, 5, 7, 5, 27)
    test(33258918856225L, 1123, 11, 8, 1, 4, 34, 16)
    test(32270012544164L, 1092, 7, 5, 0, 12, 22, 24)
    test(26786570947801L, 918, 9, 31, 3, 18, 9, 7)
    test(18311220369554L, 650, 3, 5, 0, 10, 6, 9)
    test(13317439658811L, 492, 0, 6, 1, 0, 47, 38)
    test(12566402703667L, 468, 2, 19, 2, 11, 25, 3)
    test(5782592879274L, 253, 2, 30, 5, 3, 47, 59)
    test(2647264422444L, 153, 10, 20, 4, 15, 13, 42)
    test(1438185308498L, 115, 6, 29, 3, 15, 55, 8)
    test(1428035218076L, 115, 3, 3, 5, 4, 26, 58)
    test(-493700455846L, 54, 4, 10, 1, 20, 59, 4)
    test(-29183007698061L, -855, 2, 18, 1, 16, 58, 21)
    test(-38639617496656L, -1155, 6, 20, 2, 4, 55, 3)
    test(-54045640857406L, -1643, 4, 12, 2, 17, 59, 2)
    test(-54842186819106L, -1668, 1, 14, 2, 11, 13, 0)
    test(-61571195674902L, -1882, 10, 22, 2, 10, 5, 25)
    // And now years go back up towards 0 because we enter the BC era -_-'
    test(-71905403644441L, -1590, 5, 3, 2, 12, 45, 55)
    test(-81499512678878L, -1286, 4, 27, 0, 14, 28, 41)
    test(-82456568363058L, -1256, 0, 27, 4, 13, 40, 36)
    test(-97594691897105L, -776, 4, 17, 4, 19, 21, 42)
    test(-97897314087185L, -766, 9, 15, 2, 5, 38, 32)
    test(-99303558586855L, -722, 2, 24, 1, 6, 10, 13)

    // setTime() invalidates the cache
    locally {
      val d = new Date(1428035218076L)
      testWith(d, 115, 3, 3, 5, 4, 26, 58) // fill cache
      d.setTime(-493700455846L) // must invalidate the cache
      testWith(d, 54, 4, 10, 1, 20, 59, 4)
    }
  }

  @noinline
  private def testSetter(expected: Long, initial: Long, newValue: Int)(
      setter: (Date, Int) => Unit): Unit = {
    val date = new Date(initial)
    setter(date, newValue)
    assertEquals(s"$initial set $newValue", expected, date.getTime())
  }

  @Test def setYear(): Unit = {
    def test(expected: Long, initial: Long, newValue: Int): Unit =
      testSetter(expected, initial, newValue)(_.setYear(_))

    test(79393491344194L, -287871855806L, 2585)
    test(-71406625383548L, 89218664216452L, -2193)
    test(35441711269763L, -98707853530237L, 1193)
    test(9906084044406L, -97924917555594L, 383)
    test(1947573953806L, 57266988353806L, 131)
    test(-48189753682787L, -61443945682787L, -1458)
    test(-52998987023648L, 93393716976352L, -1610)
    test(7860408493787L, 25406088493787L, 319)
    test(-65195993178101L, 68290192421899L, -1996)
    test(-52794226021896L, -84162523621896L, -1603)
    test(51078631833231L, 21825319833231L, 1688)
    test(3261020682076L, 99257123082076L, 173)
    test(87110478926282L, -80331165873718L, 2830)
    test(59844827207474L, 83007630407474L, 1966)
    test(-77635564802517L, -15340905602517L, -2391)
    test(57139824642608L, -2913100157392L, 1880)
    test(14117102751370L, 42202718751370L, 517)
    test(-8645510841901L, -63649478841901L, -204)
    test(-68921504024418L, -37553292824418L, -2115)
    test(-94286356289795L, -96684733889795L, -2918)
  }

  @Test def setMonth(): Unit = {
    def test(expected: Long, initial: Long, newValue: Int): Unit =
      testSetter(expected, initial, newValue)(_.setMonth(_))

    test(-55264618814483L, -55262026814483L, 8)
    test(12801226961434L, 12843390161434L, -5)
    test(77271532574308L, 77295119774308L, -5)
    test(-47407443472430L, -47373229072430L, -4)
    test(-78642447981854L, -78574019181854L, -25)
    test(-28882615794838L, -28911559794838L, 20)
    test(27109122857966L, 27106444457966L, 12)
    test(-52921512167646L, -52924104167646L, 11)
    test(59681429792935L, 59697327392935L, 2)
    test(2753535846952L, 2769347046952L, 3)
    test(-12483333096351L, -12478062696351L, 4)
    test(89599509291124L, 89610050091124L, 3)
    test(54917918705237L, 54899601905237L, 15)
    test(86722649571768L, 86793497571768L, -23)
    test(-39359000174305L, -39369540974305L, 9)
    test(726317499385L, 778848699385L, -12)
    test(-44555908258027L, -44529642658027L, 0)
    test(26905338123988L, 26902659723988L, 7)
    test(-98562628815097L, -98565307215097L, 8)
    test(70284850111486L, 70300747711486L, 2)
  }

  @Test def setDate(): Unit = {
    def test(expected: Long, initial: Long, newValue: Int): Unit =
      testSetter(expected, initial, newValue)(_.setDate(_))

    test(76532783701118L, 76531228501118L, 25)
    test(20864165250209L, 20863646850209L, 28)
    test(-62090405034284L, -62090232234284L, 10)
    test(-92290969792197L, -92291488192197L, 10)
    test(-85934580157880L, -85935271357880L, 11)
    test(-26926769355269L, -26926510155269L, 15)
    test(13360796533449L, 13361574133449L, 21)
    test(-46986025694074L, -46987840094074L, 25)
    test(-5134623771979L, -5134710171979L, 17)
    test(4873941073602L, 4874977873602L, 13)
    test(26636716477745L, 26646911677745L, -89)
    test(45810597092289L, 45815176292289L, -24)
    test(78877590627373L, 78887094627373L, -79)
    test(-4926381742802L, -4920506542802L, -40)
    test(95725658455542L, 95728596055542L, -25)
    test(-80176623787574L, -80171094187574L, -34)
    test(-91502392537399L, -91497381337399L, -55)
    test(-2679673120112L, -2681141920112L, 31)
    test(14488915920738L, 14491075920738L, -10)
    test(-3443498568308L, -3437191368308L, -44)
  }

  @Test def setHours(): Unit = {
    def test(expected: Long, initial: Long, newValue: Int): Unit =
      testSetter(expected, initial, newValue)(_.setHours(_))

    test(-17488963046067L, -17488995446067L, 10)
    test(20558498037054L, 20558490837054L, 13)
    test(5312786026257L, 5312742826257L, 13)
    test(-65966784064652L, -65966812864652L, 13)
    test(-68790482213316L, -68790500213316L, 20)
    test(-11491697044338L, -11491643044338L, 5)
    test(-64868933043469L, -64868889843469L, 3)
    test(20379542983344L, 20379571783344L, 8)
    test(-61950407165078L, -61950428765078L, 9)
    test(73736170221394L, 73736109021394L, 21)
    test(90566664522591L, 90566776122591L, -18)
    test(-87819242016796L, -87819141216796L, -10)
    test(29345423459562L, 29345481059562L, 2)
    test(46349518463981L, 46349396063981L, 42)
    test(-38641530814740L, -38641581214740L, 25)
    test(67124714832512L, 67125020832512L, -62)
    test(88841609636685L, 88841775236685L, -40)
    test(40010656373609L, 40010976773609L, -89)
    test(81865947325636L, 81865652125636L, 84)
    test(27907328578556L, 27907245778556L, 35)
  }

  @Test def setMinutes(): Unit = {
    def test(expected: Long, initial: Long, newValue: Int): Unit =
      testSetter(expected, initial, newValue)(_.setMinutes(_))

    test(64315845496143L, 64315845376143L, 38)
    test(-9903546919527L, -9903548419527L, 44)
    test(23392080130556L, 23392081210556L, 2)
    test(-36309966906037L, -36309964086037L, 4)
    test(-92453321669244L, -92453323769244L, 45)
    test(26082597712442L, 26082599932442L, 1)
    test(-13001098921998L, -13001100721998L, 37)
    test(22808221349696L, 22808224649696L, 2)
    test(20862412619567L, 20862412919567L, 36)
    test(38825155309936L, 38825156029936L, 21)
    test(-75754984947944L, -75755146887944L, 2737)
    test(6939261825864L, 6939267885864L, -77)
    test(3974566501322L, 3974466301322L, 1695)
    test(-55574815076054L, -55574860316054L, 802)
    test(-73126905171394L, -73126838511394L, -1093)
    test(8195994590020L, 8195827190020L, 2829)
    test(73067202776788L, 73067125496788L, 1332)
    test(88946681407202L, 88946858467202L, -2910)
    test(15418859258006L, 15419010818006L, -2493)
    test(-4462950467694L, -4462950107694L, 32)
  }

  @Test def setSeconds(): Unit = {
    def test(expected: Long, initial: Long, newValue: Int): Unit =
      testSetter(expected, initial, newValue)(_.setSeconds(_))

    test(-56862422032005L, -56862422024005L, 7)
    test(-88709798217310L, -88709798216310L, 2)
    test(14361189950164L, 14361189911164L, 50)
    test(85754971234137L, 85754971231137L, 34)
    test(-27614360099220L, -27614360084220L, 0)
    test(-56829714919918L, -56829714945918L, 40)
    test(22917485891028L, 22917485882028L, 11)
    test(52726495934253L, 52726495933253L, 14)
    test(85072081820766L, 85072081822766L, 20)
    test(-48842660560263L, -48842660530263L, 19)
    test(-72259899038775L, -72259901495775L, 2481)
    test(6115199760364L, 6115197741364L, 2040)
    test(29936731892049L, 29936730464049L, 1472)
    test(28079228385585L, 28079227494585L, 945)
    test(83698936996712L, 83698939936712L, -2924)
    test(69828183263190L, 69828181733190L, 1583)
    test(55571126137699L, 55571128069699L, -1883)
    test(-25587489429797L, -25587491106797L, 1730)
    test(37701620787726L, 37701623218726L, -2373)
    test(-68282722577557L, -68282724741557L, 2202)
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
