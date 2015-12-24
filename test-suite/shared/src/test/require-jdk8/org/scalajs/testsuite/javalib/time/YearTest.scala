package org.scalajs.testsuite.javalib.time

import java.time._
import java.time.temporal._

import org.junit.Test
import org.junit.Assert._
import org.scalajs.testsuite.utils.AssertThrows._

import scala.annotation.tailrec

/** Created by alonsodomin on 24/12/2015. */
class YearTest extends TemporalTest[Year] {
  import Year._
  import ChronoUnit._
  import ChronoField._
  import DateTimeTestUtil._

  val min = Year.of(MIN_VALUE)
  val max = Year.of(MAX_VALUE)
  val leapYear = Year.of(2016)
  val nonLeapYear = Year.of(2015)
  val lastBCYear = Year.of(0)
  val firstACYear = Year.of(1)
  val negativeYear = Year.of(-100)

  val samples = Seq(min, max, leapYear, nonLeapYear, lastBCYear, firstACYear, negativeYear)

  val monthDaySamples = Month.values().map(m => MonthDay.of(m, m.minLength()))

  override def isSupported(unit: ChronoUnit): Boolean =
    unit == YEARS || unit == DECADES || unit == CENTURIES || unit == MILLENNIA || unit == ERAS

  override def isSupported(field: ChronoField): Boolean =
    field == YEAR_OF_ERA || field == YEAR || field == ERA

  override def expectedRangeFor(accessor: Year, field: TemporalField): ValueRange = {
    field match {
      case YEAR_OF_ERA =>
        if (accessor.getValue() <= 0) ValueRange.of(1, MAX_VALUE + 1)
        else ValueRange.of(1, MAX_VALUE)

      case _ =>
        super.expectedRangeFor(accessor, field)
    }
  }

  @Test def getLong(): Unit = {
    for {
      t <- samples
      field <- ChronoField.values()
    } {
      if (isSupported(field)) {
        val expected = ((field: @unchecked) match {
          case YEAR_OF_ERA => if (t.getValue < 1) 1 - t.getValue else t.getValue
          case YEAR        => t.getValue
          case ERA         => if (t.getValue < 1) 0 else 1
        }).toLong

        assertEquals(expected, t.getLong(field))
      } else {
        expectThrows(classOf[UnsupportedTemporalTypeException], t.getLong(field))
      }
    }

    assertEquals(1L, lastBCYear.getLong(YEAR_OF_ERA))
    assertEquals(0L, lastBCYear.getLong(YEAR))
    assertEquals(0L, lastBCYear.getLong(ERA))

    assertEquals(101L, negativeYear.getLong(YEAR_OF_ERA))
    assertEquals(-100L, negativeYear.getLong(YEAR))
    assertEquals(0L, negativeYear.getLong(ERA))
  }

  @Test def length(): Unit = {
    assertEquals(366, leapYear.length())
    assertEquals(365, nonLeapYear.length())

    for (t <- samples) {
      val expected = if (t.isLeap) 366 else 365
      assertEquals(expected, t.length())
    }
  }

  @Test def `with`(): Unit = {
    assertEquals(lastBCYear, lastBCYear.`with`(YEAR, 0))
    assertEquals(Year.of(0), lastBCYear.`with`(YEAR_OF_ERA, 1))
    assertEquals(Year.of(-50), lastBCYear.`with`(YEAR, -50))
    assertEquals(Year.of(50), lastBCYear.`with`(YEAR, 50))
    assertEquals(Year.of(1), lastBCYear.`with`(ERA, 1))
    assertEquals(lastBCYear, lastBCYear.`with`(ERA, 0))

    assertEquals(firstACYear, firstACYear.`with`(YEAR, 1))
    assertEquals(Year.of(1), firstACYear.`with`(YEAR_OF_ERA, 1))
    assertEquals(Year.of(-50), firstACYear.`with`(YEAR, -50))
    assertEquals(Year.of(50), firstACYear.`with`(YEAR, 50))
    assertEquals(firstACYear, firstACYear.`with`(ERA, 1))
    assertEquals(Year.of(0), firstACYear.`with`(ERA, 0))

    assertEquals(negativeYear, negativeYear.`with`(YEAR, -100))
    assertEquals(Year.of(0), negativeYear.`with`(YEAR_OF_ERA, 1))
    assertEquals(Year.of(-50), negativeYear.`with`(YEAR, -50))
    assertEquals(Year.of(50), negativeYear.`with`(YEAR, 50))
    assertEquals(negativeYear, negativeYear.`with`(ERA, 0))
    assertEquals(Year.of(101), negativeYear.`with`(ERA, 1))

    for (t <- samples) {
      expectThrows(classOf[DateTimeException], t.`with`(YEAR, Long.MinValue))
      expectThrows(classOf[DateTimeException], t.`with`(YEAR, -1000000000))
      expectThrows(classOf[DateTimeException], t.`with`(YEAR, 1000000000))
      expectThrows(classOf[DateTimeException], t.`with`(YEAR, Long.MaxValue))

      expectThrows(classOf[DateTimeException], t.`with`(YEAR_OF_ERA, Long.MinValue))
      expectThrows(classOf[DateTimeException], t.`with`(YEAR_OF_ERA, -1000000001))
      expectThrows(classOf[DateTimeException], t.`with`(YEAR_OF_ERA, -1))
      expectThrows(classOf[DateTimeException], t.`with`(YEAR_OF_ERA, 0))
      expectThrows(classOf[DateTimeException], t.`with`(YEAR_OF_ERA, 1000000001))
      expectThrows(classOf[DateTimeException], t.`with`(YEAR_OF_ERA, Long.MaxValue))

      expectThrows(classOf[DateTimeException], t.`with`(ERA, Long.MinValue))
      expectThrows(classOf[DateTimeException], t.`with`(ERA, -1))
      expectThrows(classOf[DateTimeException], t.`with`(ERA, 2))
      expectThrows(classOf[DateTimeException], t.`with`(ERA, Long.MaxValue))
    }
  }

  @Test def plus(): Unit = {
    for {
      t <- samples
      u <- ChronoUnit.values() if isSupported(u)
    } {
      assertSame(t, t.plus(0, u))
    }

    assertEquals(Year.of(10), lastBCYear.plus(1, DECADES))
    assertEquals(Year.of(-10), lastBCYear.plus(-1, DECADES))
    assertEquals(Year.of(100), lastBCYear.plus(1, CENTURIES))
    assertEquals(Year.of(-100), lastBCYear.plus(-1, CENTURIES))
    assertEquals(Year.of(1000), lastBCYear.plus(1, MILLENNIA))
    assertEquals(Year.of(-1000), lastBCYear.plus(-1, MILLENNIA))
    assertEquals(firstACYear, lastBCYear.plus(1, ERAS))
    expectThrows(classOf[DateTimeException], lastBCYear.plus(-1, ERAS))

    assertEquals(Year.of(11), firstACYear.plus(1, DECADES))
    assertEquals(Year.of(-9), firstACYear.plus(-1, DECADES))
    assertEquals(Year.of(101), firstACYear.plus(1, CENTURIES))
    assertEquals(Year.of(-99), firstACYear.plus(-1, CENTURIES))
    assertEquals(Year.of(1001), firstACYear.plus(1, MILLENNIA))
    assertEquals(Year.of(-999), firstACYear.plus(-1, MILLENNIA))
    assertEquals(lastBCYear, firstACYear.plus(-1, ERAS))
    expectThrows(classOf[DateTimeException], firstACYear.plus(1, ERAS))

    assertEquals(lastBCYear, min.plus(999999999L, YEARS))
    assertEquals(lastBCYear, max.plus(-999999999L, YEARS))
    assertEquals(max, min.plus(1999999998, YEARS))
    assertEquals(min, max.plus(-1999999998, YEARS))

    for {
      t <- samples
      v <- sampleLongs
    } {
      testDateTime(t.plus(v, YEARS))(t.plusYears(v))
      testDateTime(t.plus(v, DECADES))(t.plusYears(Math.multiplyExact(v, 10)))
      testDateTime(t.plus(v, CENTURIES))(t.plusYears(Math.multiplyExact(v, 100)))
      testDateTime(t.plus(v, MILLENNIA))(t.plusYears(Math.multiplyExact(v, 1000)))
    }
  }

  @Test def plusYears(): Unit = {
    for (t <- samples) {
      assertSame(t, t.plusYears(0))
    }

    assertEquals(firstACYear, lastBCYear.plusYears(1))
    assertEquals(lastBCYear, firstACYear.plusYears(-1))
    assertEquals(max, min.plusYears(1999999998))
    assertEquals(min, max.plusYears(-1999999998))

    expectThrows(classOf[DateTimeException], min.plusYears(-1))
    expectThrows(classOf[DateTimeException], min.plusYears(1999999999))
    expectThrows(classOf[DateTimeException], min.plusYears(Long.MinValue))
    expectThrows(classOf[DateTimeException], max.plusYears(1))
    expectThrows(classOf[DateTimeException], max.plusYears(-1999999999))
    expectThrows(classOf[DateTimeException], max.plusYears(Long.MaxValue))
  }

  @Test def minusYears(): Unit = {
    for (t <- samples) {
      assertSame(t, t.minusYears(0))
    }

    assertEquals(lastBCYear, firstACYear.minusYears(1))
    assertEquals(firstACYear, lastBCYear.minusYears(-1))
    assertEquals(max, min.minusYears(-1999999998))
    assertEquals(min, max.minusYears(1999999998))

    expectThrows(classOf[DateTimeException], min.minusYears(1))
    expectThrows(classOf[DateTimeException], min.minusYears(-1999999999))
    expectThrows(classOf[DateTimeException], min.minusYears(Long.MinValue))
    expectThrows(classOf[DateTimeException], max.minusYears(-1))
    expectThrows(classOf[DateTimeException], max.minusYears(1999999999))
    expectThrows(classOf[DateTimeException], max.minusYears(Long.MaxValue))
  }

  @Test def adjustInto(): Unit = {
    val aDate = LocalDate.of(2015, 1, 1)
    assertEquals(LocalDate.of(0, 1, 1), lastBCYear.adjustInto(aDate))
    assertEquals(LocalDate.of(1, 1, 1), firstACYear.adjustInto(aDate))
    assertEquals(LocalDate.of(-100, 1, 1), negativeYear.adjustInto(aDate))
    assertEquals(LocalDate.of(Year.MIN_VALUE, 1, 1), min.adjustInto(aDate))
    assertEquals(LocalDate.of(Year.MAX_VALUE, 1, 1), max.adjustInto(aDate))

    val leapDate = LocalDate.of(2012, 2, 29)
    assertEquals(LocalDate.of(2016, 2, 29), leapYear.adjustInto(leapDate))
    assertEquals(LocalDate.of(2015, 2, 28), nonLeapYear.adjustInto(leapDate))
  }

  @Test def until(): Unit = {
    for {
      t <- samples
      unit <- ChronoUnit.values() if isSupported(unit)
    } {
      assertEquals(0L, t.until(t, unit))
    }

    @tailrec def nextLeapYear(year: Int): Year = {
      val nextYearValue = year + 1
      if (Year.isLeap(nextYearValue)) Year.of(nextYearValue)
      else nextLeapYear(year + 1)
    }
    assertEquals(4L, leapYear.until(nextLeapYear(leapYear.getValue), YEARS))

    assertEquals(10L, lastBCYear.until(Year.of(10), YEARS))
    assertEquals(-10L, lastBCYear.until(Year.of(-10), YEARS))
    assertEquals(10L, firstACYear.until(Year.of(11), YEARS))
    assertEquals(-12L, firstACYear.until(Year.of(-11), YEARS))
    assertEquals(1L, lastBCYear.until(Year.of(10), DECADES))
    assertEquals(-1L, lastBCYear.until(Year.of(-10), DECADES))
    assertEquals(1L, firstACYear.until(Year.of(11), DECADES))
    assertEquals(-1L, firstACYear.until(Year.of(-11), DECADES))
    assertEquals(0L, lastBCYear.until(Year.of(10), CENTURIES))
    assertEquals(0L, lastBCYear.until(Year.of(-10), CENTURIES))
    assertEquals(0L, firstACYear.until(Year.of(11), CENTURIES))
    assertEquals(0L, firstACYear.until(Year.of(-11), CENTURIES))
    assertEquals(0L, lastBCYear.until(Year.of(10), MILLENNIA))
    assertEquals(0L, lastBCYear.until(Year.of(-10), MILLENNIA))
    assertEquals(0L, firstACYear.until(Year.of(11), MILLENNIA))
    assertEquals(0L, firstACYear.until(Year.of(-11), MILLENNIA))

    assertEquals(100L, lastBCYear.until(Year.of(100), YEARS))
    assertEquals(-100L, lastBCYear.until(Year.of(-100), YEARS))
    assertEquals(100L, firstACYear.until(Year.of(101), YEARS))
    assertEquals(-102L, firstACYear.until(Year.of(-101), YEARS))
    assertEquals(10L, lastBCYear.until(Year.of(100), DECADES))
    assertEquals(-10L, lastBCYear.until(Year.of(-100), DECADES))
    assertEquals(10L, firstACYear.until(Year.of(101), DECADES))
    assertEquals(-10L, firstACYear.until(Year.of(-101), DECADES))
    assertEquals(1L, lastBCYear.until(Year.of(100), CENTURIES))
    assertEquals(-1L, lastBCYear.until(Year.of(-100), CENTURIES))
    assertEquals(1L, firstACYear.until(Year.of(101), CENTURIES))
    assertEquals(-1L, firstACYear.until(Year.of(-101), CENTURIES))
    assertEquals(0L, lastBCYear.until(Year.of(100), MILLENNIA))
    assertEquals(0L, lastBCYear.until(Year.of(-100), MILLENNIA))
    assertEquals(0L, firstACYear.until(Year.of(101), MILLENNIA))
    assertEquals(0L, firstACYear.until(Year.of(-101), MILLENNIA))

    assertEquals(1000L, lastBCYear.until(Year.of(1000), YEARS))
    assertEquals(-1000L, lastBCYear.until(Year.of(-1000), YEARS))
    assertEquals(1000L, firstACYear.until(Year.of(1001), YEARS))
    assertEquals(-1002L, firstACYear.until(Year.of(-1001), YEARS))
    assertEquals(100L, lastBCYear.until(Year.of(1000), DECADES))
    assertEquals(-100L, lastBCYear.until(Year.of(-1000), DECADES))
    assertEquals(100L, firstACYear.until(Year.of(1001), DECADES))
    assertEquals(-100L, firstACYear.until(Year.of(-1001), DECADES))
    assertEquals(10L, lastBCYear.until(Year.of(1000), CENTURIES))
    assertEquals(-10L, lastBCYear.until(Year.of(-1000), CENTURIES))
    assertEquals(10L, firstACYear.until(Year.of(1001), CENTURIES))
    assertEquals(-10L, firstACYear.until(Year.of(-1001), CENTURIES))
    assertEquals(1L, lastBCYear.until(Year.of(1000), MILLENNIA))
    assertEquals(-1L, lastBCYear.until(Year.of(-1000), MILLENNIA))
    assertEquals(1L, firstACYear.until(Year.of(1001), MILLENNIA))
    assertEquals(-1L, firstACYear.until(Year.of(-1001), MILLENNIA))

    assertEquals(1L, lastBCYear.until(firstACYear, ERAS))
    assertEquals(-1L, firstACYear.until(lastBCYear, ERAS))
    assertEquals(0L, firstACYear.until(Year.of(100), ERAS))
    assertEquals(0L, lastBCYear.until(Year.of(-100), ERAS))
    assertEquals(1L, negativeYear.until(Year.of(100), ERAS))

    assertEquals(1999999998L, min.until(max, YEARS))
    assertEquals(-1999999998L, max.until(min, YEARS))
    assertEquals(199999999L, min.until(max, DECADES))
    assertEquals(-199999999L, max.until(min, DECADES))
    assertEquals(19999999L, min.until(max, CENTURIES))
    assertEquals(-19999999L, max.until(min, CENTURIES))
    assertEquals(1999999L, min.until(max, MILLENNIA))
    assertEquals(-1999999L, max.until(min, MILLENNIA))
    assertEquals(1L, min.until(max, ERAS))
    assertEquals(-1L, max.until(min, ERAS))
  }

  @Test def atDay(): Unit = {
    for (t <- samples) {
      assertEquals(LocalDate.of(t.getValue, 1, 1), t.atDay(1))
      assertEquals(LocalDate.of(t.getValue, 12, 31), t.atDay(t.length()))
      expectThrows(classOf[DateTimeException], t.atDay(-1))
    }

    assertEquals(LocalDate.of(leapYear.getValue, 2, 29), leapYear.atDay(31 + 29))
    assertEquals(LocalDate.of(leapYear.getValue, 3, 1), leapYear.atDay(31 + 30))
    assertEquals(LocalDate.of(leapYear.getValue, 12, 30), leapYear.atDay(365))
    assertEquals(LocalDate.of(leapYear.getValue, 12, 31), leapYear.atDay(366))
    expectThrows(classOf[DateTimeException], leapYear.atDay(367))

    assertEquals(LocalDate.of(nonLeapYear.getValue, 3, 1), nonLeapYear.atDay(31 + 29))
    assertEquals(LocalDate.of(nonLeapYear.getValue, 12, 31), nonLeapYear.atDay(365))
    expectThrows(classOf[DateTimeException], nonLeapYear.atDay(366))
  }

  @Test def isLeap(): Unit = {
    assertTrue(leapYear.isLeap)
    assertFalse(nonLeapYear.isLeap)

    assertTrue(Year.isLeap(leapYear.getValue))
    assertFalse(Year.isLeap(nonLeapYear.getValue))
  }

  @Test def isValidMonthDay(): Unit = {
    val leapMonthDay = MonthDay.of(2, 29)
    assertTrue(leapYear.isValidMonthDay(leapMonthDay))
    assertFalse(nonLeapYear.isValidMonthDay(leapMonthDay))

    for {
      t <- samples
      monthDay <- monthDaySamples
    } {
      assertTrue(t.isValidMonthDay(monthDay))
    }
  }

  @Test def atMonthDay(): Unit = {
    val leapMonthDay = MonthDay.of(2, 29)
    val leapDate = LocalDate.of(leapYear.getValue(),
        leapMonthDay.getMonthValue, leapMonthDay.getDayOfMonth)
    assertEquals(leapDate, leapYear.atMonthDay(leapMonthDay))

    val nonLeapDate = LocalDate.of(nonLeapYear.getValue(),
        leapMonthDay.getMonthValue, 28)
    assertEquals(nonLeapDate, nonLeapYear.atMonthDay(leapMonthDay))

    for {
      t <- samples
      monthDay <- monthDaySamples
    } {
      val expected = LocalDate.of(t.getValue, monthDay.getMonthValue, monthDay.getDayOfMonth)
      assertEquals(expected, t.atMonthDay(monthDay))
    }
  }

  @Test def compareTo(): Unit = {
    assertEquals(0, min.compareTo(min))
    assertEquals(-1999999998, min.compareTo(max))
    assertEquals(1999999998, max.compareTo(min))
    assertEquals(0, max.compareTo(max))

    assertEquals(999999999, lastBCYear.compareTo(min))
    assertEquals(-999999999, min.compareTo(lastBCYear))
    assertEquals(1000000000, firstACYear.compareTo(min))
    assertEquals(-1000000000, min.compareTo(firstACYear))

    assertEquals(-1, lastBCYear.compareTo(firstACYear))
    assertEquals(1, firstACYear.compareTo(lastBCYear))
  }

  @Test def isAfter(): Unit = {
    assertFalse(min.isAfter(min))
    assertFalse(min.isAfter(max))
    assertTrue(max.isAfter(min))
    assertFalse(max.isAfter(max))
  }

  @Test def isBefore(): Unit = {
    assertFalse(min.isBefore(min))
    assertTrue(min.isBefore(max))
    assertFalse(max.isBefore(min))
    assertFalse(max.isBefore(max))
  }

  @Test def now(): Unit = {
    val now = LocalDate.now()
    val year = Year.now()

    if (now.getYear != year.getValue) {
      assertEquals(LocalDate.now().getYear, year.getValue)
      println("Happy new year " + year + '!')
    }
  }

  @Test def of(): Unit = {
    assertEquals(lastBCYear, Year.of(0))
    assertEquals(0, Year.of(0).getValue)

    assertEquals(firstACYear, Year.of(1))
    assertEquals(1, Year.of(1).getValue)

    assertEquals(min, Year.of(MIN_VALUE))
    assertEquals(-999999999, Year.of(MIN_VALUE).getValue)

    assertEquals(max, Year.of(MAX_VALUE))
    assertEquals(999999999, Year.of(MAX_VALUE).getValue)
  }

  @Test def from(): Unit = {
    for (t <- samples)
      assertSame(t, Year.from(t))

    val dateSamples = Seq(LocalDate.MIN, LocalDate.MAX, LocalDate.of(0, 1, 1))
    for (d <- dateSamples) {
      assertEquals(Year.of(d.getYear), Year.from(d))
    }
  }

  @Test def toStringOutput(): Unit = {
    for (t <- samples) {
      val expected = t.getValue.toString
      assertEquals(expected, t.toString)
    }
  }

}
