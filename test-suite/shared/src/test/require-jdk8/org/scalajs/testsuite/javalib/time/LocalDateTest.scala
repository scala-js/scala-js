package org.scalajs.testsuite.javalib.time

import java.time._
import java.time.chrono.{IsoEra, IsoChronology}
import java.time.temporal._

import org.junit.{Ignore, Test}
import org.junit.Assert._
import org.scalajs.testsuite.utils.AssertThrows._

class LocalDateTest extends TemporalTest {

  import DateTimeTestUtil._
  import LocalDate._
  import ChronoField._
  import ChronoUnit._

  val someDate = of(2011, 2, 28)
  val leapDate = of(2012, 2, 29)

  val samples = Seq(MIN, ofEpochDay(-1), ofEpochDay(0), ofEpochDay(1), someDate,
      leapDate, MAX)

  def isSupported(unit: ChronoUnit): Boolean = unit.isDateBased

  def isSupported(field: ChronoField): Boolean = field.isDateBased

  @Test override def test_range(): Unit = {
    for {
      d <- samples
      field <- ChronoField.values
    } {
      lazy val expected = field match {
        case DAY_OF_MONTH => ValueRange.of(1, d.lengthOfMonth)
        case DAY_OF_YEAR  => ValueRange.of(1, d.lengthOfYear)

        case ALIGNED_WEEK_OF_MONTH =>
          ValueRange.of(1, if (d.lengthOfMonth > 28) 5 else 4)

        case YEAR_OF_ERA =>
          val maxYear = if (d.getEra == IsoEra.CE) 999999999 else 1000000000
          ValueRange.of(1, maxYear)

        case _ => field.range
      }
      if (isSupported(field))
        assertEquals(expected, d.range(field))
      else
        expectThrows(classOf[UnsupportedTemporalTypeException], d.range(field))
    }
  }

  @Test def test_getLong(): Unit = {
    for (d <- samples) {
      assertEquals(d.getDayOfWeek.getValue.toLong, d.getLong(DAY_OF_WEEK))
      assertEquals(d.getDayOfMonth.toLong, d.getLong(DAY_OF_MONTH))
      assertEquals(d.getDayOfYear.toLong, d.getLong(DAY_OF_YEAR))
      assertEquals(d.toEpochDay, d.getLong(EPOCH_DAY))
      assertEquals(d.getMonthValue.toLong, d.getLong(MONTH_OF_YEAR))
      assertEquals(d.getYear.toLong, d.getLong(YEAR))
      assertEquals(d.getEra.getValue.toLong, d.getLong(ERA))
    }

    assertEquals(1L, MIN.getLong(ALIGNED_DAY_OF_WEEK_IN_MONTH))
    assertEquals(1L, MIN.getLong(ALIGNED_DAY_OF_WEEK_IN_YEAR))
    assertEquals(1L, MIN.getLong(ALIGNED_WEEK_OF_MONTH))
    assertEquals(1L, MIN.getLong(ALIGNED_WEEK_OF_YEAR))
    assertEquals(-11999999988L, MIN.getLong(PROLEPTIC_MONTH))
    assertEquals(1000000000L, MIN.getLong(YEAR_OF_ERA))

    assertEquals(7L, someDate.getLong(ALIGNED_DAY_OF_WEEK_IN_MONTH))
    assertEquals(3L, someDate.getLong(ALIGNED_DAY_OF_WEEK_IN_YEAR))
    assertEquals(4L, someDate.getLong(ALIGNED_WEEK_OF_MONTH))
    assertEquals(9L, someDate.getLong(ALIGNED_WEEK_OF_YEAR))
    assertEquals(24133L, someDate.getLong(PROLEPTIC_MONTH))
    assertEquals(2011L, someDate.getLong(YEAR_OF_ERA))

    assertEquals(1L, leapDate.getLong(ALIGNED_DAY_OF_WEEK_IN_MONTH))
    assertEquals(4L, leapDate.getLong(ALIGNED_DAY_OF_WEEK_IN_YEAR))
    assertEquals(5L, leapDate.getLong(ALIGNED_WEEK_OF_MONTH))
    assertEquals(9L, leapDate.getLong(ALIGNED_WEEK_OF_YEAR))
    assertEquals(24145L, leapDate.getLong(PROLEPTIC_MONTH))
    assertEquals(2012L, leapDate.getLong(YEAR_OF_ERA))

    assertEquals(3L, MAX.getLong(ALIGNED_DAY_OF_WEEK_IN_MONTH))
    assertEquals(1L, MAX.getLong(ALIGNED_DAY_OF_WEEK_IN_YEAR))
    assertEquals(5L, MAX.getLong(ALIGNED_WEEK_OF_MONTH))
    assertEquals(53L, MAX.getLong(ALIGNED_WEEK_OF_YEAR))
    assertEquals(11999999999L, MAX.getLong(PROLEPTIC_MONTH))
    assertEquals(999999999L, MAX.getLong(YEAR_OF_ERA))
  }

  @Test def test_getChronology(): Unit = {
    for (d <- samples)
      assertEquals(IsoChronology.INSTANCE, d.getChronology)
  }

  @Test def test_getEra(): Unit = {
    assertEquals(IsoEra.BCE, MIN.getEra)
    assertEquals(IsoEra.CE, someDate.getEra)
    assertEquals(IsoEra.CE, leapDate.getEra)
    assertEquals(IsoEra.CE, MAX.getEra)
  }

  @Test def test_getYear(): Unit = {
    assertEquals(-999999999, MIN.getYear)
    assertEquals(2011, someDate.getYear)
    assertEquals(2012, leapDate.getYear)
    assertEquals(999999999, MAX.getYear)
  }

  @Test def test_getMonthValue(): Unit = {
    for (d <- samples)
      assertEquals(d.getMonth.getValue, d.getMonthValue)
  }

  @Test def test_getMonth(): Unit = {
    assertEquals(Month.JANUARY, MIN.getMonth)
    assertEquals(Month.FEBRUARY, someDate.getMonth)
    assertEquals(Month.FEBRUARY, leapDate.getMonth)
    assertEquals(Month.DECEMBER, MAX.getMonth)
  }

  @Test def test_getDayOfMonth(): Unit = {
    assertEquals(1, MIN.getDayOfMonth)
    assertEquals(28, someDate.getDayOfMonth)
    assertEquals(29, leapDate.getDayOfMonth)
    assertEquals(31, MAX.getDayOfMonth)
  }

  @Test def test_getDayOfYear(): Unit = {
    assertEquals(1, MIN.getDayOfYear)
    assertEquals(59, someDate.getDayOfYear)
    assertEquals(60, leapDate.getDayOfYear)
    assertEquals(366, of(2012, 12, 31).getDayOfYear)
    assertEquals(365, MAX.getDayOfYear)
  }

  @Test def test_getDayOfWeek(): Unit = {
    assertEquals(DayOfWeek.MONDAY, MIN.getDayOfWeek)
    assertEquals(DayOfWeek.MONDAY, someDate.getDayOfWeek)
    assertEquals(DayOfWeek.WEDNESDAY, leapDate.getDayOfWeek)
    assertEquals(DayOfWeek.FRIDAY, MAX.getDayOfWeek)
  }

  @Test def test_isLeapYear(): Unit = {
    assertFalse(MIN.isLeapYear)
    assertTrue(of(-400, 6, 30).isLeapYear)
    assertFalse(of(-100, 3, 1).isLeapYear)
    assertTrue(of(0, 1, 1).isLeapYear)
    assertFalse(of(1900, 9, 30).isLeapYear)
    assertTrue(of(2000, 1, 1).isLeapYear)
    assertFalse(someDate.isLeapYear)
    assertTrue(leapDate.isLeapYear)
    assertFalse(MAX.isLeapYear)
  }

  @Test def test_lengthOfMonth(): Unit = {
    for (d <- samples ++ Seq(of(2001, 2, 1), of(2012, 9, 30)))
      assertEquals(d.getMonth.length(d.isLeapYear), d.lengthOfMonth)
  }

  @Test def test_lengthOfYear(): Unit = {
    for (d <- samples)
      assertEquals(if (d.isLeapYear) 366 else 365, d.lengthOfYear)
  }

  @Test def test_with(): Unit = {
    testDateTime(MAX.`with`(DAY_OF_WEEK, 1))(of(999999999, 12, 27))
    testDateTime(MAX.`with`(DAY_OF_WEEK, 5))(MAX)
    testDateTime(MIN.`with`(DAY_OF_WEEK, 1))(MIN)
    testDateTime(MIN.`with`(DAY_OF_WEEK, 7))(of(-999999999, 1, 7))
    testDateTime(someDate.`with`(DAY_OF_WEEK, 1))(someDate)
    testDateTime(someDate.`with`(DAY_OF_WEEK, 7))(of(2011, 3, 6))
    testDateTime(leapDate.`with`(DAY_OF_WEEK, 1))(of(2012, 2, 27))
    testDateTime(leapDate.`with`(DAY_OF_WEEK, 7))(of(2012, 3, 4))
    testDateTime(MAX.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, 1))(of(999999999, 12, 29))
    testDateTime(MAX.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, 3))(MAX)
    testDateTime(MIN.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, 1))(MIN)
    testDateTime(MIN.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, 7))(of(-999999999, 1, 7))
    testDateTime(someDate.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, 1))(of(2011, 2, 22))
    testDateTime(someDate.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, 7))(someDate)
    testDateTime(leapDate.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, 1))(leapDate)
    testDateTime(leapDate.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, 7))(of(2012, 3, 6))
    testDateTime(MAX.`with`(ALIGNED_DAY_OF_WEEK_IN_YEAR, 1))(MAX)
    testDateTime(MIN.`with`(ALIGNED_DAY_OF_WEEK_IN_YEAR, 1))(MIN)
    testDateTime(MIN.`with`(ALIGNED_DAY_OF_WEEK_IN_YEAR, 7))(of(-999999999, 1, 7))
    testDateTime(someDate.`with`(ALIGNED_DAY_OF_WEEK_IN_YEAR, 1))(of(2011, 2, 26))
    testDateTime(someDate.`with`(ALIGNED_DAY_OF_WEEK_IN_YEAR, 7))(of(2011, 3, 4))
    testDateTime(leapDate.`with`(ALIGNED_DAY_OF_WEEK_IN_YEAR, 1))(of(2012, 2, 26))
    testDateTime(leapDate.`with`(ALIGNED_DAY_OF_WEEK_IN_YEAR, 7))(of(2012, 3, 3))
    testDateTime(someDate.`with`(DAY_OF_MONTH, 1))(of(2011, 2, 1))
    testDateTime(leapDate.`with`(DAY_OF_MONTH, 28))(of(2012, 2, 28))
    testDateTime(someDate.`with`(DAY_OF_YEAR, 1))(of(2011, 1, 1))
    testDateTime(someDate.`with`(DAY_OF_YEAR, 365))(of(2011, 12, 31))
    testDateTime(leapDate.`with`(DAY_OF_YEAR, 366))(of(2012, 12, 31))
    for {
      d1 <- samples
      d2 <- samples
    } {
      testDateTime(d1.`with`(EPOCH_DAY, d2.toEpochDay))(d2)
    }
    testDateTime(MAX.`with`(ALIGNED_WEEK_OF_MONTH, 1))(of(999999999, 12, 3))
    testDateTime(MAX.`with`(ALIGNED_WEEK_OF_MONTH, 5))(MAX)
    testDateTime(MIN.`with`(ALIGNED_WEEK_OF_MONTH, 1))(MIN)
    testDateTime(MIN.`with`(ALIGNED_WEEK_OF_MONTH, 5))(of(-999999999, 1, 29))
    testDateTime(someDate.`with`(ALIGNED_WEEK_OF_MONTH, 1))(of(2011, 2, 7))
    testDateTime(someDate.`with`(ALIGNED_WEEK_OF_MONTH, 5))(of(2011, 3, 7))
    testDateTime(leapDate.`with`(ALIGNED_WEEK_OF_MONTH, 1))(of(2012, 2, 1))
    testDateTime(leapDate.`with`(ALIGNED_WEEK_OF_MONTH, 5))(leapDate)
    testDateTime(MAX.`with`(ALIGNED_WEEK_OF_YEAR, 1))(of(999999999, 1, 1))
    testDateTime(MAX.`with`(ALIGNED_WEEK_OF_YEAR, 53))(MAX)
    testDateTime(MIN.`with`(ALIGNED_WEEK_OF_YEAR, 1))(MIN)
    testDateTime(MIN.`with`(ALIGNED_WEEK_OF_YEAR, 53))(of(-999999999, 12, 31))
    testDateTime(someDate.`with`(ALIGNED_WEEK_OF_YEAR, 1))(of(2011, 1, 3))
    testDateTime(someDate.`with`(ALIGNED_WEEK_OF_YEAR, 53))(of(2012, 1, 2))
    testDateTime(leapDate.`with`(ALIGNED_WEEK_OF_YEAR, 1))(of(2012, 1, 4))
    testDateTime(leapDate.`with`(ALIGNED_WEEK_OF_YEAR, 53))(of(2013, 1, 2))
    testDateTime(MAX.`with`(MONTH_OF_YEAR, 2))(of(999999999, 2, 28))
    testDateTime(MAX.`with`(MONTH_OF_YEAR, 11))(of(999999999, 11, 30))
    testDateTime(someDate.`with`(MONTH_OF_YEAR, 1))(of(2011, 1, 28))
    testDateTime(leapDate.`with`(MONTH_OF_YEAR, 2))(leapDate)
    testDateTime(MAX.`with`(PROLEPTIC_MONTH, 1))(of(0, 2, 29))
    testDateTime(MIN.`with`(PROLEPTIC_MONTH, -1))(of(-1, 12, 1))
    testDateTime(someDate.`with`(PROLEPTIC_MONTH, -11999999988L))(of(-999999999, 1, 28))
    testDateTime(leapDate.`with`(PROLEPTIC_MONTH, 11999999999L))(of(999999999, 12, 29))
    testDateTime(MIN.`with`(YEAR_OF_ERA, 1000000000))(MIN)
    testDateTime(MIN.`with`(YEAR_OF_ERA, 1))(of(0, 1, 1))
    testDateTime(MAX.`with`(YEAR_OF_ERA, 999999999))(MAX)
    testDateTime(MAX.`with`(YEAR_OF_ERA, 1))(of(1, 12, 31))
    testDateTime(leapDate.`with`(YEAR_OF_ERA, 2011))(someDate)
    testDateTime(MIN.`with`(YEAR, -999999999))(MIN)
    testDateTime(MIN.`with`(YEAR, 999999999))(of(999999999, 1, 1))
    testDateTime(MAX.`with`(YEAR, -999999999))(of(-999999999, 12, 31))
    testDateTime(MAX.`with`(YEAR, 999999999))(MAX)
    testDateTime(leapDate.`with`(YEAR, 2011))(someDate)
    testDateTime(MIN.`with`(ERA, 0))(MIN)
    testDateTime(MAX.`with`(ERA, 0))(of(-999999998, 12, 31))
    testDateTime(MAX.`with`(ERA, 1))(MAX)
    testDateTime(someDate.`with`(ERA, 1))(someDate)
    testDateTime(leapDate.`with`(ERA, 0))(of(-2011, 2, 28))

    expectThrows(classOf[DateTimeException], MAX.`with`(DAY_OF_WEEK, 6))
    expectThrows(classOf[DateTimeException],
        MAX.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, 4))
    expectThrows(classOf[DateTimeException],
        MAX.`with`(ALIGNED_DAY_OF_WEEK_IN_YEAR, 2))
    expectThrows(classOf[DateTimeException], someDate.`with`(DAY_OF_MONTH, 29))
    expectThrows(classOf[DateTimeException], leapDate.`with`(DAY_OF_MONTH, 30))
    expectThrows(classOf[DateTimeException], someDate.`with`(DAY_OF_YEAR, 366))
    expectThrows(classOf[DateTimeException],
        someDate.`with`(YEAR_OF_ERA, 1000000000))
    expectThrows(classOf[DateTimeException], MIN.`with`(ERA, 1))

    for (d <- samples) {
      for (n <- Seq(Long.MinValue, 0L, 8L, Long.MaxValue)) {
        expectThrows(classOf[DateTimeException], d.`with`(DAY_OF_WEEK, n))
        expectThrows(classOf[DateTimeException],
            d.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, n))
        expectThrows(classOf[DateTimeException],
            d.`with`(ALIGNED_DAY_OF_WEEK_IN_YEAR, n))
      }
      for (n <- Seq(Long.MinValue, 0L, 32L, Long.MaxValue))
        expectThrows(classOf[DateTimeException], d.`with`(DAY_OF_MONTH, n))
      for (n <- Seq(Long.MinValue, 0L, 367L, Long.MaxValue))
        expectThrows(classOf[DateTimeException], d.`with`(DAY_OF_YEAR, n))
      for (n <- Seq(Long.MinValue, -365243219163L, 365241780472L, Long.MaxValue))
        expectThrows(classOf[DateTimeException], d.`with`(EPOCH_DAY, n))
      for (n <- Seq(Long.MinValue, 0L, 6L, Long.MaxValue)) {
        expectThrows(classOf[DateTimeException],
            d.`with`(ALIGNED_WEEK_OF_MONTH, n))
      }
      for (n <- Seq(Long.MinValue, 0L, 54L, Long.MaxValue)) {
        expectThrows(classOf[DateTimeException],
            d.`with`(ALIGNED_WEEK_OF_YEAR, n))
      }
      for (n <- Seq(Long.MinValue, 0L, 13L, Long.MaxValue))
        expectThrows(classOf[DateTimeException], d.`with`(MONTH_OF_YEAR, n))
      for (n <- Seq(Long.MinValue, -11999999989L, 12000000000L, Long.MaxValue))
        expectThrows(classOf[DateTimeException], d.`with`(PROLEPTIC_MONTH, n))
      for (n <- Seq(Long.MinValue, 0L, 1000000001L, Long.MaxValue))
        expectThrows(classOf[DateTimeException], d.`with`(YEAR_OF_ERA, n))
      for (n <- Seq(Long.MinValue, -1000000000L, 1000000000L, Long.MaxValue))
        expectThrows(classOf[DateTimeException], d.`with`(YEAR, n))
      for (n <- Seq(Long.MinValue, -1L, 2L, Long.MaxValue))
        expectThrows(classOf[DateTimeException], d.`with`(ERA, n))
    }
  }

  @Test def test_withYear(): Unit = {
    testDateTime(MIN.withYear(-999999999))(MIN)
    testDateTime(MIN.withYear(999999999))(of(999999999, 1, 1))
    testDateTime(MAX.withYear(-999999999))(of(-999999999, 12, 31))
    testDateTime(MAX.withYear(999999999))(MAX)

    val years = Seq(Int.MinValue, -1000000000, 1000000000, Int.MaxValue)
    for {
      d <- samples
      n <- years
    } {
      expectThrows(classOf[DateTimeException], d.withYear(n))
    }
  }

  @Test def test_withMonth(): Unit = {
    testDateTime(MAX.withMonth(2))(of(999999999, 2, 28))
    testDateTime(MAX.withMonth(11))(of(999999999, 11, 30))
    testDateTime(someDate.withMonth(1))(of(2011, 1, 28))
    testDateTime(leapDate.withMonth(2))(leapDate)

    val months = Seq(Int.MinValue, 0, 13, Int.MaxValue)
    for {
      d <- samples
      n <- months
    } {
      expectThrows(classOf[DateTimeException], d.withMonth(n))
    }
  }

  @Test def test_withDayOfMonth(): Unit = {
    testDateTime(someDate.withDayOfMonth(1))(of(2011, 2, 1))
    testDateTime(leapDate.withDayOfMonth(28))(of(2012, 2, 28))

    expectThrows(classOf[DateTimeException], someDate.withDayOfMonth(29))
    expectThrows(classOf[DateTimeException], leapDate.withDayOfMonth(30))
    expectThrows(classOf[DateTimeException], of(0, 4, 30).withDayOfMonth(31))
    val days = Seq(Int.MinValue, 0, 32, Int.MaxValue)
    for {
      d <- samples
      n <- days
    } {
      expectThrows(classOf[DateTimeException], d.withDayOfMonth(n))
    }
  }

  @Test def test_withDayOfYear(): Unit = {
    testDateTime(someDate.withDayOfYear(1))(of(2011, 1, 1))
    testDateTime(someDate.withDayOfYear(365))(of(2011, 12, 31))
    testDateTime(leapDate.withDayOfYear(366))(of(2012, 12, 31))

    expectThrows(classOf[DateTimeException], someDate.withDayOfYear(366))
    val days = Seq(Int.MinValue, 0, 367, Int.MaxValue)
    for {
      d <- samples
      n <- days
    } {
      expectThrows(classOf[DateTimeException], d.withDayOfMonth(n))
    }
  }

  @Test def test_plus(): Unit = {
    val values = Seq(Long.MinValue, Int.MinValue.toLong, -1000L, -366L, -365L,
        -100L, -12L, -10L, -7L, -1L, 0L, 1L, 7L, 10L, 12L, 100L,
        365L, 366L, 1000L, Int.MaxValue.toLong, Long.MaxValue)

    for {
      d <- samples
      n <- values
    } {
      testDateTime(d.plus(n, DAYS))(d.plusDays(n))
      testDateTime(d.plus(n, WEEKS))(d.plusWeeks(n))
      testDateTime(d.plus(n, MONTHS))(d.plusMonths(n))
      testDateTime(d.plus(n, YEARS))(d.plusYears(n))
      testDateTime(d.plus(n, DECADES))(d.plusYears(Math.multiplyExact(n, 10)))
      testDateTime(d.plus(n, CENTURIES))(d.plusYears(Math.multiplyExact(n, 100)))
      testDateTime(d.plus(n, MILLENNIA))(d.plusYears(Math.multiplyExact(n, 1000)))
      testDateTime(d.plus(n, ERAS))(d.`with`(ERA, Math.addExact(n, d.get(ERA))))
    }
  }

  @Test def test_plusYears(): Unit = {
    for (d <- samples)
      testDateTime(d.plusYears(0))(d)
    testDateTime(someDate.plusYears(-2))(of(2009, 2, 28))
    testDateTime(someDate.plusYears(-1))(of(2010, 2, 28))
    testDateTime(someDate.plusYears(1))(of(2012, 2, 28))
    testDateTime(someDate.plusYears(2))(of(2013, 2, 28))
    testDateTime(leapDate.plusYears(-2))(of(2010, 2, 28))
    testDateTime(leapDate.plusYears(-1))(someDate)
    testDateTime(leapDate.plusYears(1))(of(2013, 2, 28))
    testDateTime(leapDate.plusYears(2))(of(2014, 2, 28))
    testDateTime(MIN.plusYears(1999999998))(of(999999999, 1, 1))
    testDateTime(MAX.plusYears(-1999999998))(of(-999999999, 12, 31))
    expectThrows(classOf[DateTimeException], MIN.plusYears(-1))
    expectThrows(classOf[DateTimeException], MIN.plusYears(1999999999))
    expectThrows(classOf[DateTimeException], MAX.plusYears(-1999999999))
    expectThrows(classOf[DateTimeException], MAX.plusYears(1))
    expectThrows(classOf[DateTimeException], MIN.plusYears(Long.MinValue))
    expectThrows(classOf[DateTimeException], MAX.plusYears(Long.MaxValue))
  }

  @Test def test_plusMonths(): Unit = {
    for (d <- samples)
      testDateTime(d.plusMonths(0))(d)
    testDateTime(someDate.plusMonths(-12))(of(2010, 2, 28))
    testDateTime(someDate.plusMonths(-1))(of(2011, 1, 28))
    testDateTime(someDate.plusMonths(1))(of(2011, 3, 28))
    testDateTime(someDate.plusMonths(12))(of(2012, 2, 28))
    testDateTime(leapDate.plusMonths(-12))(someDate)
    testDateTime(leapDate.plusMonths(-1))(of(2012, 1, 29))
    testDateTime(leapDate.plusMonths(1))(of(2012, 3, 29))
    testDateTime(leapDate.plusMonths(12))(of(2013, 2, 28))
    testDateTime(of(2011, 1, 31).plusMonths(1))(someDate)
    testDateTime(of(2011, 3, 31).plusMonths(-1))(someDate)
    testDateTime(of(2011, 3, 31).plusMonths(1))(of(2011, 4, 30))
    testDateTime(of(2012, 1, 31).plusMonths(1))(leapDate)
    testDateTime(of(2012, 3, 31).plusMonths(-1))(leapDate)
    testDateTime(of(2012, 3, 31).plusMonths(1))(of(2012, 4, 30))
    testDateTime(MIN.plusMonths(23999999987L))(of(999999999, 12, 1))
    testDateTime(MAX.plusMonths(-23999999987L))(of(-999999999, 1, 31))
    expectThrows(classOf[DateTimeException], MIN.plusMonths(-1))
    expectThrows(classOf[DateTimeException], MIN.plusMonths(23999999988L))
    expectThrows(classOf[DateTimeException], MAX.plusMonths(-23999999988L))
    expectThrows(classOf[DateTimeException], MAX.plusMonths(1))
    expectThrows(classOf[DateTimeException], MIN.plusMonths(Long.MinValue))
    expectThrows(classOf[DateTimeException], MAX.plusMonths(Long.MaxValue))
  }

  @Test def test_plusWeeks(): Unit = {
    for (d <- samples)
      testDateTime(d.plusWeeks(0))(d)
    testDateTime(someDate.plusWeeks(-53))(of(2010, 2, 22))
    testDateTime(someDate.plusWeeks(-52))(of(2010, 3, 1))
    testDateTime(someDate.plusWeeks(-1))(of(2011, 2, 21))
    testDateTime(someDate.plusWeeks(1))(of(2011, 3, 7))
    testDateTime(someDate.plusWeeks(52))(of(2012, 2, 27))
    testDateTime(someDate.plusWeeks(53))(of(2012, 3, 5))
    testDateTime(leapDate.plusWeeks(-53))(of(2011, 2, 23))
    testDateTime(leapDate.plusWeeks(-52))(of(2011, 3, 2))
    testDateTime(leapDate.plusWeeks(-1))(of(2012, 2, 22))
    testDateTime(leapDate.plusWeeks(1))(of(2012, 3, 7))
    testDateTime(leapDate.plusWeeks(52))(of(2013, 2, 27))
    testDateTime(leapDate.plusWeeks(53))(of(2013, 3, 6))
    testDateTime(MIN.plusWeeks(104354999947L))(of(999999999, 12, 27))
    testDateTime(MAX.plusWeeks(-104354999947L))(of(-999999999, 1, 5))
    expectThrows(classOf[DateTimeException], MIN.plusWeeks(-1))
    expectThrows(classOf[DateTimeException], MIN.plusWeeks(104354999948L))
    expectThrows(classOf[DateTimeException], MAX.plusWeeks(-1043549999478L))
    expectThrows(classOf[DateTimeException], MAX.plusWeeks(1))
    expectThrows(classOf[ArithmeticException], MIN.plusWeeks(Long.MinValue))
    expectThrows(classOf[ArithmeticException], MAX.plusWeeks(Long.MaxValue))
  }

  @Test def test_plusDays(): Unit = {
    for (d <- samples)
      testDateTime(d.plusDays(0))(d)
    testDateTime(someDate.plusDays(-365))(of(2010, 2, 28))
    testDateTime(someDate.plusDays(-1))(of(2011, 2, 27))
    testDateTime(someDate.plusDays(1))(of(2011, 3, 1))
    testDateTime(someDate.plusDays(365))(of(2012, 2, 28))
    testDateTime(someDate.plusDays(366))(leapDate)
    testDateTime(leapDate.plusDays(-366))(someDate)
    testDateTime(leapDate.plusDays(-365))(of(2011, 3, 1))
    testDateTime(leapDate.plusDays(-1))(of(2012, 2, 28))
    testDateTime(leapDate.plusDays(1))(of(2012, 3, 1))
    testDateTime(leapDate.plusDays(365))(of(2013, 2, 28))
    testDateTime(leapDate.plusDays(366))(of(2013, 3, 1))
    testDateTime(MIN.plusDays(730484999633L))(MAX)
    testDateTime(MAX.plusDays(-730484999633L))(MIN)
    expectThrows(classOf[DateTimeException], MIN.plusDays(-1))
    expectThrows(classOf[DateTimeException], MIN.plusDays(730484999634L))
    expectThrows(classOf[DateTimeException], MAX.plusDays(-730484999634L))
    expectThrows(classOf[DateTimeException], MAX.plusDays(1))
    expectThrows(classOf[ArithmeticException],
        ofEpochDay(-1).plusDays(Long.MinValue))
    expectThrows(classOf[DateTimeException],
        ofEpochDay(0).plusDays(Long.MinValue))
    expectThrows(classOf[DateTimeException],
        ofEpochDay(0).plusDays(Long.MaxValue))
    expectThrows(classOf[ArithmeticException],
        ofEpochDay(1).plusDays(Long.MaxValue))
  }

  @Test def test_minusYears(): Unit = {
    for (d <- samples)
      testDateTime(d.minusYears(0))(d)
    testDateTime(someDate.minusYears(2))(of(2009, 2, 28))
    testDateTime(someDate.minusYears(1))(of(2010, 2, 28))
    testDateTime(someDate.minusYears(-1))(of(2012, 2, 28))
    testDateTime(someDate.minusYears(-2))(of(2013, 2, 28))
    testDateTime(leapDate.minusYears(2))(of(2010, 2, 28))
    testDateTime(leapDate.minusYears(1))(someDate)
    testDateTime(leapDate.minusYears(-1))(of(2013, 2, 28))
    testDateTime(leapDate.minusYears(-2))(of(2014, 2, 28))
    testDateTime(MIN.minusYears(-1999999998))(of(999999999, 1, 1))
    testDateTime(MAX.minusYears(1999999998))(of(-999999999, 12, 31))
    expectThrows(classOf[DateTimeException], MIN.minusYears(1))
    expectThrows(classOf[DateTimeException], MIN.minusYears(-1999999999))
    expectThrows(classOf[DateTimeException], MAX.minusYears(1999999999))
    expectThrows(classOf[DateTimeException], MAX.minusYears(-1))
    expectThrows(classOf[DateTimeException], MIN.minusYears(Long.MaxValue))
    expectThrows(classOf[DateTimeException], MAX.minusYears(Long.MinValue))
  }

  @Test def test_minusMonths(): Unit = {
    for (d <- samples)
      testDateTime(d.minusMonths(0))(d)
    testDateTime(someDate.minusMonths(12))(of(2010, 2, 28))
    testDateTime(someDate.minusMonths(1))(of(2011, 1, 28))
    testDateTime(someDate.minusMonths(-1))(of(2011, 3, 28))
    testDateTime(someDate.minusMonths(-12))(of(2012, 2, 28))
    testDateTime(leapDate.minusMonths(12))(someDate)
    testDateTime(leapDate.minusMonths(1))(of(2012, 1, 29))
    testDateTime(leapDate.minusMonths(-1))(of(2012, 3, 29))
    testDateTime(leapDate.minusMonths(-12))(of(2013, 2, 28))
    testDateTime(of(2011, 1, 31).minusMonths(-1))(someDate)
    testDateTime(of(2011, 3, 31).minusMonths(1))(someDate)
    testDateTime(of(2011, 3, 31).minusMonths(-1))(of(2011, 4, 30))
    testDateTime(of(2012, 1, 31).minusMonths(-1))(leapDate)
    testDateTime(of(2012, 3, 31).minusMonths(1))(leapDate)
    testDateTime(of(2012, 3, 31).minusMonths(-1))(of(2012, 4, 30))
    testDateTime(MIN.minusMonths(-23999999987L))(of(999999999, 12, 1))
    testDateTime(MAX.minusMonths(23999999987L))(of(-999999999, 1, 31))
    expectThrows(classOf[DateTimeException], MIN.minusMonths(1))
    expectThrows(classOf[DateTimeException], MIN.minusMonths(-23999999988L))
    expectThrows(classOf[DateTimeException], MAX.minusMonths(23999999988L))
    expectThrows(classOf[DateTimeException], MAX.minusMonths(-1))
    expectThrows(classOf[DateTimeException], MIN.minusMonths(Long.MaxValue))
    expectThrows(classOf[DateTimeException], MAX.minusMonths(Long.MinValue))
  }

  @Test def test_minusWeeks(): Unit = {
    for (d <- samples)
      testDateTime(d.minusWeeks(0))(d)
    testDateTime(someDate.minusWeeks(53))(of(2010, 2, 22))
    testDateTime(someDate.minusWeeks(52))(of(2010, 3, 1))
    testDateTime(someDate.minusWeeks(1))(of(2011, 2, 21))
    testDateTime(someDate.minusWeeks(-1))(of(2011, 3, 7))
    testDateTime(someDate.minusWeeks(-52))(of(2012, 2, 27))
    testDateTime(someDate.minusWeeks(-53))(of(2012, 3, 5))
    testDateTime(leapDate.minusWeeks(53))(of(2011, 2, 23))
    testDateTime(leapDate.minusWeeks(52))(of(2011, 3, 2))
    testDateTime(leapDate.minusWeeks(1))(of(2012, 2, 22))
    testDateTime(leapDate.minusWeeks(-1))(of(2012, 3, 7))
    testDateTime(leapDate.minusWeeks(-52))(of(2013, 2, 27))
    testDateTime(leapDate.minusWeeks(-53))(of(2013, 3, 6))
    testDateTime(MIN.minusWeeks(-104354999947L))(of(999999999, 12, 27))
    testDateTime(MAX.minusWeeks(104354999947L))(of(-999999999, 1, 5))
    expectThrows(classOf[DateTimeException], MIN.minusWeeks(1))
    expectThrows(classOf[DateTimeException], MIN.minusWeeks(-104354999948L))
    expectThrows(classOf[DateTimeException], MAX.minusWeeks(1043549999478L))
    expectThrows(classOf[DateTimeException], MAX.minusWeeks(-1))
    expectThrows(classOf[ArithmeticException], MIN.minusWeeks(Long.MaxValue))
    expectThrows(classOf[ArithmeticException], MAX.minusWeeks(Long.MinValue))
  }

  @Test def test_minusDays(): Unit = {
    for (d <- samples)
      testDateTime(d.minusDays(0))(d)
    testDateTime(someDate.minusDays(365))(of(2010, 2, 28))
    testDateTime(someDate.minusDays(1))(of(2011, 2, 27))
    testDateTime(someDate.minusDays(-1))(of(2011, 3, 1))
    testDateTime(someDate.minusDays(-365))(of(2012, 2, 28))
    testDateTime(someDate.minusDays(-366))(leapDate)
    testDateTime(leapDate.minusDays(366))(someDate)
    testDateTime(leapDate.minusDays(365))(of(2011, 3, 1))
    testDateTime(leapDate.minusDays(1))(of(2012, 2, 28))
    testDateTime(leapDate.minusDays(-1))(of(2012, 3, 1))
    testDateTime(leapDate.minusDays(-365))(of(2013, 2, 28))
    testDateTime(leapDate.minusDays(-366))(of(2013, 3, 1))
    testDateTime(MIN.minusDays(-730484999633L))(MAX)
    testDateTime(MAX.minusDays(730484999633L))(MIN)
    expectThrows(classOf[DateTimeException], MIN.minusDays(1))
    expectThrows(classOf[DateTimeException], MIN.minusDays(-730484999634L))
    expectThrows(classOf[DateTimeException], MAX.minusDays(730484999634L))
    expectThrows(classOf[DateTimeException], MAX.minusDays(-1))
    expectThrows(classOf[ArithmeticException],
        ofEpochDay(-2).minusDays(Long.MaxValue))
    expectThrows(classOf[ArithmeticException],
        ofEpochDay(1).minusDays(Long.MinValue))
  }

  @Test def test_adjustInto(): Unit = {
    for {
      d1 <- samples
      d2 <- samples
    } {
      testDateTime(d1.adjustInto(d2))(d1)
    }

    val ts = Seq(LocalTime.MIN, LocalTime.MAX)
    for {
      d <- samples
      t <- ts
    } {
      expectThrows(classOf[DateTimeException], d.adjustInto(t))
    }
  }

  @Test def test_until(): Unit = {
    val samples1 = samples ++ Seq(of(2012, 1, 29), of(2012, 1, 30), of(2012, 2, 28),
        of(2013, 2, 28), of(2013, 3, 1), of(0, 12, 31), of(1, 1, 1))

    for {
      d <- samples1
      u <- dateBasedUnits
    } {
      assertEquals(0L, d.until(d, u))
    }

    assertEquals(730484999633L, MIN.until(MAX, DAYS))
    assertEquals(104354999947L, MIN.until(MAX, WEEKS))
    assertEquals(12L, someDate.until(leapDate, MONTHS))
    assertEquals(1L, of(2012, 1, 29).until(leapDate, MONTHS))
    assertEquals(0L, of(2012, 1, 30).until(leapDate, MONTHS))
    assertEquals(1999999998L, MIN.until(MAX, YEARS))
    assertEquals(1L, someDate.until(of(2012, 2, 28), YEARS))
    assertEquals(0L, leapDate.until(of(2013, 2, 28), YEARS))
    assertEquals(199999999L, MIN.until(MAX, DECADES))
    assertEquals(19999999L, MIN.until(MAX, CENTURIES))
    assertEquals(1999999L, MIN.until(MAX, MILLENNIA))
    assertEquals(1L, MIN.until(MAX, ERAS))
    assertEquals(1L, of(0, 12, 31).until(of(1, 1, 1), ERAS))

    for {
      d1 <- samples1
      d2 <- samples1 if d2.isAfter(d1)
      u <- dateBasedUnits
    } {
      assertEquals(-d1.until(d2, u), d2.until(d1, u))
    }

    for (d <- samples1)
      assertEquals(Period.ZERO, d.until(d))

    for {
      d1 <- samples1
      d2 <- samples1
      u <- timeBasedUnits
    } {
      expectThrows(classOf[UnsupportedTemporalTypeException], d1.until(d2, u))
    }

    assertEquals(Period.of(1999999998, 11, 30), MIN.until(MAX))
    assertEquals(Period.of(1, 0, 1), someDate.until(leapDate))
    assertEquals(Period.of(0, 11, 30), leapDate.until(of(2013, 2, 28)))
    assertEquals(Period.of(1, 0, 1), leapDate.until(of(2013, 3, 1)))
  }

  @Ignore("Disabled due to #2109")
  @Test def test_until_disabled(): Unit = {
    val samples1 = samples ++ Seq(of(2012, 1, 29), of(2012, 1, 30), of(2012, 2, 28),
        of(2013, 2, 28), of(2013, 3, 1), of(0, 12, 31), of(1, 1, 1))

    for {
      d1 <- samples1
      d2 <- samples1 if d2.isAfter(d1)
    } {
      assertEquals(d1.until(d2).negated, d2.until(d1))
    }
  }

  @Test def test_toEpochDay(): Unit = {
    assertEquals(-365243219162L, MIN.toEpochDay)
    assertEquals(-1L, of(1969, 12, 31).toEpochDay)
    assertEquals(0L, of(1970, 1, 1).toEpochDay)
    assertEquals(15033L, someDate.toEpochDay)
    assertEquals(15399L, leapDate.toEpochDay)
    assertEquals(365241780471L, MAX.toEpochDay)
  }

  @Test def test_compareTo(): Unit = {
    assertEquals(0, MIN.compareTo(MIN))
    assertTrue(MIN.compareTo(someDate) < 0)
    assertTrue(MIN.compareTo(MAX) < 0)
    assertTrue(someDate.compareTo(MIN) > 0)
    assertEquals(0, someDate.compareTo(someDate))
    assertTrue(someDate.compareTo(MAX) < 0)
    assertTrue(MAX.compareTo(MIN) > 0)
    assertTrue(MAX.compareTo(someDate) > 0)
    assertEquals(0, MAX.compareTo(MAX))
  }

  @Test def test_isAfter(): Unit = {
    assertFalse(MIN.isAfter(MIN))
    assertFalse(MIN.isAfter(someDate))
    assertFalse(MIN.isAfter(MAX))
    assertTrue(someDate.isAfter(MIN))
    assertFalse(someDate.isAfter(someDate))
    assertFalse(someDate.isAfter(MAX))
    assertTrue(MAX.isAfter(MIN))
    assertTrue(MAX.isAfter(someDate))
    assertFalse(MAX.isAfter(MAX))
  }

  @Test def test_isBefore(): Unit = {
    assertFalse(MIN.isBefore(MIN))
    assertTrue(MIN.isBefore(someDate))
    assertTrue(MIN.isBefore(MAX))
    assertFalse(someDate.isBefore(MIN))
    assertFalse(someDate.isBefore(someDate))
    assertTrue(someDate.isBefore(MAX))
    assertFalse(MAX.isBefore(MIN))
    assertFalse(MAX.isBefore(someDate))
    assertFalse(MAX.isBefore(MAX))
  }

  @Test def test_toString(): Unit = {
    assertEquals("-999999999-01-01", MIN.toString)
    assertEquals("-0001-12-31", of(-1, 12, 31).toString)
    assertEquals("0000-01-01", of(0, 1, 1).toString)
    assertEquals("2011-02-28", someDate.toString)
    assertEquals("2012-02-29", leapDate.toString)
    assertEquals("9999-12-31", of(9999, 12, 31).toString)
    assertEquals("+10000-01-01", of(10000, 1, 1).toString)
    assertEquals("+999999999-12-31", MAX.toString)
  }

  @Test def test_now(): Unit = {
    assertEquals(IsoEra.CE, now().getEra)
  }

  @Test def test_of(): Unit = {
    val years = Seq(Int.MinValue, -1000000000, -999999999, 0, 999999999,
        1000000000, Int.MaxValue)
    val days = Seq(Int.MinValue, 0, 1, 28, 29, 30, 31, 32, Int.MaxValue)

    for {
      year <- years
      month <- Month.values
      day <- days
    } {
      testDateTime(of(year, month, day))(of(year, month.getValue, day))
    }

    expectThrows(classOf[DateTimeException], of(Int.MinValue, 1, 1))
    expectThrows(classOf[DateTimeException], of(-1000000000, 1, 1))
    expectThrows(classOf[DateTimeException], of(2011, Int.MinValue, 1))
    expectThrows(classOf[DateTimeException], of(2011, 0, 1))
    expectThrows(classOf[DateTimeException], of(2011, 13, 1))
    expectThrows(classOf[DateTimeException], of(2011, Int.MaxValue, 1))

    for (month <- Month.values) {
      val m = month.getValue
      expectThrows(classOf[DateTimeException], of(2011, m, Int.MinValue))
      expectThrows(classOf[DateTimeException], of(2011, m, 0))
      expectThrows(classOf[DateTimeException],
          of(2011, m, month.length(false) + 1))
      expectThrows(classOf[DateTimeException],
          of(2012, m, month.length(true) + 1))
      expectThrows(classOf[DateTimeException], of(2011, m, Int.MaxValue))
    }
  }

  @Test def test_ofYearDay(): Unit = {
    testDateTime(ofYearDay(2011, 1))(of(2011, 1, 1))
    testDateTime(ofYearDay(2011, 31))(of(2011, 1, 31))
    testDateTime(ofYearDay(2011, 32))(of(2011, 2, 1))
    testDateTime(ofYearDay(2011, 59))(of(2011, 2, 28))
    testDateTime(ofYearDay(2011, 60))(of(2011, 3, 1))
    testDateTime(ofYearDay(2011, 90))(of(2011, 3, 31))
    testDateTime(ofYearDay(2011, 91))(of(2011, 4, 1))
    testDateTime(ofYearDay(2011, 120))(of(2011, 4, 30))
    testDateTime(ofYearDay(2011, 121))(of(2011, 5, 1))
    testDateTime(ofYearDay(2011, 151))(of(2011, 5, 31))
    testDateTime(ofYearDay(2011, 152))(of(2011, 6, 1))
    testDateTime(ofYearDay(2011, 181))(of(2011, 6, 30))
    testDateTime(ofYearDay(2011, 182))(of(2011, 7, 1))
    testDateTime(ofYearDay(2011, 212))(of(2011, 7, 31))
    testDateTime(ofYearDay(2011, 213))(of(2011, 8, 1))
    testDateTime(ofYearDay(2011, 243))(of(2011, 8, 31))
    testDateTime(ofYearDay(2011, 244))(of(2011, 9, 1))
    testDateTime(ofYearDay(2011, 273))(of(2011, 9, 30))
    testDateTime(ofYearDay(2011, 274))(of(2011, 10, 1))
    testDateTime(ofYearDay(2011, 304))(of(2011, 10, 31))
    testDateTime(ofYearDay(2011, 305))(of(2011, 11, 1))
    testDateTime(ofYearDay(2011, 334))(of(2011, 11, 30))
    testDateTime(ofYearDay(2011, 335))(of(2011, 12, 1))
    testDateTime(ofYearDay(2011, 365))(of(2011, 12, 31))
    testDateTime(ofYearDay(2012, 1))(of(2012, 1, 1))
    testDateTime(ofYearDay(2012, 31))(of(2012, 1, 31))
    testDateTime(ofYearDay(2012, 32))(of(2012, 2, 1))
    testDateTime(ofYearDay(2012, 60))(of(2012, 2, 29))
    testDateTime(ofYearDay(2012, 61))(of(2012, 3, 1))
    testDateTime(ofYearDay(2012, 91))(of(2012, 3, 31))
    testDateTime(ofYearDay(2012, 92))(of(2012, 4, 1))
    testDateTime(ofYearDay(2012, 121))(of(2012, 4, 30))
    testDateTime(ofYearDay(2012, 122))(of(2012, 5, 1))
    testDateTime(ofYearDay(2012, 152))(of(2012, 5, 31))
    testDateTime(ofYearDay(2012, 153))(of(2012, 6, 1))
    testDateTime(ofYearDay(2012, 182))(of(2012, 6, 30))
    testDateTime(ofYearDay(2012, 183))(of(2012, 7, 1))
    testDateTime(ofYearDay(2012, 213))(of(2012, 7, 31))
    testDateTime(ofYearDay(2012, 214))(of(2012, 8, 1))
    testDateTime(ofYearDay(2012, 244))(of(2012, 8, 31))
    testDateTime(ofYearDay(2012, 245))(of(2012, 9, 1))
    testDateTime(ofYearDay(2012, 274))(of(2012, 9, 30))
    testDateTime(ofYearDay(2012, 275))(of(2012, 10, 1))
    testDateTime(ofYearDay(2012, 305))(of(2012, 10, 31))
    testDateTime(ofYearDay(2012, 306))(of(2012, 11, 1))
    testDateTime(ofYearDay(2012, 335))(of(2012, 11, 30))
    testDateTime(ofYearDay(2012, 336))(of(2012, 12, 1))
    testDateTime(ofYearDay(2012, 366))(of(2012, 12, 31))

    expectThrows(classOf[DateTimeException], ofYearDay(Int.MinValue, 1))
    expectThrows(classOf[DateTimeException], ofYearDay(-1000000000, 1))
    expectThrows(classOf[DateTimeException], ofYearDay(1000000000, 1))
    expectThrows(classOf[DateTimeException], ofYearDay(Int.MaxValue, 1))
    expectThrows(classOf[DateTimeException], ofYearDay(2011, Int.MinValue))
    expectThrows(classOf[DateTimeException], ofYearDay(2011, 0))
    expectThrows(classOf[DateTimeException], ofYearDay(2011, 366))
    expectThrows(classOf[DateTimeException], ofYearDay(2012, 367))
    expectThrows(classOf[DateTimeException], ofYearDay(2011, Int.MaxValue))
  }

  @Test def test_ofEpochDay(): Unit = {
    testDateTime(ofEpochDay(-365243219162L))(MIN)
    testDateTime(ofEpochDay(-1))(of(1969, 12, 31))
    testDateTime(ofEpochDay(0))(of(1970, 1, 1))
    testDateTime(ofEpochDay(1))(of(1970, 1, 2))
    testDateTime(ofEpochDay(365241780471L))(MAX)

    expectThrows(classOf[DateTimeException], ofEpochDay(Long.MinValue))
    expectThrows(classOf[DateTimeException], ofEpochDay(-365243219163L))
    expectThrows(classOf[DateTimeException], ofEpochDay(365241780472L))
    expectThrows(classOf[DateTimeException], ofEpochDay(Long.MaxValue))
  }

  @Test def test_from(): Unit = {
    for (d <- samples)
      testDateTime(from(d))(d)

    for (t <- Seq(LocalTime.MIN, LocalTime.NOON, LocalTime.MAX))
      expectThrows(classOf[DateTimeException], from(t))
  }
}
