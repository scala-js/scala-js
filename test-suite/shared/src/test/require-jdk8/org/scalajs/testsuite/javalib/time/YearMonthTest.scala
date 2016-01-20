package org.scalajs.testsuite.javalib.time

import java.time._
import java.time.temporal.{TemporalField, ValueRange, ChronoUnit, ChronoField}

import org.junit.Test
import org.junit.Assert._
import org.scalajs.testsuite.utils.AssertThrows._

/** Created by alonsodomin on 25/12/2015. */
class YearMonthTest extends TemporalTest[YearMonth] {
  import DateTimeTestUtil._
  import ChronoField._
  import ChronoUnit._

  val min = YearMonth.of(-999999999, 1)
  val max = YearMonth.of(999999999, 12)
  val janLastBC = YearMonth.of(0, 1)
  val decLastBC = YearMonth.of(0, 12)
  val janFirstAC = YearMonth.of(1, 1)
  val decFirstAC = YearMonth.of(1, 12)
  val someYearMonth = YearMonth.of(2015, 6)
  val febLeapYear = YearMonth.of(2016, 2)
  val febNonLeapYear = YearMonth.of(2014, 2)

  val samples = Seq(min, max, janLastBC, decLastBC, janFirstAC, decFirstAC,
      someYearMonth, febLeapYear, febNonLeapYear)

  override def isSupported(field: ChronoField): Boolean = {
    field == YEAR || field == YEAR_OF_ERA || field == MONTH_OF_YEAR ||
    field == PROLEPTIC_MONTH || field == ERA
  }

  override def isSupported(unit: ChronoUnit): Boolean = {
    unit == MONTHS || unit == YEARS || unit == DECADES ||
    unit == CENTURIES || unit == MILLENNIA || unit == ERAS
  }

  override def expectedRangeFor(accessor: YearMonth, field: TemporalField): ValueRange = {
    field match {
      case YEAR_OF_ERA =>
        if (accessor.getYear <= 0) ValueRange.of(1, 1000000000)
        else ValueRange.of(1, 999999999)

      case _ =>
        super.expectedRangeFor(accessor, field)
    }
  }

  @Test def getLong(): Unit = {
    for (ym <- samples) {
      assertEquals(ym.getYear.toLong, ym.getLong(YEAR))
      assertEquals(ym.getMonthValue.toLong, ym.getLong(MONTH_OF_YEAR))
      assertEquals(ym.getMonth.getValue.toLong, ym.getLong(MONTH_OF_YEAR))
    }

    assertEquals(-11999999988L, min.getLong(PROLEPTIC_MONTH))
    assertEquals(1000000000L, min.getLong(YEAR_OF_ERA))
    assertEquals(11999999999L, max.getLong(PROLEPTIC_MONTH))
    assertEquals(999999999L, max.getLong(YEAR_OF_ERA))

    assertEquals(24193L, febLeapYear.getLong(PROLEPTIC_MONTH))
    assertEquals(24185L, someYearMonth.getLong(PROLEPTIC_MONTH))

    assertEquals(1L, decLastBC.getLong(YEAR_OF_ERA))
    assertEquals(1L, janFirstAC.getLong(YEAR_OF_ERA))
    assertEquals(0L, janLastBC.getLong(PROLEPTIC_MONTH))
    assertEquals(11L, decLastBC.getLong(PROLEPTIC_MONTH))
    assertEquals(12L, janFirstAC.getLong(PROLEPTIC_MONTH))
    assertEquals(23L, decFirstAC.getLong(PROLEPTIC_MONTH))
    assertEquals(0L, decLastBC.getLong(ERA))
    assertEquals(1L, janFirstAC.getLong(ERA))
  }

  @Test def isLeapYear(): Unit = {
    assertFalse(someYearMonth.isLeapYear)
    assertTrue(febLeapYear.isLeapYear)
    assertFalse(febNonLeapYear.isLeapYear)
  }

  @Test def isValidDay(): Unit = {
    for (ym <- samples) {
      assertFalse(ym.isValidDay(0))
      assertTrue(ym.isValidDay(ym.lengthOfMonth()))
      assertFalse(ym.isValidDay(ym.lengthOfMonth() + 1))
    }

    assertTrue(febLeapYear.isValidDay(29))
    assertFalse(febNonLeapYear.isValidDay(29))
    assertFalse(someYearMonth.isValidDay(31))
  }

  @Test def lenghtOfMonth(): Unit = {
    assertEquals(31, min.lengthOfMonth())
    assertEquals(31, max.lengthOfMonth())
    assertEquals(30, someYearMonth.lengthOfMonth())
    assertEquals(29, febLeapYear.lengthOfMonth())
    assertEquals(28, febNonLeapYear.lengthOfMonth())
  }

  @Test def lengthOfYear(): Unit = {
    assertEquals(365, someYearMonth.lengthOfYear())
    assertEquals(366, febLeapYear.lengthOfYear())
    assertEquals(365, febNonLeapYear.lengthOfYear())
  }

  @Test def `with`(): Unit = {
    testDateTime(max.`with`(YEAR, 999999999))(max)
    testDateTime(max.`with`(YEAR, -999999999))(YearMonth.of(-999999999, 12))
    testDateTime(max.`with`(MONTH_OF_YEAR, 1))(YearMonth.of(999999999, 1))
    testDateTime(max.`with`(MONTH_OF_YEAR, 12))(max)
    testDateTime(max.`with`(PROLEPTIC_MONTH, 0))(YearMonth.of(0, 1))
    testDateTime(max.`with`(PROLEPTIC_MONTH, -1))(YearMonth.of(-1, 12))
    testDateTime(max.`with`(ERA, 1))(max)
    testDateTime(max.`with`(ERA, 0))(YearMonth.of(-999999998, 12))

    testDateTime(min.`with`(YEAR, 999999999))(YearMonth.of(999999999, 1))
    testDateTime(min.`with`(YEAR, -999999999))(min)
    testDateTime(min.`with`(MONTH_OF_YEAR, 1))(min)
    testDateTime(min.`with`(MONTH_OF_YEAR, 12))(YearMonth.of(-999999999, 12))
    testDateTime(min.`with`(PROLEPTIC_MONTH, 0))(YearMonth.of(0, 1))
    testDateTime(min.`with`(PROLEPTIC_MONTH, -1))(YearMonth.of(-1, 12))
    testDateTime(min.`with`(ERA, 1))(YearMonth.of(0, 12))
    testDateTime(min.`with`(ERA, 0))(min)

    testDateTime(someYearMonth.`with`(YEAR, 2000))(YearMonth.of(2000, 6))
    testDateTime(someYearMonth.`with`(MONTH_OF_YEAR, 1))(YearMonth.of(2015, 1))
    testDateTime(someYearMonth.`with`(ERA, 0))(YearMonth.of(-2014, 6))

    for {
      ym1 <- samples
      ym2 <- samples
    } {
      testDateTime(ym1.`with`(PROLEPTIC_MONTH, ym2.getLong(PROLEPTIC_MONTH)))(ym2)
    }

    for (ym <- samples) {
      expectThrows(classOf[DateTimeException], ym.`with`(YEAR, Long.MinValue))
      expectThrows(classOf[DateTimeException], ym.`with`(YEAR, -1000000000L))
      expectThrows(classOf[DateTimeException], ym.`with`(YEAR, 1000000000L))
      expectThrows(classOf[DateTimeException], ym.`with`(YEAR, Long.MaxValue))

      expectThrows(classOf[DateTimeException], ym.`with`(MONTH_OF_YEAR, Long.MinValue))
      expectThrows(classOf[DateTimeException], ym.`with`(MONTH_OF_YEAR, 0L))
      expectThrows(classOf[DateTimeException], ym.`with`(MONTH_OF_YEAR, 13L))
      expectThrows(classOf[DateTimeException], ym.`with`(MONTH_OF_YEAR, Long.MaxValue))

      expectThrows(classOf[DateTimeException], ym.`with`(PROLEPTIC_MONTH, Long.MinValue))
      expectThrows(classOf[DateTimeException], ym.`with`(PROLEPTIC_MONTH, -11999999989L))
      expectThrows(classOf[DateTimeException], ym.`with`(PROLEPTIC_MONTH, 12000000000L))
      expectThrows(classOf[DateTimeException], ym.`with`(PROLEPTIC_MONTH, Long.MaxValue))

      expectThrows(classOf[DateTimeException], ym.`with`(ERA, Long.MinValue))
      expectThrows(classOf[DateTimeException], ym.`with`(ERA, -1L))
      expectThrows(classOf[DateTimeException], ym.`with`(ERA, -2L))
      expectThrows(classOf[DateTimeException], ym.`with`(ERA, Long.MaxValue))
    }
  }

  @Test def withYear(): Unit = {
    assertSame(min, min.withYear(-999999999))
    assertEquals(YearMonth.of(999999999, 1), min.withYear(999999999))

    assertSame(max, max.withYear(999999999))
    assertEquals(YearMonth.of(-999999999, 12), max.withYear(-999999999))

    assertEquals(YearMonth.of(1, 1), janLastBC.withYear(1))
    assertEquals(YearMonth.of(1, 12), decLastBC.withYear(1))
    assertEquals(YearMonth.of(0, 1), janFirstAC.withYear(0))
    assertEquals(YearMonth.of(0, 12), decFirstAC.withYear(0))

    for (ym <- samples) {
      expectThrows(classOf[DateTimeException], ym.withYear(Int.MinValue))
      expectThrows(classOf[DateTimeException], ym.withYear(-1000000000))
      expectThrows(classOf[DateTimeException], ym.withYear(1000000000))
      expectThrows(classOf[DateTimeException], ym.withYear(Int.MaxValue))
    }
  }

  @Test def withMonth(): Unit = {
    assertSame(min, min.withMonth(1))
    assertSame(max, max.withMonth(12))

    assertEquals(YearMonth.of(0, 12), janLastBC.withMonth(12))
    assertEquals(YearMonth.of(0, 1), decLastBC.withMonth(1))
    assertEquals(YearMonth.of(1, 12), janFirstAC.withMonth(12))
    assertEquals(YearMonth.of(1, 1), decFirstAC.withMonth(1))

    for (ym <- samples) {
      expectThrows(classOf[DateTimeException], ym.withMonth(Int.MinValue))
      expectThrows(classOf[DateTimeException], ym.withMonth(0))
      expectThrows(classOf[DateTimeException], ym.withMonth(13))
      expectThrows(classOf[DateTimeException], ym.withMonth(Int.MaxValue))
    }
  }

  @Test def plus(): Unit = {
    for (ym <- samples;n <- sampleLongs) {
      testDateTime(ym.plus(n, YEARS))(ym.plusYears(n))
      testDateTime(ym.plus(n, MONTHS))(ym.plusMonths(n))
      testDateTime(ym.plus(n, DECADES))(ym.plusYears(Math.multiplyExact(n, 10)))
      testDateTime(ym.plus(n, CENTURIES))(ym.plusYears(Math.multiplyExact(n, 100)))
      testDateTime(ym.plus(n, MILLENNIA))(ym.plusYears(Math.multiplyExact(n, 1000)))
      testDateTime(ym.plus(n, ERAS))(ym.`with`(ERA, Math.addExact(n, ym.getLong(ERA))))
    }
  }

  @Test def plusYears(): Unit = {
    for (ym <- samples) {
      assertSame(ym, ym.plusYears(0))
    }

    assertEquals(YearMonth.of(0, 1), min.plusYears(max.getYear))
    assertEquals(YearMonth.of(0, 12), max.plusYears(min.getYear))
    assertEquals(febLeapYear, febNonLeapYear.plusYears(2))

    assertEquals(YearMonth.of(999999999, 1), min.plusYears(1999999998))
    assertEquals(YearMonth.of(-999999999, 12), max.plusYears(-1999999998))

    expectThrows(classOf[DateTimeException], min.plusYears(-1))
    expectThrows(classOf[DateTimeException], min.plusYears(1999999999))
    expectThrows(classOf[DateTimeException], max.plusYears(1))
    expectThrows(classOf[DateTimeException], max.plusYears(-1999999999))
    expectThrows(classOf[DateTimeException], min.plusYears(Long.MinValue))
    expectThrows(classOf[DateTimeException], max.plusYears(Long.MaxValue))
  }

  @Test def plusMonths(): Unit = {
    for (ym <- samples) {
      assertSame(ym, ym.plusMonths(0))
    }

    assertEquals(janFirstAC, decLastBC.plusMonths(1))
    assertEquals(someYearMonth, janLastBC.plusMonths(24185))
    assertEquals(janFirstAC, min.plusMonths(max.getLong(PROLEPTIC_MONTH) + 1))
    assertEquals(janFirstAC, max.plusMonths(min.getLong(PROLEPTIC_MONTH) + 1))

    assertEquals(YearMonth.of(999999999, 12), min.plusMonths(23999999987L))
    assertEquals(YearMonth.of(-999999999, 1), max.plusMonths(-23999999987L))

    expectThrows(classOf[DateTimeException], min.plusMonths(-1))
    expectThrows(classOf[DateTimeException], min.plusMonths(23999999988L))
    expectThrows(classOf[DateTimeException], max.plusMonths(-23999999988L))
    expectThrows(classOf[DateTimeException], max.plusMonths(1))
    expectThrows(classOf[DateTimeException], min.plusMonths(Long.MinValue))
    expectThrows(classOf[DateTimeException], max.plusMonths(Long.MaxValue))
  }

  @Test def minusYears(): Unit = {
    for (ym <- samples) {
      assertSame(ym, ym.minusYears(0))
    }

    assertEquals(febNonLeapYear, febLeapYear.minusYears(2))

    assertEquals(YearMonth.of(999999999, 1), min.minusYears(-1999999998))
    assertEquals(YearMonth.of(-999999999, 12), max.minusYears(1999999998))

    expectThrows(classOf[DateTimeException], min.minusYears(1))
    expectThrows(classOf[DateTimeException], min.minusYears(-1999999999))
    expectThrows(classOf[DateTimeException], max.minusYears(-1))
    expectThrows(classOf[DateTimeException], max.minusYears(1999999999))
    expectThrows(classOf[DateTimeException], min.minusYears(Long.MaxValue))
    expectThrows(classOf[DateTimeException], max.minusYears(Long.MinValue))
  }

  @Test def minusMonths(): Unit = {
    for (ym <- samples) {
      assertSame(ym, ym.minusMonths(0))
    }

    assertEquals(decLastBC, janFirstAC.minusMonths(1))
    assertEquals(janLastBC, someYearMonth.minusMonths(24185))

    assertEquals(YearMonth.of(999999999, 12), min.minusMonths(-23999999987L))
    assertEquals(YearMonth.of(-999999999, 1), max.minusMonths(23999999987L))

    expectThrows(classOf[DateTimeException], min.minusMonths(1))
    expectThrows(classOf[DateTimeException], min.minusMonths(-23999999988L))
    expectThrows(classOf[DateTimeException], max.minusMonths(23999999988L))
    expectThrows(classOf[DateTimeException], max.minusMonths(-1))
    expectThrows(classOf[DateTimeException], min.minusMonths(Long.MaxValue))
    expectThrows(classOf[DateTimeException], max.minusMonths(Long.MinValue))
  }

  @Test def adjustInto(): Unit = {
    for (ym1 <- samples; ym2 <- samples) {
      testDateTime(ym1.adjustInto(ym2))(ym1)
    }

    val someDate = LocalDate.of(2015, 1, 1)
    for (ym <- samples) {
      testDateTime(ym.adjustInto(someDate))(LocalDate.of(ym.getYear, ym.getMonthValue, 1))
    }
  }

  @Test def until(): Unit = {
    for {
      ym <- samples
      unit <- ChronoUnit.values() if isSupported(unit)
    } {
      assertEquals(0L, ym.until(ym, unit))
    }

    assertEquals(1999999998L, min.until(max, YEARS))
    assertEquals(23999999987L, min.until(max, MONTHS))
    assertEquals(-1999999998L, max.until(min, YEARS))
    assertEquals(-23999999987L, max.until(min, MONTHS))
    assertEquals(199999999L, min.until(max, DECADES))
    assertEquals(19999999L, min.until(max, CENTURIES))
    assertEquals(1999999L, min.until(max, MILLENNIA))
    assertEquals(1L, min.until(max, ERAS))

    assertEquals(1L, janLastBC.until(janFirstAC, YEARS))
    assertEquals(12L, janLastBC.until(janFirstAC, MONTHS))
    assertEquals(1L, janLastBC.until(decFirstAC, YEARS))
    assertEquals(23L, janLastBC.until(decFirstAC, MONTHS))
    assertEquals(0L, decLastBC.until(janFirstAC, YEARS))
    assertEquals(1L, decLastBC.until(janFirstAC, MONTHS))

    for {
      ym1 <- samples
      ym2 <- samples if ym2.isAfter(ym1)
      unit <- ChronoUnit.values() if isSupported(unit)
    } {
      assertEquals(-ym1.until(ym2, unit), ym2.until(ym1, unit))
    }
  }

  @Test def atDay(): Unit = {
    for {
      ym <- samples
      day <- 1 to ym.lengthOfMonth()
    } {
      assertEquals(LocalDate.of(ym.getYear, ym.getMonthValue, day), ym.atDay(day))
    }

    for (ym <- samples) {
      expectThrows(classOf[DateTimeException], ym.atDay(0))
      expectThrows(classOf[DateTimeException], ym.atDay(ym.lengthOfMonth() + 1))
    }
  }

  @Test def atEndOfMonth(): Unit = {
    for (ym <- samples) {
      val endOfMonth = ym.lengthOfMonth()
      assertEquals(LocalDate.of(ym.getYear, ym.getMonthValue, endOfMonth), ym.atEndOfMonth())
    }
  }

  @Test def compareTo(): Unit = {
    assertEquals(0, min.compareTo(min))
    assertEquals(-1999999998, min.compareTo(max))
    assertEquals(-11, min.compareTo(YearMonth.of(Year.MIN_VALUE, 12)))
    assertEquals(1999999998, max.compareTo(min))
    assertEquals(11, max.compareTo(YearMonth.of(Year.MAX_VALUE, 1)))
    assertEquals(0, max.compareTo(max))
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

  @Test def equalsHashCode(): Unit = {
    assertEquals(YearMonth.of(Year.MIN_VALUE, 1), min)
    assertEquals(YearMonth.of(Year.MIN_VALUE, 1).hashCode(), min.hashCode())
    assertEquals(YearMonth.of(Year.MAX_VALUE, 12), max)
    assertEquals(YearMonth.of(Year.MAX_VALUE, 12).hashCode(), max.hashCode())

    for {
      ym1 <- samples
      ym2 <- samples
    } {
      if (ym1.hashCode() == ym2.hashCode()) {
        assertTrue(ym1.equals(ym2))
      } else {
        assertFalse(ym1.equals(ym2))
      }
    }
  }

  @Test def toStringOutput(): Unit = {
    assertEquals("-999999999-01", min.toString)
    assertEquals("999999999-12", max.toString)
    assertEquals("-10000-01", YearMonth.of(-10000, 1).toString)
    assertEquals("10000-12", YearMonth.of(10000, 12).toString)
    assertEquals("2015-06", someYearMonth.toString)
    assertEquals("0000-01", janLastBC.toString)
    assertEquals("0000-12", decLastBC.toString)
    assertEquals("0001-01", janFirstAC.toString)
    assertEquals("0001-12", decFirstAC.toString)
  }

  @Test def ofMonth(): Unit = {
    val yearMonth = YearMonth.of(23, Month.JANUARY)
    assertEquals(23, yearMonth.getYear)
    assertEquals(1, yearMonth.getMonthValue)
    assertEquals(Month.JANUARY, yearMonth.getMonth)

    assertEquals(Year.MIN_VALUE, min.getYear)
    assertEquals(Month.JANUARY, min.getMonth)
    assertEquals(Year.MAX_VALUE, max.getYear)
    assertEquals(Month.DECEMBER, max.getMonth)

    for (ym <- samples) {
      assertEquals(ym, YearMonth.of(ym.getYear, ym.getMonth))
    }

    expectThrows(classOf[NullPointerException], YearMonth.of(0, null))
    for (m <- Month.values()) {
      expectThrows(classOf[DateTimeException], YearMonth.of(Int.MinValue, m))
      expectThrows(classOf[DateTimeException], YearMonth.of(Int.MaxValue, m))
    }
  }

  @Test def of(): Unit = {
    val yearMonth = YearMonth.of(293, 11)
    assertEquals(293, yearMonth.getYear)
    assertEquals(11, yearMonth.getMonthValue)
    assertEquals(Month.NOVEMBER, yearMonth.getMonth)

    assertEquals(Year.MIN_VALUE, min.getYear)
    assertEquals(1, min.getMonthValue)
    assertEquals(Year.MAX_VALUE, max.getYear)
    assertEquals(12, max.getMonthValue)

    for (ym <- samples) {
      assertEquals(ym, YearMonth.of(ym.getYear, ym.getMonthValue))
    }

    expectThrows(classOf[DateTimeException], YearMonth.of(Int.MinValue, 0))
    expectThrows(classOf[DateTimeException], YearMonth.of(Int.MaxValue, 0))
    expectThrows(classOf[DateTimeException], YearMonth.of(min.getYear, 0))
    expectThrows(classOf[DateTimeException], YearMonth.of(min.getYear, 13))
    expectThrows(classOf[DateTimeException], YearMonth.of(max.getYear, 0))
    expectThrows(classOf[DateTimeException], YearMonth.of(max.getYear, 13))
  }

  @Test def now(): Unit = {
    val now = LocalDate.now()
    val yearMonth = YearMonth.now()
    if (yearMonth.getMonthValue != now.getMonthValue) {
      println("Month changed in the middle of the test!")
      val newNow = LocalDate.now()
      assertEquals(newNow.getMonth, yearMonth.getMonth)
      assertEquals(newNow.getMonthValue, yearMonth.getMonthValue)
      assertEquals(newNow.getYear, yearMonth.getYear)
    }
  }

  @Test def from(): Unit = {
    for (ym <- samples) {
      assertSame(ym, YearMonth.from(ym))
    }

    val someDate = LocalDate.of(2015, 1, 1)
    assertEquals(YearMonth.of(2015, 1), YearMonth.from(someDate))

    expectThrows(classOf[DateTimeException], YearMonth.from(Month.JANUARY))
  }

}
