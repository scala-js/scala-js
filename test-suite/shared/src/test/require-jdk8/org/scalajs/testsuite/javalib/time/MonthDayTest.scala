package org.scalajs.testsuite.javalib.time

import java.time.chrono.IsoChronology
import java.time.temporal.{TemporalField, UnsupportedTemporalTypeException, ValueRange, ChronoField}
import java.time.{DateTimeException, LocalDate, Month, MonthDay}

import org.junit.Assert._
import org.junit.Test
import org.scalajs.testsuite.utils.AssertThrows._

/** Created by alonsodomin on 22/12/2015. */
class MonthDayTest extends TemporalAccessorTest[MonthDay] {
  import ChronoField._

  final val min = MonthDay.of(Month.JANUARY, 1)
  final val max = MonthDay.of(Month.DECEMBER, 31)
  final val leapMonth = MonthDay.of(Month.FEBRUARY, 29)

  val samples = Month.values().map(month => (month, month.minLength(), month.maxLength())).flatMap {
    case (month, minDay, maxDay) =>
      if (minDay != maxDay) Seq(MonthDay.of(month, 1), MonthDay.of(month, minDay), MonthDay.of(month, maxDay))
      else Seq(MonthDay.of(month, 1), MonthDay.of(month, minDay))
  }.toSeq

  val yearSamples = 2000 to 2020
  def leapYear(year: Int): Boolean = IsoChronology.INSTANCE.isLeapYear(year)

  override def isSupported(field: ChronoField): Boolean =
    field == ChronoField.MONTH_OF_YEAR || field == ChronoField.DAY_OF_MONTH

  override def expectedRangeFor(accessor: MonthDay, field: TemporalField): ValueRange = {
    field match {
      case DAY_OF_MONTH =>
        ValueRange.of(1, accessor.getMonth.minLength(), accessor.getMonth.maxLength())

      case _ =>
        super.expectedRangeFor(accessor, field)
    }
  }

  @Test def getLong(): Unit = {
    assertEquals(1L, min.getLong(MONTH_OF_YEAR))
    assertEquals(1L, min.getLong(DAY_OF_MONTH))

    assertEquals(12L, max.getLong(MONTH_OF_YEAR))
    assertEquals(31L, max.getLong(DAY_OF_MONTH))
  }

  @Test def getMonthValue(): Unit = {
    assertEquals(1, min.getMonthValue)
    assertEquals(12, max.getMonthValue)
  }

  @Test def getDayOfMonth(): Unit = {
    assertEquals(1, min.getDayOfMonth)
    assertEquals(31, max.getDayOfMonth)
  }

  @Test def getMonth(): Unit = {
    assertEquals(Month.JANUARY, min.getMonth)
    assertEquals(Month.DECEMBER, max.getMonth)
  }

  @Test def isValidYear(): Unit = {
    for {
      t <- samples
      y <- yearSamples
    } {
      if (t == leapMonth) {
        assertEquals(leapYear(y), t.isValidYear(y))
      } else {
        assertTrue(t.isValidYear(y))
      }
    }
  }

  @Test def `with`(): Unit = {
    assertEquals(min, min.`with`(Month.JANUARY))
    assertEquals(MonthDay.of(Month.FEBRUARY, 1), min.`with`(Month.FEBRUARY))
    assertEquals(max, max.`with`(Month.DECEMBER))
    assertEquals(MonthDay.of(Month.NOVEMBER, 30), max.`with`(Month.NOVEMBER))
    assertEquals(MonthDay.of(Month.FEBRUARY, 29), max.`with`(Month.FEBRUARY))
  }

  @Test def withMonth(): Unit = {
    assertEquals(min, min.withMonth(1))
    assertEquals(MonthDay.of(Month.FEBRUARY, 1), min.withMonth(2))
    assertEquals(max, max.withMonth(12))
    assertEquals(MonthDay.of(Month.NOVEMBER, 30), max.withMonth(11))
    assertEquals(MonthDay.of(Month.FEBRUARY, 29), max.withMonth(2))

    for (t <- samples) {
      expectThrows(classOf[DateTimeException], t.withMonth(Int.MinValue))
      expectThrows(classOf[DateTimeException], t.withMonth(Int.MaxValue))
      expectThrows(classOf[DateTimeException], t.withMonth(0))
    }
  }

  @Test def withDayOfMonth(): Unit = {
    assertEquals(min, min.withDayOfMonth(1))
    assertEquals(MonthDay.of(Month.JANUARY, 31), min.withDayOfMonth(31))

    assertEquals(max, max.withDayOfMonth(31))
    assertEquals(MonthDay.of(Month.DECEMBER, 1), max.withDayOfMonth(1))

    for (t <- samples) {
      expectThrows(classOf[DateTimeException], t.withDayOfMonth(Int.MinValue))
      expectThrows(classOf[DateTimeException], t.withDayOfMonth(Int.MaxValue))
      expectThrows(classOf[DateTimeException], t.withDayOfMonth(t.getMonth.maxLength() + 1))
    }
  }

  @Test def adjustInto(): Unit = {
    // Intentionally using a leap year here to be able to test the full sample
    val leapYearDate = LocalDate.of(2016, 1, 1)
    for (t <- samples) {
      val expectedDate = LocalDate.of(leapYearDate.getYear,
          t.getMonthValue, t.getDayOfMonth)
      assertEquals(expectedDate, t.adjustInto(leapYearDate))
    }

    val nonLeapYearDate = LocalDate.of(2015, 1, 1)
    assertEquals(LocalDate.of(2015, 2, 28), leapMonth.adjustInto(nonLeapYearDate))
  }

  @Test def atYear(): Unit = {
    val years = Seq(-999999999, 0, 1, 999999999)
    for {
      y <- years
      t <- samples if !(t.getMonthValue == 2 && t.getDayOfMonth == 29)
    } {
      assertEquals(LocalDate.of(y, t.getMonthValue, t.getDayOfMonth), t.atYear(y))
    }

    val invalidYears = Seq(Int.MinValue, -1000000000, 1000000000, Int.MaxValue)
    for (t <- samples; y <- invalidYears) {
      expectThrows(classOf[DateTimeException], t.atYear(y))
    }

    for (y <- yearSamples) {
      val expectedDay = if (leapYear(y)) 29 else 28
      assertEquals(LocalDate.of(y, 2, expectedDay), leapMonth.atYear(y))
    }
  }

  @Test def compareTo(): Unit = {
    assertTrue(min.compareTo(min) == 0)
    assertTrue(min.compareTo(max) < 0)
    assertTrue(max.compareTo(min) > 0)
    assertTrue(max.compareTo(max) == 0)
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

  @Test def toStringOutput(): Unit = {
    for (t <- samples) {
      val expected = f"--${t.getMonthValue}%02d-${t.getDayOfMonth}%02d"
      assertEquals(expected, t.toString)
    }
  }

  @Test def now(): Unit = {
    val now = LocalDate.now()
    val monthDay = MonthDay.now()
    assertEquals(now.getMonthValue, monthDay.getMonthValue)
    assertEquals(now.getDayOfMonth, monthDay.getDayOfMonth)
  }

  @Test def ofMonth(): Unit = {
    expectThrows(classOf[NullPointerException], MonthDay.of(null, 1))
    expectThrows(classOf[DateTimeException], MonthDay.of(Month.JANUARY, Int.MinValue))
    expectThrows(classOf[DateTimeException], MonthDay.of(Month.JANUARY, Int.MaxValue))
    expectThrows(classOf[DateTimeException], MonthDay.of(Month.JANUARY, 32))
    expectThrows(classOf[DateTimeException], MonthDay.of(Month.FEBRUARY, 30))

    assertEquals(min, MonthDay.of(Month.JANUARY, 1))
    assertEquals(max, MonthDay.of(Month.DECEMBER, 31))
  }

  @Test def of(): Unit = {
    expectThrows(classOf[DateTimeException], MonthDay.of(Int.MinValue, 1))
    expectThrows(classOf[DateTimeException], MonthDay.of(Int.MaxValue, 1))

    assertEquals(min, MonthDay.of(1, 1))
    assertEquals(max, MonthDay.of(12, 31))
  }

  @Test def from(): Unit = {
    assertSame(min, MonthDay.from(min))
    assertSame(max, MonthDay.from(max))

    val now = LocalDate.now()
    assertEquals(MonthDay.of(now.getMonthValue, now.getDayOfMonth), MonthDay.from(now))
  }

}
