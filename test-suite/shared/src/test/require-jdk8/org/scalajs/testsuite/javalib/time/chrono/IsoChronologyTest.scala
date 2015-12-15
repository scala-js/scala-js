package org.scalajs.testsuite.javalib.time.chrono

import java.time.{Period, LocalDate, DateTimeException}
import java.time.chrono.{IsoChronology, IsoEra}
import java.time.temporal.ChronoField

import org.junit.Test
import org.junit.Assert._
import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.javalib.time.DateTimeTestUtil._

class IsoChronologyTest {
  final val iso = IsoChronology.INSTANCE

  @Test def test_getId(): Unit = {
    assertEquals("ISO", iso.getId)
  }

  @Test def test_getCalendarType(): Unit = {
    assertEquals("iso8601", iso.getCalendarType)
  }

  @Test def test_date(): Unit = {
    val years = Seq(Int.MinValue, -1000000000, -999999999, -1, 0,
        1, 999999999, 1000000000, Int.MaxValue)
    val months = Seq(Int.MinValue, 0, 1, 12, 13, Int.MaxValue)
    val days = Seq(Int.MinValue, 0, 1, 28, 29, 30, 31, 32, Int.MaxValue)

    for {
      year <- years
      month <- months
      day <- days
    } {
      testDateTime(iso.date(IsoEra.CE, year, month, day))(LocalDate.of(year, month, day))
      testDateTime(iso.date(IsoEra.BCE, 1 - year, month, day))(LocalDate.of(year, month, day))
      testDateTime(iso.date(year, month, day))(LocalDate.of(year, month, day))
      expectThrows(classOf[ClassCastException], iso.date(null, year, month, day))
    }
  }

  @Test def test_dateYearDay(): Unit = {
    val years = Seq(Int.MinValue, -1000000000, -999999999, -1, 0,
        1, 999999999, 1000000000, Int.MaxValue)
    val months = Seq(Int.MinValue, 0, 1, 12, 13, Int.MaxValue)
    val days = Seq(Int.MinValue, 0, 1, 365, 366, Int.MaxValue)

    for {
      year <- years
      day <- days
    } {
      testDateTime(iso.dateYearDay(IsoEra.CE, year, day))(LocalDate.ofYearDay(year, day))
      testDateTime(iso.dateYearDay(IsoEra.BCE, 1 - year, day))(LocalDate.ofYearDay(year, day))
      testDateTime(iso.dateYearDay(year, day))(LocalDate.ofYearDay(year, day))
      expectThrows(classOf[ClassCastException], iso.dateYearDay(null, year, day))
    }
  }

  @Test def test_dateEpochDay(): Unit = {
    for (day <- Seq(Long.MinValue, -365243219163L, -365243219162L, -1L, 0L,
        1L, 365241780471L, 365241780472L, Long.MaxValue)) {
      testDateTime(iso.dateEpochDay(day))(LocalDate.ofEpochDay(day))
    }
  }

  @Test def test_dateNow(): Unit = {
    assertEquals(IsoEra.CE, iso.dateNow().getEra)
  }

  @Test def test_isLeapYear(): Unit = {
    for (year <- Seq(Int.MinValue, -400, -104, -96, -4, 0, 4, 1896, 1904,
        1996, 2000, 2004, 2147483644)) {
      assertTrue(iso.isLeapYear(year))
    }
    for (year <- Seq(-2147483647, -100, -99, -1, 1, 1900, 1999, 2001, 2002,
        2003, 2005, Int.MaxValue)) {
      assertFalse(iso.isLeapYear(year))
    }
  }

  @Test def test_prolepticYear(): Unit = {
    for (year <- Seq(-Int.MinValue, -1, 0, 1, Int.MaxValue)) {
      assertEquals(year, iso.prolepticYear(IsoEra.CE, year))
      assertEquals(1 - year, iso.prolepticYear(IsoEra.BCE, year))
    }
  }

  @Test def test_eraOf(): Unit = {
    assertEquals(IsoEra.BCE, iso.eraOf(0))
    assertEquals(IsoEra.CE, iso.eraOf(1))

    for (n <- Seq(-Int.MinValue, -1, 2, Int.MaxValue))
      expectThrows(classOf[DateTimeException], iso.eraOf(n))
  }

  @Test def test_eras(): Unit = {
    val eras = iso.eras
    assertEquals(2, eras.size)
    assertEquals(IsoEra.BCE, eras.get(0))
    assertEquals(IsoEra.CE, eras.get(1))
  }

  @Test def test_range(): Unit = {
    for (f <- ChronoField.values)
      assertEquals(f.range, iso.range(f))
  }

  @Test def test_period(): Unit = {
    val yearss = Seq(Int.MinValue, -1, 0, 1, Int.MaxValue)
    val monthss = Seq(Int.MinValue, -1, 0, 1, Int.MaxValue)
    val dayss = Seq(Int.MinValue, -1, 0, 1, Int.MaxValue)

    for {
      years <- yearss
      months <- monthss
      days <- dayss
    } {
      assertEquals(Period.of(years, months, days), iso.period(years, months, days))
    }
  }

  @Test def test_toString(): Unit = {
    assertEquals("ISO", iso.toString)
  }
}
