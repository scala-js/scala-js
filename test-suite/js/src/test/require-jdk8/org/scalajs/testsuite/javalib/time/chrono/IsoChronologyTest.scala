package org.scalajs.testsuite.javalib.time.chrono

import java.time.{Period, LocalDate, DateTimeException}
import java.time.chrono.{IsoChronology, IsoEra}
import java.time.temporal.ChronoField

import org.scalajs.testsuite.javalib.time.temporal.TemporalTest

object IsoChronologyTest extends TemporalTest {
  final val iso = IsoChronology.INSTANCE

  describe("java.time.chrono.IsoChronology") {
    it("should respond to `getId`") {
      expect(iso.getId).toEqual("ISO")
    }

    it("should respond to `getCalendarType`") {
      expect(iso.getCalendarType).toEqual("iso8601")
    }

    it("should respond to `date`") {
      val years = Seq(Int.MinValue, -1000000000, -999999999, -1, 0,
        1, 999999999, 1000000000, Int.MaxValue)
      val months = Seq(Int.MinValue, 0, 1, 12, 13, Int.MaxValue)
      val days = Seq(Int.MinValue, 0, 1, 28, 29, 30, 31, 32, Int.MaxValue)

      for {
        year <- years
        month <- months
        day <- days
      } {
        testTemporal(iso.date(IsoEra.CE, year, month, day))(LocalDate.of(year, month, day))
        testTemporal(iso.date(IsoEra.BCE, 1 - year, month, day))(LocalDate.of(year, month, day))
        testTemporal(iso.date(year, month, day))(LocalDate.of(year, month, day))
        expectThrows[ClassCastException](iso.date(null, year, month, day))
      }
    }

    it("should respond to `dateYearDay`") {
      val years = Seq(Int.MinValue, -1000000000, -999999999, -1, 0,
        1, 999999999, 1000000000, Int.MaxValue)
      val months = Seq(Int.MinValue, 0, 1, 12, 13, Int.MaxValue)
      val days = Seq(Int.MinValue, 0, 1, 365, 366, Int.MaxValue)

      for {
        year <- years
        day <- days
      } {
        testTemporal(iso.dateYearDay(IsoEra.CE, year, day))(LocalDate.ofYearDay(year, day))
        testTemporal(iso.dateYearDay(IsoEra.BCE, 1 - year, day))(LocalDate.ofYearDay(year, day))
        testTemporal(iso.dateYearDay(year, day))(LocalDate.ofYearDay(year, day))
        expectThrows[ClassCastException](iso.dateYearDay(null, year, day))
      }
    }

    it("should respond to `dateEpochDay`") {
      for (day <- Seq(Long.MinValue, -365243219163L, -365243219162L, -1L, 0L,
          1L, 365241780471L, 365241780472L, Long.MaxValue)) {
        testTemporal(iso.dateEpochDay(day))(LocalDate.ofEpochDay(day))
      }
    }

    it("should respond to `dateNow`") {
      expect(iso.dateNow().getEra == IsoEra.CE).toBeTruthy
    }

    it("should respond to `isLeapYear`") {
      for (year <- Seq(Int.MinValue, -400, -104, -96, -4, 0, 4, 1896, 1904,
          1996, 2000, 2004, 2147483644)) {
        expect(iso.isLeapYear(year)).toBeTruthy
      }
      for (year <- Seq(-2147483647, -100, -99, -1, 1, 1900, 1999, 2001, 2002,
          2003, 2005, Int.MaxValue)) {
        expect(iso.isLeapYear(year)).toBeFalsy
      }
    }

    it("should respond to `prolepticYear`") {
      for (year <- Seq(-Int.MinValue, -1, 0, 1, Int.MaxValue)) {
        expect(iso.prolepticYear(IsoEra.CE, year)).toEqual(year)
        expect(iso.prolepticYear(IsoEra.BCE, year)).toEqual(1 - year)
      }
    }

    it("should respond to `eraOf`") {
      expect(iso.eraOf(0) == IsoEra.BCE).toBeTruthy
      expect(iso.eraOf(1) == IsoEra.CE).toBeTruthy

      for (n <- Seq(-Int.MinValue, -1, 2, Int.MaxValue))
        expectThrows[DateTimeException](iso.eraOf(n))
    }

    it("should respond to `eras`") {
      val eras = iso.eras()

      expect(eras.size).toEqual(2)
      expect(eras.get(0) == IsoEra.BCE).toBeTruthy
      expect(eras.get(1) == IsoEra.CE).toBeTruthy
    }

    it("should respond to `range`") {
      for (f <- ChronoField.values)
        expect(iso.range(f) == f.range).toBeTruthy
    }

    it("should respond to `period`") {
      val yearss = Seq(Int.MinValue, -1, 0, 1, Int.MaxValue)
      val monthss = Seq(Int.MinValue, -1, 0, 1, Int.MaxValue)
      val dayss = Seq(Int.MinValue, -1, 0, 1, Int.MaxValue)

      for {
        years <- yearss
        months <- monthss
        days <- dayss
      } {
        expect(iso.period(years, months, days) ==
            Period.of(years, months, days)).toBeTruthy
      }
    }
  }
}
