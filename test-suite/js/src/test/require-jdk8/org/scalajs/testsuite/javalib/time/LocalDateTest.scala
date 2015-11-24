package org.scalajs.testsuite.javalib.time

import java.time._
import java.time.chrono.{IsoEra, IsoChronology}
import java.time.temporal._

import org.scalajs.testsuite.javalib.time.temporal.TemporalTest

object LocalDateTest extends TemporalTest {
  import LocalDate._
  import ChronoField._
  import ChronoUnit._

  val someDate = of(2011, 2, 28)
  val leapDate = of(2012, 2, 29)
  val samples = Seq(MIN, someDate, leapDate, MAX)

  describe("java.time.LocalDate") {
    testTemporalApi(samples: _*)

    it("should respond to `isSupported`") {
      for {
        d <- samples
        f <- ChronoField.values
      } {
        expect(d.isSupported(f) == f.isDateBased).toBeTruthy
      }

      for {
        d <- samples
        u <- ChronoUnit.values
      } {
        expect(d.isSupported(u) == u.isDateBased).toBeTruthy
      }
    }

    it("should respond to `getLong`") {
      for (d <- samples) {
        expect(d.getLong(DAY_OF_WEEK) == d.getDayOfWeek.getValue).toBeTruthy
        expect(d.getLong(DAY_OF_MONTH) == d.getDayOfMonth).toBeTruthy
        expect(d.getLong(DAY_OF_YEAR) == d.getDayOfYear).toBeTruthy
        expect(d.getLong(EPOCH_DAY) == d.toEpochDay).toBeTruthy
        expect(d.getLong(MONTH_OF_YEAR) == d.getMonthValue).toBeTruthy
        expect(d.getLong(YEAR) == d.getYear).toBeTruthy
        expect(d.getLong(ERA) == d.getEra.getValue).toBeTruthy
      }

      expect(MIN.getLong(ALIGNED_DAY_OF_WEEK_IN_MONTH) == 1).toBeTruthy
      expect(MIN.getLong(ALIGNED_DAY_OF_WEEK_IN_YEAR) == 1).toBeTruthy
      expect(MIN.getLong(ALIGNED_WEEK_OF_MONTH) == 1).toBeTruthy
      expect(MIN.getLong(ALIGNED_WEEK_OF_YEAR) == 1).toBeTruthy
      expect(MIN.getLong(PROLEPTIC_MONTH) == -11999999988L).toBeTruthy
      expect(MIN.getLong(YEAR_OF_ERA) == 1000000000).toBeTruthy

      expect(someDate.getLong(ALIGNED_DAY_OF_WEEK_IN_MONTH) == 7).toBeTruthy
      expect(someDate.getLong(ALIGNED_DAY_OF_WEEK_IN_YEAR) == 3).toBeTruthy
      expect(someDate.getLong(ALIGNED_WEEK_OF_MONTH) == 4).toBeTruthy
      expect(someDate.getLong(ALIGNED_WEEK_OF_YEAR) == 9).toBeTruthy
      expect(someDate.getLong(PROLEPTIC_MONTH) == 24133).toBeTruthy
      expect(someDate.getLong(YEAR_OF_ERA) == 2011).toBeTruthy

      expect(leapDate.getLong(ALIGNED_DAY_OF_WEEK_IN_MONTH) == 1).toBeTruthy
      expect(leapDate.getLong(ALIGNED_DAY_OF_WEEK_IN_YEAR) == 4).toBeTruthy
      expect(leapDate.getLong(ALIGNED_WEEK_OF_MONTH) == 5).toBeTruthy
      expect(leapDate.getLong(ALIGNED_WEEK_OF_YEAR) == 9).toBeTruthy
      expect(leapDate.getLong(PROLEPTIC_MONTH) == 24145).toBeTruthy
      expect(leapDate.getLong(YEAR_OF_ERA) == 2012).toBeTruthy

      expect(MAX.getLong(ALIGNED_DAY_OF_WEEK_IN_MONTH) == 3).toBeTruthy
      expect(MAX.getLong(ALIGNED_DAY_OF_WEEK_IN_YEAR) == 1).toBeTruthy
      expect(MAX.getLong(ALIGNED_WEEK_OF_MONTH) == 5).toBeTruthy
      expect(MAX.getLong(ALIGNED_WEEK_OF_YEAR) == 53).toBeTruthy
      expect(MAX.getLong(PROLEPTIC_MONTH) == 11999999999L).toBeTruthy
      expect(MAX.getLong(YEAR_OF_ERA) == 999999999).toBeTruthy
    }

    it("should respond to `getChronology`") {
      for (d <- samples)
        expect(d.getChronology == IsoChronology.INSTANCE).toBeTruthy
    }

    it("should respond to `getEra`") {
      expect(MIN.getEra == IsoEra.BCE).toBeTruthy
      expect(someDate.getEra == IsoEra.CE).toBeTruthy
      expect(leapDate.getEra == IsoEra.CE).toBeTruthy
      expect(MAX.getEra == IsoEra.CE).toBeTruthy
    }

    it("should respond to `getYear`") {
      expect(MIN.getYear).toEqual(-999999999)
      expect(someDate.getYear).toEqual(2011)
      expect(leapDate.getYear).toEqual(2012)
      expect(MAX.getYear).toEqual(999999999)
    }

    it("should respond to `getMonthValue`") {
      for (d <- samples)
        expect(d.getMonthValue == d.getMonth.getValue).toBeTruthy
    }

    it("should respond to `getMonth`") {
      expect(MIN.getMonth == Month.JANUARY).toBeTruthy
      expect(someDate.getMonth == Month.FEBRUARY).toBeTruthy
      expect(leapDate.getMonth == Month.FEBRUARY).toBeTruthy
      expect(MAX.getMonth == Month.DECEMBER).toBeTruthy
    }

    it("should respond to `getDayOfMonth`") {
      expect(MIN.getDayOfMonth).toEqual(1)
      expect(someDate.getDayOfMonth).toEqual(28)
      expect(leapDate.getDayOfMonth).toEqual(29)
      expect(MAX.getDayOfMonth).toEqual(31)
    }

    it("should respond to `getDayOfYear`") {
      expect(MIN.getDayOfYear).toEqual(1)
      expect(someDate.getDayOfYear).toEqual(59)
      expect(leapDate.getDayOfYear).toEqual(60)
      expect(of(2012, 12, 31).getDayOfYear).toEqual(366)
      expect(MAX.getDayOfYear).toEqual(365)
    }

    it("should respond to `getDayOfWeek`") {
      expect(MIN.getDayOfWeek == DayOfWeek.MONDAY).toBeTruthy
      expect(someDate.getDayOfWeek == DayOfWeek.MONDAY).toBeTruthy
      expect(leapDate.getDayOfWeek == DayOfWeek.WEDNESDAY).toBeTruthy
      expect(MAX.getDayOfWeek == DayOfWeek.FRIDAY).toBeTruthy
    }

    it("should respond to `isLeapYear`") {
      expect(MIN.isLeapYear).toBeFalsy
      expect(of(-400, 6, 30).isLeapYear).toBeTruthy
      expect(of(-100, 3, 1).isLeapYear).toBeFalsy
      expect(of(0, 1, 1).isLeapYear).toBeTruthy
      expect(of(1900, 9, 30).isLeapYear).toBeFalsy
      expect(of(2000, 1, 1).isLeapYear).toBeTruthy
      expect(someDate.isLeapYear).toBeFalsy
      expect(leapDate.isLeapYear).toBeTruthy
      expect(MAX.isLeapYear).toBeFalsy
    }

    it("should respond to `lengthOfMonth`") {
      for (d <- samples ++ Seq(of(2001, 2, 1), of(2012, 9, 30)))
        expect(d.lengthOfMonth).toEqual(d.getMonth.length(d.isLeapYear))
    }

    it("should respond to `lengthOfYear`") {
      for (d <- samples)
        expect(d.lengthOfYear).toEqual(if (d.isLeapYear) 366 else 365)
    }

    it("should respond to `with`") {
      testTemporal(MAX.`with`(DAY_OF_WEEK, 1))(of(999999999, 12, 27))
      testTemporal(MAX.`with`(DAY_OF_WEEK, 5))(MAX)
      testTemporal(MIN.`with`(DAY_OF_WEEK, 1))(MIN)
      testTemporal(MIN.`with`(DAY_OF_WEEK, 7))(of(-999999999, 1, 7))
      testTemporal(someDate.`with`(DAY_OF_WEEK, 1))(someDate)
      testTemporal(someDate.`with`(DAY_OF_WEEK, 7))(of(2011, 3, 6))
      testTemporal(leapDate.`with`(DAY_OF_WEEK, 1))(of(2012, 2, 27))
      testTemporal(leapDate.`with`(DAY_OF_WEEK, 7))(of(2012, 3, 4))
      testTemporal(MAX.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, 1))(of(999999999, 12, 29))
      testTemporal(MAX.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, 3))(MAX)
      testTemporal(MIN.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, 1))(MIN)
      testTemporal(MIN.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, 7))(of(-999999999, 1, 7))
      testTemporal(someDate.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, 1))(of(2011, 2, 22))
      testTemporal(someDate.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, 7))(someDate)
      testTemporal(leapDate.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, 1))(leapDate)
      testTemporal(leapDate.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, 7))(of(2012, 3, 6))
      testTemporal(MAX.`with`(ALIGNED_DAY_OF_WEEK_IN_YEAR, 1))(MAX)
      testTemporal(MIN.`with`(ALIGNED_DAY_OF_WEEK_IN_YEAR, 1))(MIN)
      testTemporal(MIN.`with`(ALIGNED_DAY_OF_WEEK_IN_YEAR, 7))(of(-999999999, 1, 7))
      testTemporal(someDate.`with`(ALIGNED_DAY_OF_WEEK_IN_YEAR, 1))(of(2011, 2, 26))
      testTemporal(someDate.`with`(ALIGNED_DAY_OF_WEEK_IN_YEAR, 7))(of(2011, 3, 4))
      testTemporal(leapDate.`with`(ALIGNED_DAY_OF_WEEK_IN_YEAR, 1))(of(2012, 2, 26))
      testTemporal(leapDate.`with`(ALIGNED_DAY_OF_WEEK_IN_YEAR, 7))(of(2012, 3, 3))
      testTemporal(someDate.`with`(DAY_OF_MONTH, 1))(of(2011, 2, 1))
      testTemporal(leapDate.`with`(DAY_OF_MONTH, 28))(of(2012, 2, 28))
      testTemporal(someDate.`with`(DAY_OF_YEAR, 1))(of(2011, 1, 1))
      testTemporal(someDate.`with`(DAY_OF_YEAR, 365))(of(2011, 12, 31))
      testTemporal(leapDate.`with`(DAY_OF_YEAR, 366))(of(2012, 12, 31))
      for {
        d1 <- samples
        d2 <- samples
      } {
        testTemporal(d1.`with`(EPOCH_DAY, d2.toEpochDay))(d2)
      }
      testTemporal(MAX.`with`(ALIGNED_WEEK_OF_MONTH, 1))(of(999999999, 12, 3))
      testTemporal(MAX.`with`(ALIGNED_WEEK_OF_MONTH, 5))(MAX)
      testTemporal(MIN.`with`(ALIGNED_WEEK_OF_MONTH, 1))(MIN)
      testTemporal(MIN.`with`(ALIGNED_WEEK_OF_MONTH, 5))(of(-999999999, 1, 29))
      testTemporal(someDate.`with`(ALIGNED_WEEK_OF_MONTH, 1))(of(2011, 2, 7))
      testTemporal(someDate.`with`(ALIGNED_WEEK_OF_MONTH, 5))(of(2011, 3, 7))
      testTemporal(leapDate.`with`(ALIGNED_WEEK_OF_MONTH, 1))(of(2012, 2, 1))
      testTemporal(leapDate.`with`(ALIGNED_WEEK_OF_MONTH, 5))(leapDate)
      testTemporal(MAX.`with`(ALIGNED_WEEK_OF_YEAR, 1))(of(999999999, 1, 1))
      testTemporal(MAX.`with`(ALIGNED_WEEK_OF_YEAR, 53))(MAX)
      testTemporal(MIN.`with`(ALIGNED_WEEK_OF_YEAR, 1))(MIN)
      testTemporal(MIN.`with`(ALIGNED_WEEK_OF_YEAR, 53))(of(-999999999, 12, 31))
      testTemporal(someDate.`with`(ALIGNED_WEEK_OF_YEAR, 1))(of(2011, 1, 3))
      testTemporal(someDate.`with`(ALIGNED_WEEK_OF_YEAR, 53))(of(2012, 1, 2))
      testTemporal(leapDate.`with`(ALIGNED_WEEK_OF_YEAR, 1))(of(2012, 1, 4))
      testTemporal(leapDate.`with`(ALIGNED_WEEK_OF_YEAR, 53))(of(2013, 1, 2))
      testTemporal(MAX.`with`(MONTH_OF_YEAR, 2))(of(999999999, 2, 28))
      testTemporal(MAX.`with`(MONTH_OF_YEAR, 11))(of(999999999, 11, 30))
      testTemporal(someDate.`with`(MONTH_OF_YEAR, 1))(of(2011, 1, 28))
      testTemporal(leapDate.`with`(MONTH_OF_YEAR, 2))(leapDate)
      testTemporal(MAX.`with`(PROLEPTIC_MONTH, 1))(of(0, 2, 29))
      testTemporal(MIN.`with`(PROLEPTIC_MONTH, -1))(of(-1, 12, 1))
      testTemporal(someDate.`with`(PROLEPTIC_MONTH, -11999999988L))(of(-999999999, 1, 28))
      testTemporal(leapDate.`with`(PROLEPTIC_MONTH, 11999999999L))(of(999999999, 12, 29))
      testTemporal(MIN.`with`(YEAR_OF_ERA, 1000000000))(MIN)
      testTemporal(MIN.`with`(YEAR_OF_ERA, 1))(of(0, 1, 1))
      testTemporal(MAX.`with`(YEAR_OF_ERA, 999999999))(MAX)
      testTemporal(MAX.`with`(YEAR_OF_ERA, 1))(of(1, 12, 31))
      testTemporal(leapDate.`with`(YEAR_OF_ERA, 2011))(someDate)
      testTemporal(MIN.`with`(YEAR, -999999999))(MIN)
      testTemporal(MIN.`with`(YEAR, 999999999))(of(999999999, 1, 1))
      testTemporal(MAX.`with`(YEAR, -999999999))(of(-999999999, 12, 31))
      testTemporal(MAX.`with`(YEAR, 999999999))(MAX)
      testTemporal(leapDate.`with`(YEAR, 2011))(someDate)
      testTemporal(MIN.`with`(ERA, 0))(MIN)
      testTemporal(MAX.`with`(ERA, 0))(of(-999999998, 12, 31))
      testTemporal(MAX.`with`(ERA, 1))(MAX)
      testTemporal(someDate.`with`(ERA, 1))(someDate)
      testTemporal(leapDate.`with`(ERA, 0))(of(-2011, 2, 28))

      expectThrows[DateTimeException](MAX.`with`(DAY_OF_WEEK, 6))
      expectThrows[DateTimeException](MAX.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, 4))
      expectThrows[DateTimeException](MAX.`with`(ALIGNED_DAY_OF_WEEK_IN_YEAR, 2))
      expectThrows[DateTimeException](someDate.`with`(DAY_OF_MONTH, 29))
      expectThrows[DateTimeException](leapDate.`with`(DAY_OF_MONTH, 30))
      expectThrows[DateTimeException](someDate.`with`(DAY_OF_YEAR, 366))
      expectThrows[DateTimeException](someDate.`with`(YEAR_OF_ERA, 1000000000))
      expectThrows[DateTimeException](MIN.`with`(ERA, 1))

      for (d <- samples) {
        for (n <- Seq(Long.MinValue, 0L, 8L, Long.MaxValue)) {
          expectThrows[DateTimeException](d.`with`(DAY_OF_WEEK, n))
          expectThrows[DateTimeException](d.`with`(ALIGNED_DAY_OF_WEEK_IN_MONTH, n))
          expectThrows[DateTimeException](d.`with`(ALIGNED_DAY_OF_WEEK_IN_YEAR, n))
        }
        for (n <- Seq(Long.MinValue, 0L, 32L, Long.MaxValue)) {
          expectThrows[DateTimeException](d.`with`(DAY_OF_MONTH, n))
        }
        for (n <- Seq(Long.MinValue, 0L, 367L, Long.MaxValue)) {
          expectThrows[DateTimeException](d.`with`(DAY_OF_YEAR, n))
        }
        for (n <- Seq(Long.MinValue, -365243219163L, 365241780472L, Long.MaxValue)) {
          expectThrows[DateTimeException](d.`with`(EPOCH_DAY, n))
        }
        for (n <- Seq(Long.MinValue, 0L, 6L, Long.MaxValue)) {
          expectThrows[DateTimeException](d.`with`(ALIGNED_WEEK_OF_MONTH, n))
        }
        for (n <- Seq(Long.MinValue, 0L, 54L, Long.MaxValue)) {
          expectThrows[DateTimeException](d.`with`(ALIGNED_WEEK_OF_YEAR, n))
        }
        for (n <- Seq(Long.MinValue, 0L, 13L, Long.MaxValue)) {
          expectThrows[DateTimeException](d.`with`(MONTH_OF_YEAR, n))
        }
        for (n <- Seq(Long.MinValue, -11999999989L, 12000000000L, Long.MaxValue)) {
          expectThrows[DateTimeException](d.`with`(PROLEPTIC_MONTH, n))
        }
        for (n <- Seq(Long.MinValue, 0L, 1000000001L, Long.MaxValue)) {
          expectThrows[DateTimeException](d.`with`(YEAR_OF_ERA, n))
        }
        for (n <- Seq(Long.MinValue, -1000000000L, 1000000000L, Long.MaxValue)) {
          expectThrows[DateTimeException](d.`with`(YEAR, n))
        }
        for (n <- Seq(Long.MinValue, -1L, 2L, Long.MaxValue)) {
          expectThrows[DateTimeException](d.`with`(ERA, n))
        }
      }
    }

    it("should respond to `withYear`") {
      testTemporal(MIN.withYear(-999999999))(MIN)
      testTemporal(MIN.withYear(999999999))(of(999999999, 1, 1))
      testTemporal(MAX.withYear(-999999999))(of(-999999999, 12, 31))
      testTemporal(MAX.withYear(999999999))(MAX)

      val years = Seq(Int.MinValue, -1000000000, 1000000000, Int.MaxValue)
      for {
        d <- samples
        n <- years
      } {
        expectThrows[DateTimeException](d.withYear(n))
      }
    }

    it("should respond to `withMonth`") {
      testTemporal(MAX.withMonth(2))(of(999999999, 2, 28))
      testTemporal(MAX.withMonth(11))(of(999999999, 11, 30))
      testTemporal(someDate.withMonth(1))(of(2011, 1, 28))
      testTemporal(leapDate.withMonth(2))(leapDate)

      val months = Seq(Int.MinValue, 0, 13, Int.MaxValue)
      for {
        d <- samples
        n <- months
      } {
        expectThrows[DateTimeException](d.withMonth(n))
      }
    }

    it("should respond to `withDayOfMonth`") {
      testTemporal(someDate.withDayOfMonth(1))(of(2011, 2, 1))
      testTemporal(leapDate.withDayOfMonth(28))(of(2012, 2, 28))

      expectThrows[DateTimeException](someDate.withDayOfMonth(29))
      expectThrows[DateTimeException](leapDate.withDayOfMonth(30))
      expectThrows[DateTimeException](of(0, 4, 30).withDayOfMonth(31))
      val days = Seq(Int.MinValue, 0, 32, Int.MaxValue)
      for {
        d <- samples
        n <- days
      } {
        expectThrows[DateTimeException](d.withDayOfMonth(n))
      }
    }

    it("should respond to `withDayOfYear`") {
      testTemporal(someDate.withDayOfYear(1))(of(2011, 1, 1))
      testTemporal(someDate.withDayOfYear(365))(of(2011, 12, 31))
      testTemporal(leapDate.withDayOfYear(366))(of(2012, 12, 31))

      expectThrows[DateTimeException](someDate.withDayOfYear(366))
      val days = Seq(Int.MinValue, 0, 367, Int.MaxValue)
      for {
        d <- samples
        n <- days
      } {
        expectThrows[DateTimeException](d.withDayOfMonth(n))
      }
    }

    it("should respond to `plus`") {
      val values = Seq(Long.MinValue, Int.MinValue.toLong, -1000L, -366L, -365L,
          -100L, -12L, -10L, -7L, -1L, 0L, 1L, 7L, 10L, 12L, 100L,
          365L, 366L, 1000L, Int.MaxValue.toLong, Long.MaxValue)

      for {
        d <- samples
        n <- values
      } {
        testTemporal(d.plus(n, DAYS))(d.plusDays(n))
        testTemporal(d.plus(n, WEEKS))(d.plusWeeks(n))
        testTemporal(d.plus(n, MONTHS))(d.plusMonths(n))
        testTemporal(d.plus(n, YEARS))(d.plusYears(n))
        testTemporal(d.plus(n, DECADES))(d.plusYears(Math.multiplyExact(n, 10)))
        testTemporal(d.plus(n, CENTURIES))(d.plusYears(Math.multiplyExact(n, 100)))
        testTemporal(d.plus(n, MILLENNIA))(d.plusYears(Math.multiplyExact(n, 1000)))
        testTemporal(d.plus(n, ERAS))(d.`with`(ERA, Math.addExact(n, d.get(ERA))))
      }
    }

    it("should respond to `plusYears`") {
      for (d <- samples)
        testTemporal(d.plusYears(0))(d)
      testTemporal(someDate.plusYears(-2))(of(2009, 2, 28))
      testTemporal(someDate.plusYears(-1))(of(2010, 2, 28))
      testTemporal(someDate.plusYears(1))(of(2012, 2, 28))
      testTemporal(someDate.plusYears(2))(of(2013, 2, 28))
      testTemporal(leapDate.plusYears(-2))(of(2010, 2, 28))
      testTemporal(leapDate.plusYears(-1))(someDate)
      testTemporal(leapDate.plusYears(1))(of(2013, 2, 28))
      testTemporal(leapDate.plusYears(2))(of(2014, 2, 28))
      testTemporal(MIN.plusYears(1999999998))(of(999999999, 1, 1))
      testTemporal(MAX.plusYears(-1999999998))(of(-999999999, 12, 31))
      expectThrows[DateTimeException](MIN.plusYears(-1))
      expectThrows[DateTimeException](MIN.plusYears(1999999999))
      expectThrows[DateTimeException](MAX.plusYears(-1999999999))
      expectThrows[DateTimeException](MAX.plusYears(1))
      expectThrows[DateTimeException](MIN.plusYears(Long.MinValue))
      expectThrows[DateTimeException](MAX.plusYears(Long.MaxValue))
    }

    it("should respond to `plusMonths`") {
      for (d <- samples)
        testTemporal(d.plusMonths(0))(d)
      testTemporal(someDate.plusMonths(-12))(of(2010, 2, 28))
      testTemporal(someDate.plusMonths(-1))(of(2011, 1, 28))
      testTemporal(someDate.plusMonths(1))(of(2011, 3, 28))
      testTemporal(someDate.plusMonths(12))(of(2012, 2, 28))
      testTemporal(leapDate.plusMonths(-12))(someDate)
      testTemporal(leapDate.plusMonths(-1))(of(2012, 1, 29))
      testTemporal(leapDate.plusMonths(1))(of(2012, 3, 29))
      testTemporal(leapDate.plusMonths(12))(of(2013, 2, 28))
      testTemporal(of(2011, 1, 31).plusMonths(1))(someDate)
      testTemporal(of(2011, 3, 31).plusMonths(-1))(someDate)
      testTemporal(of(2011, 3, 31).plusMonths(1))(of(2011, 4, 30))
      testTemporal(of(2012, 1, 31).plusMonths(1))(leapDate)
      testTemporal(of(2012, 3, 31).plusMonths(-1))(leapDate)
      testTemporal(of(2012, 3, 31).plusMonths(1))(of(2012, 4, 30))
      testTemporal(MIN.plusMonths(23999999987L))(of(999999999, 12, 1))
      testTemporal(MAX.plusMonths(-23999999987L))(of(-999999999, 1, 31))
      expectThrows[DateTimeException](MIN.plusMonths(-1))
      expectThrows[DateTimeException](MIN.plusMonths(23999999988L))
      expectThrows[DateTimeException](MAX.plusMonths(-23999999988L))
      expectThrows[DateTimeException](MAX.plusMonths(1))
      expectThrows[DateTimeException](MIN.plusMonths(Long.MinValue))
      expectThrows[DateTimeException](MAX.plusMonths(Long.MaxValue))
    }

    it("should respond to `plusWeeks`") {
      for (d <- samples)
        testTemporal(d.plusWeeks(0))(d)
      testTemporal(someDate.plusWeeks(-53))(of(2010, 2, 22))
      testTemporal(someDate.plusWeeks(-52))(of(2010, 3, 1))
      testTemporal(someDate.plusWeeks(-1))(of(2011, 2, 21))
      testTemporal(someDate.plusWeeks(1))(of(2011, 3, 7))
      testTemporal(someDate.plusWeeks(52))(of(2012, 2, 27))
      testTemporal(someDate.plusWeeks(53))(of(2012, 3, 5))
      testTemporal(leapDate.plusWeeks(-53))(of(2011, 2, 23))
      testTemporal(leapDate.plusWeeks(-52))(of(2011, 3, 2))
      testTemporal(leapDate.plusWeeks(-1))(of(2012, 2, 22))
      testTemporal(leapDate.plusWeeks(1))(of(2012, 3, 7))
      testTemporal(leapDate.plusWeeks(52))(of(2013, 2, 27))
      testTemporal(leapDate.plusWeeks(53))(of(2013, 3, 6))
      testTemporal(MIN.plusWeeks(104354999947L))(of(999999999, 12, 27))
      testTemporal(MAX.plusWeeks(-104354999947L))(of(-999999999, 1, 5))
      expectThrows[DateTimeException](MIN.plusWeeks(-1))
      expectThrows[DateTimeException](MIN.plusWeeks(104354999948L))
      expectThrows[DateTimeException](MAX.plusWeeks(-1043549999478L))
      expectThrows[DateTimeException](MAX.plusWeeks(1))
      expectThrows[ArithmeticException](MIN.plusWeeks(Long.MinValue))
      expectThrows[ArithmeticException](MAX.plusWeeks(Long.MaxValue))
    }

    it("should respond to `plusDays`") {
      for (d <- samples)
        testTemporal(d.plusDays(0))(d)
      testTemporal(someDate.plusDays(-365))(of(2010, 2, 28))
      testTemporal(someDate.plusDays(-1))(of(2011, 2, 27))
      testTemporal(someDate.plusDays(1))(of(2011, 3, 1))
      testTemporal(someDate.plusDays(365))(of(2012, 2, 28))
      testTemporal(someDate.plusDays(366))(leapDate)
      testTemporal(leapDate.plusDays(-366))(someDate)
      testTemporal(leapDate.plusDays(-365))(of(2011, 3, 1))
      testTemporal(leapDate.plusDays(-1))(of(2012, 2, 28))
      testTemporal(leapDate.plusDays(1))(of(2012, 3, 1))
      testTemporal(leapDate.plusDays(365))(of(2013, 2, 28))
      testTemporal(leapDate.plusDays(366))(of(2013, 3, 1))
      testTemporal(MIN.plusDays(730484999633L))(MAX)
      testTemporal(MAX.plusDays(-730484999633L))(MIN)
      expectThrows[DateTimeException](MIN.plusDays(-1))
      expectThrows[DateTimeException](MIN.plusDays(730484999634L))
      expectThrows[DateTimeException](MAX.plusDays(-730484999634L))
      expectThrows[DateTimeException](MAX.plusDays(1))
      expectThrows[ArithmeticException](ofEpochDay(-1).plusDays(Long.MinValue))
      expectThrows[ArithmeticException](ofEpochDay(1).plusDays(Long.MaxValue))
    }

    it("should respond to `minusYears`") {
      for (d <- samples)
        testTemporal(d.minusYears(0))(d)
      testTemporal(someDate.minusYears(2))(of(2009, 2, 28))
      testTemporal(someDate.minusYears(1))(of(2010, 2, 28))
      testTemporal(someDate.minusYears(-1))(of(2012, 2, 28))
      testTemporal(someDate.minusYears(-2))(of(2013, 2, 28))
      testTemporal(leapDate.minusYears(2))(of(2010, 2, 28))
      testTemporal(leapDate.minusYears(1))(someDate)
      testTemporal(leapDate.minusYears(-1))(of(2013, 2, 28))
      testTemporal(leapDate.minusYears(-2))(of(2014, 2, 28))
      testTemporal(MIN.minusYears(-1999999998))(of(999999999, 1, 1))
      testTemporal(MAX.minusYears(1999999998))(of(-999999999, 12, 31))
      expectThrows[DateTimeException](MIN.minusYears(1))
      expectThrows[DateTimeException](MIN.minusYears(-1999999999))
      expectThrows[DateTimeException](MAX.minusYears(1999999999))
      expectThrows[DateTimeException](MAX.minusYears(-1))
      expectThrows[DateTimeException](MIN.minusYears(Long.MaxValue))
      expectThrows[DateTimeException](MAX.minusYears(Long.MinValue))
    }

    it("should respond to `minusMonths`") {
      for (d <- samples)
        testTemporal(d.minusMonths(0))(d)
      testTemporal(someDate.minusMonths(12))(of(2010, 2, 28))
      testTemporal(someDate.minusMonths(1))(of(2011, 1, 28))
      testTemporal(someDate.minusMonths(-1))(of(2011, 3, 28))
      testTemporal(someDate.minusMonths(-12))(of(2012, 2, 28))
      testTemporal(leapDate.minusMonths(12))(someDate)
      testTemporal(leapDate.minusMonths(1))(of(2012, 1, 29))
      testTemporal(leapDate.minusMonths(-1))(of(2012, 3, 29))
      testTemporal(leapDate.minusMonths(-12))(of(2013, 2, 28))
      testTemporal(of(2011, 1, 31).minusMonths(-1))(someDate)
      testTemporal(of(2011, 3, 31).minusMonths(1))(someDate)
      testTemporal(of(2011, 3, 31).minusMonths(-1))(of(2011, 4, 30))
      testTemporal(of(2012, 1, 31).minusMonths(-1))(leapDate)
      testTemporal(of(2012, 3, 31).minusMonths(1))(leapDate)
      testTemporal(of(2012, 3, 31).minusMonths(-1))(of(2012, 4, 30))
      testTemporal(MIN.minusMonths(-23999999987L))(of(999999999, 12, 1))
      testTemporal(MAX.minusMonths(23999999987L))(of(-999999999, 1, 31))
      expectThrows[DateTimeException](MIN.minusMonths(1))
      expectThrows[DateTimeException](MIN.minusMonths(-23999999988L))
      expectThrows[DateTimeException](MAX.minusMonths(23999999988L))
      expectThrows[DateTimeException](MAX.minusMonths(-1))
      expectThrows[DateTimeException](MIN.minusMonths(Long.MaxValue))
      expectThrows[DateTimeException](MAX.minusMonths(Long.MinValue))
    }

    it("should respond to `minusWeeks`") {
      for (d <- samples)
        testTemporal(d.minusWeeks(0))(d)
      testTemporal(someDate.minusWeeks(53))(of(2010, 2, 22))
      testTemporal(someDate.minusWeeks(52))(of(2010, 3, 1))
      testTemporal(someDate.minusWeeks(1))(of(2011, 2, 21))
      testTemporal(someDate.minusWeeks(-1))(of(2011, 3, 7))
      testTemporal(someDate.minusWeeks(-52))(of(2012, 2, 27))
      testTemporal(someDate.minusWeeks(-53))(of(2012, 3, 5))
      testTemporal(leapDate.minusWeeks(53))(of(2011, 2, 23))
      testTemporal(leapDate.minusWeeks(52))(of(2011, 3, 2))
      testTemporal(leapDate.minusWeeks(1))(of(2012, 2, 22))
      testTemporal(leapDate.minusWeeks(-1))(of(2012, 3, 7))
      testTemporal(leapDate.minusWeeks(-52))(of(2013, 2, 27))
      testTemporal(leapDate.minusWeeks(-53))(of(2013, 3, 6))
      testTemporal(MIN.minusWeeks(-104354999947L))(of(999999999, 12, 27))
      testTemporal(MAX.minusWeeks(104354999947L))(of(-999999999, 1, 5))
      expectThrows[DateTimeException](MIN.minusWeeks(1))
      expectThrows[DateTimeException](MIN.minusWeeks(-104354999948L))
      expectThrows[DateTimeException](MAX.minusWeeks(1043549999478L))
      expectThrows[DateTimeException](MAX.minusWeeks(-1))
      expectThrows[ArithmeticException](MIN.minusWeeks(Long.MaxValue))
      expectThrows[ArithmeticException](MAX.minusWeeks(Long.MinValue))
    }

    it("should respond to `minusDays`") {
      for (d <- samples)
        testTemporal(d.minusDays(0))(d)
      testTemporal(someDate.minusDays(365))(of(2010, 2, 28))
      testTemporal(someDate.minusDays(1))(of(2011, 2, 27))
      testTemporal(someDate.minusDays(-1))(of(2011, 3, 1))
      testTemporal(someDate.minusDays(-365))(of(2012, 2, 28))
      testTemporal(someDate.minusDays(-366))(leapDate)
      testTemporal(leapDate.minusDays(366))(someDate)
      testTemporal(leapDate.minusDays(365))(of(2011, 3, 1))
      testTemporal(leapDate.minusDays(1))(of(2012, 2, 28))
      testTemporal(leapDate.minusDays(-1))(of(2012, 3, 1))
      testTemporal(leapDate.minusDays(-365))(of(2013, 2, 28))
      testTemporal(leapDate.minusDays(-366))(of(2013, 3, 1))
      testTemporal(MIN.minusDays(-730484999633L))(MAX)
      testTemporal(MAX.minusDays(730484999633L))(MIN)
      expectThrows[DateTimeException](MIN.minusDays(1))
      expectThrows[DateTimeException](MIN.minusDays(-730484999634L))
      expectThrows[DateTimeException](MAX.minusDays(730484999634L))
      expectThrows[DateTimeException](MAX.minusDays(-1))
      expectThrows[ArithmeticException](ofEpochDay(-2).minusDays(Long.MaxValue))
      expectThrows[ArithmeticException](ofEpochDay(1).minusDays(Long.MinValue))
    }

    it("should respond to `adjustInto`") {
      for {
        d1 <- samples
        d2 <- samples
      } {
        testTemporal(d1.adjustInto(d2))(d1)
      }

      val ts = Seq(LocalTime.MIN, LocalTime.MAX)
      for {
        d <- samples
        t <- ts
      } {
        expectThrows[DateTimeException](d.adjustInto(t))
      }
    }

    it("should respond to `until`") {
      val samples1 = samples ++ Seq(of(2012, 1, 29), of(2012, 1, 30), of(2012, 2, 28),
          of(2013, 2, 28), of(2013, 3, 1), of(0, 12, 31), of(1, 1, 1))

      for {
        d <- samples1
        u <- dateBasedUnits
      } {
        expect(d.until(d, u) == 0).toBeTruthy
      }

      expect(MIN.until(MAX, DAYS) == 730484999633L).toBeTruthy
      expect(MIN.until(MAX, WEEKS) == 104354999947L).toBeTruthy
      expect(someDate.until(leapDate, MONTHS) == 12).toBeTruthy
      expect(of(2012, 1, 29).until(leapDate, MONTHS) == 1).toBeTruthy
      expect(of(2012, 1, 30).until(leapDate, MONTHS) == 0).toBeTruthy
      expect(MIN.until(MAX, YEARS) == 1999999998).toBeTruthy
      expect(someDate.until(of(2012, 2, 28), YEARS) == 1).toBeTruthy
      expect(leapDate.until(of(2013, 2, 28), YEARS) == 0).toBeTruthy
      expect(MIN.until(MAX, DECADES) == 199999999).toBeTruthy
      expect(MIN.until(MAX, CENTURIES) == 19999999).toBeTruthy
      expect(MIN.until(MAX, MILLENNIA) == 1999999).toBeTruthy
      expect(MIN.until(MAX, ERAS) == 1).toBeTruthy
      expect(of(0, 12, 31).until(of(1, 1, 1), ERAS) == 1).toBeTruthy

      for {
        d1 <- samples1
        d2 <- samples1 if d2.isAfter(d1)
        u <- dateBasedUnits
      } {
        expect(d2.until(d1, u) == -d1.until(d2, u)).toBeTruthy
      }

      for (d <- samples1)
        expect(d.until(d) == Period.ZERO).toBeTruthy

      for {
        d1 <- samples1
        d2 <- samples1
        u <- timeBasedUnits
      } {
        expectThrows[UnsupportedTemporalTypeException](d1.until(d2, u))
      }

      expect(MIN.until(MAX) == Period.of(1999999998, 11, 30)).toBeTruthy
      expect(someDate.until(leapDate) == Period.of(1, 0, 1)).toBeTruthy
      expect(leapDate.until(of(2013, 2, 28)) == Period.of(0, 11, 30)).toBeTruthy
      expect(leapDate.until(of(2013, 3, 1)) == Period.of(1, 0, 1)).toBeTruthy

      for {
        d1 <- samples1
        d2 <- samples1 if d2.isAfter(d1)
      } {
        expect(d2.until(d1) == d1.until(d2).negated).toBeTruthy
      }
    }

    it("should respond to `toEpochDay`") {
      expect(MIN.toEpochDay == -365243219162L).toBeTruthy
      expect(of(1969, 12, 31).toEpochDay == -1).toBeTruthy
      expect(of(1970, 1, 1).toEpochDay == 0).toBeTruthy
      expect(someDate.toEpochDay == 15033).toBeTruthy
      expect(leapDate.toEpochDay == 15399).toBeTruthy
      expect(MAX.toEpochDay == 365241780471L).toBeTruthy
    }

    it("should be comparable") {
      expect(MIN.compareTo(MIN)).toEqual(0)
      expect(MIN.compareTo(someDate)).toBeLessThan(0)
      expect(MIN.compareTo(MAX)).toBeLessThan(0)
      expect(someDate.compareTo(MIN)).toBeGreaterThan(0)
      expect(someDate.compareTo(someDate)).toEqual(0)
      expect(someDate.compareTo(MAX)).toBeLessThan(0)
      expect(MAX.compareTo(MIN)).toBeGreaterThan(0)
      expect(MAX.compareTo(someDate)).toBeGreaterThan(0)
      expect(MAX.compareTo(MAX)).toEqual(0)
    }

    it("should respond to `isAfter`") {
      expect(MIN.isAfter(MIN)).toBeFalsy
      expect(MIN.isAfter(someDate)).toBeFalsy
      expect(MIN.isAfter(MAX)).toBeFalsy
      expect(someDate.isAfter(MIN)).toBeTruthy
      expect(someDate.isAfter(someDate)).toBeFalsy
      expect(someDate.isAfter(MAX)).toBeFalsy
      expect(MAX.isAfter(MIN)).toBeTruthy
      expect(MAX.isAfter(someDate)).toBeTruthy
      expect(MAX.isAfter(MAX)).toBeFalsy
    }

    it("should respond to `isBefore`") {
      expect(MIN.isBefore(MIN)).toBeFalsy
      expect(MIN.isBefore(someDate)).toBeTruthy
      expect(MIN.isBefore(MAX)).toBeTruthy
      expect(someDate.isBefore(MIN)).toBeFalsy
      expect(someDate.isBefore(someDate)).toBeFalsy
      expect(someDate.isBefore(MAX)).toBeTruthy
      expect(MAX.isBefore(MIN)).toBeFalsy
      expect(MAX.isBefore(someDate)).toBeFalsy
      expect(MAX.isBefore(MAX)).toBeFalsy
    }

    it("should override `toString`") {
      expect(MIN.toString).toEqual("-999999999-01-01")
      expect(of(-1, 12, 31).toString).toEqual("-0001-12-31")
      expect(of(0, 1, 1).toString).toEqual("0000-01-01")
      expect(someDate.toString).toEqual("2011-02-28")
      expect(leapDate.toString).toEqual("2012-02-29")
      expect(of(9999, 12, 31).toString).toEqual("9999-12-31")
      expect(of(10000, 1, 1).toString).toEqual("+10000-01-01")
      expect(MAX.toString).toEqual("+999999999-12-31")
    }

    it("should respond to `now`") {
      expect(now().getEra == IsoEra.CE).toBeTruthy
    }

    it("should respond to `of`") {
      val years = Seq(Int.MinValue, -1000000000, -999999999, 0, 999999999,
          1000000000, Int.MaxValue)
      val days = Seq(Int.MinValue, 0, 1, 28, 29, 30, 31, 32, Int.MaxValue)

      for {
        year <- years
        month <- Month.values
        day <- days
      } {
        testTemporal(of(year, month, day))(of(year, month.getValue, day))
      }

      expectThrows[DateTimeException](of(Int.MinValue, 1, 1))
      expectThrows[DateTimeException](of(-1000000000, 1, 1))
      expectThrows[DateTimeException](of(2011, Int.MinValue, 1))
      expectThrows[DateTimeException](of(2011, 0, 1))
      expectThrows[DateTimeException](of(2011, 13, 1))
      expectThrows[DateTimeException](of(2011, Int.MaxValue, 1))

      for (month <- Month.values) {
        val m = month.getValue
        expectThrows[DateTimeException](of(2011, m, Int.MinValue))
        expectThrows[DateTimeException](of(2011, m, 0))
        expectThrows[DateTimeException](of(2011, m, month.length(false) + 1))
        expectThrows[DateTimeException](of(2012, m, month.length(true) + 1))
        expectThrows[DateTimeException](of(2011, m, Int.MaxValue))
      }
    }

    it("should respond to `ofYearDay`") {
      testTemporal(ofYearDay(2011, 1))(of(2011, 1, 1))
      testTemporal(ofYearDay(2011, 31))(of(2011, 1, 31))
      testTemporal(ofYearDay(2011, 32))(of(2011, 2, 1))
      testTemporal(ofYearDay(2011, 59))(of(2011, 2, 28))
      testTemporal(ofYearDay(2011, 60))(of(2011, 3, 1))
      testTemporal(ofYearDay(2011, 90))(of(2011, 3, 31))
      testTemporal(ofYearDay(2011, 91))(of(2011, 4, 1))
      testTemporal(ofYearDay(2011, 120))(of(2011, 4, 30))
      testTemporal(ofYearDay(2011, 121))(of(2011, 5, 1))
      testTemporal(ofYearDay(2011, 151))(of(2011, 5, 31))
      testTemporal(ofYearDay(2011, 152))(of(2011, 6, 1))
      testTemporal(ofYearDay(2011, 181))(of(2011, 6, 30))
      testTemporal(ofYearDay(2011, 182))(of(2011, 7, 1))
      testTemporal(ofYearDay(2011, 212))(of(2011, 7, 31))
      testTemporal(ofYearDay(2011, 213))(of(2011, 8, 1))
      testTemporal(ofYearDay(2011, 243))(of(2011, 8, 31))
      testTemporal(ofYearDay(2011, 244))(of(2011, 9, 1))
      testTemporal(ofYearDay(2011, 273))(of(2011, 9, 30))
      testTemporal(ofYearDay(2011, 274))(of(2011, 10, 1))
      testTemporal(ofYearDay(2011, 304))(of(2011, 10, 31))
      testTemporal(ofYearDay(2011, 305))(of(2011, 11, 1))
      testTemporal(ofYearDay(2011, 334))(of(2011, 11, 30))
      testTemporal(ofYearDay(2011, 335))(of(2011, 12, 1))
      testTemporal(ofYearDay(2011, 365))(of(2011, 12, 31))
      testTemporal(ofYearDay(2012, 1))(of(2012, 1, 1))
      testTemporal(ofYearDay(2012, 31))(of(2012, 1, 31))
      testTemporal(ofYearDay(2012, 32))(of(2012, 2, 1))
      testTemporal(ofYearDay(2012, 60))(of(2012, 2, 29))
      testTemporal(ofYearDay(2012, 61))(of(2012, 3, 1))
      testTemporal(ofYearDay(2012, 91))(of(2012, 3, 31))
      testTemporal(ofYearDay(2012, 92))(of(2012, 4, 1))
      testTemporal(ofYearDay(2012, 121))(of(2012, 4, 30))
      testTemporal(ofYearDay(2012, 122))(of(2012, 5, 1))
      testTemporal(ofYearDay(2012, 152))(of(2012, 5, 31))
      testTemporal(ofYearDay(2012, 153))(of(2012, 6, 1))
      testTemporal(ofYearDay(2012, 182))(of(2012, 6, 30))
      testTemporal(ofYearDay(2012, 183))(of(2012, 7, 1))
      testTemporal(ofYearDay(2012, 213))(of(2012, 7, 31))
      testTemporal(ofYearDay(2012, 214))(of(2012, 8, 1))
      testTemporal(ofYearDay(2012, 244))(of(2012, 8, 31))
      testTemporal(ofYearDay(2012, 245))(of(2012, 9, 1))
      testTemporal(ofYearDay(2012, 274))(of(2012, 9, 30))
      testTemporal(ofYearDay(2012, 275))(of(2012, 10, 1))
      testTemporal(ofYearDay(2012, 305))(of(2012, 10, 31))
      testTemporal(ofYearDay(2012, 306))(of(2012, 11, 1))
      testTemporal(ofYearDay(2012, 335))(of(2012, 11, 30))
      testTemporal(ofYearDay(2012, 336))(of(2012, 12, 1))
      testTemporal(ofYearDay(2012, 366))(of(2012, 12, 31))

      expectThrows[DateTimeException](ofYearDay(Int.MinValue, 1))
      expectThrows[DateTimeException](ofYearDay(-1000000000, 1))
      expectThrows[DateTimeException](ofYearDay(1000000000, 1))
      expectThrows[DateTimeException](ofYearDay(Int.MaxValue, 1))
      expectThrows[DateTimeException](ofYearDay(2011, Int.MinValue))
      expectThrows[DateTimeException](ofYearDay(2011, 0))
      expectThrows[DateTimeException](ofYearDay(2011, 366))
      expectThrows[DateTimeException](ofYearDay(2012, 367))
      expectThrows[DateTimeException](ofYearDay(2011, Int.MaxValue))
    }

    it("should respond to `ofEpochDay`") {
      testTemporal(ofEpochDay(-365243219162L))(MIN)
      testTemporal(ofEpochDay(-1))(of(1969, 12, 31))
      testTemporal(ofEpochDay(0))(of(1970, 1, 1))
      testTemporal(ofEpochDay(1))(of(1970, 1, 2))
      testTemporal(ofEpochDay(365241780471L))(MAX)

      expectThrows[DateTimeException](ofEpochDay(Long.MinValue))
      expectThrows[DateTimeException](ofEpochDay(-365243219163L))
      expectThrows[DateTimeException](ofEpochDay(365241780472L))
      expectThrows[DateTimeException](ofEpochDay(Long.MaxValue))
    }

    it("should respond to `from`") {
      for (d <- samples)
        testTemporal(from(d))(d)

      for (t <- Seq(LocalTime.MIN, LocalTime.NOON, LocalTime.MAX))
        expectThrows[DateTimeException](from(t))
    }
  }
}
