package org.scalajs.testsuite.javalib.time

import java.time._
import java.time.temporal.ChronoField

import org.scalajs.testsuite.javalib.time.temporal.TemporalAccessorTest

object MonthTest extends TemporalAccessorTest {
  import Month._

  describe("java.time.Month") {
    testTemporalAccessorApi(values():_*)

    it("should respond to `getValue`") {
      expect(JANUARY.getValue).toEqual(1)
      expect(FEBRUARY.getValue).toEqual(2)
      expect(MARCH.getValue).toEqual(3)
      expect(APRIL.getValue).toEqual(4)
      expect(MAY.getValue).toEqual(5)
      expect(JUNE.getValue).toEqual(6)
      expect(JULY.getValue).toEqual(7)
      expect(AUGUST.getValue).toEqual(8)
      expect(SEPTEMBER.getValue).toEqual(9)
      expect(OCTOBER.getValue).toEqual(10)
      expect(NOVEMBER.getValue).toEqual(11)
      expect(DECEMBER.getValue).toEqual(12)
    }

    it("should respond to `isSupported`") {
      for {
        m <- Month.values()
        f <- ChronoField.values()
      } {
        if (f == ChronoField.MONTH_OF_YEAR)
          expect(m.isSupported(f)).toBeTruthy
        else
          expect(m.isSupported(f)).toBeFalsy
      }
    }

    it("should respond to `getLong`") {
      for (m <- Month.values())
        expect(m.getLong(ChronoField.MONTH_OF_YEAR)).toEqual(m.getValue)
    }

    it("should respond to `plus`") {
      expect(JANUARY.plus(Long.MinValue) == MAY).toBeTruthy
      expect(FEBRUARY.plus(-12) == FEBRUARY).toBeTruthy
      expect(MARCH.plus(-1) == FEBRUARY).toBeTruthy
      expect(APRIL.plus(0) == APRIL).toBeTruthy
      expect(MAY.plus(1) == JUNE).toBeTruthy
      expect(JUNE.plus(12) == JUNE).toBeTruthy
      expect(JULY.plus(Long.MaxValue) == FEBRUARY).toBeTruthy
    }

    it("should respond to `minus`") {
      expect(JANUARY.minus(Long.MinValue) == SEPTEMBER).toBeTruthy
      expect(FEBRUARY.minus(-12) == FEBRUARY).toBeTruthy
      expect(MARCH.minus(-1) == APRIL).toBeTruthy
      expect(APRIL.minus(0) == APRIL).toBeTruthy
      expect(MAY.minus(1) == APRIL).toBeTruthy
      expect(JUNE.minus(12) == JUNE).toBeTruthy
      expect(JULY.minus(Long.MaxValue) == DECEMBER).toBeTruthy
    }

    it("should respond to `minLength`") {
      expect(JANUARY.minLength).toEqual(31)
      expect(FEBRUARY.minLength).toEqual(28)
      expect(MARCH.minLength).toEqual(31)
      expect(APRIL.minLength).toEqual(30)
      expect(MAY.minLength).toEqual(31)
      expect(JUNE.minLength).toEqual(30)
      expect(JULY.minLength).toEqual(31)
      expect(AUGUST.minLength).toEqual(31)
      expect(SEPTEMBER.minLength).toEqual(30)
      expect(OCTOBER.minLength).toEqual(31)
      expect(NOVEMBER.minLength).toEqual(30)
      expect(DECEMBER.minLength).toEqual(31)
    }

    it("should respond to `maxLength`") {
      expect(JANUARY.maxLength).toEqual(31)
      expect(FEBRUARY.maxLength).toEqual(29)
      expect(MARCH.maxLength).toEqual(31)
      expect(APRIL.maxLength).toEqual(30)
      expect(MAY.maxLength).toEqual(31)
      expect(JUNE.maxLength).toEqual(30)
      expect(JULY.maxLength).toEqual(31)
      expect(AUGUST.maxLength).toEqual(31)
      expect(SEPTEMBER.maxLength).toEqual(30)
      expect(OCTOBER.maxLength).toEqual(31)
      expect(NOVEMBER.maxLength).toEqual(30)
      expect(DECEMBER.maxLength).toEqual(31)
    }

    it("should respond to `length`") {
      for (m <- Month.values()) {
        expect(m.length(false)).toEqual(m.minLength)
        expect(m.length(true)).toEqual(m.maxLength)
      }
    }

    it("should respond to `firstDayOfYear`") {
      expect(JANUARY.firstDayOfYear(false)).toEqual(1)
      expect(JANUARY.firstDayOfYear(true)).toEqual(1)
      expect(FEBRUARY.firstDayOfYear(false)).toEqual(32)
      expect(FEBRUARY.firstDayOfYear(true)).toEqual(32)
      expect(MARCH.firstDayOfYear(false)).toEqual(60)
      expect(MARCH.firstDayOfYear(true)).toEqual(61)
      expect(APRIL.firstDayOfYear(false)).toEqual(91)
      expect(APRIL.firstDayOfYear(true)).toEqual(92)
      expect(MAY.firstDayOfYear(false)).toEqual(121)
      expect(MAY.firstDayOfYear(true)).toEqual(122)
      expect(JUNE.firstDayOfYear(false)).toEqual(152)
      expect(JUNE.firstDayOfYear(true)).toEqual(153)
      expect(JULY.firstDayOfYear(false)).toEqual(182)
      expect(JULY.firstDayOfYear(true)).toEqual(183)
      expect(AUGUST.firstDayOfYear(false)).toEqual(213)
      expect(AUGUST.firstDayOfYear(true)).toEqual(214)
      expect(SEPTEMBER.firstDayOfYear(false)).toEqual(244)
      expect(SEPTEMBER.firstDayOfYear(true)).toEqual(245)
      expect(OCTOBER.firstDayOfYear(false)).toEqual(274)
      expect(OCTOBER.firstDayOfYear(true)).toEqual(275)
      expect(NOVEMBER.firstDayOfYear(false)).toEqual(305)
      expect(NOVEMBER.firstDayOfYear(true)).toEqual(306)
      expect(DECEMBER.firstDayOfYear(false)).toEqual(335)
      expect(DECEMBER.firstDayOfYear(true)).toEqual(336)
    }

    it("should respond to `firstMonthOfQuarter`") {
      expect(JANUARY.firstMonthOfQuarter == JANUARY).toBeTruthy
      expect(FEBRUARY.firstMonthOfQuarter == JANUARY).toBeTruthy
      expect(MARCH.firstMonthOfQuarter == JANUARY).toBeTruthy
      expect(APRIL.firstMonthOfQuarter == APRIL).toBeTruthy
      expect(MAY.firstMonthOfQuarter == APRIL).toBeTruthy
      expect(JUNE.firstMonthOfQuarter == APRIL).toBeTruthy
      expect(JULY.firstMonthOfQuarter == JULY).toBeTruthy
      expect(AUGUST.firstMonthOfQuarter == JULY).toBeTruthy
      expect(SEPTEMBER.firstMonthOfQuarter == JULY).toBeTruthy
      expect(OCTOBER.firstMonthOfQuarter == OCTOBER).toBeTruthy
      expect(NOVEMBER.firstMonthOfQuarter == OCTOBER).toBeTruthy
      expect(DECEMBER.firstMonthOfQuarter == OCTOBER).toBeTruthy
    }

    it("should be comparable") {
      expect(JULY.compareTo(JULY)).toEqual(0)
      expect(JANUARY.compareTo(MARCH)).toBeLessThan(0)
      expect(DECEMBER.compareTo(OCTOBER)).toBeGreaterThan(0)
    }

    it("should respond to `values`") {
      val months = Month.values()

      expect(months.length).toEqual(12)
      expect(months(0) == JANUARY).toBeTruthy
      expect(months(1) == FEBRUARY).toBeTruthy
      expect(months(2) == MARCH).toBeTruthy
      expect(months(3) == APRIL).toBeTruthy
      expect(months(4) == MAY).toBeTruthy
      expect(months(5) == JUNE).toBeTruthy
      expect(months(6) == JULY).toBeTruthy
      expect(months(7) == AUGUST).toBeTruthy
      expect(months(8) == SEPTEMBER).toBeTruthy
      expect(months(9) == OCTOBER).toBeTruthy
      expect(months(10) == NOVEMBER).toBeTruthy
      expect(months(11) == DECEMBER).toBeTruthy
    }

    it("should respond to `valueOf`") {
      expect(valueOf("JANUARY") == JANUARY).toBeTruthy
      expect(valueOf("FEBRUARY") == FEBRUARY).toBeTruthy
      expect(valueOf("MARCH") == MARCH).toBeTruthy
      expect(valueOf("APRIL") == APRIL).toBeTruthy
      expect(valueOf("MAY") == MAY).toBeTruthy
      expect(valueOf("JUNE") == JUNE).toBeTruthy
      expect(valueOf("JULY") == JULY).toBeTruthy
      expect(valueOf("AUGUST") == AUGUST).toBeTruthy
      expect(valueOf("SEPTEMBER") == SEPTEMBER).toBeTruthy
      expect(valueOf("OCTOBER") == OCTOBER).toBeTruthy
      expect(valueOf("NOVEMBER") == NOVEMBER).toBeTruthy
      expect(valueOf("DECEMBER") == DECEMBER).toBeTruthy

      expectThrows[IllegalArgumentException](valueOf(""))
    }

    it("should respond to `of`") {
      expect(of(1) == JANUARY).toBeTruthy
      expect(of(2) == FEBRUARY).toBeTruthy
      expect(of(3) == MARCH).toBeTruthy
      expect(of(4) == APRIL).toBeTruthy
      expect(of(5) == MAY).toBeTruthy
      expect(of(6) == JUNE).toBeTruthy
      expect(of(7) == JULY).toBeTruthy
      expect(of(8) == AUGUST).toBeTruthy
      expect(of(9) == SEPTEMBER).toBeTruthy
      expect(of(10) == OCTOBER).toBeTruthy
      expect(of(11) == NOVEMBER).toBeTruthy
      expect(of(12) == DECEMBER).toBeTruthy

      for (n <- Seq(Int.MinValue, 0, 13, Int.MaxValue))
        expectThrows[DateTimeException](of(n))
    }

    it("should respond to `from`") {
      for (m <- Month.values()) {
        expect(from(m) == m).toBeTruthy
        expect(from(LocalDate.of(1, m, 1)) == m).toBeTruthy
      }

      expectThrows[DateTimeException](from(DayOfWeek.MONDAY))
      expectThrows[DateTimeException](from(LocalTime.MIN))
    }
  }
}
