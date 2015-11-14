package org.scalajs.testsuite.javalib.time

import java.time.temporal.ChronoField
import java.time._

import org.scalajs.testsuite.javalib.time.temporal.TemporalAccessorTest

object DayOfWeekTest extends TemporalAccessorTest {
  import DayOfWeek._

  describe("java.time.DayOfWeek") {
    testTemporalAccessorApi(values():_*)

    it("should respond to `getValue`") {
      expect(MONDAY.getValue).toEqual(1)
      expect(TUESDAY.getValue).toEqual(2)
      expect(WEDNESDAY.getValue).toEqual(3)
      expect(THURSDAY.getValue).toEqual(4)
      expect(FRIDAY.getValue).toEqual(5)
      expect(SATURDAY.getValue).toEqual(6)
      expect(SUNDAY.getValue).toEqual(7)
    }

    it("should respond to `isSupported`") {
      for {
        d <- values
        f <- ChronoField.values
      } {
        if (f == ChronoField.DAY_OF_WEEK)
          expect(d.isSupported(f)).toBeTruthy
        else
          expect(d.isSupported(f)).toBeFalsy
      }
    }

    it("should respond to `getLong`") {
      for (d <- values())
        expect(d.getLong(ChronoField.DAY_OF_WEEK)).toEqual(d.getValue)
    }

    it("should respond to `plus`") {
      expect(SATURDAY.plus(Long.MinValue) == FRIDAY).toBeTruthy
      expect(THURSDAY.plus(-7) == THURSDAY).toBeTruthy
      expect(WEDNESDAY.plus(-3) == SUNDAY).toBeTruthy
      expect(TUESDAY.plus(-1) == MONDAY).toBeTruthy
      expect(MONDAY.plus(0) == MONDAY).toBeTruthy
      expect(THURSDAY.plus(1) == FRIDAY).toBeTruthy
      expect(FRIDAY.plus(3) == MONDAY).toBeTruthy
      expect(SATURDAY.plus(7) == SATURDAY).toBeTruthy
      expect(SUNDAY.plus(Long.MaxValue) == SUNDAY).toBeTruthy
    }

    it("should respond to `minus`") {
      expect(SATURDAY.minus(Long.MinValue) == SUNDAY).toBeTruthy
      expect(THURSDAY.minus(-7) == THURSDAY).toBeTruthy
      expect(FRIDAY.minus(-3) == MONDAY).toBeTruthy
      expect(TUESDAY.minus(-1) == WEDNESDAY).toBeTruthy
      expect(MONDAY.minus(0) == MONDAY).toBeTruthy
      expect(THURSDAY.minus(1) == WEDNESDAY).toBeTruthy
      expect(WEDNESDAY.minus(3) == SUNDAY).toBeTruthy
      expect(SATURDAY.minus(7) == SATURDAY).toBeTruthy
      expect(SUNDAY.plus(Long.MaxValue) == SUNDAY).toBeTruthy
    }

    it("should be comparable") {
      expect(WEDNESDAY.compareTo(WEDNESDAY)).toEqual(0)
      expect(MONDAY.compareTo(SUNDAY)).toBeLessThan(0)
      expect(SATURDAY.compareTo(TUESDAY)).toBeGreaterThan(0)
    }

    it("should respond to `values`") {
      val days = values()

      expect(days.length).toEqual(7)
      expect(days(0) == MONDAY).toBeTruthy
      expect(days(1) == TUESDAY).toBeTruthy
      expect(days(2) == WEDNESDAY).toBeTruthy
      expect(days(3) == THURSDAY).toBeTruthy
      expect(days(4) == FRIDAY).toBeTruthy
      expect(days(5) == SATURDAY).toBeTruthy
      expect(days(6) == SUNDAY).toBeTruthy
    }

    it("should respond to `valueOf`") {
      expect(valueOf("MONDAY") == MONDAY).toBeTruthy
      expect(valueOf("TUESDAY") == TUESDAY).toBeTruthy
      expect(valueOf("WEDNESDAY") == WEDNESDAY).toBeTruthy
      expect(valueOf("THURSDAY") == THURSDAY).toBeTruthy
      expect(valueOf("FRIDAY") == FRIDAY).toBeTruthy
      expect(valueOf("SATURDAY") == SATURDAY).toBeTruthy
      expect(valueOf("SUNDAY") == SUNDAY).toBeTruthy

      expectThrows[IllegalArgumentException](valueOf(""))
    }

    it("should respond to `of`") {
      expect(of(1) == MONDAY).toBeTruthy
      expect(of(2) == TUESDAY).toBeTruthy
      expect(of(3) == WEDNESDAY).toBeTruthy
      expect(of(4) == THURSDAY).toBeTruthy
      expect(of(5) == FRIDAY).toBeTruthy
      expect(of(6) == SATURDAY).toBeTruthy
      expect(of(7) == SUNDAY).toBeTruthy

      for (n <- Seq(Int.MinValue, 0, 8, Int.MaxValue))
        expectThrows[DateTimeException](of(n))
    }

    it("should respond to `from`") {
      for (d <- values)
        expect(from(d) == d).toBeTruthy
      for (d <- Seq(LocalDate.MIN, LocalDate.of(2012, 2, 29), LocalDate.MAX))
        expect(from(d) == d.getDayOfWeek).toBeTruthy

      expectThrows[DateTimeException](from(LocalTime.MIN))
      expectThrows[DateTimeException](from(Month.JANUARY))
    }
  }
}
