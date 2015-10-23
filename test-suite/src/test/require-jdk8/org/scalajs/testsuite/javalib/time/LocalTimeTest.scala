package org.scalajs.testsuite.javalib.time

import java.time.temporal._
import java.time.{DateTimeException, LocalTime}

import temporal.TemporalAccessorTest

object LocalTimeTest extends TemporalAccessorTest {
  import LocalTime._
  import ChronoUnit._
  import ChronoField._

  describe("java.time.LocalTime") {
    testTemporalAccessorApi(MAX, MIN, NOON)

    it("should respond to `isSupported`") {
      for {
        t <- Seq(MAX, MIN, NOON)
        f <- ChronoField.values()
      } {
        expect(t.isSupported(f) == f.isTimeBased).toBeTruthy
      }

      for {
        t <- Seq(MAX, MIN, NOON)
        u <- ChronoUnit.values()
      } {
        expect(t.isSupported(u) == u.isTimeBased).toBeTruthy
      }

      for (t <- Seq(MAX, MIN, NOON)) {
        expect(t.isSupported(null: TemporalUnit)).toBeFalsy
        expect(t.isSupported(null: TemporalField)).toBeFalsy
      }
    }

    it("should respond to `getLong`") {
      expect(MIN.getLong(NANO_OF_SECOND) == 0).toBeTruthy
      expect(MIN.getLong(NANO_OF_DAY) == 0).toBeTruthy
      expect(MIN.getLong(MICRO_OF_SECOND) == 0).toBeTruthy
      expect(MIN.getLong(MICRO_OF_DAY) == 0).toBeTruthy
      expect(MIN.getLong(MILLI_OF_SECOND) == 0).toBeTruthy
      expect(MIN.getLong(MILLI_OF_DAY) == 0).toBeTruthy
      expect(MIN.getLong(SECOND_OF_MINUTE) == 0).toBeTruthy
      expect(MIN.getLong(SECOND_OF_DAY) == 0).toBeTruthy
      expect(MIN.getLong(MINUTE_OF_HOUR) == 0).toBeTruthy
      expect(MIN.getLong(MINUTE_OF_DAY) == 0).toBeTruthy
      expect(MIN.getLong(HOUR_OF_AMPM) == 0).toBeTruthy
      expect(MIN.getLong(CLOCK_HOUR_OF_AMPM) == 12).toBeTruthy
      expect(MIN.getLong(HOUR_OF_DAY) == 0).toBeTruthy
      expect(MIN.getLong(CLOCK_HOUR_OF_DAY) == 24).toBeTruthy
      expect(MIN.getLong(AMPM_OF_DAY) == 0).toBeTruthy

      expect(MAX.getLong(NANO_OF_SECOND) == 999999999).toBeTruthy
      expect(MAX.getLong(NANO_OF_DAY) == 86399999999999L).toBeTruthy
      expect(MAX.getLong(MICRO_OF_SECOND) == 999999).toBeTruthy
      expect(MAX.getLong(MICRO_OF_DAY) == 86399999999L).toBeTruthy
      expect(MAX.getLong(MILLI_OF_SECOND) == 999).toBeTruthy
      expect(MAX.getLong(MILLI_OF_DAY) == 86399999).toBeTruthy
      expect(MAX.getLong(SECOND_OF_MINUTE) == 59).toBeTruthy
      expect(MAX.getLong(SECOND_OF_DAY) == 86399).toBeTruthy
      expect(MAX.getLong(MINUTE_OF_HOUR) == 59).toBeTruthy
      expect(MAX.getLong(MINUTE_OF_DAY) == 1439).toBeTruthy
      expect(MAX.getLong(HOUR_OF_AMPM) == 11).toBeTruthy
      expect(MAX.getLong(CLOCK_HOUR_OF_AMPM) == 11).toBeTruthy
      expect(MAX.getLong(HOUR_OF_DAY) == 23).toBeTruthy
      expect(MAX.getLong(CLOCK_HOUR_OF_DAY) == 23).toBeTruthy
      expect(MAX.getLong(AMPM_OF_DAY) == 1).toBeTruthy

      for {
        t <- Seq(MIN, MAX)
        field <- ChronoField.values() if !t.isSupported(field)
      } {
        expectThrows[UnsupportedTemporalTypeException](t.getLong(field))
      }
    }

    it("should respond to `getHour`") {
      expect(MIN.getHour).toEqual(0)
      expect(NOON.getHour).toEqual(12)
      expect(MAX.getHour).toEqual(23)
    }

    it("should respond to `getMinute`") {
      expect(MIN.getMinute).toEqual(0)
      expect(of(0, 30).getMinute).toEqual(30)
      expect(MAX.getMinute).toEqual(59)
    }

    it("should respond to `getSecond`") {
      expect(MIN.getSecond).toEqual(0)
      expect(of(0, 0, 30).getSecond).toEqual(30)
      expect(MAX.getSecond).toEqual(59)
    }

    it("should respond to `getNano") {
      expect(MIN.getNano).toEqual(0)
      expect(MAX.getNano).toEqual(999999999)
    }

    it("should respond to `with`") {
      for (t <- Seq(MIN, MAX)) {
        for (n <- Seq(0, 999, 999999, 999999999))
          expect(t.`with`(NANO_OF_SECOND, n) == t.withNano(n)).toBeTruthy
        for (n <- Seq(0L, 1000000000L, 86399999999999L))
          expect(t.`with`(NANO_OF_DAY, n) == ofNanoOfDay(n)).toBeTruthy
        for (n <- Seq(0, 999, 999999))
          expect(t.`with`(MICRO_OF_SECOND, n) == t.withNano(n * 1000)).toBeTruthy
        for (n <- Seq(0L, 1000000L, 86399999999L))
          expect(t.`with`(MICRO_OF_DAY, n) == ofNanoOfDay(n * 1000)).toBeTruthy
        for (n <- Seq(0, 500, 999))
          expect(t.`with`(MILLI_OF_SECOND, n) == t.withNano(n * 1000000)).toBeTruthy
        for (n <- Seq(0L, 1000L, 86399999L))
          expect(t.`with`(MILLI_OF_DAY, n) == ofNanoOfDay(n * 1000000)).toBeTruthy
        for (n <- Seq(0, 30, 59))
          expect(t.`with`(SECOND_OF_MINUTE, n) == t.withSecond(n)).toBeTruthy
        for (n <- Seq(0, 60, 86399)) {
          val res = ofSecondOfDay(n).withNano(t.getNano)
          expect(t.`with`(SECOND_OF_DAY, n) == res).toBeTruthy
        }
        for (n <- Seq(0, 30, 59))
          expect(t.`with`(MINUTE_OF_HOUR, n) == t.withMinute(n)).toBeTruthy
        for (n <- Seq(0, 60, 1439)) {
          val res = ofSecondOfDay(n * 60).withSecond(t.getSecond).withNano(t.getNano)
          expect(t.`with`(MINUTE_OF_DAY, n) == res).toBeTruthy
        }
        for (n <- Seq(0, 6, 11)) {
          val h = (t.getHour / 12) * 12 + n
          expect(t.`with`(HOUR_OF_AMPM, n) == t.withHour(h)).toBeTruthy
        }
        for (n <- Seq(1, 6, 12)) {
          val h = (t.getHour / 12) * 12 + (n % 12)
          expect(t.`with`(CLOCK_HOUR_OF_AMPM, n) == t.withHour(h)).toBeTruthy
        }
        for (n <- Seq(0, 12, 23))
          expect(t.`with`(HOUR_OF_DAY, n) == t.withHour(n)).toBeTruthy
        for (n <- Seq(1, 12, 24))
          expect(t.`with`(CLOCK_HOUR_OF_DAY, n) == t.withHour(n % 24)).toBeTruthy
        for (n <- Seq(0, 1)) {
          val h = t.getHour % 12 + n * 12
          expect(t.`with`(AMPM_OF_DAY, n) == t.withHour(h)).toBeTruthy
        }

        for (n <- Seq(Long.MinValue, -1L, 1000000000L, Long.MaxValue))
          expectThrows[DateTimeException](t.`with`(NANO_OF_SECOND, n))
        for (n <- Seq(Long.MinValue, -1L, 86400000000000L, Long.MaxValue))
          expectThrows[DateTimeException](t.`with`(NANO_OF_DAY, n))
        for (n <- Seq(Long.MinValue, -1L, 1000000L, Long.MaxValue))
          expectThrows[DateTimeException](t.`with`(MICRO_OF_SECOND, n))
        for (n <- Seq(Long.MinValue, -1L, 86400000000L, Long.MaxValue))
          expectThrows[DateTimeException](t.`with`(MICRO_OF_DAY, n))
        for (n <- Seq(Long.MinValue, -1L, 1000L, Long.MaxValue))
          expectThrows[DateTimeException](t.`with`(MILLI_OF_SECOND, n))
        for (n <- Seq(Long.MinValue, -1L, 86400000L, Long.MaxValue))
          expectThrows[DateTimeException](t.`with`(MILLI_OF_DAY, n))
        for (n <- Seq(Long.MinValue, -1L, 60L, Long.MaxValue))
          expectThrows[DateTimeException](t.`with`(SECOND_OF_MINUTE, n))
        for (n <- Seq(Long.MinValue, -1L, 86400L, Long.MaxValue))
          expectThrows[DateTimeException](t.`with`(SECOND_OF_DAY, n))
        for (n <- Seq(Long.MinValue, -1L, 60L, Long.MaxValue))
          expectThrows[DateTimeException](t.`with`(MINUTE_OF_HOUR, n))
        for (n <- Seq(Long.MinValue, -1L, 1440L, Long.MaxValue))
          expectThrows[DateTimeException](t.`with`(MINUTE_OF_DAY, n))
        for (n <- Seq(Long.MinValue, -1L, 12L, Long.MaxValue))
          expectThrows[DateTimeException](t.`with`(HOUR_OF_AMPM, n))
        for (n <- Seq(Long.MinValue, 0L, 13L, Long.MaxValue))
          expectThrows[DateTimeException](t.`with`(CLOCK_HOUR_OF_AMPM, n))
        for (n <- Seq(Long.MinValue, -1L, 24L, Long.MaxValue))
          expectThrows[DateTimeException](t.`with`(HOUR_OF_DAY, n))
        for (n <- Seq(Long.MinValue, 0L, 25L, Long.MaxValue))
          expectThrows[DateTimeException](t.`with`(CLOCK_HOUR_OF_DAY, n))
        for (n <- Seq(Long.MinValue, -1L, 2L, Long.MaxValue))
          expectThrows[DateTimeException](t.`with`(AMPM_OF_DAY, n))
        for {
          field <- ChronoField.values() if !t.isSupported(field)
          n <- Seq(Long.MinValue, 0, 1, Long.MaxValue)
        } {
          expectThrows[UnsupportedTemporalTypeException](t.`with`(field, n))
        }
      }
    }

    it("should respond to `withHour`") {
      expect(MIN.withHour(0) == MIN).toBeTruthy
      expect(MIN.withHour(12) == NOON).toBeTruthy
      expect(MIN.withHour(23) == of(23, 0)).toBeTruthy
      expect(MAX.withHour(0) == of(0, 59, 59, 999999999)).toBeTruthy
      expect(MAX.withHour(23) == MAX).toBeTruthy

      for (t <- Seq(MIN, MAX)) {
        expectThrows[DateTimeException](t.withHour(Int.MinValue))
        expectThrows[DateTimeException](t.withHour(-1))
        expectThrows[DateTimeException](t.withHour(24))
        expectThrows[DateTimeException](t.withHour(Int.MaxValue))
      }
    }

    it("should respond to `withMinute`") {
      expect(MIN.withMinute(0) == MIN).toBeTruthy
      expect(MIN.withMinute(30) == of(0, 30)).toBeTruthy
      expect(MIN.withMinute(59) == of(0, 59)).toBeTruthy
      expect(MAX.withMinute(0) == of(23, 0, 59, 999999999)).toBeTruthy
      expect(MAX.withMinute(59) == MAX).toBeTruthy

      for (t <- Seq(MIN, MAX)) {
        expectThrows[DateTimeException](t.withMinute(Int.MinValue))
        expectThrows[DateTimeException](t.withMinute(-1))
        expectThrows[DateTimeException](t.withMinute(60))
        expectThrows[DateTimeException](t.withMinute(Int.MaxValue))
      }
    }

    it("should respond to `withSecond`") {
      expect(MIN.withSecond(0) == MIN).toBeTruthy
      expect(MIN.withSecond(30) == of(0, 0, 30)).toBeTruthy
      expect(MIN.withSecond(59) == of(0, 0, 59)).toBeTruthy
      expect(MAX.withSecond(0) == of(23, 59, 0, 999999999)).toBeTruthy
      expect(MAX.withSecond(59) == MAX).toBeTruthy

      for (t <- Seq(MIN, MAX)) {
        expectThrows[DateTimeException](t.withSecond(Int.MinValue))
        expectThrows[DateTimeException](t.withSecond(-1))
        expectThrows[DateTimeException](t.withSecond(60))
        expectThrows[DateTimeException](t.withSecond(Int.MaxValue))
      }
    }

    it("should respond to `withNano`") {
      expect(MIN.withNano(0) == MIN).toBeTruthy
      expect(MIN.withNano(500000000) == of(0, 0, 0, 500000000)).toBeTruthy
      expect(MIN.withNano(999999999) == of(0, 0, 0, 999999999)).toBeTruthy
      expect(MAX.withNano(0) == of(23, 59, 59, 0)).toBeTruthy
      expect(MAX.withNano(999999999) == MAX).toBeTruthy

      for (t <- Seq(MIN, MAX)) {
        expectThrows[DateTimeException](t.withNano(Int.MinValue))
        expectThrows[DateTimeException](t.withNano(-1))
        expectThrows[DateTimeException](t.withNano(1000000000))
        expectThrows[DateTimeException](t.withNano(Int.MaxValue))
      }
    }

    it("should respond to `truncatedTo`") {
      expect(MIN.truncatedTo(NANOS) == MIN).toBeTruthy
      expect(MAX.truncatedTo(NANOS) == MAX).toBeTruthy
      expect(MIN.truncatedTo(MICROS) == MIN).toBeTruthy
      expect(MAX.truncatedTo(MICROS) == of(23, 59, 59, 999999000)).toBeTruthy
      expect(MIN.truncatedTo(MILLIS) == MIN).toBeTruthy
      expect(MAX.truncatedTo(MILLIS) == of(23, 59, 59, 999000000)).toBeTruthy
      expect(MIN.truncatedTo(SECONDS) == MIN).toBeTruthy
      expect(MAX.truncatedTo(SECONDS) == of(23, 59, 59)).toBeTruthy
      expect(MIN.truncatedTo(MINUTES) == MIN).toBeTruthy
      expect(MAX.truncatedTo(MINUTES) == of(23, 59)).toBeTruthy
      expect(MIN.truncatedTo(HOURS) == MIN).toBeTruthy
      expect(MAX.truncatedTo(HOURS) == of(23, 0)).toBeTruthy
      expect(MIN.truncatedTo(HALF_DAYS) == MIN).toBeTruthy
      expect(MAX.truncatedTo(HALF_DAYS) == of(12, 0)).toBeTruthy
      expect(MIN.truncatedTo(DAYS) == MIN).toBeTruthy
      expect(MAX.truncatedTo(DAYS) == MIN).toBeTruthy

      for {
        t <- Seq(MIN, MAX)
        u <- ChronoUnit.values() if u != DAYS && u.isDateBased
      } {
        expectThrows[UnsupportedTemporalTypeException](t.truncatedTo(u))
      }
    }

    it("should respond to `plus`") {
      for {
        t <- Seq(MIN, MAX)
        n <- Seq(Long.MinValue, -1000000000L, -86400L, -3600L, -60L, -1L, 0L,
          1L, 60L, 3600L, 86400L, 1000000000L, Long.MaxValue)
      } {
        expect(t.plus(n, NANOS) == t.plusNanos(n)).toBeTruthy
        expect(t.plus(n, MICROS) == t.plusNanos((n % 86400000000L) * 1000)).toBeTruthy
        expect(t.plus(n, MILLIS) == t.plusNanos((n % 86400000) * 1000000)).toBeTruthy
        expect(t.plus(n, SECONDS) == t.plusSeconds(n)).toBeTruthy
        expect(t.plus(n, MINUTES) == t.plusMinutes(n)).toBeTruthy
        expect(t.plus(n, HOURS) == t.plusHours(n)).toBeTruthy
        expect(t.plus(n, HALF_DAYS) == t.plusHours((n % 2) * 12)).toBeTruthy

        for(u <- ChronoUnit.values() if u.isDateBased)
          expectThrows[UnsupportedTemporalTypeException](t.plus(n, u))
      }
    }

    it("should respond to `plusHours") {
      expect(MIN.plusHours(Long.MinValue) == of(16, 0)).toBeTruthy
      expect(MIN.plusHours(-24) == MIN).toBeTruthy
      expect(MIN.plusHours(-1) == of(23, 0)).toBeTruthy
      expect(MIN.plusHours(0) == MIN).toBeTruthy
      expect(MIN.plusHours(1) == of(1, 0)).toBeTruthy
      expect(MIN.plusHours(24) == MIN).toBeTruthy
      expect(MIN.plusHours(Long.MaxValue) == of(7, 0)).toBeTruthy
      expect(MAX.plusHours(Long.MinValue) == of(15, 59, 59, 999999999)).toBeTruthy
      expect(MAX.plusHours(-24) == MAX).toBeTruthy
      expect(MAX.plusHours(-1) == of(22, 59, 59, 999999999)).toBeTruthy
      expect(MAX.plusHours(0) == MAX).toBeTruthy
      expect(MAX.plusHours(1) == of(0, 59, 59, 999999999)).toBeTruthy
      expect(MAX.plusHours(24) == MAX).toBeTruthy
      expect(MAX.plusHours(Long.MaxValue) == of(6, 59, 59, 999999999)).toBeTruthy
    }

    it("should respond to `plusMinutes") {
      expect(MIN.plusMinutes(Long.MinValue) == of(5, 52)).toBeTruthy
      expect(MIN.plusMinutes(-1440) == MIN).toBeTruthy
      expect(MIN.plusMinutes(-60) == of(23, 0)).toBeTruthy
      expect(MIN.plusMinutes(-1) == of(23, 59)).toBeTruthy
      expect(MIN.plusMinutes(0) == MIN).toBeTruthy
      expect(MIN.plusMinutes(1) == of(0, 1)).toBeTruthy
      expect(MIN.plusMinutes(60) == of(1, 0)).toBeTruthy
      expect(MIN.plusMinutes(1440) == MIN).toBeTruthy
      expect(MIN.plusMinutes(Long.MaxValue) == of(18, 7)).toBeTruthy
      expect(MAX.plusMinutes(Long.MinValue) == of(5, 51, 59, 999999999)).toBeTruthy
      expect(MAX.plusMinutes(-1440) == MAX).toBeTruthy
      expect(MAX.plusMinutes(-60) == of(22, 59, 59, 999999999)).toBeTruthy
      expect(MAX.plusMinutes(-1) == of(23, 58, 59, 999999999)).toBeTruthy
      expect(MAX.plusMinutes(0) == MAX).toBeTruthy
      expect(MAX.plusMinutes(1) == of(0, 0, 59, 999999999)).toBeTruthy
      expect(MAX.plusMinutes(60) == of(0, 59, 59, 999999999)).toBeTruthy
      expect(MAX.plusMinutes(1440) == MAX).toBeTruthy
      expect(MAX.plusMinutes(Long.MaxValue) == of(18, 6, 59, 999999999)).toBeTruthy
    }

    it("should respond to `plusSeconds") {
      expect(MIN.plusSeconds(Long.MinValue) == of(8, 29, 52)).toBeTruthy
      expect(MIN.plusSeconds(-86400) == MIN).toBeTruthy
      expect(MIN.plusSeconds(-60) == of(23, 59)).toBeTruthy
      expect(MIN.plusSeconds(-1) == of(23, 59, 59)).toBeTruthy
      expect(MIN.plusSeconds(0) == MIN).toBeTruthy
      expect(MIN.plusSeconds(1) == of(0, 0, 1)).toBeTruthy
      expect(MIN.plusSeconds(60) == of(0, 1)).toBeTruthy
      expect(MIN.plusSeconds(86400) == MIN).toBeTruthy
      expect(MIN.plusSeconds(Long.MaxValue) == of(15, 30, 7)).toBeTruthy
      expect(MAX.plusSeconds(Long.MinValue) == of(8, 29, 51, 999999999)).toBeTruthy
      expect(MAX.plusSeconds(-86400) == MAX).toBeTruthy
      expect(MAX.plusSeconds(-60) == of(23, 58, 59, 999999999)).toBeTruthy
      expect(MAX.plusSeconds(-1) == of(23, 59, 58, 999999999)).toBeTruthy
      expect(MAX.plusSeconds(0) == MAX).toBeTruthy
      expect(MAX.plusSeconds(1) == of(0, 0, 0, 999999999)).toBeTruthy
      expect(MAX.plusSeconds(60) == of(0, 0, 59, 999999999)).toBeTruthy
      expect(MAX.plusSeconds(86400) == MAX).toBeTruthy
      expect(MAX.plusSeconds(Long.MaxValue) == of(15, 30, 6, 999999999)).toBeTruthy
    }

    it("should respond to `plusNanos") {
      expect(MIN.plusNanos(Long.MinValue) == of(0, 12, 43, 145224192)).toBeTruthy
      expect(MIN.plusNanos(-86400000000000L) == MIN).toBeTruthy
      expect(MIN.plusNanos(-1000000000) == of(23, 59, 59)).toBeTruthy
      expect(MIN.plusNanos(-1) == MAX).toBeTruthy
      expect(MIN.plusNanos(0) == MIN).toBeTruthy
      expect(MIN.plusNanos(1) == of(0, 0, 0, 1)).toBeTruthy
      expect(MIN.plusNanos(1000000000) == of(0, 0, 1)).toBeTruthy
      expect(MIN.plusNanos(86400000000000L) == MIN).toBeTruthy
      expect(MIN.plusNanos(Long.MaxValue) == of(23, 47, 16, 854775807)).toBeTruthy
      expect(MAX.plusNanos(Long.MinValue) == of(0, 12, 43, 145224191)).toBeTruthy
      expect(MAX.plusNanos(-86400000000000L) == MAX).toBeTruthy
      expect(MAX.plusNanos(-1000000000) == of(23, 59, 58, 999999999)).toBeTruthy
      expect(MAX.plusNanos(-1) == of(23, 59, 59, 999999998)).toBeTruthy
      expect(MAX.plusNanos(0) == MAX).toBeTruthy
      expect(MAX.plusNanos(1) == MIN).toBeTruthy
      expect(MAX.plusNanos(1000000000) == of(0, 0, 0, 999999999)).toBeTruthy
      expect(MAX.plusNanos(86400000000000L) == MAX).toBeTruthy
      expect(MAX.plusNanos(Long.MaxValue) == of(23, 47, 16, 854775806)).toBeTruthy
    }

    it("should respond to `minus`") {
      for {
        t <- Seq(MIN, MAX)
        n <- Seq(Long.MinValue, -1000000000L, -86400L, -3600L, -60L, -1L, 0L,
          1L, 60L, 3600L, 86400L, 1000000000L, Long.MaxValue)
      } {
        expect(t.minus(n, NANOS) == t.minusNanos(n)).toBeTruthy
        expect(t.minus(n, MICROS) == t.minusNanos((n % 86400000000L) * 1000)).toBeTruthy
        expect(t.minus(n, MILLIS) == t.minusNanos((n % 86400000) * 1000000)).toBeTruthy
        expect(t.minus(n, SECONDS) == t.minusSeconds(n)).toBeTruthy
        expect(t.minus(n, MINUTES) == t.minusMinutes(n)).toBeTruthy
        expect(t.minus(n, HOURS) == t.minusHours(n)).toBeTruthy
        expect(t.minus(n, HALF_DAYS) == t.minusHours((n % 2) * 12)).toBeTruthy

        for(u <- ChronoUnit.values() if u.isDateBased)
          expectThrows[UnsupportedTemporalTypeException](t.minus(n, u))
      }
    }

    it("should respond to `minusHours") {
      expect(MIN.minusHours(Long.MinValue) == of(8, 0)).toBeTruthy
      expect(MIN.minusHours(-24) == MIN).toBeTruthy
      expect(MIN.minusHours(-1) == of(1, 0)).toBeTruthy
      expect(MIN.minusHours(0) == MIN).toBeTruthy
      expect(MIN.minusHours(1) == of(23, 0)).toBeTruthy
      expect(MIN.minusHours(24) == MIN).toBeTruthy
      expect(MIN.minusHours(Long.MaxValue) == of(17, 0)).toBeTruthy
      expect(MAX.minusHours(Long.MinValue) == of(7, 59, 59, 999999999)).toBeTruthy
      expect(MAX.minusHours(-24) == MAX).toBeTruthy
      expect(MAX.minusHours(-1) == of(0, 59, 59, 999999999)).toBeTruthy
      expect(MAX.minusHours(0) == MAX).toBeTruthy
      expect(MAX.minusHours(1) == of(22, 59, 59, 999999999)).toBeTruthy
      expect(MAX.minusHours(24) == MAX).toBeTruthy
      expect(MAX.minusHours(Long.MaxValue) == of(16, 59, 59, 999999999)).toBeTruthy
    }

    it("should respond to `minusMinutes") {
      expect(MIN.minusMinutes(Long.MinValue) == of(18, 8)).toBeTruthy
      expect(MIN.minusMinutes(-1440) == MIN).toBeTruthy
      expect(MIN.minusMinutes(-60) == of(1, 0)).toBeTruthy
      expect(MIN.minusMinutes(-1) == of(0, 1)).toBeTruthy
      expect(MIN.minusMinutes(0) == MIN).toBeTruthy
      expect(MIN.minusMinutes(1) == of(23, 59)).toBeTruthy
      expect(MIN.minusMinutes(60) == of(23, 0)).toBeTruthy
      expect(MIN.minusMinutes(1440) == MIN).toBeTruthy
      expect(MIN.minusMinutes(Long.MaxValue) == of(5, 53)).toBeTruthy
      expect(MAX.minusMinutes(Long.MinValue) == of(18, 7, 59, 999999999)).toBeTruthy
      expect(MAX.minusMinutes(-1440) == MAX).toBeTruthy
      expect(MAX.minusMinutes(-60) == of(0, 59, 59, 999999999)).toBeTruthy
      expect(MAX.minusMinutes(-1) == of(0, 0, 59, 999999999)).toBeTruthy
      expect(MAX.minusMinutes(0) == MAX).toBeTruthy
      expect(MAX.minusMinutes(1) == of(23, 58, 59, 999999999)).toBeTruthy
      expect(MAX.minusMinutes(60) == of(22, 59, 59, 999999999)).toBeTruthy
      expect(MAX.minusMinutes(1440) == MAX).toBeTruthy
      expect(MAX.minusMinutes(Long.MaxValue) == of(5, 52, 59, 999999999)).toBeTruthy
    }

    it("should respond to `minusSeconds") {
      expect(MIN.minusSeconds(Long.MinValue) == of(15, 30, 8)).toBeTruthy
      expect(MIN.minusSeconds(-86400) == MIN).toBeTruthy
      expect(MIN.minusSeconds(-60) == of(0, 1)).toBeTruthy
      expect(MIN.minusSeconds(-1) == of(0, 0, 1)).toBeTruthy
      expect(MIN.minusSeconds(0) == MIN).toBeTruthy
      expect(MIN.minusSeconds(1) == of(23, 59, 59)).toBeTruthy
      expect(MIN.minusSeconds(60) == of(23, 59)).toBeTruthy
      expect(MIN.minusSeconds(86400) == MIN).toBeTruthy
      expect(MIN.minusSeconds(Long.MaxValue) == of(8, 29, 53)).toBeTruthy
      expect(MAX.minusSeconds(Long.MinValue) == of(15, 30, 7, 999999999)).toBeTruthy
      expect(MAX.minusSeconds(-86400) == MAX).toBeTruthy
      expect(MAX.minusSeconds(-60) == of(0, 0, 59, 999999999)).toBeTruthy
      expect(MAX.minusSeconds(-1) == of(0, 0, 0, 999999999)).toBeTruthy
      expect(MAX.minusSeconds(0) == MAX).toBeTruthy
      expect(MAX.minusSeconds(1) == of(23, 59, 58, 999999999)).toBeTruthy
      expect(MAX.minusSeconds(60) == of(23, 58, 59, 999999999)).toBeTruthy
      expect(MAX.minusSeconds(86400) == MAX).toBeTruthy
      expect(MAX.minusSeconds(Long.MaxValue) == of(8, 29, 52, 999999999)).toBeTruthy
    }

    it("should respond to `minusNanos") {
      expect(MIN.minusNanos(Long.MinValue) == of(23, 47, 16, 854775808)).toBeTruthy
      expect(MIN.minusNanos(-86400000000000L) == MIN).toBeTruthy
      expect(MIN.minusNanos(-1000000000) == of(0, 0, 1)).toBeTruthy
      expect(MIN.minusNanos(-1) == of(0, 0, 0, 1)).toBeTruthy
      expect(MIN.minusNanos(0) == MIN).toBeTruthy
      expect(MIN.minusNanos(1) == MAX).toBeTruthy
      expect(MIN.minusNanos(1000000000) == of(23, 59, 59)).toBeTruthy
      expect(MIN.minusNanos(86400000000000L) == MIN).toBeTruthy
      expect(MIN.minusNanos(Long.MaxValue) == of(0, 12, 43, 145224193)).toBeTruthy
      expect(MAX.minusNanos(Long.MinValue) == of(23, 47, 16, 854775807)).toBeTruthy
      expect(MAX.minusNanos(-86400000000000L) == MAX).toBeTruthy
      expect(MAX.minusNanos(-1000000000) == of(0, 0, 0, 999999999)).toBeTruthy
      expect(MAX.minusNanos(-1) == MIN).toBeTruthy
      expect(MAX.minusNanos(0) == MAX).toBeTruthy
      expect(MAX.minusNanos(1) == of(23, 59, 59, 999999998)).toBeTruthy
      expect(MAX.minusNanos(1000000000) == of(23, 59, 58, 999999999)).toBeTruthy
      expect(MAX.minusNanos(86400000000000L) == MAX).toBeTruthy
      expect(MAX.minusNanos(Long.MaxValue) == of(0, 12, 43, 145224192)).toBeTruthy
    }

    it("should respond to `adjustInto`") {
      for {
        t1 <- Seq(MIN, MAX)
        t2 <- Seq(MIN, MAX)
      } {
        expect(t1.adjustInto(t2) == t1).toBeTruthy
      }

      // TODO: Add tests for other Temporals
    }

    it("should respond to `until`") {
      expect(MIN.until(MAX, NANOS) == 86399999999999L).toBeTruthy
      expect(MIN.until(MAX, MICROS) == 86399999999L).toBeTruthy
      expect(MIN.until(MAX, MILLIS) == 86399999).toBeTruthy
      expect(MIN.until(MAX, SECONDS) == 86399).toBeTruthy
      expect(MIN.until(MAX, MINUTES) == 1439).toBeTruthy
      expect(MIN.until(MAX, HOURS) == 23).toBeTruthy
      expect(MIN.until(MAX, HALF_DAYS) == 1).toBeTruthy
      for (u <- ChronoUnit.values() if u.isTimeBased) {
        expect(MAX.until(MIN, u) == -MIN.until(MAX, u)).toBeTruthy
        expect(MIN.until(MIN, u) == 0).toBeTruthy
        expect(MAX.until(MAX, u) == 0).toBeTruthy
      }

      for {
        t1 <- Seq(MIN, MAX)
        t2 <- Seq(MIN, MAX)
        u <- ChronoUnit.values() if u.isDateBased
      } {
        expectThrows[UnsupportedTemporalTypeException](t1.until(t2, u))
      }
    }

    it("should respond to `toSecondOfDay") {
      expect(MIN.toSecondOfDay).toEqual(0)
      expect(MAX.toSecondOfDay).toEqual(86399)
    }

    it("should respond to `toNanoOfDay`") {
      expect(MIN.toNanoOfDay == 0).toBeTruthy
      expect(MAX.toNanoOfDay == 86399999999999L).toBeTruthy
    }

    it("should be comparable") {
      expect(MIN.compareTo(MIN)).toEqual(0)
      expect(MIN.compareTo(MAX)).toBeLessThan(0)
      expect(MAX.compareTo(MIN)).toBeGreaterThan(0)
      expect(MAX.compareTo(MAX)).toEqual(0)
    }

    it("should respond to `isAfter") {
      expect(MIN.isAfter(MIN)).toBeFalsy
      expect(MIN.isAfter(MAX)).toBeFalsy
      expect(MAX.isAfter(MIN)).toBeTruthy
      expect(MAX.isAfter(MAX)).toBeFalsy
    }

    it("should respond to `isBefore") {
      expect(MIN.isBefore(MIN)).toBeFalsy
      expect(MIN.isBefore(MAX)).toBeTruthy
      expect(MAX.isBefore(MIN)).toBeFalsy
      expect(MAX.isBefore(MAX)).toBeFalsy
    }

    it("should override `toString`") {
      expect(MIN.toString).toEqual("00:00")
      expect(MAX.toString).toEqual("23:59:59.999999999")
      expect(of(1 ,1).toString).toEqual("01:01")
      expect(of(1, 1, 1).toString).toEqual("01:01:01")
      expect(of(1, 1, 1, 1).toString).toEqual("01:01:01.000000001")
      expect(of(1, 1, 1, 100000000).toString).toEqual("01:01:01.100")
      expect(of(1, 1, 1, 100100000).toString).toEqual("01:01:01.100100")
      expect(of(1, 1, 1, 100100100).toString).toEqual("01:01:01.100100100")
    }

    it("should respond to `now`") {
      now()
    }

    it("should respond to `of`") {
      expect(of(0, 0) == MIN).toBeTruthy
      expect(of(0, 0, 0) == MIN).toBeTruthy
      expect(of(0, 0, 0, 0) == MIN).toBeTruthy
      expect(of(23, 59) == of(23, 59, 0, 0)).toBeTruthy
      expect(of(23, 59, 59) == of(23, 59, 59, 0)).toBeTruthy
      expect(of(23, 59, 59, 999999999) == MAX).toBeTruthy

      expectThrows[DateTimeException](of(-1, 0))
      expectThrows[DateTimeException](of(0, -1))
      expectThrows[DateTimeException](of(0, 0, -1))
      expectThrows[DateTimeException](of(0, 0, 0, -1))
      expectThrows[DateTimeException](of(24, 0))
      expectThrows[DateTimeException](of(0, 60))
      expectThrows[DateTimeException](of(0, 0, 60))
      expectThrows[DateTimeException](of(0, 0, 0, 1000000000))
    }

    it("should respond to `ofSecondOfDay`") {
      expect(ofSecondOfDay(0) == MIN).toBeTruthy
      expect(ofSecondOfDay(1) == of(0, 0, 1)).toBeTruthy
      expect(ofSecondOfDay(60) == of(0, 1)).toBeTruthy
      expect(ofSecondOfDay(86399) == of(23, 59, 59)).toBeTruthy

      expectThrows[DateTimeException](ofSecondOfDay(-1))
      expectThrows[DateTimeException](ofSecondOfDay(86400))
    }

    it("should respond to `ofNanoOfDay`") {
      expect(ofNanoOfDay(0) == MIN).toBeTruthy
      expect(ofNanoOfDay(1) == of(0, 0, 0, 1)).toBeTruthy
      expect(ofNanoOfDay(1000000000) == of(0, 0, 1)).toBeTruthy
      expect(ofNanoOfDay(86399999999999L) == MAX).toBeTruthy

      expectThrows[DateTimeException](ofNanoOfDay(-1))
      expectThrows[DateTimeException](ofNanoOfDay(86400000000000L))
    }

    it("should respond to `from`") {
      expect(from(MIN) == MIN).toBeTruthy
      expect(from(MAX) == MAX).toBeTruthy

      // TODO: Add tests for other classes
    }
  }
}
