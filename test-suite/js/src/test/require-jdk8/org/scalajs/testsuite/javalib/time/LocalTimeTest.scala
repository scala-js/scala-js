package org.scalajs.testsuite.javalib.time

import java.time.temporal._
import java.time._

import org.scalajs.testsuite.javalib.time.temporal.TemporalTest

object LocalTimeTest extends TemporalTest {
  import LocalTime._
  import ChronoUnit._
  import ChronoField._

  describe("java.time.LocalTime") {
    val samples = Seq(MAX, NOON, MIN)

    testTemporalApi(samples: _*)

    it("should respond to `isSupported`") {
      for {
        t <- samples
        f <- ChronoField.values
      } {
        expect(t.isSupported(f) == f.isTimeBased).toBeTruthy
      }

      for {
        t <- samples
        u <- ChronoUnit.values
      } {
        expect(t.isSupported(u) == u.isTimeBased).toBeTruthy
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
        t <- samples
        field <- ChronoField.values if !t.isSupported(field)
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

    it("should respond to `getNano`") {
      expect(MIN.getNano).toEqual(0)
      expect(MAX.getNano).toEqual(999999999)
    }

    it("should respond to `with`") {
      for (t <- samples) {
        for (n <- Seq(0, 999, 999999, 999999999))
          testTemporal(t.`with`(NANO_OF_SECOND, n))(t.withNano(n))
        for (n <- Seq(0L, 1000000000L, 86399999999999L))
          testTemporal(t.`with`(NANO_OF_DAY, n))(ofNanoOfDay(n))
        for (n <- Seq(0, 999, 999999))
          testTemporal(t.`with`(MICRO_OF_SECOND, n))(t.withNano(n * 1000))
        for (n <- Seq(0L, 1000000L, 86399999999L))
          testTemporal(t.`with`(MICRO_OF_DAY, n))(ofNanoOfDay(n * 1000))
        for (n <- Seq(0, 500, 999))
          testTemporal(t.`with`(MILLI_OF_SECOND, n))(t.withNano(n * 1000000))
        for (n <- Seq(0L, 1000L, 86399999L))
          testTemporal(t.`with`(MILLI_OF_DAY, n))(ofNanoOfDay(n * 1000000))
        for (n <- Seq(0, 30, 59))
          testTemporal(t.`with`(SECOND_OF_MINUTE, n))(t.withSecond(n))
        for (n <- Seq(0, 60, 86399))
          testTemporal(t.`with`(SECOND_OF_DAY, n))(ofSecondOfDay(n).withNano(t.getNano))
        for (n <- Seq(0, 30, 59))
          testTemporal(t.`with`(MINUTE_OF_HOUR, n))(t.withMinute(n))
        for (n <- Seq(0, 60, 1439)) {
          testTemporal(t.`with`(MINUTE_OF_DAY, n)) {
            ofSecondOfDay(n * 60).withSecond(t.getSecond).withNano(t.getNano)
          }
        }
        for (n <- Seq(0, 6, 11)) {
          val h = (t.getHour / 12) * 12 + n
          testTemporal(t.`with`(HOUR_OF_AMPM, n))(t.withHour(h))
        }
        for (n <- Seq(1, 6, 12)) {
          val h = (t.getHour / 12) * 12 + (n % 12)
          testTemporal(t.`with`(CLOCK_HOUR_OF_AMPM, n))(t.withHour(h))
        }
        for (n <- Seq(0, 12, 23))
          testTemporal(t.`with`(HOUR_OF_DAY, n))(t.withHour(n))
        for (n <- Seq(1, 12, 24))
          testTemporal(t.`with`(CLOCK_HOUR_OF_DAY, n))(t.withHour(n % 24))
        for (n <- Seq(0, 1)) {
          val h = t.getHour % 12 + n * 12
          testTemporal(t.`with`(AMPM_OF_DAY, n))(t.withHour(h))
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
      }
    }

    it("should respond to `withHour`") {
      testTemporal(MIN.withHour(0))(MIN)
      testTemporal(MIN.withHour(12))(NOON)
      testTemporal(MIN.withHour(23))(of(23, 0))
      testTemporal(MAX.withHour(0))(of(0, 59, 59, 999999999))
      testTemporal(MAX.withHour(23))(MAX)

      for (t <- samples) {
        expectThrows[DateTimeException](t.withHour(Int.MinValue))
        expectThrows[DateTimeException](t.withHour(-1))
        expectThrows[DateTimeException](t.withHour(24))
        expectThrows[DateTimeException](t.withHour(Int.MaxValue))
      }
    }

    it("should respond to `withMinute`") {
      testTemporal(MIN.withMinute(0))(MIN)
      testTemporal(MIN.withMinute(30))(of(0, 30))
      testTemporal(MIN.withMinute(59))(of(0, 59))
      testTemporal(MAX.withMinute(0))(of(23, 0, 59, 999999999))
      testTemporal(MAX.withMinute(59))(MAX)

      for (t <- samples) {
        expectThrows[DateTimeException](t.withMinute(Int.MinValue))
        expectThrows[DateTimeException](t.withMinute(-1))
        expectThrows[DateTimeException](t.withMinute(60))
        expectThrows[DateTimeException](t.withMinute(Int.MaxValue))
      }
    }

    it("should respond to `withSecond`") {
      testTemporal(MIN.withSecond(0))(MIN)
      testTemporal(MIN.withSecond(30))(of(0, 0, 30))
      testTemporal(MIN.withSecond(59))(of(0, 0, 59))
      testTemporal(MAX.withSecond(0))(of(23, 59, 0, 999999999))
      testTemporal(MAX.withSecond(59))(MAX)

      for (t <- samples) {
        expectThrows[DateTimeException](t.withSecond(Int.MinValue))
        expectThrows[DateTimeException](t.withSecond(-1))
        expectThrows[DateTimeException](t.withSecond(60))
        expectThrows[DateTimeException](t.withSecond(Int.MaxValue))
      }
    }

    it("should respond to `withNano`") {
      testTemporal(MIN.withNano(0))(MIN)
      testTemporal(MIN.withNano(500000000))(of(0, 0, 0, 500000000))
      testTemporal(MIN.withNano(999999999))(of(0, 0, 0, 999999999))
      testTemporal(MAX.withNano(0))(of(23, 59, 59, 0))
      testTemporal(MAX.withNano(999999999))(MAX)

      for (t <- samples) {
        expectThrows[DateTimeException](t.withNano(Int.MinValue))
        expectThrows[DateTimeException](t.withNano(-1))
        expectThrows[DateTimeException](t.withNano(1000000000))
        expectThrows[DateTimeException](t.withNano(Int.MaxValue))
      }
    }

    it("should respond to `truncatedTo`") {
      testTemporal(MIN.truncatedTo(NANOS))(MIN)
      testTemporal(MAX.truncatedTo(NANOS))(MAX)
      testTemporal(MIN.truncatedTo(MICROS))(MIN)
      testTemporal(MAX.truncatedTo(MICROS))(of(23, 59, 59, 999999000))
      testTemporal(MIN.truncatedTo(MILLIS))(MIN)
      testTemporal(MAX.truncatedTo(MILLIS))(of(23, 59, 59, 999000000))
      testTemporal(MIN.truncatedTo(SECONDS))(MIN)
      testTemporal(MAX.truncatedTo(SECONDS))(of(23, 59, 59))
      testTemporal(MIN.truncatedTo(MINUTES))(MIN)
      testTemporal(MAX.truncatedTo(MINUTES))(of(23, 59))
      testTemporal(MIN.truncatedTo(HOURS))(MIN)
      testTemporal(MAX.truncatedTo(HOURS))(of(23, 0))
      testTemporal(MIN.truncatedTo(HALF_DAYS))(MIN)
      testTemporal(MAX.truncatedTo(HALF_DAYS))(of(12, 0))
      testTemporal(MIN.truncatedTo(DAYS))(MIN)
      testTemporal(MAX.truncatedTo(DAYS))(MIN)

      val illegalUnits = dateBasedUnits.filter(_ != DAYS)
      for {
        t <- samples
        u <- illegalUnits
      } {
        expectThrows[UnsupportedTemporalTypeException](t.truncatedTo(u))
      }
    }

    it("should respond to `plus`") {
      val values = Seq(Long.MinValue, -1000000000L, -86400L, -3600L, -60L, -1L, 0L,
          1L, 60L, 3600L, 86400L, 1000000000L, Long.MaxValue)

      for {
        t <- samples
        n <- values
      } {
        testTemporal(t.plus(n, NANOS))(t.plusNanos(n))
        testTemporal(t.plus(n, MICROS))(t.plusNanos((n % 86400000000L) * 1000))
        testTemporal(t.plus(n, MILLIS))(t.plusNanos((n % 86400000) * 1000000))
        testTemporal(t.plus(n, SECONDS))(t.plusSeconds(n))
        testTemporal(t.plus(n, MINUTES))(t.plusMinutes(n))
        testTemporal(t.plus(n, HOURS))(t.plusHours(n))
        testTemporal(t.plus(n, HALF_DAYS))(t.plusHours((n % 2) * 12))
      }
    }

    it("should respond to `plusHours`") {
      testTemporal(MIN.plusHours(Long.MinValue))(of(16, 0))
      testTemporal(MIN.plusHours(-24))(MIN)
      testTemporal(MIN.plusHours(-1))(of(23, 0))
      testTemporal(MIN.plusHours(0))(MIN)
      testTemporal(MIN.plusHours(1))(of(1, 0))
      testTemporal(MIN.plusHours(24))(MIN)
      testTemporal(MIN.plusHours(Long.MaxValue))(of(7, 0))
      testTemporal(MAX.plusHours(Long.MinValue))(of(15, 59, 59, 999999999))
      testTemporal(MAX.plusHours(-24))(MAX)
      testTemporal(MAX.plusHours(-1))(of(22, 59, 59, 999999999))
      testTemporal(MAX.plusHours(0))(MAX)
      testTemporal(MAX.plusHours(1))(of(0, 59, 59, 999999999))
      testTemporal(MAX.plusHours(24))(MAX)
      testTemporal(MAX.plusHours(Long.MaxValue))(of(6, 59, 59, 999999999))
    }

    it("should respond to `plusMinutes`") {
      testTemporal(MIN.plusMinutes(Long.MinValue))(of(5, 52))
      testTemporal(MIN.plusMinutes(-1440))(MIN)
      testTemporal(MIN.plusMinutes(-60))(of(23, 0))
      testTemporal(MIN.plusMinutes(-1))(of(23, 59))
      testTemporal(MIN.plusMinutes(0))(MIN)
      testTemporal(MIN.plusMinutes(1))(of(0, 1))
      testTemporal(MIN.plusMinutes(60))(of(1, 0))
      testTemporal(MIN.plusMinutes(1440))(MIN)
      testTemporal(MIN.plusMinutes(Long.MaxValue))(of(18, 7))
      testTemporal(MAX.plusMinutes(Long.MinValue))(of(5, 51, 59, 999999999))
      testTemporal(MAX.plusMinutes(-1440))(MAX)
      testTemporal(MAX.plusMinutes(-60))(of(22, 59, 59, 999999999))
      testTemporal(MAX.plusMinutes(-1))(of(23, 58, 59, 999999999))
      testTemporal(MAX.plusMinutes(0))(MAX)
      testTemporal(MAX.plusMinutes(1))(of(0, 0, 59, 999999999))
      testTemporal(MAX.plusMinutes(60))(of(0, 59, 59, 999999999))
      testTemporal(MAX.plusMinutes(1440))(MAX)
      testTemporal(MAX.plusMinutes(Long.MaxValue))(of(18, 6, 59, 999999999))
    }

    it("should respond to `plusSeconds`") {
      testTemporal(MIN.plusSeconds(Long.MinValue))(of(8, 29, 52))
      testTemporal(MIN.plusSeconds(-86400))(MIN)
      testTemporal(MIN.plusSeconds(-60))(of(23, 59))
      testTemporal(MIN.plusSeconds(-1))(of(23, 59, 59))
      testTemporal(MIN.plusSeconds(0))(MIN)
      testTemporal(MIN.plusSeconds(1))(of(0, 0, 1))
      testTemporal(MIN.plusSeconds(60))(of(0, 1))
      testTemporal(MIN.plusSeconds(86400))(MIN)
      testTemporal(MIN.plusSeconds(Long.MaxValue))(of(15, 30, 7))
      testTemporal(MAX.plusSeconds(Long.MinValue))(of(8, 29, 51, 999999999))
      testTemporal(MAX.plusSeconds(-86400))(MAX)
      testTemporal(MAX.plusSeconds(-60))(of(23, 58, 59, 999999999))
      testTemporal(MAX.plusSeconds(-1))(of(23, 59, 58, 999999999))
      testTemporal(MAX.plusSeconds(0))(MAX)
      testTemporal(MAX.plusSeconds(1))(of(0, 0, 0, 999999999))
      testTemporal(MAX.plusSeconds(60))(of(0, 0, 59, 999999999))
      testTemporal(MAX.plusSeconds(86400))(MAX)
      testTemporal(MAX.plusSeconds(Long.MaxValue))(of(15, 30, 6, 999999999))
    }

    it("should respond to `plusNanos`") {
      testTemporal(MIN.plusNanos(Long.MinValue))(of(0, 12, 43, 145224192))
      testTemporal(MIN.plusNanos(-86400000000000L))(MIN)
      testTemporal(MIN.plusNanos(-1000000000))(of(23, 59, 59))
      testTemporal(MIN.plusNanos(-1))(MAX)
      testTemporal(MIN.plusNanos(0))(MIN)
      testTemporal(MIN.plusNanos(1))(of(0, 0, 0, 1))
      testTemporal(MIN.plusNanos(1000000000))(of(0, 0, 1))
      testTemporal(MIN.plusNanos(86400000000000L))(MIN)
      testTemporal(MIN.plusNanos(Long.MaxValue))(of(23, 47, 16, 854775807))
      testTemporal(MAX.plusNanos(Long.MinValue))(of(0, 12, 43, 145224191))
      testTemporal(MAX.plusNanos(-86400000000000L))(MAX)
      testTemporal(MAX.plusNanos(-1000000000))(of(23, 59, 58, 999999999))
      testTemporal(MAX.plusNanos(-1))(of(23, 59, 59, 999999998))
      testTemporal(MAX.plusNanos(0))(MAX)
      testTemporal(MAX.plusNanos(1))(MIN)
      testTemporal(MAX.plusNanos(1000000000))(of(0, 0, 0, 999999999))
      testTemporal(MAX.plusNanos(86400000000000L))(MAX)
      testTemporal(MAX.plusNanos(Long.MaxValue))(of(23, 47, 16, 854775806))
    }

    it("should respond to `minusHours`") {
      testTemporal(MIN.minusHours(Long.MinValue))(of(8, 0))
      testTemporal(MIN.minusHours(-24))(MIN)
      testTemporal(MIN.minusHours(-1))(of(1, 0))
      testTemporal(MIN.minusHours(0))(MIN)
      testTemporal(MIN.minusHours(1))(of(23, 0))
      testTemporal(MIN.minusHours(24))(MIN)
      testTemporal(MIN.minusHours(Long.MaxValue))(of(17, 0))
      testTemporal(MAX.minusHours(Long.MinValue))(of(7, 59, 59, 999999999))
      testTemporal(MAX.minusHours(-24))(MAX)
      testTemporal(MAX.minusHours(-1))(of(0, 59, 59, 999999999))
      testTemporal(MAX.minusHours(0))(MAX)
      testTemporal(MAX.minusHours(1))(of(22, 59, 59, 999999999))
      testTemporal(MAX.minusHours(24))(MAX)
      testTemporal(MAX.minusHours(Long.MaxValue))(of(16, 59, 59, 999999999))
    }

    it("should respond to `minusMinutes`") {
      testTemporal(MIN.minusMinutes(Long.MinValue))(of(18, 8))
      testTemporal(MIN.minusMinutes(-1440))(MIN)
      testTemporal(MIN.minusMinutes(-60))(of(1, 0))
      testTemporal(MIN.minusMinutes(-1))(of(0, 1))
      testTemporal(MIN.minusMinutes(0))(MIN)
      testTemporal(MIN.minusMinutes(1))(of(23, 59))
      testTemporal(MIN.minusMinutes(60))(of(23, 0))
      testTemporal(MIN.minusMinutes(1440))(MIN)
      testTemporal(MIN.minusMinutes(Long.MaxValue))(of(5, 53))
      testTemporal(MAX.minusMinutes(Long.MinValue))(of(18, 7, 59, 999999999))
      testTemporal(MAX.minusMinutes(-1440))(MAX)
      testTemporal(MAX.minusMinutes(-60))(of(0, 59, 59, 999999999))
      testTemporal(MAX.minusMinutes(-1))(of(0, 0, 59, 999999999))
      testTemporal(MAX.minusMinutes(0))(MAX)
      testTemporal(MAX.minusMinutes(1))(of(23, 58, 59, 999999999))
      testTemporal(MAX.minusMinutes(60))(of(22, 59, 59, 999999999))
      testTemporal(MAX.minusMinutes(1440))(MAX)
      testTemporal(MAX.minusMinutes(Long.MaxValue))(of(5, 52, 59, 999999999))
    }

    it("should respond to `minusSeconds`") {
      testTemporal(MIN.minusSeconds(Long.MinValue))(of(15, 30, 8))
      testTemporal(MIN.minusSeconds(-86400))(MIN)
      testTemporal(MIN.minusSeconds(-60))(of(0, 1))
      testTemporal(MIN.minusSeconds(-1))(of(0, 0, 1))
      testTemporal(MIN.minusSeconds(0))(MIN)
      testTemporal(MIN.minusSeconds(1))(of(23, 59, 59))
      testTemporal(MIN.minusSeconds(60))(of(23, 59))
      testTemporal(MIN.minusSeconds(86400))(MIN)
      testTemporal(MIN.minusSeconds(Long.MaxValue))(of(8, 29, 53))
      testTemporal(MAX.minusSeconds(Long.MinValue))(of(15, 30, 7, 999999999))
      testTemporal(MAX.minusSeconds(-86400))(MAX)
      testTemporal(MAX.minusSeconds(-60))(of(0, 0, 59, 999999999))
      testTemporal(MAX.minusSeconds(-1))(of(0, 0, 0, 999999999))
      testTemporal(MAX.minusSeconds(0))(MAX)
      testTemporal(MAX.minusSeconds(1))(of(23, 59, 58, 999999999))
      testTemporal(MAX.minusSeconds(60))(of(23, 58, 59, 999999999))
      testTemporal(MAX.minusSeconds(86400))(MAX)
      testTemporal(MAX.minusSeconds(Long.MaxValue))(of(8, 29, 52, 999999999))
    }

    it("should respond to `minusNanos`") {
      testTemporal(MIN.minusNanos(Long.MinValue))(of(23, 47, 16, 854775808))
      testTemporal(MIN.minusNanos(-86400000000000L))(MIN)
      testTemporal(MIN.minusNanos(-1000000000))(of(0, 0, 1))
      testTemporal(MIN.minusNanos(-1))(of(0, 0, 0, 1))
      testTemporal(MIN.minusNanos(0))(MIN)
      testTemporal(MIN.minusNanos(1))(MAX)
      testTemporal(MIN.minusNanos(1000000000))(of(23, 59, 59))
      testTemporal(MIN.minusNanos(86400000000000L))(MIN)
      testTemporal(MIN.minusNanos(Long.MaxValue))(of(0, 12, 43, 145224193))
      testTemporal(MAX.minusNanos(Long.MinValue))(of(23, 47, 16, 854775807))
      testTemporal(MAX.minusNanos(-86400000000000L))(MAX)
      testTemporal(MAX.minusNanos(-1000000000))(of(0, 0, 0, 999999999))
      testTemporal(MAX.minusNanos(-1))(MIN)
      testTemporal(MAX.minusNanos(0))(MAX)
      testTemporal(MAX.minusNanos(1))(of(23, 59, 59, 999999998))
      testTemporal(MAX.minusNanos(1000000000))(of(23, 59, 58, 999999999))
      testTemporal(MAX.minusNanos(86400000000000L))(MAX)
      testTemporal(MAX.minusNanos(Long.MaxValue))(of(0, 12, 43, 145224192))
    }

    it("should respond to `adjustInto`") {
      for {
        t1 <- samples
        t2 <- samples
      } {
        testTemporal(t1.adjustInto(t2))(t1)
      }

      val ds = Seq(LocalDate.MIN, LocalDate.MAX)
      for {
        t <- samples
        d <- ds
      } {
        expectThrows[DateTimeException](t.adjustInto(d))
      }
    }

    it("should respond to `until`") {
      expect(MIN.until(MAX, NANOS) == 86399999999999L).toBeTruthy
      expect(MIN.until(MAX, MICROS) == 86399999999L).toBeTruthy
      expect(MIN.until(MAX, MILLIS) == 86399999).toBeTruthy
      expect(MIN.until(MAX, SECONDS) == 86399).toBeTruthy
      expect(MIN.until(MAX, MINUTES) == 1439).toBeTruthy
      expect(MIN.until(MAX, HOURS) == 23).toBeTruthy
      expect(MIN.until(MAX, HALF_DAYS) == 1).toBeTruthy

      for (u <- timeBasedUnits) {
        expect(MAX.until(MIN, u) == -MIN.until(MAX, u)).toBeTruthy
        expect(MIN.until(MIN, u) == 0).toBeTruthy
        expect(MAX.until(MAX, u) == 0).toBeTruthy
      }
    }

    it("should respond to `toSecondOfDay`") {
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

    it("should respond to `isAfter`") {
      expect(MIN.isAfter(MIN)).toBeFalsy
      expect(MIN.isAfter(MAX)).toBeFalsy
      expect(MAX.isAfter(MIN)).toBeTruthy
      expect(MAX.isAfter(MAX)).toBeFalsy
    }

    it("should respond to `isBefore`") {
      expect(MIN.isBefore(MIN)).toBeFalsy
      expect(MIN.isBefore(MAX)).toBeTruthy
      expect(MAX.isBefore(MIN)).toBeFalsy
      expect(MAX.isBefore(MAX)).toBeFalsy
    }

    it("should override `toString`") {
      expect(MIN.toString).toEqual("00:00")
      expect(MAX.toString).toEqual("23:59:59.999999999")
      expect(of(1, 1).toString).toEqual("01:01")
      expect(of(1, 1, 1).toString).toEqual("01:01:01")
      expect(of(1, 1, 1, 1).toString).toEqual("01:01:01.000000001")
      expect(of(1, 1, 1, 100000000).toString).toEqual("01:01:01.100")
      expect(of(1, 1, 1, 100100000).toString).toEqual("01:01:01.100100")
      expect(of(1, 1, 1, 100100100).toString).toEqual("01:01:01.100100100")
    }

    it("should respond to `now`") {
      expect(now() != null).toBeTruthy
    }

    it("should respond to `of`") {
      testTemporal(of(0, 0))(MIN)
      testTemporal(of(0, 0, 0))(MIN)
      testTemporal(of(0, 0, 0, 0))(MIN)
      testTemporal(of(23, 59))(of(23, 59, 0, 0))
      testTemporal(of(23, 59, 59))(of(23, 59, 59, 0))
      testTemporal(of(23, 59, 59, 999999999))(MAX)

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
      testTemporal(ofSecondOfDay(0))(MIN)
      testTemporal(ofSecondOfDay(1))(of(0, 0, 1))
      testTemporal(ofSecondOfDay(60))(of(0, 1))
      testTemporal(ofSecondOfDay(86399))(of(23, 59, 59))

      expectThrows[DateTimeException](ofSecondOfDay(-1))
      expectThrows[DateTimeException](ofSecondOfDay(86400))
    }

    it("should respond to `ofNanoOfDay`") {
      testTemporal(ofNanoOfDay(0))(MIN)
      testTemporal(ofNanoOfDay(1))(of(0, 0, 0, 1))
      testTemporal(ofNanoOfDay(1000000000))(of(0, 0, 1))
      testTemporal(ofNanoOfDay(86399999999999L))(MAX)

      expectThrows[DateTimeException](ofNanoOfDay(-1))
      expectThrows[DateTimeException](ofNanoOfDay(86400000000000L))
    }

    it("should respond to `from`") {
      for (t <- samples)
        testTemporal(from(t))(t)

      expectThrows[DateTimeException](from(LocalDate.of(2012, 2, 29)))
      expectThrows[DateTimeException](from(Month.JANUARY))
      expectThrows[DateTimeException](from(DayOfWeek.MONDAY))
    }
  }
}
