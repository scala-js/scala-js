package org.scalajs.testsuite.javalib.time

import java.time.temporal.{ChronoUnit, UnsupportedTemporalTypeException}
import java.time._

import org.scalajs.jasminetest.JasmineTest
import org.scalajs.testsuite.utils.ExpectExceptions

object DurationTest extends JasmineTest with ExpectExceptions {
  import Duration._
  import ChronoUnit._

  final val dmin = Duration.ofSeconds(Long.MinValue)
  final val dmax = Duration.ofSeconds(Long.MaxValue, 999999999)

  describe("java.time.Duration") {
    it("should respond to `get`") {
      val d0 = ofSeconds(0, 1)

      expect(dmin.get(SECONDS) == Long.MinValue).toBeTruthy
      expect(dmin.get(NANOS) == 0).toBeTruthy
      expect(ZERO.get(SECONDS) == 0).toBeTruthy
      expect(ZERO.get(NANOS) == 0).toBeTruthy
      expect(d0.get(SECONDS) == 0).toBeTruthy
      expect(d0.get(NANOS) == 1).toBeTruthy
      expect(dmax.get(SECONDS) == Long.MaxValue).toBeTruthy
      expect(dmax.get(NANOS) == 999999999).toBeTruthy

      for (d <- Seq(dmin, ZERO, d0, dmax)) {
        expectThrows[UnsupportedTemporalTypeException](d.get(MICROS))
        expectThrows[UnsupportedTemporalTypeException](d.get(MILLIS))
        expectThrows[UnsupportedTemporalTypeException](d.get(MINUTES))
        expectThrows[UnsupportedTemporalTypeException](d.get(HOURS))
        expectThrows[UnsupportedTemporalTypeException](d.get(HALF_DAYS))
        expectThrows[UnsupportedTemporalTypeException](d.get(DAYS))
        expectThrows[UnsupportedTemporalTypeException](d.get(WEEKS))
        expectThrows[UnsupportedTemporalTypeException](d.get(MONTHS))
        expectThrows[UnsupportedTemporalTypeException](d.get(YEARS))
        expectThrows[UnsupportedTemporalTypeException](d.get(DECADES))
        expectThrows[UnsupportedTemporalTypeException](d.get(CENTURIES))
        expectThrows[UnsupportedTemporalTypeException](d.get(MILLENNIA))
        expectThrows[UnsupportedTemporalTypeException](d.get(ERAS))
        expectThrows[UnsupportedTemporalTypeException](d.get(FOREVER))
      }
    }

    it("should respond to `getUnits`") {
      for (d <- Seq(dmin, ZERO, ofSeconds(0, 1), dmax)) {
        expect(d.getUnits.size()).toEqual(2)
        expect(d.getUnits.get(0) == SECONDS).toBeTruthy
        expect(d.getUnits.get(1) == NANOS).toBeTruthy
      }
    }

    it("should respond to `isZero`") {
      expect(dmin.isZero).toBeFalsy
      expect(ofSeconds(-1).isZero).toBeFalsy
      expect(ofSeconds(0, -1).isZero).toBeFalsy
      expect(ZERO.isZero).toBeTruthy
      expect(ofSeconds(0, 1).isZero).toBeFalsy
      expect(ofSeconds(1).isZero).toBeFalsy
      expect(dmax.isZero).toBeFalsy
    }

    it("should respond to `isNegative`") {
      expect(dmin.isNegative).toBeTruthy
      expect(ofSeconds(-1).isNegative).toBeTruthy
      expect(ofSeconds(0, -1).isNegative).toBeTruthy
      expect(ZERO.isNegative).toBeFalsy
      expect(ofSeconds(0, 1).isNegative).toBeFalsy
      expect(dmax.isNegative).toBeFalsy
    }

    it("should respond to `getSeconds`") {
      expect(dmin.getSeconds == Long.MinValue).toBeTruthy
      expect(ofSeconds(0, -1).getSeconds == -1).toBeTruthy
      expect(ZERO.getSeconds == 0).toBeTruthy
      expect(ofSeconds(1, -1).getSeconds == 0).toBeTruthy
      expect(ofSeconds(1).getSeconds == 1).toBeTruthy
      expect(dmax.getSeconds == Long.MaxValue).toBeTruthy
    }

    it("should respond to `getNano`") {
      expect(dmin.getNano).toEqual(0)
      expect(ZERO.getNano).toEqual(0)
      expect(ofSeconds(0, 1).getNano).toEqual(1)
      expect(dmax.getNano).toEqual(999999999)
    }

    it("should respond to `withSeconds`") {
      expect(dmin.withSeconds(0) == ZERO).toBeTruthy
      expect(dmax.withSeconds(1) == ofSeconds(2, -1)).toBeTruthy
      expect(ZERO.withSeconds(Long.MinValue) == dmin).toBeTruthy
    }

    it("should respond to `withNanos`") {
      val d0 = ofSeconds(1, 1)

      expect(dmin.withNanos(999999999) == ofSeconds(Long.MinValue + 1, -1)).toBeTruthy
      expect(d0.withNanos(0) == ofSeconds(1, 0)).toBeTruthy
      expect(d0.withNanos(999999999) == ofSeconds(2, -1)).toBeTruthy
      expect(dmax.withNanos(0) == ofSeconds(Long.MaxValue)).toBeTruthy

      for (d <- Seq(dmin, ZERO, d0, dmax)) {
        expectThrows[DateTimeException](d.withNanos(-1))
        expectThrows[DateTimeException](d.withNanos(1000000000))
      }
    }

    it("should respond to `plus`") {
      val d0 = ofSeconds(0, 1)
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(1, 999999999)
      val d3 = ofSeconds(-2, 1)

      expect(d1.plus(d1) == ofSeconds(2, 2)).toBeTruthy
      expect(d1.plus(ZERO) == d1).toBeTruthy
      expect(ZERO.plus(d1) == d1).toBeTruthy
      expect(d1.plus(d2) == ofSeconds(3)).toBeTruthy
      expect(d1.plus(d3) == ofSeconds(-1, 2)).toBeTruthy

      expectThrows[ArithmeticException](dmax.plus(d0))
      expectThrows[ArithmeticException](dmin.plus(d0.negated))

      for {
        d <- Seq(ZERO, d0, d1, d2, d3)
        n <- Seq(-100000000000000L, 1L, 0L, 1L, 100000000000000L)
      } {
        expect(d.plus(n, NANOS) == d.plusNanos(n)).toBeTruthy
        expect(d.plus(n, MICROS) == d.plusNanos(n * 1000)).toBeTruthy
        expect(d.plus(n, MILLIS) == d.plusMillis(n)).toBeTruthy
        expect(d.plus(n, SECONDS) == d.plusSeconds(n)).toBeTruthy
        expect(d.plus(n, MINUTES) == d.plusMinutes(n)).toBeTruthy
        expect(d.plus(n, HOURS) == d.plusHours(n)).toBeTruthy
        expect(d.plus(n, HALF_DAYS) == d.plusHours(n * 12)).toBeTruthy
        expect(d.plus(n, DAYS) == d.plusDays(n)).toBeTruthy
      }

      expectThrows[ArithmeticException](dmax.plus(1, NANOS))
      expectThrows[ArithmeticException](dmin.plus(-1, NANOS))

      for {
        d <- Seq(ZERO, d0, d1, d2, d3, dmax, dmin)
        n <- Seq(Long.MinValue, Long.MaxValue)
      } {
        expectThrows[ArithmeticException](d.plus(n, MINUTES))
        expectThrows[ArithmeticException](d.plus(n, HOURS))
        expectThrows[ArithmeticException](d.plus(n, HALF_DAYS))
        expectThrows[ArithmeticException](d.plus(n, DAYS))
      }

      for {
        d <- Seq(ZERO, d0, d1, d2, d3, dmax, dmin)
        n <- Seq(-10, 0, 10)
      } {
        expectThrows[UnsupportedTemporalTypeException](d.plus(n, WEEKS))
        expectThrows[UnsupportedTemporalTypeException](d.plus(n, MONTHS))
        expectThrows[UnsupportedTemporalTypeException](d.plus(n, YEARS))
        expectThrows[UnsupportedTemporalTypeException](d.plus(n, DECADES))
        expectThrows[UnsupportedTemporalTypeException](d.plus(n, CENTURIES))
        expectThrows[UnsupportedTemporalTypeException](d.plus(n, MILLENNIA))
        expectThrows[UnsupportedTemporalTypeException](d.plus(n, ERAS))
        expectThrows[UnsupportedTemporalTypeException](d.plus(n, FOREVER))
      }
    }

    it("should respond to `plusDays`") {
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(Long.MaxValue - 86400, 999999999)
      val d3 = ofSeconds(Long.MinValue + 86400)
      val d4 = ofSeconds(Long.MaxValue - 86399)
      val d5 = ofSeconds(Long.MinValue + 86400, -1)

      expect(d1.plusDays(-5) == ofSeconds(-431999, 1)).toBeTruthy
      expect(d1.plusDays(-1) == ofSeconds(-86399, 1)).toBeTruthy
      expect(d1.plusDays(0) == d1).toBeTruthy
      expect(d1.plusDays(1) == ofSeconds(86401, 1)).toBeTruthy
      expect(d1.plusDays(5) == ofSeconds(432001, 1)).toBeTruthy
      expect(d2.plusDays(1) == dmax).toBeTruthy
      expect(d3.plusDays(-1) == dmin).toBeTruthy
      expect(dmax.plusDays(0) == dmax).toBeTruthy
      expect(dmax.plusDays(-1) == d2).toBeTruthy
      expect(dmin.plusDays(0) == dmin).toBeTruthy
      expect(dmin.plusDays(1) == d3).toBeTruthy

      expectThrows[ArithmeticException](d4.plusDays(1))
      expectThrows[ArithmeticException](d2.plusDays(2))
      expectThrows[ArithmeticException](d5.plusDays(-1))
      expectThrows[ArithmeticException](d3.plusDays(-2))
    }

    it("should respond to `plusHours`") {
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(Long.MaxValue - 3600, 999999999)
      val d3 = ofSeconds(Long.MinValue + 3600)
      val d4 = ofSeconds(Long.MaxValue - 3599)
      val d5 = ofSeconds(Long.MinValue + 3600, -1)

      expect(d1.plusHours(-5) == ofSeconds(-17999, 1)).toBeTruthy
      expect(d1.plusHours(-1) == ofSeconds(-3599, 1)).toBeTruthy
      expect(d1.plusHours(0) == d1).toBeTruthy
      expect(d1.plusHours(1) == ofSeconds(3601, 1)).toBeTruthy
      expect(d1.plusHours(5) == ofSeconds(18001, 1)).toBeTruthy
      expect(d2.plusHours(1) == dmax).toBeTruthy
      expect(d3.plusHours(-1) == dmin).toBeTruthy
      expect(dmax.plusHours(0) == dmax).toBeTruthy
      expect(dmax.plusHours(-1) == d2).toBeTruthy
      expect(dmin.plusHours(0) == dmin).toBeTruthy
      expect(dmin.plusHours(1) == d3).toBeTruthy

      expectThrows[ArithmeticException](d4.plusHours(1))
      expectThrows[ArithmeticException](d2.plusHours(2))
      expectThrows[ArithmeticException](d5.plusHours(-1))
      expectThrows[ArithmeticException](d3.plusHours(-2))
    }

    it("should respond to `plusMinutes`") {
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(Long.MaxValue - 60, 999999999)
      val d3 = ofSeconds(Long.MinValue + 60)
      val d4 = ofSeconds(Long.MaxValue - 59)
      val d5 = ofSeconds(Long.MinValue + 60, -1)

      expect(d1.plusMinutes(-5) == ofSeconds(-299, 1)).toBeTruthy
      expect(d1.plusMinutes(-1) == ofSeconds(-59, 1)).toBeTruthy
      expect(d1.plusMinutes(0) == d1).toBeTruthy
      expect(d1.plusMinutes(1) == ofSeconds(61, 1)).toBeTruthy
      expect(d1.plusMinutes(5) == ofSeconds(301, 1)).toBeTruthy
      expect(d2.plusMinutes(1) == dmax).toBeTruthy
      expect(d3.plusMinutes(-1) == dmin).toBeTruthy
      expect(dmax.plusMinutes(0) == dmax).toBeTruthy
      expect(dmax.plusMinutes(-1) == d2).toBeTruthy
      expect(dmin.plusMinutes(0) == dmin).toBeTruthy
      expect(dmin.plusMinutes(1) == d3).toBeTruthy

      expectThrows[ArithmeticException](d4.plusMinutes(1))
      expectThrows[ArithmeticException](d2.plusMinutes(2))
      expectThrows[ArithmeticException](d5.plusMinutes(-1))
      expectThrows[ArithmeticException](d3.plusMinutes(-2))
    }

    it("should respond to `plusSeconds`") {
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(Long.MaxValue - 1, 999999999)
      val d3 = ofSeconds(Long.MinValue + 1)
      val d4 = ofSeconds(Long.MaxValue)
      val d5 = ofSeconds(Long.MinValue + 1, -1)

      expect(d1.plusSeconds(-5) == ofSeconds(-4, 1)).toBeTruthy
      expect(d1.plusSeconds(-1) == ofSeconds(0, 1)).toBeTruthy
      expect(d1.plusSeconds(0) == d1).toBeTruthy
      expect(d1.plusSeconds(1) == ofSeconds(2, 1)).toBeTruthy
      expect(d1.plusSeconds(5) == ofSeconds(6, 1)).toBeTruthy
      expect(d2.plusSeconds(1) == dmax).toBeTruthy
      expect(d3.plusSeconds(-1) == dmin).toBeTruthy
      expect(dmax.plusSeconds(0) == dmax).toBeTruthy
      expect(dmax.plusSeconds(-1) == d2).toBeTruthy
      expect(dmin.plusSeconds(0) == dmin).toBeTruthy
      expect(dmin.plusSeconds(1) == d3).toBeTruthy

      expectThrows[ArithmeticException](d4.plusSeconds(1))
      expectThrows[ArithmeticException](d2.plusSeconds(2))
      expectThrows[ArithmeticException](d5.plusSeconds(-1))
      expectThrows[ArithmeticException](d3.plusSeconds(-2))
    }

    it("should respond to `plusMillis`") {
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(Long.MaxValue, 998999999)
      val d3 = ofSeconds(Long.MinValue, 1000000)
      val d4 = ofSeconds(Long.MaxValue, 999000000)
      val d5 = ofSeconds(Long.MinValue, 999999)

      expect(d1.plusMillis(-5000) == ofSeconds(-4, 1)).toBeTruthy
      expect(d1.plusMillis(-100) == ofSeconds(0, 900000001)).toBeTruthy
      expect(d1.plusMillis(0) == d1).toBeTruthy
      expect(d1.plusMillis(100) == ofSeconds(1, 100000001)).toBeTruthy
      expect(d1.plusMillis(5000) == ofSeconds(6, 1)).toBeTruthy
      expect(d2.plusMillis(1) == dmax).toBeTruthy
      expect(d3.plusMillis(-1) == dmin).toBeTruthy
      expect(dmax.plusMillis(0) == dmax).toBeTruthy
      expect(dmax.plusMillis(-1) == d2).toBeTruthy
      expect(dmin.plusMillis(0) == dmin).toBeTruthy
      expect(dmin.plusMillis(1) == d3).toBeTruthy

      expectThrows[ArithmeticException](d4.plusMillis(1))
      expectThrows[ArithmeticException](d2.plusMillis(2))
      expectThrows[ArithmeticException](d5.plusMillis(-1))
      expectThrows[ArithmeticException](d3.plusMillis(-2))
    }

    it("should respond to `plusNanos`") {
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(Long.MaxValue, 999999998)
      val d3 = ofSeconds(Long.MinValue, 1)

      expect(d1.plusNanos(-5000000000L) == ofSeconds(-4, 1)).toBeTruthy
      expect(d1.plusNanos(-1000) == ofSeconds(0, 999999001)).toBeTruthy
      expect(d1.plusNanos(0) == d1).toBeTruthy
      expect(d1.plusNanos(1000) == ofSeconds(1, 1001)).toBeTruthy
      expect(d1.plusNanos(5000000000L) == ofSeconds(6, 1)).toBeTruthy
      expect(d2.plusNanos(1) == dmax).toBeTruthy
      expect(d3.plusNanos(-1) == dmin).toBeTruthy
      expect(dmax.plusNanos(0) == dmax).toBeTruthy
      expect(dmax.plusNanos(-1) == d2).toBeTruthy
      expect(dmin.plusNanos(0) == dmin).toBeTruthy
      expect(dmin.plusNanos(1) == d3).toBeTruthy

      expectThrows[ArithmeticException](dmax.plusNanos(1))
      expectThrows[ArithmeticException](d2.plusNanos(2))
      expectThrows[ArithmeticException](dmin.plusNanos(-1))
      expectThrows[ArithmeticException](d3.plusNanos(-2))
    }

    it("should respond to `minus`") {
      val d0 = ofSeconds(0, 1)
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(1, 999999999)
      val d3 = ofSeconds(-2, 1)

      expect(d1.minus(d1) == ZERO).toBeTruthy
      expect(d1.minus(ZERO) == d1).toBeTruthy
      expect(ZERO.minus(d1) == d1.negated).toBeTruthy
      expect(d1.minus(d2) == ofSeconds(-1, 2)).toBeTruthy
      expect(d1.minus(d3) == ofSeconds(3)).toBeTruthy

      expectThrows[ArithmeticException](dmax.minus(d0.negated))
      expectThrows[ArithmeticException](dmin.minus(d0))

      for {
        d <- Seq(ZERO, d0, d1, d2, d3)
        n <- Seq(-100000000000000L, 1L, 0L, 1L, 100000000000000L)
      } {
        expect(d.minus(n, NANOS) == d.minusNanos(n)).toBeTruthy
        expect(d.minus(n, MICROS) == d.minusNanos(n * 1000)).toBeTruthy
        expect(d.minus(n, MILLIS) == d.minusMillis(n)).toBeTruthy
        expect(d.minus(n, SECONDS) == d.minusSeconds(n)).toBeTruthy
        expect(d.minus(n, MINUTES) == d.minusMinutes(n)).toBeTruthy
        expect(d.minus(n, HOURS) == d.minusHours(n)).toBeTruthy
        expect(d.minus(n, HALF_DAYS) == d.minusHours(n * 12)).toBeTruthy
        expect(d.minus(n, DAYS) == d.minusDays(n)).toBeTruthy
      }

      expectThrows[ArithmeticException](dmax.minus(-1, NANOS))
      expectThrows[ArithmeticException](dmin.minus(1, NANOS))

      for {
        d <- Seq(ZERO, d0, d1, d2, d3, dmax, dmin)
        n <- Seq(Long.MinValue, Long.MaxValue)
      } {
        expectThrows[ArithmeticException](d.minus(n, MINUTES))
        expectThrows[ArithmeticException](d.minus(n, HOURS))
        expectThrows[ArithmeticException](d.minus(n, HALF_DAYS))
        expectThrows[ArithmeticException](d.minus(n, DAYS))
      }

      for {
        d <- Seq(ZERO, d0, d1, d2, d3, dmax, dmin)
        n <- Seq(-10, 0, 10)
      } {
        expectThrows[UnsupportedTemporalTypeException](d.minus(n, WEEKS))
        expectThrows[UnsupportedTemporalTypeException](d.minus(n, MONTHS))
        expectThrows[UnsupportedTemporalTypeException](d.minus(n, YEARS))
        expectThrows[UnsupportedTemporalTypeException](d.minus(n, DECADES))
        expectThrows[UnsupportedTemporalTypeException](d.minus(n, CENTURIES))
        expectThrows[UnsupportedTemporalTypeException](d.minus(n, MILLENNIA))
        expectThrows[UnsupportedTemporalTypeException](d.minus(n, ERAS))
        expectThrows[UnsupportedTemporalTypeException](d.minus(n, FOREVER))
      }
    }

    it("should respond to `minusDays`") {
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(Long.MaxValue - 86400, 999999999)
      val d3 = ofSeconds(Long.MinValue + 86400)
      val d4 = ofSeconds(Long.MaxValue - 86399)
      val d5 = ofSeconds(Long.MinValue + 86400, -1)

      expect(d1.minusDays(5) == ofSeconds(-431999, 1)).toBeTruthy
      expect(d1.minusDays(1) == ofSeconds(-86399, 1)).toBeTruthy
      expect(d1.minusDays(0) == d1).toBeTruthy
      expect(d1.minusDays(-1) == ofSeconds(86401, 1)).toBeTruthy
      expect(d1.minusDays(-5) == ofSeconds(432001, 1)).toBeTruthy
      expect(d2.minusDays(-1) == dmax).toBeTruthy
      expect(d3.minusDays(1) == dmin).toBeTruthy
      expect(dmax.minusDays(0) == dmax).toBeTruthy
      expect(dmax.minusDays(1) == d2).toBeTruthy
      expect(dmin.minusDays(0) == dmin).toBeTruthy
      expect(dmin.minusDays(-1) == d3).toBeTruthy

      expectThrows[ArithmeticException](d4.minusDays(-1))
      expectThrows[ArithmeticException](d2.minusDays(-2))
      expectThrows[ArithmeticException](d5.minusDays(1))
      expectThrows[ArithmeticException](d3.minusDays(2))
    }

    it("should respond to `minusHours`") {
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(Long.MaxValue - 3600, 999999999)
      val d3 = ofSeconds(Long.MinValue + 3600)
      val d4 = ofSeconds(Long.MaxValue - 3599)
      val d5 = ofSeconds(Long.MinValue + 3600, -1)

      expect(d1.minusHours(5) == ofSeconds(-17999, 1)).toBeTruthy
      expect(d1.minusHours(1) == ofSeconds(-3599, 1)).toBeTruthy
      expect(d1.minusHours(0) == d1).toBeTruthy
      expect(d1.minusHours(-1) == ofSeconds(3601, 1)).toBeTruthy
      expect(d1.minusHours(-5) == ofSeconds(18001, 1)).toBeTruthy
      expect(d2.minusHours(-1) == dmax).toBeTruthy
      expect(d3.minusHours(1) == dmin).toBeTruthy
      expect(dmax.minusHours(0) == dmax).toBeTruthy
      expect(dmax.minusHours(1) == d2).toBeTruthy
      expect(dmin.minusHours(0) == dmin).toBeTruthy
      expect(dmin.minusHours(-1) == d3).toBeTruthy

      expectThrows[ArithmeticException](d4.minusHours(-1))
      expectThrows[ArithmeticException](d2.minusHours(-2))
      expectThrows[ArithmeticException](d5.minusHours(1))
      expectThrows[ArithmeticException](d3.minusHours(2))
    }

    it("should respond to `minusMinutes`") {
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(Long.MaxValue - 60, 999999999)
      val d3 = ofSeconds(Long.MinValue + 60)
      val d4 = ofSeconds(Long.MaxValue - 59)
      val d5 = ofSeconds(Long.MinValue + 60, -1)

      expect(d1.minusMinutes(5) == ofSeconds(-299, 1)).toBeTruthy
      expect(d1.minusMinutes(1) == ofSeconds(-59, 1)).toBeTruthy
      expect(d1.minusMinutes(0) == d1).toBeTruthy
      expect(d1.minusMinutes(-1) == ofSeconds(61, 1)).toBeTruthy
      expect(d1.minusMinutes(-5) == ofSeconds(301, 1)).toBeTruthy
      expect(d2.minusMinutes(-1) == dmax).toBeTruthy
      expect(d3.minusMinutes(1) == dmin).toBeTruthy
      expect(dmax.minusMinutes(0) == dmax).toBeTruthy
      expect(dmax.minusMinutes(1) == d2).toBeTruthy
      expect(dmin.minusMinutes(0) == dmin).toBeTruthy
      expect(dmin.minusMinutes(-1) == d3).toBeTruthy

      expectThrows[ArithmeticException](d4.minusMinutes(-1))
      expectThrows[ArithmeticException](d2.minusMinutes(-2))
      expectThrows[ArithmeticException](d5.minusMinutes(1))
      expectThrows[ArithmeticException](d3.minusMinutes(2))
    }

    it("should respond to `minusSeconds`") {
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(Long.MaxValue - 1, 999999999)
      val d3 = ofSeconds(Long.MinValue + 1)
      val d4 = ofSeconds(Long.MaxValue)
      val d5 = ofSeconds(Long.MinValue + 1, -1)

      expect(d1.minusSeconds(5) == ofSeconds(-4, 1)).toBeTruthy
      expect(d1.minusSeconds(1) == ofSeconds(0, 1)).toBeTruthy
      expect(d1.minusSeconds(0) == d1).toBeTruthy
      expect(d1.minusSeconds(-1) == ofSeconds(2, 1)).toBeTruthy
      expect(d1.minusSeconds(-5) == ofSeconds(6, 1)).toBeTruthy
      expect(d2.minusSeconds(-1) == dmax).toBeTruthy
      expect(d3.minusSeconds(1) == dmin).toBeTruthy
      expect(dmax.minusSeconds(0) == dmax).toBeTruthy
      expect(dmax.minusSeconds(1) == d2).toBeTruthy
      expect(dmin.minusSeconds(0) == dmin).toBeTruthy
      expect(dmin.minusSeconds(-1) == d3).toBeTruthy

      expectThrows[ArithmeticException](d4.minusSeconds(-1))
      expectThrows[ArithmeticException](d2.minusSeconds(-2))
      expectThrows[ArithmeticException](d5.minusSeconds(1))
      expectThrows[ArithmeticException](d3.minusSeconds(2))
    }

    it("should respond to `minusMillis`") {
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(Long.MaxValue, 998999999)
      val d3 = ofSeconds(Long.MinValue, 1000000)
      val d4 = ofSeconds(Long.MaxValue, 999000000)
      val d5 = ofSeconds(Long.MinValue, 999999)

      expect(d1.minusMillis(5000) == ofSeconds(-4, 1)).toBeTruthy
      expect(d1.minusMillis(100) == ofSeconds(0, 900000001)).toBeTruthy
      expect(d1.minusMillis(0) == d1).toBeTruthy
      expect(d1.minusMillis(-100) == ofSeconds(1, 100000001)).toBeTruthy
      expect(d1.minusMillis(-5000) == ofSeconds(6, 1)).toBeTruthy
      expect(d2.minusMillis(-1) == dmax).toBeTruthy
      expect(d3.minusMillis(1) == dmin).toBeTruthy
      expect(dmax.minusMillis(0) == dmax).toBeTruthy
      expect(dmax.minusMillis(1) == d2).toBeTruthy
      expect(dmin.minusMillis(0) == dmin).toBeTruthy
      expect(dmin.minusMillis(-1) == d3).toBeTruthy

      expectThrows[ArithmeticException](d4.minusMillis(-1))
      expectThrows[ArithmeticException](d2.minusMillis(-2))
      expectThrows[ArithmeticException](d5.minusMillis(1))
      expectThrows[ArithmeticException](d3.minusMillis(2))
    }

    it("should respond to `minusNanos`") {
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(Long.MaxValue, 999999998)
      val d3 = ofSeconds(Long.MinValue, 1)


      expect(d1.minusNanos(5000000000L) == ofSeconds(-4, 1)).toBeTruthy
      expect(d1.minusNanos(1000) == ofSeconds(0, 999999001)).toBeTruthy
      expect(d1.minusNanos(0) == d1).toBeTruthy
      expect(d1.minusNanos(-1000) == ofSeconds(1, 1001)).toBeTruthy
      expect(d1.minusNanos(-5000000000L) == ofSeconds(6, 1)).toBeTruthy
      expect(d2.minusNanos(-1) == dmax).toBeTruthy
      expect(d3.minusNanos(1) == dmin).toBeTruthy
      expect(dmax.minusNanos(0) == dmax).toBeTruthy
      expect(dmax.minusNanos(1) == d2).toBeTruthy
      expect(dmin.minusNanos(0) == dmin).toBeTruthy
      expect(dmin.minusNanos(-1) == d3).toBeTruthy

      expectThrows[ArithmeticException](dmax.minusNanos(-1))
      expectThrows[ArithmeticException](d2.minusNanos(-2))
      expectThrows[ArithmeticException](dmin.minusNanos(1))
      expectThrows[ArithmeticException](d3.minusNanos(2))
    }

    it("should respond to `multipliedBy`") {
      val d1 = ofSeconds(1, 1)
      val d2 = ofSeconds(-1, -1)

      for (d <- Seq(d1, d2, dmin, dmax, ZERO)) {
        expect(d.multipliedBy(0) == ZERO).toBeTruthy
        expect(d.multipliedBy(1) == d).toBeTruthy
      }
      expect(ZERO.multipliedBy(-1) == ZERO).toBeTruthy
      expect(ZERO.multipliedBy(1000000000) == ZERO).toBeTruthy
      expect(ZERO.multipliedBy(-1000000000) == ZERO).toBeTruthy
      expect(d1.multipliedBy(-1) == d2).toBeTruthy
      expect(d1.multipliedBy(2) == ofSeconds(2, 2)).toBeTruthy
      expect(d1.multipliedBy(-2) == ofSeconds(-2, -2)).toBeTruthy
      expect(d1.multipliedBy(1000000000) == ofSeconds(1000000001)).toBeTruthy
      expect(d1.multipliedBy(-1000000000) == ofSeconds(-1000000001)).toBeTruthy
      expect(d2.multipliedBy(-1) == d1).toBeTruthy
      expect(d2.multipliedBy(2) == ofSeconds(-2, -2)).toBeTruthy
      expect(d2.multipliedBy(-2) == ofSeconds(2, 2)).toBeTruthy
      expect(d2.multipliedBy(1000000000) == ofSeconds(-1000000001)).toBeTruthy
      expect(d2.multipliedBy(-1000000000) == ofSeconds(1000000001)).toBeTruthy
      expect(dmax.multipliedBy(-1) == dmin.plusNanos(1)).toBeTruthy

      expectThrows[ArithmeticException](dmin.multipliedBy(-1))
      expectThrows[ArithmeticException](dmin.multipliedBy(2))
      expectThrows[ArithmeticException](dmax.multipliedBy(2))
      expectThrows[ArithmeticException](d1.multipliedBy(Long.MaxValue))
      expectThrows[ArithmeticException](d1.multipliedBy(Long.MinValue))
    }

    it("should respond to `dividedBy`") {
      val d1 = ofSeconds(10, 100)
      val d2 = ofNanos(10)
      val d3 = Duration.ofSeconds(9223372036L, 999999999)

      expect(d1.dividedBy(1) == d1).toBeTruthy
      expect(d1.dividedBy(-1) == d1.negated).toBeTruthy
      expect(d1.dividedBy(2) == ofSeconds(5, 50))
      expect(d1.dividedBy(-2) == ofSeconds(-5, -50)).toBeTruthy
      expect(d1.dividedBy(3) == ofSeconds(3, 333333366)).toBeTruthy
      expect(d1.dividedBy(-3) == ofSeconds(-3, -333333366)).toBeTruthy
      expect(d1.dividedBy(10) == ofSeconds(1, 10)).toBeTruthy
      expect(d1.dividedBy(-10) == ofSeconds(-1, -10)).toBeTruthy
      expect(d1.dividedBy(100) == ofNanos(100000001)).toBeTruthy
      expect(d1.dividedBy(-100) == ofNanos(-100000001)).toBeTruthy
      expect(d1.dividedBy(1000) == ofMillis(10)).toBeTruthy
      expect(d1.dividedBy(-1000) == ofMillis(-10)).toBeTruthy
      expect(d1.dividedBy(3000) == ofNanos(3333333)).toBeTruthy
      expect(d1.dividedBy(-3000) == ofNanos(-3333333)).toBeTruthy
      expect(d1.dividedBy(1000000000) == ofNanos(10)).toBeTruthy
      expect(d1.dividedBy(-1000000000) == ofNanos(-10)).toBeTruthy
      expect(d1.dividedBy(10000000000L) == ofNanos(1)).toBeTruthy
      expect(d1.dividedBy(-10000000000L) == ofNanos(-1)).toBeTruthy
      expect(d1.dividedBy(10000000100L) == ofNanos(1)).toBeTruthy
      expect(d1.dividedBy(-10000000100L) == ofNanos(-1)).toBeTruthy
      expect(d1.dividedBy(10000000101L) == ZERO).toBeTruthy
      expect(d1.dividedBy(-10000000101L) == ZERO).toBeTruthy
      expect(d2.dividedBy(10) == ofNanos(1)).toBeTruthy
      expect(d2.dividedBy(-10) == ofNanos(-1)).toBeTruthy
      expect(d2.dividedBy(11) == ZERO).toBeTruthy
      expect(d2.dividedBy(-11) == ZERO).toBeTruthy
      expect(d3.dividedBy(Long.MaxValue) == ofNanos(1)).toBeTruthy
      expect(d3.dividedBy(Long.MinValue) == ofNanos(-1)).toBeTruthy
      expect(ZERO.dividedBy(1) == ZERO).toBeTruthy
      expect(ZERO.dividedBy(-1) == ZERO).toBeTruthy
      expect(dmin.dividedBy(Long.MinValue) == ofSeconds(1)).toBeTruthy
      expect(dmin.dividedBy(Long.MaxValue) == ofSeconds(-1)).toBeTruthy
      expect(dmax.dividedBy(Long.MinValue) == ofSeconds(-1, 1)).toBeTruthy
      expect(dmax.dividedBy(Long.MaxValue) == ofSeconds(1)).toBeTruthy
      expect(dmin.plusNanos(1).dividedBy(Long.MinValue) == ofSeconds(1, -1)).toBeTruthy
      expect(dmin.plusNanos(1).dividedBy(Long.MaxValue) == ofSeconds(-1)).toBeTruthy
      expect(dmin.plusNanos(2).dividedBy(Long.MaxValue) == ofSeconds(-1)).toBeTruthy
      expect(dmax.minusNanos(1).dividedBy(Long.MaxValue) == ofSeconds(1)).toBeTruthy

      for (d <- Seq(d1, d2, d3, ZERO, dmin, dmax))
        expectThrows[ArithmeticException](d.dividedBy(0))
    }

    it("should respond to `negated`") {
      expect(ZERO.negated == ZERO).toBeTruthy
      expect(ofSeconds(1).negated == ofSeconds(-1)).toBeTruthy
      expect(ofSeconds(-1).negated == ofSeconds(1)).toBeTruthy
      expect(ofSeconds(1, 1).negated == ofSeconds(-1, -1)).toBeTruthy
      expect(ofSeconds(-1, -1).negated == ofSeconds(1, 1)).toBeTruthy
      expect(dmax.negated == ofSeconds(Long.MinValue, 1)).toBeTruthy
      expect(ofSeconds(Long.MinValue, 1).negated == dmax).toBeTruthy

      expectThrows[ArithmeticException](dmin.negated)
    }

    it("should respond to `abs`") {
      expect(ZERO.abs == ZERO).toBeTruthy
      expect(ofSeconds(1).abs == ofSeconds(1)).toBeTruthy
      expect(ofSeconds(-1).abs == ofSeconds(1)).toBeTruthy
      expect(ofSeconds(1, 1).abs == ofSeconds(1, 1)).toBeTruthy
      expect(ofSeconds(-1, -1).abs == ofSeconds(1, 1)).toBeTruthy
      expect(dmax.abs == dmax).toBeTruthy
      expect(ofSeconds(Long.MinValue, 1).abs == dmax).toBeTruthy

      expectThrows[ArithmeticException](dmin.abs)
    }

    it("should respond to `addTo`") {
      val t = LocalTime.NOON
      val d = LocalDate.MIN

      expect(ZERO.addTo(t) == t).toBeTruthy
      expect(dmin.addTo(t) == LocalTime.of(20, 29, 52)).toBeTruthy
      expect(dmax.addTo(t) == LocalTime.of(3, 30, 7, 999999999)).toBeTruthy
      expect(ZERO.addTo(d) == d).toBeTruthy

      expectThrows[UnsupportedTemporalTypeException](ofNanos(1).addTo(d))
      expectThrows[UnsupportedTemporalTypeException](ofSeconds(1).addTo(d))
    }

    it("should respond to `subtractFrom`") {
      val t = LocalTime.NOON
      val d = LocalDate.MIN

      expect(ZERO.subtractFrom(t) == t).toBeTruthy
      expect(dmin.subtractFrom(t) == LocalTime.of(3, 30, 8)).toBeTruthy
      expect(dmax.subtractFrom(t) == LocalTime.of(20, 29, 52, 1)).toBeTruthy
      expect(ZERO.subtractFrom(d) == d).toBeTruthy

      expectThrows[UnsupportedTemporalTypeException](ofNanos(1).subtractFrom(d))
      expectThrows[UnsupportedTemporalTypeException](ofSeconds(1).subtractFrom(d))
    }

    it("should respond to `toDays`") {
      expect(dmin.toDays == -106751991167300L).toBeTruthy
      expect(ofSeconds(-172799, -1).toDays == -2).toBeTruthy
      expect(ofSeconds(-172799).toDays == -1).toBeTruthy
      expect(ofSeconds(-86400).toDays == -1).toBeTruthy
      expect(ofSeconds(-86399, -1).toDays == -1).toBeTruthy
      expect(ofSeconds(-86399).toDays == 0).toBeTruthy
      expect(ZERO.toDays == 0).toBeTruthy
      expect(ofSeconds(86399).toDays == 0).toBeTruthy
      expect(ofSeconds(86400, -1).toDays == 0).toBeTruthy
      expect(ofSeconds(86400).toDays == 1).toBeTruthy
      expect(ofSeconds(172800, -1).toDays == 1).toBeTruthy
      expect(ofSeconds(172800).toDays == 2).toBeTruthy
      expect(dmax.toDays == 106751991167300L).toBeTruthy
    }

    it("should respond to `toHours`") {
      expect(dmin.toHours == -2562047788015215L).toBeTruthy
      expect(ofSeconds(-7199, -1).toHours == -2).toBeTruthy
      expect(ofSeconds(-7199).toHours == -1).toBeTruthy
      expect(ofSeconds(-3600).toHours == -1).toBeTruthy
      expect(ofSeconds(-3599, -1).toHours == -1).toBeTruthy
      expect(ofSeconds(-3599).toHours == 0).toBeTruthy
      expect(ZERO.toHours == 0).toBeTruthy
      expect(ofSeconds(3599).toHours == 0).toBeTruthy
      expect(ofSeconds(3600, -1).toHours == 0).toBeTruthy
      expect(ofSeconds(3600).toHours == 1).toBeTruthy
      expect(ofSeconds(7200, -1).toHours == 1).toBeTruthy
      expect(ofSeconds(7200).toHours == 2).toBeTruthy
      expect(dmax.toHours == 2562047788015215L).toBeTruthy
    }

    it("should respond to `toMinutes`") {
      expect(dmin.toMinutes == -153722867280912930L).toBeTruthy
      expect(ofSeconds(-119, -1).toMinutes == -2).toBeTruthy
      expect(ofSeconds(-119).toMinutes == -1).toBeTruthy
      expect(ofSeconds(-60).toMinutes == -1).toBeTruthy
      expect(ofSeconds(-59, -1).toMinutes == -1).toBeTruthy
      expect(ofSeconds(-59).toMinutes == 0).toBeTruthy
      expect(ZERO.toMinutes == 0).toBeTruthy
      expect(ofSeconds(59).toMinutes == 0).toBeTruthy
      expect(ofSeconds(60, -1).toMinutes == 0).toBeTruthy
      expect(ofSeconds(60).toMinutes == 1).toBeTruthy
      expect(ofSeconds(120, -1).toMinutes == 1).toBeTruthy
      expect(ofSeconds(120).toMinutes == 2).toBeTruthy
      expect(dmax.toMinutes == 153722867280912930L).toBeTruthy
    }

    it("should respond to `toMillis`") {
      expect(ofSeconds(-9223372036854775L).toMillis == -9223372036854775000L).toBeTruthy
      expect(ofSeconds(-1).toMillis == -1000).toBeTruthy
      expect(ofNanos(-1000001).toMillis == -2).toBeTruthy
      expect(ofNanos(-1000000).toMillis == -1).toBeTruthy
      expect(ofNanos(-1).toMillis == -1).toBeTruthy
      expect(ZERO.toMillis == 0).toBeTruthy
      expect(ofNanos(999999).toMillis == 0).toBeTruthy
      expect(ofNanos(1000000).toMillis == 1).toBeTruthy
      expect(ofNanos(1999999).toMillis == 1).toBeTruthy
      expect(ofNanos(2000000).toMillis == 2).toBeTruthy
      expect(ofSeconds(1).toMillis == 1000).toBeTruthy
      expect(ofSeconds(9223372036854775L, 807999999).toMillis == 9223372036854775807L).toBeTruthy

      expectThrows[ArithmeticException](dmin.toMillis)
      expectThrows[ArithmeticException](dmax.toMillis)
      // this could yield a valid long, but the reference implementation throws
      expectThrows[ArithmeticException](ofSeconds(-9223372036854775L, -1).toMillis)
      expectThrows[ArithmeticException](ofSeconds(9223372036854775L, 808000000).toMillis)
    }

    it("should respond to `toNanos`") {
      expect(ofSeconds(-9223372036L).toNanos == -9223372036000000000L).toBeTruthy
      expect(ofNanos(Int.MinValue).toNanos == Int.MinValue).toBeTruthy
      expect(ofNanos(-1000).toNanos == -1000).toBeTruthy
      expect(ofNanos(-1).toNanos == -1).toBeTruthy
      expect(ofNanos(0).toNanos == 0).toBeTruthy
      expect(ofNanos(1).toNanos == 1).toBeTruthy
      expect(ofNanos(1000).toNanos == 1000).toBeTruthy
      expect(ofNanos(Int.MaxValue).toNanos == Int.MaxValue).toBeTruthy
      expect(ofSeconds(9223372036L, 854775807).toNanos == Long.MaxValue).toBeTruthy

      expectThrows[ArithmeticException](dmin.toNanos)
      expectThrows[ArithmeticException](dmax.toNanos)
      // this should yield a valid long, but the reference implementation throws
      expectThrows[ArithmeticException](ofSeconds(-9223372036L, -1).toNanos)
      expectThrows[ArithmeticException](ofSeconds(9223372036L, 854775808).toNanos)
    }

    it("should be comparable") {
      val d1 = ofSeconds(0, -1)
      val d0 = ZERO
      val d2 = ofSeconds(0, 1)

      expect(dmin.compareTo(dmin)).toEqual(0)
      expect(dmin.compareTo(d1)).toBeLessThan(0)
      expect(dmin.compareTo(d0)).toBeLessThan(0)
      expect(dmin.compareTo(d2)).toBeLessThan(0)
      expect(dmin.compareTo(dmax)).toBeLessThan(0)
      expect(d1.compareTo(dmin)).toBeGreaterThan(0)
      expect(d1.compareTo(d1)).toEqual(0)
      expect(d1.compareTo(d0)).toBeLessThan(0)
      expect(d1.compareTo(d2)).toBeLessThan(0)
      expect(d1.compareTo(dmax)).toBeLessThan(0)
      expect(d0.compareTo(dmin)).toBeGreaterThan(0)
      expect(d0.compareTo(d1)).toBeGreaterThan(0)
      expect(d0.compareTo(d0)).toEqual(0)
      expect(d0.compareTo(d2)).toBeLessThan(0)
      expect(d0.compareTo(dmax)).toBeLessThan(0)
      expect(d2.compareTo(dmin)).toBeGreaterThan(0)
      expect(d2.compareTo(d1)).toBeGreaterThan(0)
      expect(d2.compareTo(d0)).toBeGreaterThan(0)
      expect(d2.compareTo(d2)).toEqual(0)
      expect(d2.compareTo(dmax)).toBeLessThan(0)
      expect(dmax.compareTo(dmin)).toBeGreaterThan(0)
      expect(dmax.compareTo(d1)).toBeGreaterThan(0)
      expect(dmax.compareTo(d0)).toBeGreaterThan(0)
      expect(dmax.compareTo(d2)).toBeGreaterThan(0)
      expect(dmax.compareTo(dmax)).toEqual(0)
    }

    it("should override `toString`") {
      expect(ZERO.toString).toEqual("PT0S")
      expect(ofSeconds(-1, 1).toString).toEqual("PT-0.999999999S")
      expect(ofSeconds(-1, -1).toString).toEqual("PT-1.000000001S")
      expect(ofSeconds(60).toString).toEqual("PT1M")
      expect(ofSeconds(-60).toString).toEqual("PT-1M")
      expect(ofSeconds(60, -1).toString).toEqual("PT59.999999999S")
      expect(ofSeconds(60, 1).toString).toEqual("PT1M0.000000001S")
      expect(ofSeconds(-61, 1).toString).toEqual("PT-1M-0.999999999S")
      expect(ofSeconds(120, 10).toString).toEqual("PT2M0.00000001S")
      expect(ofSeconds(-120, 10).toString).toEqual("PT-1M-59.99999999S")
      expect(ofSeconds(3600).toString).toEqual("PT1H")
      expect(ofSeconds(-3600).toString).toEqual("PT-1H")
      expect(dmin.toString).toEqual("PT-2562047788015215H-30M-8S")
      expect(dmax.toString).toEqual("PT2562047788015215H30M7.999999999S")
    }

    it("should respond to `ofDays`") {
      val maxDays = 106751991167300L
      val maxSecs = maxDays * 86400

      expect(ofDays(-maxDays) == ofSeconds(-maxSecs)).toBeTruthy
      expect(ofDays(-1) == ofSeconds(-86400)).toBeTruthy
      expect(ofDays(0) == ZERO).toBeTruthy
      expect(ofDays(1) == ofSeconds(86400)).toBeTruthy
      expect(ofDays(maxDays) == ofSeconds(maxSecs)).toBeTruthy

      expectThrows[ArithmeticException](ofDays(-maxDays - 1))
      expectThrows[ArithmeticException](ofDays(maxDays + 1))
    }

    it("should respond to `ofHours`") {
      val maxHrs = 2562047788015215L
      val maxSecs = maxHrs * 3600

      expect(ofHours(-maxHrs) == ofSeconds(-maxSecs)).toBeTruthy
      expect(ofHours(-1) == ofSeconds(-3600)).toBeTruthy
      expect(ofHours(0) == ZERO).toBeTruthy
      expect(ofHours(1) == ofSeconds(3600)).toBeTruthy
      expect(ofHours(maxHrs) == ofSeconds(maxSecs)).toBeTruthy

      expectThrows[ArithmeticException](ofHours(-maxHrs - 1))
      expectThrows[ArithmeticException](ofHours(maxHrs + 1))
    }

    it("should respond to `ofMinutes`") {
      val maxMins = 153722867280912930L
      val maxSecs = maxMins * 60

      expect(ofMinutes(-maxMins) == ofSeconds(-maxSecs)).toBeTruthy
      expect(ofMinutes(-1) == ofSeconds(-60)).toBeTruthy
      expect(ofMinutes(0) == ZERO).toBeTruthy
      expect(ofMinutes(1) == ofSeconds(60)).toBeTruthy
      expect(ofMinutes(maxMins) == ofSeconds(maxSecs)).toBeTruthy

      expectThrows[ArithmeticException](ofMinutes(-maxMins - 1))
      expectThrows[ArithmeticException](ofMinutes(maxMins + 1))
    }

    it("should respond to `ofSeconds`") {
      expect(ofSeconds(-10, -1) == ofSeconds(-11, 999999999)).toBeTruthy
      expect(ofSeconds(-1, 0) == ofSeconds(-1)).toBeTruthy
      expect(ofSeconds(-1, 1000000000) == ZERO).toBeTruthy
      expect(ofSeconds(0) == ZERO).toBeTruthy
      expect(ofSeconds(0, 0) == ZERO).toBeTruthy
      expect(ofSeconds(1, -1000000000) == ZERO).toBeTruthy
      expect(ofSeconds(1, 0) == ofSeconds(1)).toBeTruthy
      expect(ofSeconds(10, -1) == ofSeconds(9, 999999999)).toBeTruthy

      expectThrows[ArithmeticException](ofSeconds(Long.MinValue, -1))
      expectThrows[ArithmeticException](ofSeconds(Long.MaxValue, 1000000000))
    }

    it("should respond to `ofMillis`") {
      expect(ofMillis(Long.MinValue) ==
        ofSeconds(-9223372036854776L, 192000000)).toBeTruthy
      expect(ofMillis(-1000) == ofSeconds(-1)).toBeTruthy
      expect(ofMillis(-1) == ofSeconds(0, -1000000)).toBeTruthy
      expect(ofMillis(0) == ZERO).toBeTruthy
      expect(ofMillis(1) == ofSeconds(0, 1000000)).toBeTruthy
      expect(ofMillis(1000) == ofSeconds(1)).toBeTruthy
      expect(ofMillis(Long.MaxValue) == ofSeconds(9223372036854775L, 807000000)).toBeTruthy
    }

    it("should respond to `ofNanos`") {
      expect(ofNanos(Long.MinValue) == ofSeconds(-9223372037L, 145224192)).toBeTruthy
      expect(ofNanos(-1000000000) == ofSeconds(-1)).toBeTruthy
      expect(ofNanos(-1) == ofSeconds(0, -1)).toBeTruthy
      expect(ofNanos(0) == ZERO).toBeTruthy
      expect(ofNanos(1) == ofSeconds(0, 1)).toBeTruthy
      expect(ofNanos(1000000000) == ofSeconds(1)).toBeTruthy
      expect(ofNanos(Long.MaxValue) == ofSeconds(9223372036L, 854775807)).toBeTruthy
    }

    it("should respont to `of`") {
      for (n <- Seq(-100000000000000L, -1L, 0L, 1L, 100000000000000L)) {
        expect(of(n, NANOS) == ofNanos(n)).toBeTruthy
        expect(of(n, MICROS) == ofNanos(n * 1000)).toBeTruthy
        expect(of(n, MILLIS) == ofMillis(n)).toBeTruthy
        expect(of(n, SECONDS) == ofSeconds(n)).toBeTruthy
        expect(of(n, MINUTES) == ofMinutes(n)).toBeTruthy
        expect(of(n, HOURS) == ofHours(n)).toBeTruthy
        expect(of(n, HALF_DAYS) == ofHours(n * 12)).toBeTruthy
        expect(of(n, DAYS) == ofDays(n)).toBeTruthy
      }

      for (s <- Seq(-1, 1)) {
        expectThrows[ArithmeticException](of(106751991167301L * s, DAYS))
        expectThrows[ArithmeticException](of(213503982334602L * s, HALF_DAYS))
        expectThrows[ArithmeticException](of(2562047788015216L * s, HOURS))
        expectThrows[ArithmeticException](of(153722867280912931L * s, MINUTES))
      }

      for (n <- Seq(-10, 0, 10)) {
        expectThrows[UnsupportedTemporalTypeException](of(n, WEEKS))
        expectThrows[UnsupportedTemporalTypeException](of(n, MONTHS))
        expectThrows[UnsupportedTemporalTypeException](of(n, YEARS))
        expectThrows[UnsupportedTemporalTypeException](of(n, DECADES))
        expectThrows[UnsupportedTemporalTypeException](of(n, CENTURIES))
        expectThrows[UnsupportedTemporalTypeException](of(n, MILLENNIA))
        expectThrows[UnsupportedTemporalTypeException](of(n, ERAS))
        expectThrows[UnsupportedTemporalTypeException](of(n, FOREVER))
      }
    }

    it("should respond to `from`") {
      expect(from(dmin) == dmin).toBeTruthy
      expect(from(ZERO) == ZERO).toBeTruthy
      expect(from(dmax) == dmax).toBeTruthy

      expectThrows[UnsupportedTemporalTypeException](from(Period.ZERO))
    }

    it("should respond to `between`") {
      val MIN = LocalTime.MIN
      val MAX = LocalTime.MAX

      expect(between(MIN, MAX) == ofNanos(86399999999999L)).toBeTruthy
      expect(between(MIN, LocalTime.of(0, 0, 0, 1)) == ofNanos(1)).toBeTruthy

      expectThrows[DateTimeException](between(MIN, LocalDate.of(2012, 2, 29)))
      expectThrows[DateTimeException](between(LocalDate.of(2012, 2, 29),
        LocalDate.of(2012, 3, 1)))
    }
  }
}
