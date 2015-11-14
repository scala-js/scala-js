package org.scalajs.testsuite.javalib.time

import java.time._
import java.time.chrono.IsoChronology
import java.time.temporal.ChronoUnit

import org.scalajs.jasminetest.JasmineTest
import org.scalajs.testsuite.utils.ExpectExceptions

object PeriodTest extends JasmineTest with ExpectExceptions {
  import Period._
  import ChronoUnit._

  final val pmin = of(Int.MinValue, Int.MinValue, Int.MinValue)
  final val pmin1 = of(-Int.MaxValue, -Int.MaxValue, -Int.MaxValue)
  final val pmax = of(Int.MaxValue, Int.MaxValue, Int.MaxValue)
  final val oneYear = of(1, 0, 0)
  final val oneMonth = of(0, 1, 0)
  final val oneDay = of(0, 0, 1)
  final val samples1 = Seq(ZERO, oneYear, oneYear.negated,
      oneMonth, oneMonth.negated, oneDay, oneDay.negated)
  final val samples = samples1 ++ Seq(pmin, pmin1, pmax)

  describe("java.time.Period") {
    it("should respond to `get`") {
      for (p <- samples) {
        expect(p.get(YEARS) == p.getYears).toBeTruthy
        expect(p.get(MONTHS) == p.getMonths).toBeTruthy
        expect(p.get(DAYS) == p.getDays).toBeTruthy
      }
    }

    it("should respond to `getUnits`") {
      for (p <- samples) {
        val units = p.getUnits
        expect(units.size).toEqual(3)
        expect(units.get(0) == YEARS).toBeTruthy
        expect(units.get(1) == MONTHS).toBeTruthy
        expect(units.get(2) == DAYS).toBeTruthy
      }
    }

    it("should respond to `getChronology`") {
      for (p <- samples) {
        expect(p.getChronology == IsoChronology.INSTANCE).toBeTruthy
      }
    }

    it("should respond to `isZero`") {
      for (p <- samples) {
        if (p == ZERO) expect(p.isZero).toBeTruthy
        else expect(p.isZero).toBeFalsy
      }
    }

    it("should respond to `isNegative`") {
      for (p <- Seq(ZERO, oneYear, oneMonth, oneDay, pmax))
        expect(p.isNegative).toBeFalsy
      for (p <- Seq(oneYear.negated, oneMonth.negated, oneDay.negated, pmin, pmin1))
        expect(p.isNegative).toBeTruthy
      for (p <- Seq(of(-1, 1, 1), of(1, -1, 1), of(1, 1, -1)))
        expect(p.isNegative).toBeTruthy
    }

    it("should respond to `getYears`") {
      expect(oneYear.getYears).toEqual(1)
      expect(oneMonth.getYears).toEqual(0)
      expect(oneDay.getYears).toEqual(0)
      expect(pmin.getYears).toEqual(Int.MinValue)
      expect(pmin1.getYears).toEqual(Int.MinValue + 1)
      expect(pmax.getYears).toEqual(Int.MaxValue)
    }

    it("should respond to `getMonths`") {
      expect(oneYear.getMonths).toEqual(0)
      expect(oneMonth.getMonths).toEqual(1)
      expect(oneDay.getMonths).toEqual(0)
      expect(pmin.getMonths).toEqual(Int.MinValue)
      expect(pmin1.getMonths).toEqual(Int.MinValue + 1)
      expect(pmax.getMonths).toEqual(Int.MaxValue)
    }

    it("should respond to `getDays`") {
      expect(oneYear.getDays).toEqual(0)
      expect(oneMonth.getDays).toEqual(0)
      expect(oneDay.getDays).toEqual(1)
      expect(pmin.getDays).toEqual(Int.MinValue)
      expect(pmin1.getDays).toEqual(Int.MinValue + 1)
      expect(pmax.getDays).toEqual(Int.MaxValue)
    }

    it("should respond to `withYears`") {
      for {
        p <- samples
        n <- Seq(Int.MinValue, 0, Int.MaxValue)
      } {
        val p1 = p.withYears(n)
        expect(p1.getYears).toEqual(n)
        expect(p1.getMonths).toEqual(p.getMonths)
        expect(p1.getDays).toEqual(p.getDays)
      }
    }

    it("should respond to `withMonths`") {
      for {
        p <- samples
        n <- Seq(Int.MinValue, 0, Int.MaxValue)
      } {
        val p1 = p.withMonths(n)
        expect(p1.getYears).toEqual(p.getYears)
        expect(p1.getMonths).toEqual(n)
        expect(p1.getDays).toEqual(p.getDays)
      }
    }

    it("should respond to `withDays`") {
      for {
        p <- samples
        n <- Seq(Int.MinValue, 0, Int.MaxValue)
      } {
        val p1 = p.withDays(n)
        expect(p1.getYears).toEqual(p.getYears)
        expect(p1.getMonths).toEqual(p.getMonths)
        expect(p1.getDays).toEqual(n)
      }
    }

    it("should respond to `plus`") {
      for {
        p1 <- samples1 :+ pmin1
        p2 <- if (p1 != pmin1) samples1 :+ pmin1 else samples1
      } {
        val p = p1.plus(p2)
        expect(p.getYears).toEqual(p1.getYears + p2.getYears)
        expect(p.getMonths).toEqual(p1.getMonths + p2.getMonths)
        expect(p.getDays).toEqual(p1.getDays + p2.getDays)
      }

      for (p <- Seq(oneYear, oneMonth, oneDay))
        expectThrows[ArithmeticException](pmax.plus(p))
      for (p <- Seq(oneYear.negated, oneMonth.negated, oneDay.negated))
        expectThrows[ArithmeticException](pmin.plus(p))
      for (p <- samples)
        expectThrows[DateTimeException](p.plus(Duration.ZERO))
    }

    it("should respond to `plusYears`") {
      for {
        p <- samples1
        n <- Seq(Int.MinValue + 1, -1, 0, 1, Int.MaxValue - 1)
      } {
        val p1 = p.plusYears(n)
        expect(p1.getYears).toEqual(p.getYears + n)
        expect(p1.getMonths).toEqual(p.getMonths)
        expect(p1.getDays).toEqual(p.getDays)
      }

      expectThrows[ArithmeticException](oneYear.plusYears(Int.MaxValue))
      expectThrows[ArithmeticException](oneYear.negated.plusYears(Int.MinValue))
      expectThrows[ArithmeticException](pmax.plusYears(1))
      expectThrows[ArithmeticException](pmin.plusYears(-1))
    }

    it("should respond to `plusMonths`") {
      for {
        p <- samples1
        n <- Seq(Int.MinValue + 1, -1, 0, 1, Int.MaxValue - 1)
      } {
        val p1 = p.plusMonths(n)
        expect(p1.getYears).toEqual(p.getYears)
        expect(p1.getMonths).toEqual(p.getMonths + n)
        expect(p1.getDays).toEqual(p.getDays)
      }

      expectThrows[ArithmeticException](oneMonth.plusMonths(Int.MaxValue))
      expectThrows[ArithmeticException](oneMonth.negated.plusMonths(Int.MinValue))
      expectThrows[ArithmeticException](pmax.plusMonths(1))
      expectThrows[ArithmeticException](pmin.plusMonths(-1))
    }

    it("should respond to `plusDays`") {
      for {
        p <- samples1
        n <- Seq(Int.MinValue + 1, -1, 0, 1, Int.MaxValue - 1)
      } {
        val p1 = p.plusDays(n)
        expect(p1.getYears).toEqual(p.getYears)
        expect(p1.getMonths).toEqual(p.getMonths)
        expect(p1.getDays).toEqual(p.getDays + n)
      }

      expectThrows[ArithmeticException](oneDay.plusDays(Int.MaxValue))
      expectThrows[ArithmeticException](oneDay.negated.plusDays(Int.MinValue))
      expectThrows[ArithmeticException](pmax.plusDays(1))
      expectThrows[ArithmeticException](pmin.plusDays(-1))
    }

    it("should respond to `minus`") {
      for {
        p1 <- samples1 :+ pmin1
        p2 <- if (p1.isNegative) samples1 :+ pmin1 else samples1
      } {
        val p = p1.minus(p2)
        expect(p.getYears).toEqual(p1.getYears - p2.getYears)
        expect(p.getMonths).toEqual(p1.getMonths - p2.getMonths)
        expect(p.getDays).toEqual(p1.getDays - p2.getDays)
      }

      for (p <- Seq(oneYear, oneMonth, oneDay))
        expectThrows[ArithmeticException](pmin.minus(p))
      for (p <- Seq(oneYear.negated, oneMonth.negated, oneDay.negated))
        expectThrows[ArithmeticException](pmax.minus(p))
      for (p <- samples)
        expectThrows[DateTimeException](p.minus(Duration.ZERO))
    }

    it("should respond to `minusYears`") {
      for {
        p <- samples1
        n <- Seq(Int.MinValue + 2, -1, 0, 1, Int.MaxValue)
      } {
        val p1 = p.minusYears(n)
        expect(p1.getYears).toEqual(p.getYears - n)
        expect(p1.getMonths).toEqual(p.getMonths)
        expect(p1.getDays).toEqual(p.getDays)
      }

      expectThrows[ArithmeticException](oneYear.minusYears(Int.MinValue + 1))
      expectThrows[ArithmeticException](pmin.minusYears(1))
      expectThrows[ArithmeticException](pmax.minusYears(-1))
    }

    it("should respond to `minusMonths`") {
      for {
        p <- samples1
        n <- Seq(Int.MinValue + 2, -1, 0, 1, Int.MaxValue)
      } {
        val p1 = p.minusMonths(n)
        expect(p1.getYears).toEqual(p.getYears)
        expect(p1.getMonths).toEqual(p.getMonths - n)
        expect(p1.getDays).toEqual(p.getDays)
      }

      expectThrows[ArithmeticException](oneMonth.minusMonths(Int.MinValue + 1))
      expectThrows[ArithmeticException](pmin.minusMonths(1))
      expectThrows[ArithmeticException](pmax.minusMonths(-1))
    }

    it("should respond to `minusDays`") {
      for {
        p <- samples1
        n <- Seq(Int.MinValue + 2, -1, 0, 1, Int.MaxValue)
      } {
        val p1 = p.minusDays(n)
        expect(p1.getYears).toEqual(p.getYears)
        expect(p1.getMonths).toEqual(p.getMonths)
        expect(p1.getDays).toEqual(p.getDays - n)
      }

      expectThrows[ArithmeticException](oneDay.minusDays(Int.MinValue + 1))
      expectThrows[ArithmeticException](pmin.minusDays(1))
      expectThrows[ArithmeticException](pmax.minusDays(-1))
    }

    it("should respond to `multipliedBy`") {
      for {
        p <- samples1
        min = if (p.isNegative) Int.MinValue + 1 else Int.MinValue
        n <- Seq(min, -2, 2, Int.MaxValue)
      } {
        val p1 = p.multipliedBy(n)
        expect(p1.getYears).toEqual(p.getYears * n)
        expect(p1.getMonths).toEqual(p.getMonths * n)
        expect(p1.getDays).toEqual(p.getDays * n)
      }
      for (p <- samples) {
        expect(p.multipliedBy(1) == p).toBeTruthy
        expect(p.multipliedBy(0) == ZERO).toBeTruthy
      }
      for (p <- samples if p != pmin)
        expect(p.multipliedBy(-1) == p.negated)

      expectThrows[ArithmeticException](pmin.multipliedBy(2))
      expectThrows[ArithmeticException](pmin.multipliedBy(-1))
      for (p <- Seq(pmin1, pmax)) {
        expectThrows[ArithmeticException](p.multipliedBy(2))
        expectThrows[ArithmeticException](p.multipliedBy(-2))
      }
      for (p <- samples1 if p != ZERO) {
        val p2 = p.multipliedBy(2)
        expectThrows[ArithmeticException](p2.multipliedBy(Int.MaxValue))
        expectThrows[ArithmeticException](p2.multipliedBy(Int.MinValue))
      }
      for (p <- Seq(oneYear.negated, oneMonth.negated, oneDay.negated))
        expectThrows[ArithmeticException](p.multipliedBy(Int.MinValue))
    }

    it("should respond to `negated`") {
      for (p <- samples if p != pmin) {
        val p1 = p.negated
        expect(p1.getYears).toEqual(-p.getYears)
        expect(p1.getMonths).toEqual(-p.getMonths)
        expect(p1.getDays).toEqual(-p.getDays)
      }

      expectThrows[ArithmeticException](pmin.negated)
    }

    it("should respond to `normalized`") {
      val ps = samples1 ++ Seq(of(1, -1, 0), of(-1, 1, 0)) ++
          Seq(of(1, -25, 1), of(-1, 25, -1), of(1, 13, 1), of(-1, -13, -1))
      for (p <- ps) {
        val p1 = p.normalized
        val years = p1.getYears
        val months = p1.getMonths
        expect(Math.abs(months) < 12)
        expect(years > 0 && months < 0).toBeFalsy
        expect(years < 0 && months > 0).toBeFalsy
        expect(years * 12 + months == p.getYears * 12 + p.getMonths)
      }

      for (p <- Seq(pmin, pmin1, pmax))
        expectThrows[ArithmeticException](p.normalized)
    }

    it("should respond to `toTotalMonths`") {
      for (p <- samples)
        expect(p.toTotalMonths == p.getYears.toLong * 12 + p.getMonths).toBeTruthy
    }

    it("should respond to `addTo`") {
      val ds = Seq(LocalDate.MIN, LocalDate.MAX, LocalDate.of(2011, 2, 28),
          LocalDate.of(2012, 2, 29))
      val ts = Seq(LocalTime.MIN, LocalTime.NOON, LocalTime.MAX)
      val p1 = Period.of(1, 3, 5)
      val p2 = p1.negated

      for (t <- ds ++ ts)
        expect(ZERO.addTo(t) == t).toBeTruthy

      expect(Period.of(1999999998, 11, 30).addTo(LocalDate.MIN) ==
          LocalDate.MAX).toBeTruthy
      expect(Period.of(-1999999998, -11, -30).addTo(LocalDate.MAX) ==
          LocalDate.MIN).toBeTruthy
      expect(Period.of(1, 0, 1).addTo(LocalDate.of(2011, 2, 28)) ==
          LocalDate.of(2012, 2, 29)).toBeTruthy
      expect(Period.of(-1, 0, -1).addTo(LocalDate.of(2012, 2, 29)) ==
          LocalDate.of(2011, 2, 27)).toBeTruthy
      expect(oneYear.addTo(LocalDate.of(2012, 2, 29)) ==
          LocalDate.of(2013, 2, 28)).toBeTruthy
      expect(Period.of(-1, 0, 1).addTo(LocalDate.of(2013, 2, 28)) ==
          LocalDate.of(2012, 2, 29)).toBeTruthy

      for (p <- Seq(oneYear, oneMonth, oneYear, pmin, pmax))
        expectThrows[DateTimeException](p.addTo(LocalDate.MAX))
      for (p <- Seq(oneYear.negated, oneMonth.negated, oneYear.negated, pmin, pmax))
        expectThrows[DateTimeException](p.addTo(LocalDate.MIN))
      for {
        p <- samples if p != ZERO
        t <- ts
      } {
        expectThrows[DateTimeException](p.addTo(t))
      }
    }

    it("should respond to `subtractFrom`") {
      val ds = Seq(LocalDate.MIN, LocalDate.MAX, LocalDate.of(2012, 2, 29))
      val ts = Seq(LocalTime.MIN, LocalTime.NOON, LocalTime.MAX)

      for (t <- ds ++ ts)
        expect(ZERO.subtractFrom(t) == t).toBeTruthy

      expect(Period.of(-1999999998, -11, -30).subtractFrom(LocalDate.MIN) ==
          LocalDate.MAX).toBeTruthy
      expect(Period.of(1999999998, 11, 30).subtractFrom(LocalDate.MAX) ==
          LocalDate.MIN).toBeTruthy
      expect(Period.of(-1, 0, -1).subtractFrom(LocalDate.of(2011, 2, 28)) ==
          LocalDate.of(2012, 2, 29)).toBeTruthy
      expect(Period.of(1, 0, 1).subtractFrom(LocalDate.of(2012, 2, 29)) ==
          LocalDate.of(2011, 2, 27)).toBeTruthy
      expect(oneYear.negated.subtractFrom(LocalDate.of(2012, 2, 29)) ==
          LocalDate.of(2013, 2, 28)).toBeTruthy
      expect(Period.of(1, 0, -1).subtractFrom(LocalDate.of(2013, 2, 28)) ==
          LocalDate.of(2012, 2, 29)).toBeTruthy

      for (p <- Seq(oneYear.negated, oneMonth.negated, oneYear.negated, pmin, pmax))
        expectThrows[DateTimeException](p.subtractFrom(LocalDate.MAX))
      for (p <- Seq(oneYear, oneMonth, oneYear, pmin, pmax))
        expectThrows[DateTimeException](p.subtractFrom(LocalDate.MIN))
      for {
        p <- samples if p != ZERO
        t <- ts
      } {
        expectThrows[DateTimeException](p.subtractFrom(t))
      }
    }

    it("should override `toString`") {
      expect(ZERO.toString).toEqual("P0D")
      expect(pmin.toString).toEqual("P-2147483648Y-2147483648M-2147483648D")
      expect(pmax.toString).toEqual("P2147483647Y2147483647M2147483647D")
      expect(oneYear.toString).toEqual("P1Y")
      expect(oneYear.negated.toString).toEqual("P-1Y")
      expect(oneMonth.toString).toEqual("P1M")
      expect(oneMonth.negated.toString).toEqual("P-1M")
      expect(oneDay.toString).toEqual("P1D")
      expect(oneDay.negated.toString).toEqual("P-1D")
      expect(of(2, -3, 0).toString).toEqual("P2Y-3M")
      expect(of(-5, 0, 7).toString).toEqual("P-5Y7D")
      expect(of(0, 11, -13).toString).toEqual("P11M-13D")
    }
  }
}
