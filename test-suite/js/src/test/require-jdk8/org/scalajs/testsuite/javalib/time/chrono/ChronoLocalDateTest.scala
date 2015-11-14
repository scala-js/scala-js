package org.scalajs.testsuite.javalib.time.chrono

import java.time.{DateTimeException, LocalTime, LocalDate}
import java.time.chrono.ChronoLocalDate

import org.scalajs.testsuite.javalib.time.temporal.TemporalTest

object ChronoLocalDateTest extends TemporalTest {
  import ChronoLocalDate._

  describe("java.time.chrono.ChronoLocalDate") {
    it("should respond to `timeLineOrder`") {
      val ord = timeLineOrder
      val ds = Seq(LocalDate.MIN, LocalDate.of(2011, 2, 28), LocalDate.MAX)

      for {
        d1 <- ds
        d2 <- ds
      } {
        expect(ord.compare(d1, d2) == d1.compareTo(d2)).toBeTruthy
      }
    }

    it("should respond to `from`") {
      for (d <- Seq(LocalDate.MIN, LocalDate.of(2011, 2, 28), LocalDate.MAX))
        testTemporal(from(d))(d)

      for (t <- Seq(LocalTime.MIN, LocalTime.NOON, LocalTime.MAX))
        expectThrows[DateTimeException](from(t))
    }
  }
}
