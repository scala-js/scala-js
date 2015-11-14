package org.scalajs.testsuite.javalib.time.chrono

import java.time.LocalDate

import org.scalajs.jasminetest.JasmineTest
import org.scalajs.testsuite.utils.ExpectExceptions
import java.time.chrono.ChronoPeriod

object ChronoPeriodTest extends JasmineTest with ExpectExceptions {
  describe("java.time.chrono.ChronoPeriod") {
    it("should respond to `between`") {
      val ds = Seq(LocalDate.MIN, LocalDate.of(2011, 2, 28), LocalDate.MAX)
      for {
        d1 <- ds
        d2 <- ds
      } {
        expect(ChronoPeriod.between(d1, d2) == d1.until(d2)).toBeTruthy
      }
    }
  }
}
