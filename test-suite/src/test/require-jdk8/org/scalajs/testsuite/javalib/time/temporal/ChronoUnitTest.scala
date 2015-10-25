package org.scalajs.testsuite.javalib.time.temporal

import java.time.temporal.ChronoUnit

import org.scalajs.jasminetest.JasmineTest

object ChronoUnitTest extends JasmineTest {
  import ChronoUnit._

  describe("java.time.temporal.ChronoUnit") {
    it("should respond to `isDurationEstimated`") {
      for (u <- ChronoUnit.values())
        expect(u.isDurationEstimated != u.isTimeBased).toBeTruthy
    }

    it("should respond to `isDateBased`") {
      expect(NANOS.isDateBased).toBeFalsy
      expect(MICROS.isDateBased).toBeFalsy
      expect(MILLIS.isDateBased).toBeFalsy
      expect(SECONDS.isDateBased).toBeFalsy
      expect(MINUTES.isDateBased).toBeFalsy
      expect(HOURS.isDateBased).toBeFalsy
      expect(HALF_DAYS.isDateBased).toBeFalsy
      expect(DAYS.isDateBased).toBeTruthy
      expect(WEEKS.isDateBased).toBeTruthy
      expect(MONTHS.isDateBased).toBeTruthy
      expect(YEARS.isDateBased).toBeTruthy
      expect(DECADES.isDateBased).toBeTruthy
      expect(CENTURIES.isDateBased).toBeTruthy
      expect(MILLENNIA.isDateBased).toBeTruthy
      expect(ERAS.isDateBased).toBeTruthy
      expect(FOREVER.isDateBased).toBeFalsy
    }

    it("should respond to `isTimeBased`") {
      expect(NANOS.isTimeBased).toBeTruthy
      expect(MICROS.isTimeBased).toBeTruthy
      expect(MILLIS.isTimeBased).toBeTruthy
      expect(SECONDS.isTimeBased).toBeTruthy
      expect(MINUTES.isTimeBased).toBeTruthy
      expect(HOURS.isTimeBased).toBeTruthy
      expect(HALF_DAYS.isTimeBased).toBeTruthy
      expect(DAYS.isTimeBased).toBeFalsy
      expect(WEEKS.isTimeBased).toBeFalsy
      expect(MONTHS.isTimeBased).toBeFalsy
      expect(YEARS.isTimeBased).toBeFalsy
      expect(DECADES.isTimeBased).toBeFalsy
      expect(CENTURIES.isTimeBased).toBeFalsy
      expect(MILLENNIA.isTimeBased).toBeFalsy
      expect(ERAS.isTimeBased).toBeFalsy
      expect(FOREVER.isTimeBased).toBeFalsy
    }

    it("should respond to `values`") {
      val units = ChronoUnit.values()

      expect(units(0) == NANOS).toBeTruthy
      expect(units(1) == MICROS).toBeTruthy
      expect(units(2) == MILLIS).toBeTruthy
      expect(units(3) == SECONDS).toBeTruthy
      expect(units(4) == MINUTES).toBeTruthy
      expect(units(5) == HOURS).toBeTruthy
      expect(units(6) == HALF_DAYS).toBeTruthy
      expect(units(7) == DAYS).toBeTruthy
      expect(units(8) == WEEKS).toBeTruthy
      expect(units(9) == MONTHS).toBeTruthy
      expect(units(10) == YEARS).toBeTruthy
      expect(units(11) == DECADES).toBeTruthy
      expect(units(12) == CENTURIES).toBeTruthy
      expect(units(13) == MILLENNIA).toBeTruthy
      expect(units(14) == ERAS).toBeTruthy
      expect(units(15) == FOREVER).toBeTruthy
    }

    it("should respond to `valueOf`") {
      expect(valueOf("NANOS") == NANOS).toBeTruthy
      expect(valueOf("MICROS") == MICROS).toBeTruthy
      expect(valueOf("MILLIS") == MILLIS).toBeTruthy
      expect(valueOf("SECONDS") == SECONDS).toBeTruthy
      expect(valueOf("MINUTES") == MINUTES).toBeTruthy
      expect(valueOf("HOURS") == HOURS).toBeTruthy
      expect(valueOf("HALF_DAYS") == HALF_DAYS).toBeTruthy
      expect(valueOf("DAYS") == DAYS).toBeTruthy
      expect(valueOf("WEEKS") == WEEKS).toBeTruthy
      expect(valueOf("MONTHS") == MONTHS).toBeTruthy
      expect(valueOf("YEARS") == YEARS).toBeTruthy
      expect(valueOf("DECADES") == DECADES).toBeTruthy
      expect(valueOf("CENTURIES") == CENTURIES).toBeTruthy
      expect(valueOf("MILLENNIA") == MILLENNIA).toBeTruthy
      expect(valueOf("ERAS") == ERAS).toBeTruthy
      expect(valueOf("FOREVER") == FOREVER).toBeTruthy
    }
  }
}
