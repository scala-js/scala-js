package org.scalajs.testsuite.javalib.time.temporal

import java.time.temporal.ChronoField

import org.scalajs.jasminetest.JasmineTest

object ChronoFieldTest extends JasmineTest {
  import ChronoField._

  describe("java.time.temporal.ChronoField") {
    it("should respond to `isDateBased`") {
      expect(NANO_OF_SECOND.isDateBased).toBeFalsy
      expect(NANO_OF_DAY.isDateBased).toBeFalsy
      expect(MICRO_OF_SECOND.isDateBased).toBeFalsy
      expect(MICRO_OF_DAY.isDateBased).toBeFalsy
      expect(MILLI_OF_SECOND.isDateBased).toBeFalsy
      expect(MILLI_OF_DAY.isDateBased).toBeFalsy
      expect(SECOND_OF_MINUTE.isDateBased).toBeFalsy
      expect(SECOND_OF_DAY.isDateBased).toBeFalsy
      expect(MINUTE_OF_HOUR.isDateBased).toBeFalsy
      expect(MINUTE_OF_DAY.isDateBased).toBeFalsy
      expect(HOUR_OF_AMPM.isDateBased).toBeFalsy
      expect(CLOCK_HOUR_OF_AMPM.isDateBased).toBeFalsy
      expect(HOUR_OF_DAY.isDateBased).toBeFalsy
      expect(CLOCK_HOUR_OF_DAY.isDateBased).toBeFalsy
      expect(AMPM_OF_DAY.isDateBased).toBeFalsy
      expect(DAY_OF_WEEK.isDateBased).toBeTruthy
      expect(ALIGNED_DAY_OF_WEEK_IN_MONTH.isDateBased).toBeTruthy
      expect(ALIGNED_DAY_OF_WEEK_IN_YEAR.isDateBased).toBeTruthy
      expect(DAY_OF_MONTH.isDateBased).toBeTruthy
      expect(DAY_OF_YEAR.isDateBased).toBeTruthy
      expect(EPOCH_DAY.isDateBased).toBeTruthy
      expect(ALIGNED_WEEK_OF_MONTH.isDateBased).toBeTruthy
      expect(ALIGNED_WEEK_OF_YEAR.isDateBased).toBeTruthy
      expect(MONTH_OF_YEAR.isDateBased).toBeTruthy
      expect(PROLEPTIC_MONTH.isDateBased).toBeTruthy
      expect(YEAR_OF_ERA.isDateBased).toBeTruthy
      expect(YEAR.isDateBased).toBeTruthy
      expect(ERA.isDateBased).toBeTruthy
      expect(INSTANT_SECONDS.isDateBased).toBeFalsy
      expect(OFFSET_SECONDS.isDateBased).toBeFalsy
    }

    it("should respond to `isTimeBased`") {
      expect(NANO_OF_SECOND.isTimeBased).toBeTruthy
      expect(NANO_OF_DAY.isTimeBased).toBeTruthy
      expect(MICRO_OF_SECOND.isTimeBased).toBeTruthy
      expect(MICRO_OF_DAY.isTimeBased).toBeTruthy
      expect(MILLI_OF_SECOND.isTimeBased).toBeTruthy
      expect(MILLI_OF_DAY.isTimeBased).toBeTruthy
      expect(SECOND_OF_MINUTE.isTimeBased).toBeTruthy
      expect(SECOND_OF_DAY.isTimeBased).toBeTruthy
      expect(MINUTE_OF_HOUR.isTimeBased).toBeTruthy
      expect(MINUTE_OF_DAY.isTimeBased).toBeTruthy
      expect(HOUR_OF_AMPM.isTimeBased).toBeTruthy
      expect(CLOCK_HOUR_OF_AMPM.isTimeBased).toBeTruthy
      expect(HOUR_OF_DAY.isTimeBased).toBeTruthy
      expect(CLOCK_HOUR_OF_DAY.isTimeBased).toBeTruthy
      expect(AMPM_OF_DAY.isTimeBased).toBeTruthy
      expect(DAY_OF_WEEK.isTimeBased).toBeFalsy
      expect(ALIGNED_DAY_OF_WEEK_IN_MONTH.isTimeBased).toBeFalsy
      expect(ALIGNED_DAY_OF_WEEK_IN_YEAR.isTimeBased).toBeFalsy
      expect(DAY_OF_MONTH.isTimeBased).toBeFalsy
      expect(DAY_OF_YEAR.isTimeBased).toBeFalsy
      expect(EPOCH_DAY.isTimeBased).toBeFalsy
      expect(ALIGNED_WEEK_OF_MONTH.isTimeBased).toBeFalsy
      expect(ALIGNED_WEEK_OF_YEAR.isTimeBased).toBeFalsy
      expect(MONTH_OF_YEAR.isTimeBased).toBeFalsy
      expect(PROLEPTIC_MONTH.isTimeBased).toBeFalsy
      expect(YEAR_OF_ERA.isTimeBased).toBeFalsy
      expect(YEAR.isTimeBased).toBeFalsy
      expect(ERA.isTimeBased).toBeFalsy
      expect(INSTANT_SECONDS.isTimeBased).toBeFalsy
      expect(OFFSET_SECONDS.isTimeBased).toBeFalsy
    }

    it("should respond to `values`") {
      val fields = ChronoField.values()

      expect(fields(0) == NANO_OF_SECOND).toBeTruthy
      expect(fields(1) == NANO_OF_DAY).toBeTruthy
      expect(fields(2) == MICRO_OF_SECOND).toBeTruthy
      expect(fields(3) == MICRO_OF_DAY).toBeTruthy
      expect(fields(4) == MILLI_OF_SECOND).toBeTruthy
      expect(fields(5) == MILLI_OF_DAY).toBeTruthy
      expect(fields(6) == SECOND_OF_MINUTE).toBeTruthy
      expect(fields(7) == SECOND_OF_DAY).toBeTruthy
      expect(fields(8) == MINUTE_OF_HOUR).toBeTruthy
      expect(fields(9) == MINUTE_OF_DAY).toBeTruthy
      expect(fields(10) == HOUR_OF_AMPM).toBeTruthy
      expect(fields(11) == CLOCK_HOUR_OF_AMPM).toBeTruthy
      expect(fields(12) == HOUR_OF_DAY).toBeTruthy
      expect(fields(13) == CLOCK_HOUR_OF_DAY).toBeTruthy
      expect(fields(14) == AMPM_OF_DAY).toBeTruthy
      expect(fields(15) == DAY_OF_WEEK).toBeTruthy
      expect(fields(16) == ALIGNED_DAY_OF_WEEK_IN_MONTH).toBeTruthy
      expect(fields(17) == ALIGNED_DAY_OF_WEEK_IN_YEAR).toBeTruthy
      expect(fields(18) == DAY_OF_MONTH).toBeTruthy
      expect(fields(19) == DAY_OF_YEAR).toBeTruthy
      expect(fields(20) == EPOCH_DAY).toBeTruthy
      expect(fields(21) == ALIGNED_WEEK_OF_MONTH).toBeTruthy
      expect(fields(22) == ALIGNED_WEEK_OF_YEAR).toBeTruthy
      expect(fields(23) == MONTH_OF_YEAR).toBeTruthy
      expect(fields(24) == PROLEPTIC_MONTH).toBeTruthy
      expect(fields(25) == YEAR_OF_ERA).toBeTruthy
      expect(fields(26) == YEAR).toBeTruthy
      expect(fields(27) == ERA).toBeTruthy
      expect(fields(28) == INSTANT_SECONDS).toBeTruthy
      expect(fields(29) == OFFSET_SECONDS).toBeTruthy
    }

    it("should respond to `valueOf`") {
      expect(valueOf("NANO_OF_SECOND") == NANO_OF_SECOND).toBeTruthy
      expect(valueOf("NANO_OF_DAY") == NANO_OF_DAY).toBeTruthy
      expect(valueOf("MICRO_OF_SECOND") == MICRO_OF_SECOND).toBeTruthy
      expect(valueOf("MICRO_OF_DAY") == MICRO_OF_DAY).toBeTruthy
      expect(valueOf("MILLI_OF_SECOND") == MILLI_OF_SECOND).toBeTruthy
      expect(valueOf("MILLI_OF_DAY") == MILLI_OF_DAY).toBeTruthy
      expect(valueOf("SECOND_OF_MINUTE") == SECOND_OF_MINUTE).toBeTruthy
      expect(valueOf("SECOND_OF_DAY") == SECOND_OF_DAY).toBeTruthy
      expect(valueOf("MINUTE_OF_HOUR") == MINUTE_OF_HOUR).toBeTruthy
      expect(valueOf("MINUTE_OF_DAY") == MINUTE_OF_DAY).toBeTruthy
      expect(valueOf("HOUR_OF_AMPM") == HOUR_OF_AMPM).toBeTruthy
      expect(valueOf("CLOCK_HOUR_OF_AMPM") == CLOCK_HOUR_OF_AMPM).toBeTruthy
      expect(valueOf("HOUR_OF_DAY") == HOUR_OF_DAY).toBeTruthy
      expect(valueOf("CLOCK_HOUR_OF_DAY") == CLOCK_HOUR_OF_DAY).toBeTruthy
      expect(valueOf("AMPM_OF_DAY") == AMPM_OF_DAY).toBeTruthy
      expect(valueOf("DAY_OF_WEEK") == DAY_OF_WEEK).toBeTruthy
      expect(valueOf("ALIGNED_DAY_OF_WEEK_IN_MONTH") ==
          ALIGNED_DAY_OF_WEEK_IN_MONTH).toBeTruthy
      expect(valueOf("ALIGNED_DAY_OF_WEEK_IN_YEAR") ==
          ALIGNED_DAY_OF_WEEK_IN_YEAR).toBeTruthy
      expect(valueOf("DAY_OF_MONTH") == DAY_OF_MONTH).toBeTruthy
      expect(valueOf("DAY_OF_YEAR") == DAY_OF_YEAR).toBeTruthy
      expect(valueOf("EPOCH_DAY") == EPOCH_DAY).toBeTruthy
      expect(valueOf("ALIGNED_WEEK_OF_MONTH") == ALIGNED_WEEK_OF_MONTH).toBeTruthy
      expect(valueOf("ALIGNED_WEEK_OF_YEAR") == ALIGNED_WEEK_OF_YEAR).toBeTruthy
      expect(valueOf("MONTH_OF_YEAR") == MONTH_OF_YEAR).toBeTruthy
      expect(valueOf("PROLEPTIC_MONTH") == PROLEPTIC_MONTH).toBeTruthy
      expect(valueOf("YEAR_OF_ERA") == YEAR_OF_ERA).toBeTruthy
      expect(valueOf("YEAR") == YEAR).toBeTruthy
      expect(valueOf("ERA") == ERA).toBeTruthy
      expect(valueOf("INSTANT_SECONDS") == INSTANT_SECONDS).toBeTruthy
      expect(valueOf("OFFSET_SECONDS") == OFFSET_SECONDS).toBeTruthy
    }
  }
}
