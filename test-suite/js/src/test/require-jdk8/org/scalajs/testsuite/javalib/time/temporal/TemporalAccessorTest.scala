package org.scalajs.testsuite.javalib.time.temporal

import java.time.chrono.IsoEra
import java.time.{LocalDate, DateTimeException}
import java.time.temporal.{ValueRange, UnsupportedTemporalTypeException, ChronoField, TemporalAccessor}

import org.scalajs.jasminetest.JasmineTest
import org.scalajs.testsuite.utils.ExpectExceptions

trait TemporalAccessorTest extends JasmineTest with ExpectExceptions {

  def testTemporalAccessorApi(samples: TemporalAccessor*): Unit = {
    it("should respond with false to `isSupported(null: TemporalField)`") {
      for (accessor <- samples)
        expect(accessor.isSupported(null)).toBeFalsy
    }

    it("should respond to `range`") {
      for {
        accessor <- samples
        field <- ChronoField.values()
      } {
        if (accessor.isInstanceOf[LocalDate] && field == ChronoField.DAY_OF_MONTH) {
          val date = accessor.asInstanceOf[LocalDate]
          expect(date.range(field) == ValueRange.of(1, date.lengthOfMonth)).toBeTruthy
        }
        else if (accessor.isInstanceOf[LocalDate] && field == ChronoField.DAY_OF_YEAR) {
          val date = accessor.asInstanceOf[LocalDate]
          expect(date.range(field) == ValueRange.of(1, date.lengthOfYear)).toBeTruthy
        }
        else if (accessor.isInstanceOf[LocalDate] && field == ChronoField.ALIGNED_WEEK_OF_MONTH) {
          val date = accessor.asInstanceOf[LocalDate]
          val weeks = if (date.lengthOfMonth > 28) 5 else 4
          expect(date.range(field) == ValueRange.of(1, weeks)).toBeTruthy
        }
        else if (accessor.isInstanceOf[LocalDate] && field == ChronoField.YEAR_OF_ERA) {
          val date = accessor.asInstanceOf[LocalDate]
          val maxYear = if (date.getEra == IsoEra.CE) 999999999 else 1000000000
          expect(date.range(field) == ValueRange.of(1, maxYear)).toBeTruthy
        }
        else if (accessor.isSupported(field))
          expect(accessor.range(field) == field.range).toBeTruthy
        else
          expectThrows[UnsupportedTemporalTypeException](accessor.range(field))
      }
    }

    it("should respond to `get`") {
      for {
        accessor <- samples
        field <- ChronoField.values()
      } {
        if (accessor.isSupported(field) && field.range.isIntValue)
          expect(accessor.get(field)).toEqual(accessor.getLong(field))
        else if (accessor.isSupported(field))
          expectThrows[DateTimeException](accessor.get(field))
        else
          expectThrows[UnsupportedTemporalTypeException](accessor.get(field))
      }
    }

    it("should throw at `getLong` for unsupported fields") {
      for {
        accessor <- samples
        field <- ChronoField.values() if !accessor.isSupported(field)
      } {
        expectThrows[UnsupportedTemporalTypeException](accessor.getLong(field))
      }
    }
  }
}
