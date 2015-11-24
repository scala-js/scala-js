package org.scalajs.testsuite.javalib.time.temporal

import java.time.DateTimeException
import java.time.temporal.{UnsupportedTemporalTypeException, ChronoField, TemporalAccessor}

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
        if (accessor.isSupported(field))
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
