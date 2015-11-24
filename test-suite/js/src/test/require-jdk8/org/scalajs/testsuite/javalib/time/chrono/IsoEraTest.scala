package org.scalajs.testsuite.javalib.time.chrono

import java.time.chrono.IsoEra
import java.time.temporal.ChronoField
import java.time.DateTimeException

import org.scalajs.testsuite.javalib.time.temporal.TemporalAccessorTest

object IsoEraTest extends TemporalAccessorTest {
  import IsoEra._

  describe("java.time.chrono.IsoEra") {
    testTemporalAccessorApi(BCE, CE)

    it("should respond to `getValue`") {
      expect(BCE.getValue).toEqual(0)
      expect(CE.getValue).toEqual(1)
    }

    it("should respond to `isSupported`") {
      for {
        era <- Seq(BCE, CE)
        f <- ChronoField.values()
      } {
        if (f == ChronoField.ERA)
          expect(era.isSupported(f)).toBeTruthy
        else
          expect(era.isSupported(f)).toBeFalsy
      }
    }

    it("should respond to `getLong`") {
      for (era <- Seq(BCE, CE))
        expect(era.getLong(ChronoField.ERA)).toEqual(era.getValue)
    }

    it("should be comparable") {
      expect(BCE.compareTo(BCE)).toEqual(0)
      expect(BCE.compareTo(CE)).toBeLessThan(0)
      expect(CE.compareTo(BCE)).toBeGreaterThan(0)
      expect(CE.compareTo(CE)).toEqual(0)
    }

    it("should respond to `values`") {
      val eras = IsoEra.values()

      expect(eras.length).toEqual(2)
      expect(eras(0) == BCE).toBeTruthy
      expect(eras(1) == CE).toBeTruthy
    }

    it("should respond to `valueOf`") {
      expect(valueOf("BCE") == BCE).toBeTruthy
      expect(valueOf("CE") == CE).toBeTruthy

      expectThrows[IllegalArgumentException](valueOf(""))
    }

    it("should respond to `of`") {
      expect(of(0) == BCE).toBeTruthy
      expect(of(1) == CE).toBeTruthy

      for (n <- Seq(Int.MinValue, -1, 2, Int.MaxValue))
        expectThrows[DateTimeException](of(n))
    }
  }
}
