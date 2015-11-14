package org.scalajs.testsuite.javalib.time.chrono

import java.time.DateTimeException
import java.time.chrono.{IsoChronology, Chronology}

import org.scalajs.jasminetest.JasmineTest
import org.scalajs.testsuite.utils.ExpectExceptions

object ChronologyTest extends JasmineTest with ExpectExceptions {
  import Chronology._

  describe("java.time.chrono.Chronology") {
    it("should respond to `of`") {
      expect(of("ISO") == IsoChronology.INSTANCE).toBeTruthy

      expectThrows[DateTimeException](of(""))
    }

    it("should respond to `getAvailableChronologies`") {
      val chronologies = Chronology.getAvailableChronologies
      expect(chronologies.contains(IsoChronology.INSTANCE)).toBeTruthy
    }
  }
}
