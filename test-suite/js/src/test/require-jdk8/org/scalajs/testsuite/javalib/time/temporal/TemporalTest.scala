package org.scalajs.testsuite.javalib.time.temporal

import java.time.DateTimeException
import java.time.temporal._

trait TemporalTest extends TemporalAccessorTest {
  val dateBasedUnits = ChronoUnit.values.filter(_.isDateBased)

  val timeBasedUnits = ChronoUnit.values.filter(_.isTimeBased)

  def testTemporal(actual: => Temporal)(expected: => Temporal): Unit = {
    try {
      val e = expected
      expectNoException(actual)
      expect(e == actual).toBeTruthy
    } catch {
      case _: UnsupportedTemporalTypeException =>
        expectThrows[UnsupportedTemporalTypeException](actual)

      case _: DateTimeException =>
        expectThrows[DateTimeException](actual)

      case _: ArithmeticException =>
        expectThrows[ArithmeticException](actual)
    }
  }

  def testTemporalApi(samples: Temporal*): Unit = {
    testTemporalAccessorApi(samples:_*)

    it("should respond with false to `isSupported(null: TemporalUnit)`") {
      for (temporal <- samples)
        expect(temporal.isSupported(null: TemporalUnit)).toBeFalsy
    }

    it("should throw at `with` for unsupported fields") {
      val values = Seq(Long.MinValue, Int.MinValue.toLong, -1L, 0L, 1L,
          Int.MaxValue.toLong, Long.MaxValue)

      for {
        temporal <- samples
        field <- ChronoField.values if !temporal.isSupported(field)
        n <- values
      } {
        expectThrows[UnsupportedTemporalTypeException](temporal.`with`(field, n))
      }
    }

    it("should throw at `plus` for unsupported units") {
      for {
        temporal <- samples
        unit <- ChronoUnit.values() if !temporal.isSupported(unit)
        n <- Seq(Long.MinValue, Int.MinValue.toLong, -1L, 0L, 1L,
            Int.MaxValue.toLong, Long.MaxValue)
      } {
        expectThrows[UnsupportedTemporalTypeException](temporal.plus(n, unit))
      }
    }

    it("should respond to `minus`") {
      for {
        temporal <- samples
        unit <- ChronoUnit.values() if temporal.isSupported(unit)
        n <- Seq(
            Long.MinValue, Int.MinValue.toLong, -1000000000L, -86400L,
            -3600L, -366L, -365L, -60L, -24L, -7L, -1L, 0L,
            1L, 7L, 24L, 60L, 365L, 366L, 3600L, 86400L, 1000000000L,
            Int.MaxValue.toLong, Long.MaxValue)
      } {
        testTemporal(temporal.minus(n, unit)) {
          if (n != Long.MinValue) temporal.plus(-n, unit)
          else temporal.plus(Long.MaxValue, unit).plus(1, unit)
        }
      }
    }

    it("should throw at `minus` for unsupported units") {
      for {
        temporal <- samples
        unit <- ChronoUnit.values() if !temporal.isSupported(unit)
        n <- Seq(Long.MinValue, Int.MinValue.toLong, -1L, 0L, 1L,
            Int.MaxValue.toLong, Long.MaxValue)
      } {
        expectThrows[UnsupportedTemporalTypeException](temporal.minus(n, unit))
      }
    }

    it("should throw at `until` for unsupported units") {
      for {
        temporal1 <- samples
        temporal2 <- samples
        unit <- ChronoUnit.values() if !temporal1.isSupported(unit)
      } {
        expectThrows[UnsupportedTemporalTypeException](
            temporal1.until(temporal2, unit))
      }
    }
  }
}
