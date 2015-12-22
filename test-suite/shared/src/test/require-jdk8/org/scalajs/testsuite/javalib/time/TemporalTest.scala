package org.scalajs.testsuite.javalib.time

import java.time.temporal._

import org.junit.Test
import org.junit.Assert._
import org.scalajs.testsuite.utils.AssertThrows._

abstract class TemporalTest[Temp <: Temporal] extends TemporalAccessorTest[Temp] {
  import DateTimeTestUtil._

  def isSupported(unit: ChronoUnit): Boolean

  val sampleLongs = Seq(
      Long.MinValue, Int.MinValue.toLong, -1000000000L, -86400L,
      -3600L, -366L, -365L, -60L, -24L, -7L, -1L, 0L,
      1L, 7L, 24L, 60L, 365L, 366L, 3600L, 86400L, 1000000000L,
      Int.MaxValue.toLong, Long.MaxValue)

  @Test def isSupported_TemporalUnit(): Unit = {
    for {
      temporal <- samples
      unit <- ChronoUnit.values
    } {
      if (isSupported(unit))
        assertTrue(temporal.isSupported(unit))
      else
        assertFalse(temporal.isSupported(unit))
    }
    for (temporal <- samples)
      assertFalse(temporal.isSupported(null: TemporalUnit))
  }

  @Test def with_unsupported_field(): Unit = {
    for {
      temporal <- samples
      field <- ChronoField.values if !temporal.isSupported(field)
      n <- sampleLongs.filter(field.range.isValidValue)
    } {
      expectThrows(classOf[UnsupportedTemporalTypeException],
          temporal.`with`(field, n))
    }
  }

  @Test def plus_unsupported_unit(): Unit = {
    for {
      temporal <- samples
      unit <- ChronoUnit.values if !temporal.isSupported(unit)
      n <- sampleLongs
    } {
      expectThrows(classOf[UnsupportedTemporalTypeException],
          temporal.plus(n, unit))
    }
  }

  @Test def minus(): Unit = {
    for {
      temporal <- samples
      unit <- ChronoUnit.values if temporal.isSupported(unit)
      n <- sampleLongs
    } {
      testDateTime(temporal.minus(n, unit)) {
        if (n != Long.MinValue) temporal.plus(-n, unit)
        else temporal.plus(Long.MaxValue, unit).plus(1, unit)
      }
    }
  }

  @Test def minus_unsupported_unit(): Unit = {
    for {
      temporal <- samples
      unit <- ChronoUnit.values if !temporal.isSupported(unit)
      n <- sampleLongs
    } {
      expectThrows(classOf[UnsupportedTemporalTypeException],
          temporal.minus(n, unit))
    }
  }

  @Test def until_unsupported_unit(): Unit = {
    for {
      temporal1 <- samples
      temporal2 <- samples
      unit <- ChronoUnit.values if !temporal1.isSupported(unit)
    } {
      expectThrows(classOf[UnsupportedTemporalTypeException],
          temporal1.until(temporal2, unit))
    }
  }
}
