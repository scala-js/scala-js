package org.scalajs.testsuite.javalib.time

import java.time.DateTimeException
import java.time.temporal._

import org.junit.Test
import org.junit.Assert._
import org.scalajs.testsuite.utils.AssertThrows._

abstract class TemporalAccessorTest {
  val samples: Seq[TemporalAccessor]

  def isSupported(field: ChronoField): Boolean

  @Test def test_isSupported_TemporalField(): Unit = {
    for {
      accessor <- samples
      field <- ChronoField.values
    } {
      if (isSupported(field))
        assertTrue(accessor.isSupported(field))
      else
        assertFalse(accessor.isSupported(field))
    }
    for (accessor <- samples)
      assertFalse(accessor.isSupported(null))
  }

  @Test def test_range(): Unit = {
    for {
      accessor <- samples
      field <- ChronoField.values
    } {
      if (accessor.isSupported(field))
        assertEquals(field.range, accessor.range(field))
      else
        expectThrows(classOf[UnsupportedTemporalTypeException], accessor.range(field))
    }
  }

  @Test def test_get(): Unit = {
    for {
      accessor <- samples
      field <- ChronoField.values
    } {
      if (accessor.isSupported(field) && field.range.isIntValue)
        assertEquals(accessor.getLong(field), accessor.get(field).toLong)
      else if (accessor.isSupported(field))
        expectThrows(classOf[DateTimeException], accessor.get(field))
      else
        expectThrows(classOf[UnsupportedTemporalTypeException], accessor.get(field))
    }
  }

  @Test def test_getLong_unsupported_field(): Unit = {
    for {
      accessor <- samples
      field <- ChronoField.values() if !accessor.isSupported(field)
    } {
      expectThrows(classOf[UnsupportedTemporalTypeException],
          accessor.getLong(field))
    }
  }
}
