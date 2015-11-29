package org.scalajs.testsuite.javalib.time.temporal

import java.time.temporal.ChronoField

import org.junit.Test
import org.junit.Assert._

class ChronoFieldTest {
  import ChronoField._

  @Test def test_isDateBased(): Unit = {
    assertFalse(NANO_OF_SECOND.isDateBased)
    assertFalse(NANO_OF_DAY.isDateBased)
    assertFalse(MICRO_OF_SECOND.isDateBased)
    assertFalse(MICRO_OF_DAY.isDateBased)
    assertFalse(MILLI_OF_SECOND.isDateBased)
    assertFalse(MILLI_OF_DAY.isDateBased)
    assertFalse(SECOND_OF_MINUTE.isDateBased)
    assertFalse(SECOND_OF_DAY.isDateBased)
    assertFalse(MINUTE_OF_HOUR.isDateBased)
    assertFalse(MINUTE_OF_DAY.isDateBased)
    assertFalse(HOUR_OF_AMPM.isDateBased)
    assertFalse(CLOCK_HOUR_OF_AMPM.isDateBased)
    assertFalse(HOUR_OF_DAY.isDateBased)
    assertFalse(CLOCK_HOUR_OF_DAY.isDateBased)
    assertFalse(AMPM_OF_DAY.isDateBased)
    assertTrue(DAY_OF_WEEK.isDateBased)
    assertTrue(ALIGNED_DAY_OF_WEEK_IN_MONTH.isDateBased)
    assertTrue(ALIGNED_DAY_OF_WEEK_IN_YEAR.isDateBased)
    assertTrue(DAY_OF_MONTH.isDateBased)
    assertTrue(DAY_OF_YEAR.isDateBased)
    assertTrue(EPOCH_DAY.isDateBased)
    assertTrue(ALIGNED_WEEK_OF_MONTH.isDateBased)
    assertTrue(ALIGNED_WEEK_OF_YEAR.isDateBased)
    assertTrue(MONTH_OF_YEAR.isDateBased)
    assertTrue(PROLEPTIC_MONTH.isDateBased)
    assertTrue(YEAR_OF_ERA.isDateBased)
    assertTrue(YEAR.isDateBased)
    assertTrue(ERA.isDateBased)
    assertFalse(INSTANT_SECONDS.isDateBased)
    assertFalse(OFFSET_SECONDS.isDateBased)
  }

  @Test def test_isTimeBased(): Unit = {
    assertTrue(NANO_OF_SECOND.isTimeBased)
    assertTrue(NANO_OF_DAY.isTimeBased)
    assertTrue(MICRO_OF_SECOND.isTimeBased)
    assertTrue(MICRO_OF_DAY.isTimeBased)
    assertTrue(MILLI_OF_SECOND.isTimeBased)
    assertTrue(MILLI_OF_DAY.isTimeBased)
    assertTrue(SECOND_OF_MINUTE.isTimeBased)
    assertTrue(SECOND_OF_DAY.isTimeBased)
    assertTrue(MINUTE_OF_HOUR.isTimeBased)
    assertTrue(MINUTE_OF_DAY.isTimeBased)
    assertTrue(HOUR_OF_AMPM.isTimeBased)
    assertTrue(CLOCK_HOUR_OF_AMPM.isTimeBased)
    assertTrue(HOUR_OF_DAY.isTimeBased)
    assertTrue(CLOCK_HOUR_OF_DAY.isTimeBased)
    assertTrue(AMPM_OF_DAY.isTimeBased)
    assertFalse(DAY_OF_WEEK.isTimeBased)
    assertFalse(ALIGNED_DAY_OF_WEEK_IN_MONTH.isTimeBased)
    assertFalse(ALIGNED_DAY_OF_WEEK_IN_YEAR.isTimeBased)
    assertFalse(DAY_OF_MONTH.isTimeBased)
    assertFalse(DAY_OF_YEAR.isTimeBased)
    assertFalse(EPOCH_DAY.isTimeBased)
    assertFalse(ALIGNED_WEEK_OF_MONTH.isTimeBased)
    assertFalse(ALIGNED_WEEK_OF_YEAR.isTimeBased)
    assertFalse(MONTH_OF_YEAR.isTimeBased)
    assertFalse(PROLEPTIC_MONTH.isTimeBased)
    assertFalse(YEAR_OF_ERA.isTimeBased)
    assertFalse(YEAR.isTimeBased)
    assertFalse(ERA.isTimeBased)
    assertFalse(INSTANT_SECONDS.isTimeBased)
    assertFalse(OFFSET_SECONDS.isTimeBased)
  }

  @Test def test_values(): Unit = {
    val fields = Array[AnyRef](NANO_OF_SECOND, NANO_OF_DAY, MICRO_OF_SECOND,
        MICRO_OF_DAY, MILLI_OF_SECOND, MILLI_OF_DAY, SECOND_OF_MINUTE,
        SECOND_OF_DAY, MINUTE_OF_HOUR, MINUTE_OF_DAY, HOUR_OF_AMPM,
        CLOCK_HOUR_OF_AMPM, HOUR_OF_DAY, CLOCK_HOUR_OF_DAY, AMPM_OF_DAY,
        DAY_OF_WEEK, ALIGNED_DAY_OF_WEEK_IN_MONTH, ALIGNED_DAY_OF_WEEK_IN_YEAR,
        DAY_OF_MONTH, DAY_OF_YEAR, EPOCH_DAY, ALIGNED_WEEK_OF_MONTH,
        ALIGNED_WEEK_OF_YEAR, MONTH_OF_YEAR, PROLEPTIC_MONTH, YEAR_OF_ERA, YEAR,
        ERA, INSTANT_SECONDS, OFFSET_SECONDS)
    assertArrayEquals(fields, values.asInstanceOf[Array[AnyRef]])
  }

  @Test def test_valueOf(): Unit = {
    assertEquals(NANO_OF_SECOND, valueOf("NANO_OF_SECOND"))
    assertEquals(NANO_OF_DAY, valueOf("NANO_OF_DAY"))
    assertEquals(MICRO_OF_SECOND, valueOf("MICRO_OF_SECOND"))
    assertEquals(MICRO_OF_DAY, valueOf("MICRO_OF_DAY"))
    assertEquals(MILLI_OF_SECOND, valueOf("MILLI_OF_SECOND"))
    assertEquals(MILLI_OF_DAY, valueOf("MILLI_OF_DAY"))
    assertEquals(SECOND_OF_MINUTE, valueOf("SECOND_OF_MINUTE"))
    assertEquals(SECOND_OF_DAY, valueOf("SECOND_OF_DAY"))
    assertEquals(MINUTE_OF_HOUR, valueOf("MINUTE_OF_HOUR"))
    assertEquals(MINUTE_OF_DAY, valueOf("MINUTE_OF_DAY"))
    assertEquals(HOUR_OF_AMPM, valueOf("HOUR_OF_AMPM"))
    assertEquals(CLOCK_HOUR_OF_AMPM, valueOf("CLOCK_HOUR_OF_AMPM"))
    assertEquals(HOUR_OF_DAY, valueOf("HOUR_OF_DAY"))
    assertEquals(CLOCK_HOUR_OF_DAY, valueOf("CLOCK_HOUR_OF_DAY"))
    assertEquals(AMPM_OF_DAY, valueOf("AMPM_OF_DAY"))
    assertEquals(DAY_OF_WEEK, valueOf("DAY_OF_WEEK"))
    assertEquals(ALIGNED_DAY_OF_WEEK_IN_MONTH,
        valueOf("ALIGNED_DAY_OF_WEEK_IN_MONTH"))
    assertEquals(ALIGNED_DAY_OF_WEEK_IN_YEAR,
        valueOf("ALIGNED_DAY_OF_WEEK_IN_YEAR"))
    assertEquals(DAY_OF_MONTH, valueOf("DAY_OF_MONTH"))
    assertEquals(DAY_OF_YEAR, valueOf("DAY_OF_YEAR"))
    assertEquals(EPOCH_DAY, valueOf("EPOCH_DAY"))
    assertEquals(ALIGNED_WEEK_OF_MONTH, valueOf("ALIGNED_WEEK_OF_MONTH"))
    assertEquals(ALIGNED_WEEK_OF_YEAR, valueOf("ALIGNED_WEEK_OF_YEAR"))
    assertEquals(MONTH_OF_YEAR, valueOf("MONTH_OF_YEAR"))
    assertEquals(PROLEPTIC_MONTH, valueOf("PROLEPTIC_MONTH"))
    assertEquals(YEAR_OF_ERA, valueOf("YEAR_OF_ERA"))
    assertEquals(YEAR, valueOf("YEAR"))
    assertEquals(ERA, valueOf("ERA"))
    assertEquals(INSTANT_SECONDS, valueOf("INSTANT_SECONDS"))
    assertEquals(OFFSET_SECONDS, valueOf("OFFSET_SECONDS"))
  }
}
