package org.scalajs.testsuite.javalib.time.temporal

import java.time.temporal.ChronoUnit

import org.junit.Test
import org.junit.Assert._

class ChronoUnitTest {
  import ChronoUnit._

  @Test def test_isDurationEstimated(): Unit = {
    for (u <- ChronoUnit.values)
      assertTrue(u.isDurationEstimated != u.isTimeBased)
  }

  @Test def test_isDateBased(): Unit = {
    assertFalse(NANOS.isDateBased)
    assertFalse(MICROS.isDateBased)
    assertFalse(MILLIS.isDateBased)
    assertFalse(SECONDS.isDateBased)
    assertFalse(MINUTES.isDateBased)
    assertFalse(HOURS.isDateBased)
    assertFalse(HALF_DAYS.isDateBased)
    assertTrue(DAYS.isDateBased)
    assertTrue(WEEKS.isDateBased)
    assertTrue(MONTHS.isDateBased)
    assertTrue(YEARS.isDateBased)
    assertTrue(DECADES.isDateBased)
    assertTrue(CENTURIES.isDateBased)
    assertTrue(MILLENNIA.isDateBased)
    assertTrue(ERAS.isDateBased)
    assertFalse(FOREVER.isDateBased)
  }

  @Test def test_isTimeBased(): Unit = {
    assertTrue(NANOS.isTimeBased)
    assertTrue(MICROS.isTimeBased)
    assertTrue(MILLIS.isTimeBased)
    assertTrue(SECONDS.isTimeBased)
    assertTrue(MINUTES.isTimeBased)
    assertTrue(HOURS.isTimeBased)
    assertTrue(HALF_DAYS.isTimeBased)
    assertFalse(DAYS.isTimeBased)
    assertFalse(WEEKS.isTimeBased)
    assertFalse(MONTHS.isTimeBased)
    assertFalse(YEARS.isTimeBased)
    assertFalse(DECADES.isTimeBased)
    assertFalse(CENTURIES.isTimeBased)
    assertFalse(MILLENNIA.isTimeBased)
    assertFalse(ERAS.isTimeBased)
    assertFalse(FOREVER.isTimeBased)
  }

  @Test def test_values(): Unit = {
    val units = Array[AnyRef](NANOS, MICROS, MILLIS, SECONDS, MINUTES, HOURS,
        HALF_DAYS, DAYS, WEEKS, MONTHS, YEARS, DECADES, CENTURIES, MILLENNIA,
        ERAS, FOREVER)
    assertArrayEquals(units, values.asInstanceOf[Array[AnyRef]])
  }

  @Test def test_valueOf(): Unit = {
    assertEquals(NANOS, valueOf("NANOS"))
    assertEquals(MICROS, valueOf("MICROS"))
    assertEquals(MILLIS, valueOf("MILLIS"))
    assertEquals(SECONDS, valueOf("SECONDS"))
    assertEquals(MINUTES, valueOf("MINUTES"))
    assertEquals(HOURS, valueOf("HOURS"))
    assertEquals(HALF_DAYS, valueOf("HALF_DAYS"))
    assertEquals(DAYS, valueOf("DAYS"))
    assertEquals(WEEKS, valueOf("WEEKS"))
    assertEquals(MONTHS, valueOf("MONTHS"))
    assertEquals(YEARS, valueOf("YEARS"))
    assertEquals(DECADES, valueOf("DECADES"))
    assertEquals(CENTURIES, valueOf("CENTURIES"))
    assertEquals(MILLENNIA, valueOf("MILLENNIA"))
    assertEquals(ERAS, valueOf("ERAS"))
    assertEquals(FOREVER, valueOf("FOREVER"))
  }
}
