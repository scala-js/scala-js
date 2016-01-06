package org.scalajs.testsuite.javalib.time

import java.time._
import java.time.temporal.ChronoField

import org.junit.Test
import org.junit.Assert._
import org.scalajs.testsuite.utils.AssertThrows._

class DayOfWeekTest extends TemporalAccessorTest[DayOfWeek] {
  import DayOfWeek._

  val samples = values.toSeq

  def isSupported(field: ChronoField): Boolean =
    field == ChronoField.DAY_OF_WEEK

  @Test def test_getValue(): Unit = {
    assertEquals(1, MONDAY.getValue)
    assertEquals(2, TUESDAY.getValue)
    assertEquals(3, WEDNESDAY.getValue)
    assertEquals(4, THURSDAY.getValue)
    assertEquals(5, FRIDAY.getValue)
    assertEquals(6, SATURDAY.getValue)
    assertEquals(7, SUNDAY.getValue)
  }

  @Test def test_getLong(): Unit = {
    for (d <- samples)
      assertEquals(d.getValue.toLong, d.getLong(ChronoField.DAY_OF_WEEK))
  }

  @Test def test_plus(): Unit = {
    assertEquals(FRIDAY, SATURDAY.plus(Long.MinValue))
    assertEquals(THURSDAY, THURSDAY.plus(-7))
    assertEquals(SUNDAY, WEDNESDAY.plus(-3))
    assertEquals(MONDAY, TUESDAY.plus(-1))
    assertEquals(MONDAY, MONDAY.plus(0))
    assertEquals(FRIDAY, THURSDAY.plus(1))
    assertEquals(MONDAY, FRIDAY.plus(3))
    assertEquals(SATURDAY, SATURDAY.plus(7))
    assertEquals(SUNDAY, SUNDAY.plus(Long.MaxValue))
  }

  @Test def test_minus(): Unit = {
    assertEquals(SUNDAY, SATURDAY.minus(Long.MinValue))
    assertEquals(THURSDAY, THURSDAY.minus(-7))
    assertEquals(MONDAY, FRIDAY.minus(-3))
    assertEquals(WEDNESDAY, TUESDAY.minus(-1))
    assertEquals(MONDAY, MONDAY.minus(0))
    assertEquals(WEDNESDAY, THURSDAY.minus(1))
    assertEquals(SUNDAY, WEDNESDAY.minus(3))
    assertEquals(SATURDAY, SATURDAY.minus(7))
    assertEquals(SUNDAY, SUNDAY.plus(Long.MaxValue))
  }

  @Test def test_compareTo(): Unit = {
    assertEquals(0, WEDNESDAY.compareTo(WEDNESDAY))
    assertTrue(MONDAY.compareTo(SUNDAY) < 0)
    assertTrue(SATURDAY.compareTo(TUESDAY) > 0)
  }

  @Test def test_values(): Unit = {
    val days: Array[AnyRef] =
      Array(MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY, SUNDAY)
    assertArrayEquals(days, values.asInstanceOf[Array[AnyRef]])
  }

  @Test def test_valueOf(): Unit = {
    assertEquals(MONDAY, valueOf("MONDAY"))
    assertEquals(TUESDAY, valueOf("TUESDAY"))
    assertEquals(WEDNESDAY, valueOf("WEDNESDAY"))
    assertEquals(THURSDAY, valueOf("THURSDAY"))
    assertEquals(FRIDAY, valueOf("FRIDAY"))
    assertEquals(SATURDAY, valueOf("SATURDAY"))
    assertEquals(SUNDAY, valueOf("SUNDAY"))

    expectThrows(classOf[IllegalArgumentException], valueOf(""))
  }

  @Test def test_of(): Unit = {
    assertEquals(MONDAY, of(1))
    assertEquals(TUESDAY, of(2))
    assertEquals(WEDNESDAY, of(3))
    assertEquals(THURSDAY, of(4))
    assertEquals(FRIDAY, of(5))
    assertEquals(SATURDAY, of(6))
    assertEquals(SUNDAY, of(7))

    for (n <- Seq(Int.MinValue, 0, 8, Int.MaxValue))
      expectThrows(classOf[DateTimeException], of(n))
  }

  @Test def test_from(): Unit = {
    for (d <- samples)
      assertEquals(d, from(d))
    for (d <- Seq(LocalDate.MIN, LocalDate.of(2012, 2, 29), LocalDate.MAX))
      assertEquals(d.getDayOfWeek, from(d))

    expectThrows(classOf[DateTimeException], from(LocalTime.MIN))
    expectThrows(classOf[DateTimeException], from(Month.JANUARY))
  }
}
