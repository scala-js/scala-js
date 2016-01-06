package org.scalajs.testsuite.javalib.time

import java.time._
import java.time.temporal.ChronoField

import org.junit.Test
import org.junit.Assert._
import org.scalajs.testsuite.utils.AssertThrows._

class MonthTest extends TemporalAccessorTest[Month] {
  import Month._

  val samples = values.toSeq

  def isSupported(field: ChronoField): Boolean =
    field == ChronoField.MONTH_OF_YEAR

  @Test def test_getValue(): Unit = {
    assertEquals(1, JANUARY.getValue)
    assertEquals(2, FEBRUARY.getValue)
    assertEquals(3, MARCH.getValue)
    assertEquals(4, APRIL.getValue)
    assertEquals(5, MAY.getValue)
    assertEquals(6, JUNE.getValue)
    assertEquals(7, JULY.getValue)
    assertEquals(8, AUGUST.getValue)
    assertEquals(9, SEPTEMBER.getValue)
    assertEquals(10, OCTOBER.getValue)
    assertEquals(11, NOVEMBER.getValue)
    assertEquals(12, DECEMBER.getValue)
  }

  @Test def test_getLong(): Unit = {
    for (m <- samples)
      assertEquals(m.getValue.toLong, m.getLong(ChronoField.MONTH_OF_YEAR))
  }

  @Test def test_plus(): Unit = {
    assertEquals(MAY, JANUARY.plus(Long.MinValue))
    assertEquals(FEBRUARY, FEBRUARY.plus(-12))
    assertEquals(FEBRUARY, MARCH.plus(-1))
    assertEquals(APRIL, APRIL.plus(0))
    assertEquals(JUNE, MAY.plus(1))
    assertEquals(JUNE, JUNE.plus(12))
    assertEquals(FEBRUARY, JULY.plus(Long.MaxValue))
  }

  @Test def test_minus(): Unit = {
    assertEquals(SEPTEMBER, JANUARY.minus(Long.MinValue))
    assertEquals(FEBRUARY, FEBRUARY.minus(-12))
    assertEquals(APRIL, MARCH.minus(-1))
    assertEquals(APRIL, APRIL.minus(0))
    assertEquals(APRIL, MAY.minus(1))
    assertEquals(JUNE, JUNE.minus(12))
    assertEquals(DECEMBER, JULY.minus(Long.MaxValue))
  }

  @Test def test_minLength(): Unit = {
    assertEquals(31, JANUARY.minLength)
    assertEquals(28, FEBRUARY.minLength)
    assertEquals(31, MARCH.minLength)
    assertEquals(30, APRIL.minLength)
    assertEquals(31, MAY.minLength)
    assertEquals(30, JUNE.minLength)
    assertEquals(31, JULY.minLength)
    assertEquals(31, AUGUST.minLength)
    assertEquals(30, SEPTEMBER.minLength)
    assertEquals(31, OCTOBER.minLength)
    assertEquals(30, NOVEMBER.minLength)
    assertEquals(31, DECEMBER.minLength)
  }

  @Test def test_maxLength(): Unit = {
    assertEquals(31, JANUARY.maxLength)
    assertEquals(29, FEBRUARY.maxLength)
    assertEquals(31, MARCH.maxLength)
    assertEquals(30, APRIL.maxLength)
    assertEquals(31, MAY.maxLength)
    assertEquals(30, JUNE.maxLength)
    assertEquals(31, JULY.maxLength)
    assertEquals(31, AUGUST.maxLength)
    assertEquals(30, SEPTEMBER.maxLength)
    assertEquals(31, OCTOBER.maxLength)
    assertEquals(30, NOVEMBER.maxLength)
    assertEquals(31, DECEMBER.maxLength)
  }

  @Test def test_length(): Unit = {
    for (m <- samples) {
      assertEquals(m.minLength, m.length(false))
      assertEquals(m.maxLength, m.length(true))
    }
  }

  @Test def test_firstDayOfYear(): Unit = {
    assertEquals(1, JANUARY.firstDayOfYear(false))
    assertEquals(1, JANUARY.firstDayOfYear(true))
    assertEquals(32, FEBRUARY.firstDayOfYear(false))
    assertEquals(32, FEBRUARY.firstDayOfYear(true))
    assertEquals(60, MARCH.firstDayOfYear(false))
    assertEquals(61, MARCH.firstDayOfYear(true))
    assertEquals(91, APRIL.firstDayOfYear(false))
    assertEquals(92, APRIL.firstDayOfYear(true))
    assertEquals(121, MAY.firstDayOfYear(false))
    assertEquals(122, MAY.firstDayOfYear(true))
    assertEquals(152, JUNE.firstDayOfYear(false))
    assertEquals(153, JUNE.firstDayOfYear(true))
    assertEquals(182, JULY.firstDayOfYear(false))
    assertEquals(183, JULY.firstDayOfYear(true))
    assertEquals(213, AUGUST.firstDayOfYear(false))
    assertEquals(214, AUGUST.firstDayOfYear(true))
    assertEquals(244, SEPTEMBER.firstDayOfYear(false))
    assertEquals(245, SEPTEMBER.firstDayOfYear(true))
    assertEquals(274, OCTOBER.firstDayOfYear(false))
    assertEquals(275, OCTOBER.firstDayOfYear(true))
    assertEquals(305, NOVEMBER.firstDayOfYear(false))
    assertEquals(306, NOVEMBER.firstDayOfYear(true))
    assertEquals(335, DECEMBER.firstDayOfYear(false))
    assertEquals(336, DECEMBER.firstDayOfYear(true))
  }

  @Test def test_firstMonthOfQuarter(): Unit = {
    assertEquals(JANUARY, JANUARY.firstMonthOfQuarter)
    assertEquals(JANUARY, FEBRUARY.firstMonthOfQuarter)
    assertEquals(JANUARY, MARCH.firstMonthOfQuarter)
    assertEquals(APRIL, APRIL.firstMonthOfQuarter)
    assertEquals(APRIL, MAY.firstMonthOfQuarter)
    assertEquals(APRIL, JUNE.firstMonthOfQuarter)
    assertEquals(JULY, JULY.firstMonthOfQuarter)
    assertEquals(JULY, AUGUST.firstMonthOfQuarter)
    assertEquals(JULY, SEPTEMBER.firstMonthOfQuarter)
    assertEquals(OCTOBER, OCTOBER.firstMonthOfQuarter)
    assertEquals(OCTOBER, NOVEMBER.firstMonthOfQuarter)
    assertEquals(OCTOBER, DECEMBER.firstMonthOfQuarter)
  }

  @Test def test_compareTo(): Unit = {
    assertEquals(0, JULY.compareTo(JULY))
    assertTrue(JANUARY.compareTo(MARCH) < 0)
    assertTrue(DECEMBER.compareTo(OCTOBER) > 0)
  }

  @Test def test_values(): Unit = {
    val months: Array[AnyRef] = Array(JANUARY, FEBRUARY, MARCH, APRIL, MAY,
        JUNE, JULY, AUGUST, SEPTEMBER, OCTOBER, NOVEMBER, DECEMBER)
    assertArrayEquals(months, values.asInstanceOf[Array[AnyRef]])
  }

  @Test def test_valueOf(): Unit = {
    assertEquals(JANUARY, valueOf("JANUARY"))
    assertEquals(FEBRUARY, valueOf("FEBRUARY"))
    assertEquals(MARCH, valueOf("MARCH"))
    assertEquals(APRIL, valueOf("APRIL"))
    assertEquals(MAY, valueOf("MAY"))
    assertEquals(JUNE, valueOf("JUNE"))
    assertEquals(JULY, valueOf("JULY"))
    assertEquals(AUGUST, valueOf("AUGUST"))
    assertEquals(SEPTEMBER, valueOf("SEPTEMBER"))
    assertEquals(OCTOBER, valueOf("OCTOBER"))
    assertEquals(NOVEMBER, valueOf("NOVEMBER"))
    assertEquals(DECEMBER, valueOf("DECEMBER"))

    expectThrows(classOf[IllegalArgumentException], valueOf(""))
  }

  @Test def test_of(): Unit = {
    assertEquals(JANUARY, of(1))
    assertEquals(FEBRUARY, of(2))
    assertEquals(MARCH, of(3))
    assertEquals(APRIL, of(4))
    assertEquals(MAY, of(5))
    assertEquals(JUNE, of(6))
    assertEquals(JULY, of(7))
    assertEquals(AUGUST, of(8))
    assertEquals(SEPTEMBER, of(9))
    assertEquals(OCTOBER, of(10))
    assertEquals(NOVEMBER, of(11))
    assertEquals(DECEMBER, of(12))

    for (n <- Seq(Int.MinValue, 0, 13, Int.MaxValue))
      expectThrows(classOf[DateTimeException], of(n))
  }

  @Test def test_from(): Unit = {
    for (m <- samples) {
      assertEquals(m, from(m))
      assertEquals(m, from(LocalDate.of(1, m, 1)))
    }

    expectThrows(classOf[DateTimeException], from(DayOfWeek.MONDAY))
    expectThrows(classOf[DateTimeException], from(LocalTime.MIN))
  }
}
