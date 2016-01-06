package org.scalajs.testsuite.javalib.time.chrono

import java.time.DateTimeException
import java.time.chrono.IsoEra
import java.time.temporal.ChronoField

import org.junit.Test
import org.junit.Assert._
import org.scalajs.testsuite.javalib.time.TemporalAccessorTest
import org.scalajs.testsuite.utils.AssertThrows._

class IsoEraTest extends TemporalAccessorTest[IsoEra] {
  import IsoEra._

  val samples = values.toSeq

  def isSupported(field: ChronoField): Boolean =
    field == ChronoField.ERA

  @Test def test_getValue(): Unit = {
    assertEquals(0, BCE.getValue)
    assertEquals(1, CE.getValue)
  }

  @Test def test_getLong(): Unit = {
    for (era <- samples)
      assertEquals(era.getValue.toLong, era.getLong(ChronoField.ERA))
  }

  @Test def test_compareTo(): Unit = {
    assertEquals(0, BCE.compareTo(BCE))
    assertTrue(BCE.compareTo(CE) < 0)
    assertTrue(CE.compareTo(BCE) > 0)
    assertEquals(0, CE.compareTo(CE))
  }

  @Test def test_values(): Unit = {
    val eras = Array[AnyRef](BCE, CE)
    assertArrayEquals(eras, values.asInstanceOf[Array[AnyRef]])
  }

  @Test def test_valueOf(): Unit = {
    assertEquals(BCE, valueOf("BCE"))
    assertEquals(CE, valueOf("CE"))
    expectThrows(classOf[IllegalArgumentException], valueOf(""))
  }

  @Test def test_of(): Unit = {
    assertEquals(BCE, of(0))
    assertEquals(CE, of(1))

    for (n <- Seq(Int.MinValue, -1, 2, Int.MaxValue))
      expectThrows(classOf[DateTimeException], of(n))
  }
}
