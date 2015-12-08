package org.scalajs.testsuite.javalib.time.chrono

import java.time.DateTimeException
import java.time.chrono.{IsoChronology, Chronology}

import org.junit.Test
import org.junit.Assert._
import org.scalajs.testsuite.utils.AssertThrows._

class ChronologyTest {
  import Chronology._

  @Test def test_of(): Unit = {
    assertEquals(IsoChronology.INSTANCE, of("ISO"))
    expectThrows(classOf[DateTimeException], of(""))
  }

  @Test def test_getAvailableChronologies(): Unit = {
    val chronologies = Chronology.getAvailableChronologies
    assertTrue(chronologies.contains(IsoChronology.INSTANCE))
  }
}
