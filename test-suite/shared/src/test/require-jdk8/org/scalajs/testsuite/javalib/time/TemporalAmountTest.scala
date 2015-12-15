package org.scalajs.testsuite.javalib.time

import java.time.temporal.{UnsupportedTemporalTypeException, ChronoUnit, TemporalAmount}

import scala.collection.JavaConverters._

import org.junit.Test
import org.junit.Assert._
import org.scalajs.testsuite.utils.AssertThrows._

abstract class TemporalAmountTest {
  val samples: Seq[TemporalAmount]

  val units: Seq[ChronoUnit]

  @Test def test_get_unsupported_unit(): Unit = {
    val illegalUnits = ChronoUnit.values.filterNot(units.contains)
    for {
      amount <- samples
      unit <- illegalUnits
    } {
      expectThrows(classOf[UnsupportedTemporalTypeException], amount.get(unit))
    }
  }

  @Test def test_getUnits(): Unit = {
    for (amount <- samples)
      assertEquals(units.toIterable, amount.getUnits.asScala)
  }
}
