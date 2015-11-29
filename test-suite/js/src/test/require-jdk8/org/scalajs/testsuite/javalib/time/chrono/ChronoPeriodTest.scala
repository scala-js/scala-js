package org.scalajs.testsuite.javalib.time.chrono

import java.time.LocalDate
import java.time.chrono.ChronoPeriod

import org.junit.Test
import org.junit.Assert.assertEquals

class ChronoPeriodTest {
  @Test def test_between(): Unit = {
    val ds = Seq(LocalDate.MIN, LocalDate.of(2011, 2, 28), LocalDate.MAX)
    for {
      d1 <- ds
      d2 <- ds
    } {
      assertEquals(d1.until(d2), ChronoPeriod.between(d1, d2))
    }
  }
}
