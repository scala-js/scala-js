package org.scalajs.testsuite.javalib.time.chrono

import java.time.{DateTimeException, LocalTime, LocalDate}
import java.time.chrono.ChronoLocalDate

import org.junit.Test
import org.junit.Assert._
import org.scalajs.testsuite.utils.AssertThrows._

class ChronoLocalDateTest {
  import ChronoLocalDate._

  @Test def test_timeLineOrder(): Unit = {
    val ord = timeLineOrder
    val ds = Seq(LocalDate.MIN, LocalDate.of(2011, 2, 28), LocalDate.MAX)

    for {
      d1 <- ds
      d2 <- ds
    } {
      assertEquals(math.signum(d1.compareTo(d2)),
          math.signum(ord.compare(d1, d2)))
    }
  }

  @Test def test_from(): Unit = {
    for (d <- Seq(LocalDate.MIN, LocalDate.of(2011, 2, 28), LocalDate.MAX))
      assertEquals(d, from(d))

    for (t <- Seq(LocalTime.MIN, LocalTime.NOON, LocalTime.MAX))
      expectThrows(classOf[DateTimeException], from(t))
  }
}
