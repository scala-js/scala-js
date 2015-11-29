package org.scalajs.testsuite.javalib.time

import java.time.DateTimeException
import java.time.temporal.{ChronoUnit, Temporal, UnsupportedTemporalTypeException}

import org.junit.Assert._

object DateTimeTestUtil {
  import org.scalajs.testsuite.utils.AssertThrows._

  val dateBasedUnits = ChronoUnit.values.filter(_.isDateBased)

  val timeBasedUnits = ChronoUnit.values.filter(_.isTimeBased)

  def testDateTime(actual: => Any)(expected: => Any): Unit = {
    try {
      val e = expected
      //expectNoException(actual)
      assertEquals(e, actual)
    } catch {
      case _: UnsupportedTemporalTypeException =>
        expectThrows(classOf[UnsupportedTemporalTypeException], actual)

      case _: DateTimeException =>
        expectThrows(classOf[DateTimeException], actual)

      case _: ArithmeticException =>
        expectThrows(classOf[ArithmeticException], actual)
    }
  }
}
