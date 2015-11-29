package org.scalajs.testsuite.javalib.time

import java.time._
import java.time.temporal.{ChronoUnit, UnsupportedTemporalTypeException}

import org.junit.Test
import org.junit.Assert._
import org.scalajs.testsuite.utils.AssertThrows._

class DurationTest extends TemporalAmountTest {

  import DateTimeTestUtil._
  import Duration._
  import ChronoUnit._

  final val dmin = Duration.ofSeconds(Long.MinValue)
  final val dmax = Duration.ofSeconds(Long.MaxValue, 999999999)
  final val oneSecond = Duration.ofSeconds(1)
  final val oneNano = Duration.ofNanos(1)

  val samples =
    Seq(dmin, dmax, ZERO, oneSecond, oneSecond.negated, oneNano.negated)

  val units = Seq(SECONDS, NANOS)

  val illegalUnits =
    ChronoUnit.values.filterNot(_.isTimeBased).filterNot(_ == DAYS)

  @Test def test_get(): Unit = {
    for (d <- samples) {
      assertEquals(d.getSeconds, d.get(SECONDS))
      assertEquals(d.getNano.toLong, d.get(NANOS))
    }
  }

  @Test def test_isZero(): Unit = {
    for (d <- samples if d != ZERO)
      assertFalse(d.isZero)
    assertTrue(ZERO.isZero)
  }

  @Test def test_isNegative(): Unit = {
    assertTrue(dmin.isNegative)
    assertTrue(oneSecond.negated.isNegative)
    assertTrue(oneNano.negated.isNegative)
    assertFalse(ZERO.isNegative)
    assertFalse(oneNano.isNegative)
    assertFalse(oneSecond.isNegative)
    assertFalse(dmax.isNegative)
  }

  @Test def test_getSeconds(): Unit = {
    assertEquals(Long.MinValue, dmin.getSeconds)
    assertEquals(-1L, oneNano.negated.getSeconds)
    assertEquals(0L, ZERO.getSeconds)
    assertEquals(0L, ofSeconds(1, -1).getSeconds)
    assertEquals(1L, oneSecond.getSeconds)
    assertEquals(Long.MaxValue, dmax.getSeconds)
  }

  @Test def test_getNano(): Unit = {
    assertEquals(0, dmin.getNano)
    assertEquals(999999999, oneNano.negated.getNano)
    assertEquals(0, ZERO.getNano)
    assertEquals(1, oneNano.getNano)
    assertEquals(999999999, dmax.getNano)
  }

  @Test def test_withSeconds(): Unit = {
    assertEquals(ZERO, dmin.withSeconds(0))
    assertEquals(ofSeconds(2, -1), dmax.withSeconds(1))
    assertEquals(dmin, ZERO.withSeconds(Long.MinValue))
  }

  @Test def test_withNanos(): Unit = {
    val d0 = ofSeconds(1, 1)

    assertEquals(ofSeconds(Long.MinValue + 1, -1), dmin.withNanos(999999999))
    assertEquals(oneSecond, d0.withNanos(0))
    assertEquals(ofSeconds(2, -1), d0.withNanos(999999999))
    assertEquals(ofSeconds(Long.MaxValue), dmax.withNanos(0))

    val args = Seq(Int.MinValue, -1, 1000000000, Int.MaxValue)
    for {
      d <- samples
      n <- args
    } {
      expectThrows(classOf[DateTimeException], d.withNanos(n))
    }
  }

  @Test def test_plus(): Unit = {
    val d1 = ofSeconds(1, 1)
    val d2 = ofSeconds(1, 999999999)
    val d3 = ofSeconds(-2, 1)

    for (d <- samples) {
      assertEquals(d, d.plus(ZERO))
      assertEquals(d, ZERO.plus(d))
    }
    assertEquals(ofSeconds(2, 2), d1.plus(d1))
    assertEquals(ofSeconds(3), d1.plus(d2))
    assertEquals(ofSeconds(-1, 2), d1.plus(d3))
    expectThrows(classOf[ArithmeticException], dmax.plus(oneNano))
    expectThrows(classOf[ArithmeticException], dmin.plus(oneNano.negated))

    val args = Seq(Long.MinValue, -100000000000000L, 1L, 0L, 1L,
        100000000000000L, Long.MaxValue)
    for {
      d <- samples
      n <- args
    } {
      testDateTime(d.plus(n, NANOS))(d.plusNanos(n))
      testDateTime(d.plus(n, MICROS))(d.plus(ofNanos(1000).multipliedBy(n)))
      testDateTime(d.plus(n, MILLIS))(d.plusMillis(n))
      testDateTime(d.plus(n, SECONDS))(d.plusSeconds(n))
      testDateTime(d.plus(n, MINUTES))(d.plusMinutes(n))
      testDateTime(d.plus(n, HOURS))(d.plusHours(n))
      testDateTime(d.plus(n, HALF_DAYS))(d.plus(ofHours(12).multipliedBy(n)))
      testDateTime(d.plus(n, DAYS))(d.plusDays(n))
    }
    for {
      d <- samples
      n <- args
      u <- illegalUnits
    } {
      expectThrows(classOf[UnsupportedTemporalTypeException], d.plus(n, u))
    }
  }

  @Test def test_plusDays(): Unit = {
    val d1 = ofSeconds(1, 1)
    val d2 = ofSeconds(Long.MaxValue - 86400, 999999999)
    val d3 = ofSeconds(Long.MinValue + 86400)
    val d4 = ofSeconds(Long.MaxValue - 86399)
    val d5 = ofSeconds(Long.MinValue + 86400, -1)

    assertEquals(ofSeconds(-431999, 1), d1.plusDays(-5))
    assertEquals(ofSeconds(-86399, 1), d1.plusDays(-1))
    assertEquals(d1, d1.plusDays(0))
    assertEquals(ofSeconds(86401, 1), d1.plusDays(1))
    assertEquals(ofSeconds(432001, 1), d1.plusDays(5))
    assertEquals(dmax, d2.plusDays(1))
    assertEquals(dmin, d3.plusDays(-1))
    assertEquals(dmax, dmax.plusDays(0))
    assertEquals(d2, dmax.plusDays(-1))
    assertEquals(dmin, dmin.plusDays(0))
    assertEquals(d3, dmin.plusDays(1))

    expectThrows(classOf[ArithmeticException], d4.plusDays(1))
    expectThrows(classOf[ArithmeticException], d2.plusDays(2))
    expectThrows(classOf[ArithmeticException], d5.plusDays(-1))
    expectThrows(classOf[ArithmeticException], d3.plusDays(-2))
  }

  @Test def test_plusHours(): Unit = {
    val d1 = ofSeconds(1, 1)
    val d2 = ofSeconds(Long.MaxValue - 3600, 999999999)
    val d3 = ofSeconds(Long.MinValue + 3600)
    val d4 = ofSeconds(Long.MaxValue - 3599)
    val d5 = ofSeconds(Long.MinValue + 3600, -1)

    assertEquals(ofSeconds(-17999, 1), d1.plusHours(-5))
    assertEquals(ofSeconds(-3599, 1), d1.plusHours(-1))
    assertEquals(d1, d1.plusHours(0))
    assertEquals(ofSeconds(3601, 1), d1.plusHours(1))
    assertEquals(ofSeconds(18001, 1), d1.plusHours(5))
    assertEquals(dmax, d2.plusHours(1))
    assertEquals(dmin, d3.plusHours(-1))
    assertEquals(dmax, dmax.plusHours(0))
    assertEquals(d2, dmax.plusHours(-1))
    assertEquals(dmin, dmin.plusHours(0))
    assertEquals(d3, dmin.plusHours(1))

    expectThrows(classOf[ArithmeticException], d4.plusHours(1))
    expectThrows(classOf[ArithmeticException], d2.plusHours(2))
    expectThrows(classOf[ArithmeticException], d5.plusHours(-1))
    expectThrows(classOf[ArithmeticException], d3.plusHours(-2))
  }

  @Test def test_plusMinutes(): Unit = {
    val d1 = ofSeconds(1, 1)
    val d2 = ofSeconds(Long.MaxValue - 60, 999999999)
    val d3 = ofSeconds(Long.MinValue + 60)
    val d4 = ofSeconds(Long.MaxValue - 59)
    val d5 = ofSeconds(Long.MinValue + 60, -1)

    assertEquals(ofSeconds(-299, 1), d1.plusMinutes(-5))
    assertEquals(ofSeconds(-59, 1), d1.plusMinutes(-1))
    assertEquals(d1, d1.plusMinutes(0))
    assertEquals(ofSeconds(61, 1), d1.plusMinutes(1))
    assertEquals(ofSeconds(301, 1), d1.plusMinutes(5))
    assertEquals(dmax, d2.plusMinutes(1))
    assertEquals(dmin, d3.plusMinutes(-1))
    assertEquals(dmax, dmax.plusMinutes(0))
    assertEquals(d2, dmax.plusMinutes(-1))
    assertEquals(dmin, dmin.plusMinutes(0))
    assertEquals(d3, dmin.plusMinutes(1))

    expectThrows(classOf[ArithmeticException], d4.plusMinutes(1))
    expectThrows(classOf[ArithmeticException], d2.plusMinutes(2))
    expectThrows(classOf[ArithmeticException], d5.plusMinutes(-1))
    expectThrows(classOf[ArithmeticException], d3.plusMinutes(-2))
  }

  @Test def test_plusSeconds(): Unit = {
    val d1 = ofSeconds(1, 1)
    val d2 = ofSeconds(Long.MaxValue - 1, 999999999)
    val d3 = ofSeconds(Long.MinValue + 1)
    val d4 = ofSeconds(Long.MaxValue)
    val d5 = ofSeconds(Long.MinValue + 1, -1)

    assertEquals(ofSeconds(-4, 1), d1.plusSeconds(-5))
    assertEquals(ofSeconds(0, 1), d1.plusSeconds(-1))
    assertEquals(d1, d1.plusSeconds(0))
    assertEquals(ofSeconds(2, 1), d1.plusSeconds(1))
    assertEquals(ofSeconds(6, 1), d1.plusSeconds(5))
    assertEquals(dmax, d2.plusSeconds(1))
    assertEquals(dmin, d3.plusSeconds(-1))
    assertEquals(dmax, dmax.plusSeconds(0))
    assertEquals(d2, dmax.plusSeconds(-1))
    assertEquals(dmin, dmin.plusSeconds(0))
    assertEquals(d3, dmin.plusSeconds(1))

    expectThrows(classOf[ArithmeticException], d4.plusSeconds(1))
    expectThrows(classOf[ArithmeticException], d2.plusSeconds(2))
    expectThrows(classOf[ArithmeticException], d5.plusSeconds(-1))
    expectThrows(classOf[ArithmeticException], d3.plusSeconds(-2))
  }

  @Test def test_plusMillis(): Unit = {
    val d1 = ofSeconds(1, 1)
    val d2 = ofSeconds(Long.MaxValue, 998999999)
    val d3 = ofSeconds(Long.MinValue, 1000000)
    val d4 = ofSeconds(Long.MaxValue, 999000000)
    val d5 = ofSeconds(Long.MinValue, 999999)

    assertEquals(ofSeconds(-4, 1), d1.plusMillis(-5000))
    assertEquals(ofSeconds(0, 900000001), d1.plusMillis(-100))
    assertEquals(d1, d1.plusMillis(0))
    assertEquals(ofSeconds(1, 100000001), d1.plusMillis(100))
    assertEquals(ofSeconds(6, 1), d1.plusMillis(5000))
    assertEquals(dmax, d2.plusMillis(1))
    assertEquals(dmin, d3.plusMillis(-1))
    assertEquals(dmax, dmax.plusMillis(0))
    assertEquals(d2, dmax.plusMillis(-1))
    assertEquals(dmin, dmin.plusMillis(0))
    assertEquals(d3, dmin.plusMillis(1))

    expectThrows(classOf[ArithmeticException], d4.plusMillis(1))
    expectThrows(classOf[ArithmeticException], d2.plusMillis(2))
    expectThrows(classOf[ArithmeticException], d5.plusMillis(-1))
    expectThrows(classOf[ArithmeticException], d3.plusMillis(-2))
  }

  @Test def test_plusNanos(): Unit = {
    val d1 = ofSeconds(1, 1)
    val d2 = ofSeconds(Long.MaxValue, 999999998)
    val d3 = ofSeconds(Long.MinValue, 1)

    assertEquals(ofSeconds(-4, 1), d1.plusNanos(-5000000000L))
    assertEquals(ofSeconds(0, 999999001), d1.plusNanos(-1000))
    assertEquals(d1, d1.plusNanos(0))
    assertEquals(ofSeconds(1, 1001), d1.plusNanos(1000))
    assertEquals(ofSeconds(6, 1), d1.plusNanos(5000000000L))
    assertEquals(dmax, d2.plusNanos(1))
    assertEquals(dmin, d3.plusNanos(-1))
    assertEquals(dmax, dmax.plusNanos(0))
    assertEquals(d2, dmax.plusNanos(-1))
    assertEquals(dmin, dmin.plusNanos(0))
    assertEquals(d3, dmin.plusNanos(1))

    expectThrows(classOf[ArithmeticException], dmax.plusNanos(1))
    expectThrows(classOf[ArithmeticException], d2.plusNanos(2))
    expectThrows(classOf[ArithmeticException], dmin.plusNanos(-1))
    expectThrows(classOf[ArithmeticException], d3.plusNanos(-2))
  }

  @Test def test_minus(): Unit = {
    val d1 = ofSeconds(1, 1)
    val d2 = ofSeconds(1, 999999999)
    val d3 = ofSeconds(-2, 1)

    assertEquals(ZERO, d1.minus(d1))
    assertEquals(d1, d1.minus(ZERO))
    assertEquals(d1.negated, ZERO.minus(d1))
    assertEquals(ofSeconds(-1, 2), d1.minus(d2))
    assertEquals(ofSeconds(3), d1.minus(d3))
    expectThrows(classOf[ArithmeticException], dmax.minus(oneNano.negated))
    expectThrows(classOf[ArithmeticException], dmin.minus(oneNano))

    val args = Seq(Long.MinValue, -100000000000000L, 1L, 0L, 1L,
        100000000000000L, Long.MaxValue)
    for {
      d <- samples
      n <- args
    } {
      testDateTime(d.minus(n, NANOS))(d.minusNanos(n))
      testDateTime(d.minus(n, MICROS))(d.minus(ofNanos(1000).multipliedBy(n)))
      testDateTime(d.minus(n, MILLIS))(d.minusMillis(n))
      testDateTime(d.minus(n, SECONDS))(d.minusSeconds(n))
      testDateTime(d.minus(n, MINUTES))(d.minusMinutes(n))
      testDateTime(d.minus(n, HOURS))(d.minusHours(n))
      testDateTime(d.minus(n, HALF_DAYS))(d.minus(ofHours(12).multipliedBy(n)))
      testDateTime(d.minus(n, DAYS))(d.minusDays(n))
    }
    for {
      d <- samples
      n <- args
      u <- illegalUnits
    } {
      expectThrows(classOf[UnsupportedTemporalTypeException], d.minus(n, u))
    }
  }

  @Test def test_minusDays(): Unit = {
    val d1 = ofSeconds(1, 1)
    val d2 = ofSeconds(Long.MaxValue - 86400, 999999999)
    val d3 = ofSeconds(Long.MinValue + 86400)
    val d4 = ofSeconds(Long.MaxValue - 86399)
    val d5 = ofSeconds(Long.MinValue + 86400, -1)

    assertEquals(ofSeconds(-431999, 1), d1.minusDays(5))
    assertEquals(ofSeconds(-86399, 1), d1.minusDays(1))
    assertEquals(d1, d1.minusDays(0))
    assertEquals(ofSeconds(86401, 1), d1.minusDays(-1))
    assertEquals(ofSeconds(432001, 1), d1.minusDays(-5))
    assertEquals(dmax, d2.minusDays(-1))
    assertEquals(dmin, d3.minusDays(1))
    assertEquals(dmax, dmax.minusDays(0))
    assertEquals(d2, dmax.minusDays(1))
    assertEquals(dmin, dmin.minusDays(0))
    assertEquals(d3, dmin.minusDays(-1))

    expectThrows(classOf[ArithmeticException], d4.minusDays(-1))
    expectThrows(classOf[ArithmeticException], d2.minusDays(-2))
    expectThrows(classOf[ArithmeticException], d5.minusDays(1))
    expectThrows(classOf[ArithmeticException], d3.minusDays(2))
  }

  @Test def test_minusHours(): Unit = {
    val d1 = ofSeconds(1, 1)
    val d2 = ofSeconds(Long.MaxValue - 3600, 999999999)
    val d3 = ofSeconds(Long.MinValue + 3600)
    val d4 = ofSeconds(Long.MaxValue - 3599)
    val d5 = ofSeconds(Long.MinValue + 3600, -1)

    assertEquals(ofSeconds(-17999, 1), d1.minusHours(5))
    assertEquals(ofSeconds(-3599, 1), d1.minusHours(1))
    assertEquals(d1, d1.minusHours(0))
    assertEquals(ofSeconds(3601, 1), d1.minusHours(-1))
    assertEquals(ofSeconds(18001, 1), d1.minusHours(-5))
    assertEquals(dmax, d2.minusHours(-1))
    assertEquals(dmin, d3.minusHours(1))
    assertEquals(dmax, dmax.minusHours(0))
    assertEquals(d2, dmax.minusHours(1))
    assertEquals(dmin, dmin.minusHours(0))
    assertEquals(d3, dmin.minusHours(-1))

    expectThrows(classOf[ArithmeticException], d4.minusHours(-1))
    expectThrows(classOf[ArithmeticException], d2.minusHours(-2))
    expectThrows(classOf[ArithmeticException], d5.minusHours(1))
    expectThrows(classOf[ArithmeticException], d3.minusHours(2))
  }

  @Test def test_minusMinutes(): Unit = {
    val d1 = ofSeconds(1, 1)
    val d2 = ofSeconds(Long.MaxValue - 60, 999999999)
    val d3 = ofSeconds(Long.MinValue + 60)
    val d4 = ofSeconds(Long.MaxValue - 59)
    val d5 = ofSeconds(Long.MinValue + 60, -1)

    assertEquals(ofSeconds(-299, 1), d1.minusMinutes(5))
    assertEquals(ofSeconds(-59, 1), d1.minusMinutes(1))
    assertEquals(d1, d1.minusMinutes(0))
    assertEquals(ofSeconds(61, 1), d1.minusMinutes(-1))
    assertEquals(ofSeconds(301, 1), d1.minusMinutes(-5))
    assertEquals(dmax, d2.minusMinutes(-1))
    assertEquals(dmin, d3.minusMinutes(1))
    assertEquals(dmax, dmax.minusMinutes(0))
    assertEquals(d2, dmax.minusMinutes(1))
    assertEquals(dmin, dmin.minusMinutes(0))
    assertEquals(d3, dmin.minusMinutes(-1))

    expectThrows(classOf[ArithmeticException], d4.minusMinutes(-1))
    expectThrows(classOf[ArithmeticException], d2.minusMinutes(-2))
    expectThrows(classOf[ArithmeticException], d5.minusMinutes(1))
    expectThrows(classOf[ArithmeticException], d3.minusMinutes(2))
  }

  @Test def test_minusSeconds(): Unit = {
    val d1 = ofSeconds(1, 1)
    val d2 = ofSeconds(Long.MaxValue - 1, 999999999)
    val d3 = ofSeconds(Long.MinValue + 1)
    val d4 = ofSeconds(Long.MaxValue)
    val d5 = ofSeconds(Long.MinValue + 1, -1)

    assertEquals(ofSeconds(-4, 1), d1.minusSeconds(5))
    assertEquals(ofSeconds(0, 1), d1.minusSeconds(1))
    assertEquals(d1, d1.minusSeconds(0))
    assertEquals(ofSeconds(2, 1), d1.minusSeconds(-1))
    assertEquals(ofSeconds(6, 1), d1.minusSeconds(-5))
    assertEquals(dmax, d2.minusSeconds(-1))
    assertEquals(dmin, d3.minusSeconds(1))
    assertEquals(dmax, dmax.minusSeconds(0))
    assertEquals(d2, dmax.minusSeconds(1))
    assertEquals(dmin, dmin.minusSeconds(0))
    assertEquals(d3, dmin.minusSeconds(-1))

    expectThrows(classOf[ArithmeticException], d4.minusSeconds(-1))
    expectThrows(classOf[ArithmeticException], d2.minusSeconds(-2))
    expectThrows(classOf[ArithmeticException], d5.minusSeconds(1))
    expectThrows(classOf[ArithmeticException], d3.minusSeconds(2))
  }

  @Test def test_minusMillis(): Unit = {
    val d1 = ofSeconds(1, 1)
    val d2 = ofSeconds(Long.MaxValue, 998999999)
    val d3 = ofSeconds(Long.MinValue, 1000000)
    val d4 = ofSeconds(Long.MaxValue, 999000000)
    val d5 = ofSeconds(Long.MinValue, 999999)

    assertEquals(ofSeconds(-4, 1), d1.minusMillis(5000))
    assertEquals(ofSeconds(0, 900000001), d1.minusMillis(100))
    assertEquals(d1, d1.minusMillis(0))
    assertEquals(ofSeconds(1, 100000001), d1.minusMillis(-100))
    assertEquals(ofSeconds(6, 1), d1.minusMillis(-5000))
    assertEquals(dmax, d2.minusMillis(-1))
    assertEquals(dmin, d3.minusMillis(1))
    assertEquals(dmax, dmax.minusMillis(0))
    assertEquals(d2, dmax.minusMillis(1))
    assertEquals(dmin, dmin.minusMillis(0))
    assertEquals(d3, dmin.minusMillis(-1))

    expectThrows(classOf[ArithmeticException], d4.minusMillis(-1))
    expectThrows(classOf[ArithmeticException], d2.minusMillis(-2))
    expectThrows(classOf[ArithmeticException], d5.minusMillis(1))
    expectThrows(classOf[ArithmeticException], d3.minusMillis(2))
  }

  @Test def test_minusNanos(): Unit = {
    val d1 = ofSeconds(1, 1)
    val d2 = ofSeconds(Long.MaxValue, 999999998)
    val d3 = ofSeconds(Long.MinValue, 1)

    assertEquals(ofSeconds(-4, 1), d1.minusNanos(5000000000L))
    assertEquals(ofSeconds(0, 999999001), d1.minusNanos(1000))
    assertEquals(d1, d1.minusNanos(0))
    assertEquals(ofSeconds(1, 1001), d1.minusNanos(-1000))
    assertEquals(ofSeconds(6, 1), d1.minusNanos(-5000000000L))
    assertEquals(dmax, d2.minusNanos(-1))
    assertEquals(dmin, d3.minusNanos(1))
    assertEquals(dmax, dmax.minusNanos(0))
    assertEquals(d2, dmax.minusNanos(1))
    assertEquals(dmin, dmin.minusNanos(0))
    assertEquals(d3, dmin.minusNanos(-1))

    expectThrows(classOf[ArithmeticException], dmax.minusNanos(-1))
    expectThrows(classOf[ArithmeticException], d2.minusNanos(-2))
    expectThrows(classOf[ArithmeticException], dmin.minusNanos(1))
    expectThrows(classOf[ArithmeticException], d3.minusNanos(2))
  }

  @Test def test_multipliedBy(): Unit = {
    val d1 = ofSeconds(1, 1)
    val d2 = ofSeconds(-1, -1)

    for (d <- samples) {
      assertEquals(ZERO, d.multipliedBy(0))
      assertEquals(d, d.multipliedBy(1))
      testDateTime(d.multipliedBy(-1))(d.negated)
    }
    for (n <- Seq(Long.MinValue, -1L, 0L, 1L, Long.MaxValue))
      assertEquals(ZERO, ZERO.multipliedBy(n))
    assertEquals(d2, d1.multipliedBy(-1))
    assertEquals(ofSeconds(2, 2), d1.multipliedBy(2))
    assertEquals(ofSeconds(-2, -2), d1.multipliedBy(-2))
    assertEquals(ofSeconds(1000000001), d1.multipliedBy(1000000000))
    assertEquals(ofSeconds(-1000000001), d1.multipliedBy(-1000000000))
    assertEquals(d1, d2.multipliedBy(-1))
    assertEquals(ofSeconds(-2, -2), d2.multipliedBy(2))
    assertEquals(ofSeconds(2, 2), d2.multipliedBy(-2))
    assertEquals(ofSeconds(-1000000001), d2.multipliedBy(1000000000))
    assertEquals(ofSeconds(1000000001), d2.multipliedBy(-1000000000))
    assertEquals(dmin.plusNanos(1), dmax.multipliedBy(-1))

    expectThrows(classOf[ArithmeticException], dmin.multipliedBy(-1))
    expectThrows(classOf[ArithmeticException], dmin.multipliedBy(2))
    expectThrows(classOf[ArithmeticException], dmax.multipliedBy(2))
    expectThrows(classOf[ArithmeticException], d1.multipliedBy(Long.MaxValue))
    expectThrows(classOf[ArithmeticException], d1.multipliedBy(Long.MinValue))
  }

  @Test def test_dividedBy(): Unit = {
    val d1 = ofSeconds(10, 100)
    val d2 = ofNanos(10)
    val d3 = Duration.ofSeconds(9223372036L, 999999999)

    for (d <- samples) {
      assertEquals(d, d.dividedBy(1))
      testDateTime(d.dividedBy(-1))(d.negated)
    }
    expectThrows(classOf[ArithmeticException], dmin.dividedBy(-1))
    for (n <- Seq(Long.MinValue, -1L, 1L, Long.MaxValue))
      assertEquals(ZERO, ZERO.dividedBy(n))
    assertEquals(ofSeconds(5, 50), d1.dividedBy(2))
    assertEquals(ofSeconds(-5, -50), d1.dividedBy(-2))
    assertEquals(ofSeconds(3, 333333366), d1.dividedBy(3))
    assertEquals(ofSeconds(-3, -333333366), d1.dividedBy(-3))
    assertEquals(ofSeconds(1, 10), d1.dividedBy(10))
    assertEquals(ofSeconds(-1, -10), d1.dividedBy(-10))
    assertEquals(ofNanos(100000001), d1.dividedBy(100))
    assertEquals(ofNanos(-100000001), d1.dividedBy(-100))
    assertEquals(ofMillis(10), d1.dividedBy(1000))
    assertEquals(d1.dividedBy(-1000), ofMillis(-10))
    assertEquals(ofNanos(3333333), d1.dividedBy(3000))
    assertEquals(ofNanos(-3333333), d1.dividedBy(-3000))
    assertEquals(ofNanos(10), d1.dividedBy(1000000000))
    assertEquals(ofNanos(-10), d1.dividedBy(-1000000000))
    assertEquals(oneNano, d1.dividedBy(10000000000L))
    assertEquals(oneNano.negated, d1.dividedBy(-10000000000L))
    assertEquals(oneNano, d1.dividedBy(10000000100L))
    assertEquals(oneNano.negated, d1.dividedBy(-10000000100L))
    assertEquals(ZERO, d1.dividedBy(10000000101L))
    assertEquals(ZERO, d1.dividedBy(-10000000101L))
    assertEquals(oneNano, d2.dividedBy(10))
    assertEquals(oneNano.negated, d2.dividedBy(-10))
    assertEquals(ZERO, d2.dividedBy(11))
    assertEquals(ZERO, d2.dividedBy(-11))
    assertEquals(oneNano, d3.dividedBy(Long.MaxValue))
    assertEquals(oneNano.negated, d3.dividedBy(Long.MinValue))
    assertEquals(oneSecond, dmin.dividedBy(Long.MinValue))
    assertEquals(oneSecond.negated, dmin.dividedBy(Long.MaxValue))
    assertEquals(ofSeconds(-1, 1), dmax.dividedBy(Long.MinValue))
    assertEquals(oneSecond, dmax.dividedBy(Long.MaxValue))
    assertEquals(ofSeconds(1, -1), dmin.plusNanos(1).dividedBy(Long.MinValue))
    assertEquals(oneSecond.negated, dmin.plusNanos(1).dividedBy(Long.MaxValue))
    assertEquals(oneSecond.negated, dmin.plusNanos(2).dividedBy(Long.MaxValue))
    assertEquals(oneSecond, dmax.minusNanos(1).dividedBy(Long.MaxValue))

    expectThrows(classOf[ArithmeticException], dmin.dividedBy(-1))
    for (d <- samples)
      expectThrows(classOf[ArithmeticException], d.dividedBy(0))
  }

  @Test def test_negated(): Unit = {
    assertEquals(ZERO, ZERO.negated)
    assertEquals(ofSeconds(-1), oneSecond.negated)
    assertEquals(oneSecond, ofSeconds(-1).negated)
    assertEquals(ofSeconds(-1, -1), ofSeconds(1, 1).negated)
    assertEquals(ofSeconds(1, 1), ofSeconds(-1, -1).negated)
    assertEquals(ofSeconds(Long.MinValue, 1), dmax.negated)
    assertEquals(dmax, ofSeconds(Long.MinValue, 1).negated)

    expectThrows(classOf[ArithmeticException], dmin.negated)
  }

  @Test def test_abs(): Unit = {
    assertEquals(ZERO, ZERO.abs)
    assertEquals(oneSecond, oneSecond.abs)
    assertEquals(oneSecond, ofSeconds(-1).abs)
    assertEquals(ofSeconds(1, 1), ofSeconds(1, 1).abs)
    assertEquals(ofSeconds(1, 1), ofSeconds(-1, -1).abs)
    assertEquals(dmax, dmax.abs)
    assertEquals(dmax, ofSeconds(Long.MinValue, 1).abs)

    expectThrows(classOf[ArithmeticException], dmin.abs)
  }

  @Test def test_addTo(): Unit = {
    val t = LocalTime.NOON
    val d = LocalDate.MIN

    assertEquals(t, ZERO.addTo(t))
    assertEquals(LocalTime.of(20, 29, 52), dmin.addTo(t))
    assertEquals(LocalTime.of(3, 30, 7, 999999999), dmax.addTo(t))
    assertEquals(d, ZERO.addTo(d))

    expectThrows(classOf[UnsupportedTemporalTypeException], oneNano.addTo(d))
    expectThrows(classOf[UnsupportedTemporalTypeException], oneSecond.addTo(d))
  }

  @Test def test_subtractFrom(): Unit = {
    val t = LocalTime.NOON
    val d = LocalDate.MIN

    assertEquals(t, ZERO.subtractFrom(t))
    assertEquals(LocalTime.of(3, 30, 8), dmin.subtractFrom(t))
    assertEquals(LocalTime.of(20, 29, 52, 1), dmax.subtractFrom(t))
    assertEquals(d, ZERO.subtractFrom(d))

    expectThrows(classOf[UnsupportedTemporalTypeException], oneNano.subtractFrom(d))
    expectThrows(classOf[UnsupportedTemporalTypeException], oneSecond.subtractFrom(d))
  }

  @Test def test_toDays(): Unit = {
    assertEquals(-106751991167300L, dmin.toDays)
    assertEquals(-2L, ofSeconds(-172799, -1).toDays)
    assertEquals(-1L, ofSeconds(-172799).toDays)
    assertEquals(-1L, ofSeconds(-86400).toDays)
    assertEquals(-1L, ofSeconds(-86399, -1).toDays)
    assertEquals(0L, ofSeconds(-86399).toDays)
    assertEquals(0L, ZERO.toDays)
    assertEquals(0L, ofSeconds(86399).toDays)
    assertEquals(0L, ofSeconds(86400, -1).toDays)
    assertEquals(1L, ofSeconds(86400).toDays)
    assertEquals(1L, ofSeconds(172800, -1).toDays)
    assertEquals(2L, ofSeconds(172800).toDays)
    assertEquals(106751991167300L, dmax.toDays)
  }

  @Test def test_toHours(): Unit = {
    assertEquals(-2562047788015215L, dmin.toHours)
    assertEquals(-2L, ofSeconds(-7199, -1).toHours)
    assertEquals(-1L, ofSeconds(-7199).toHours)
    assertEquals(-1L, ofSeconds(-3600).toHours)
    assertEquals(-1L, ofSeconds(-3599, -1).toHours)
    assertEquals(0L, ofSeconds(-3599).toHours)
    assertEquals(0L, ZERO.toHours)
    assertEquals(0L, ofSeconds(3599).toHours)
    assertEquals(0L, ofSeconds(3600, -1).toHours)
    assertEquals(1L, ofSeconds(3600).toHours)
    assertEquals(1L, ofSeconds(7200, -1).toHours)
    assertEquals(2L, ofSeconds(7200).toHours)
    assertEquals(2562047788015215L, dmax.toHours)
  }

  @Test def test_toMinutes(): Unit = {
    assertEquals(-153722867280912930L, dmin.toMinutes)
    assertEquals(-2L, ofSeconds(-119, -1).toMinutes)
    assertEquals(-1L, ofSeconds(-119).toMinutes)
    assertEquals(-1L, ofSeconds(-60).toMinutes)
    assertEquals(-1L, ofSeconds(-59, -1).toMinutes)
    assertEquals(0L, ofSeconds(-59).toMinutes)
    assertEquals(0L, ZERO.toMinutes)
    assertEquals(0L, ofSeconds(59).toMinutes)
    assertEquals(0L, ofSeconds(60, -1).toMinutes)
    assertEquals(1L, ofSeconds(60).toMinutes)
    assertEquals(1L, ofSeconds(120, -1).toMinutes)
    assertEquals(2L, ofSeconds(120).toMinutes)
    assertEquals(153722867280912930L, dmax.toMinutes)
  }

  @Test def test_toMillis(): Unit = {
    assertEquals(-9223372036854775000L, ofSeconds(-9223372036854775L).toMillis)
    assertEquals(-1000L, ofSeconds(-1).toMillis)
    assertEquals(-2L, ofNanos(-1000001).toMillis)
    assertEquals(-1L, ofNanos(-1000000).toMillis)
    assertEquals(-1L, ofNanos(-1).toMillis)
    assertEquals(0L, ZERO.toMillis)
    assertEquals(0L, ofNanos(999999).toMillis)
    assertEquals(1L, ofNanos(1000000).toMillis)
    assertEquals(1L, ofNanos(1999999).toMillis)
    assertEquals(2L, ofNanos(2000000).toMillis)
    assertEquals(1000L, ofSeconds(1).toMillis)
    assertEquals(9223372036854775807L,
        ofSeconds(9223372036854775L, 807999999).toMillis)

    expectThrows(classOf[ArithmeticException], dmin.toMillis)
    expectThrows(classOf[ArithmeticException], dmax.toMillis)
    // this could yield a valid long, but the reference implementation throws
    expectThrows(classOf[ArithmeticException],
        ofSeconds(-9223372036854775L, -1).toMillis)
    expectThrows(classOf[ArithmeticException],
        ofSeconds(9223372036854775L, 808000000).toMillis)
  }

  @Test def test_toNanos(): Unit = {
    assertEquals(-9223372036000000000L, ofSeconds(-9223372036L).toNanos)
    assertEquals(Int.MinValue.toLong, ofNanos(Int.MinValue).toNanos)
    assertEquals(-1000L, ofNanos(-1000).toNanos)
    assertEquals(-1L, ofNanos(-1).toNanos)
    assertEquals(0L, ofNanos(0).toNanos)
    assertEquals(1L, ofNanos(1).toNanos)
    assertEquals(1000L, ofNanos(1000).toNanos)
    assertEquals(Int.MaxValue.toLong, ofNanos(Int.MaxValue).toNanos)
    assertEquals(Long.MaxValue, ofSeconds(9223372036L, 854775807).toNanos)

    expectThrows(classOf[ArithmeticException], dmin.toNanos)
    expectThrows(classOf[ArithmeticException], dmax.toNanos)
    // this should yield a valid long, but the reference implementation throws
    expectThrows(classOf[ArithmeticException],
        ofSeconds(-9223372036L, -1).toNanos)
    expectThrows(classOf[ArithmeticException],
        ofSeconds(9223372036L, 854775808).toNanos)
  }

  @Test def test_compareTo(): Unit = {
    val d1 = ofSeconds(0, -1)
    val d0 = ZERO
    val d2 = ofSeconds(0, 1)

    assertEquals(0, dmin.compareTo(dmin))
    assertTrue(dmin.compareTo(d1) < 0)
    assertTrue(dmin.compareTo(d0) < 0)
    assertTrue(dmin.compareTo(d2) < 0)
    assertTrue(dmin.compareTo(dmax) < 0)
    assertTrue(d1.compareTo(dmin) > 0)
    assertEquals(0, d1.compareTo(d1))
    assertTrue(d1.compareTo(d0) < 0)
    assertTrue(d1.compareTo(d2) < 0)
    assertTrue(d1.compareTo(dmax) < 0)
    assertTrue(d0.compareTo(dmin) > 0)
    assertTrue(d0.compareTo(d1) > 0)
    assertEquals(0, d0.compareTo(d0))
    assertTrue(d0.compareTo(d2) < 0)
    assertTrue(d0.compareTo(dmax) < 0)
    assertTrue(d2.compareTo(dmin) > 0)
    assertTrue(d2.compareTo(d1) > 0)
    assertTrue(d2.compareTo(d0) > 0)
    assertEquals(0, d2.compareTo(d2))
    assertTrue(d2.compareTo(dmax) < 0)
    assertTrue(dmax.compareTo(dmin) > 0)
    assertTrue(dmax.compareTo(d1) > 0)
    assertTrue(dmax.compareTo(d0) > 0)
    assertTrue(dmax.compareTo(d2) > 0)
    assertEquals(0, dmax.compareTo(dmax))
  }

  @Test def test_toString(): Unit = {
    assertEquals("PT0S", ZERO.toString)
    assertEquals("PT-0.999999999S", ofSeconds(-1, 1).toString)
    assertEquals("PT-1.000000001S", ofSeconds(-1, -1).toString)
    assertEquals("PT1M", ofSeconds(60).toString)
    assertEquals("PT-1M", ofSeconds(-60).toString)
    assertEquals("PT59.999999999S", ofSeconds(60, -1).toString)
    assertEquals("PT1M0.000000001S", ofSeconds(60, 1).toString)
    assertEquals("PT-1M-0.999999999S", ofSeconds(-61, 1).toString)
    assertEquals("PT2M0.00000001S", ofSeconds(120, 10).toString)
    assertEquals("PT-1M-59.99999999S", ofSeconds(-120, 10).toString)
    assertEquals("PT1H", ofSeconds(3600).toString)
    assertEquals("PT-1H", ofSeconds(-3600).toString)
    assertEquals("PT-2562047788015215H-30M-8S", dmin.toString)
    assertEquals("PT2562047788015215H30M7.999999999S", dmax.toString)
  }

  @Test def test_ofDays(): Unit = {
    val maxDays = 106751991167300L
    val maxSecs = maxDays * 86400

    assertEquals(ofSeconds(-maxSecs), ofDays(-maxDays))
    assertEquals(ofSeconds(-86400), ofDays(-1))
    assertEquals(ZERO, ofDays(0))
    assertEquals(ofSeconds(86400), ofDays(1))
    assertEquals(ofSeconds(maxSecs), ofDays(maxDays))

    expectThrows(classOf[ArithmeticException], ofDays(-maxDays - 1))
    expectThrows(classOf[ArithmeticException], ofDays(maxDays + 1))
  }

  @Test def test_ofHours(): Unit = {
    val maxHrs = 2562047788015215L
    val maxSecs = maxHrs * 3600

    assertEquals(ofSeconds(-maxSecs), ofHours(-maxHrs))
    assertEquals(ofSeconds(-3600), ofHours(-1))
    assertEquals(ZERO, ofHours(0))
    assertEquals(ofSeconds(3600), ofHours(1))
    assertEquals(ofSeconds(maxSecs), ofHours(maxHrs))

    expectThrows(classOf[ArithmeticException], ofHours(-maxHrs - 1))
    expectThrows(classOf[ArithmeticException], ofHours(maxHrs + 1))
  }

  @Test def test_ofMinutes(): Unit = {
    val maxMins = 153722867280912930L
    val maxSecs = maxMins * 60

    assertEquals(ofSeconds(-maxSecs), ofMinutes(-maxMins))
    assertEquals(ofSeconds(-60), ofMinutes(-1))
    assertEquals(ZERO, ofMinutes(0))
    assertEquals(ofSeconds(60), ofMinutes(1))
    assertEquals(ofSeconds(maxSecs), ofMinutes(maxMins))

    expectThrows(classOf[ArithmeticException], ofMinutes(-maxMins - 1))
    expectThrows(classOf[ArithmeticException], ofMinutes(maxMins + 1))
  }

  @Test def test_ofSeconds(): Unit = {
    assertEquals(ofSeconds(-11, 999999999), ofSeconds(-10, -1))
    assertEquals(ofSeconds(-1), ofSeconds(-1, 0))
    assertEquals(ZERO, ofSeconds(-1, 1000000000))
    assertEquals(ZERO, ofSeconds(0))
    assertEquals(ZERO, ofSeconds(0, 0))
    assertEquals(ZERO, ofSeconds(1, -1000000000))
    assertEquals(ofSeconds(1), ofSeconds(1, 0))
    assertEquals(ofSeconds(9, 999999999), ofSeconds(10, -1))

    expectThrows(classOf[ArithmeticException], ofSeconds(Long.MinValue, -1))
    expectThrows(classOf[ArithmeticException],
        ofSeconds(Long.MaxValue, 1000000000))
  }

  @Test def test_ofMillis(): Unit = {
    assertEquals(ofSeconds(-9223372036854776L, 192000000),
        ofMillis(Long.MinValue))
    assertEquals(ofSeconds(-1), ofMillis(-1000))
    assertEquals(ofSeconds(0, -1000000), ofMillis(-1))
    assertEquals(ZERO, ofMillis(0))
    assertEquals(ofSeconds(0, 1000000), ofMillis(1))
    assertEquals(ofSeconds(1), ofMillis(1000))
    assertEquals(ofSeconds(9223372036854775L, 807000000),
        ofMillis(Long.MaxValue))
  }

  @Test def test_ofNanos(): Unit = {
    assertEquals(ofSeconds(-9223372037L, 145224192), ofNanos(Long.MinValue))
    assertEquals(ofSeconds(-1), ofNanos(-1000000000))
    assertEquals(ofSeconds(0, -1), ofNanos(-1))
    assertEquals(ZERO, ofNanos(0))
    assertEquals(ofSeconds(0, 1), ofNanos(1))
    assertEquals(ofSeconds(1), ofNanos(1000000000))
    assertEquals(ofSeconds(9223372036L, 854775807), ofNanos(Long.MaxValue))
  }

  @Test def test_of(): Unit = {
    for (n <- Seq(-100000000000000L, -1L, 0L, 1L, 100000000000000L)) {
      assertEquals(ofNanos(n), of(n, NANOS))
      assertEquals(ofNanos(n * 1000), of(n, MICROS))
      assertEquals(ofMillis(n), of(n, MILLIS))
      assertEquals(ofSeconds(n), of(n, SECONDS))
      assertEquals(ofMinutes(n), of(n, MINUTES))
      assertEquals(ofHours(n), of(n, HOURS))
      assertEquals(ofHours(n * 12), of(n, HALF_DAYS))
      assertEquals(ofDays(n), of(n, DAYS))
    }
    assertEquals(ofSeconds(-9223372036855L, 224192000),
        of(Long.MinValue, MICROS))
    assertEquals(ofSeconds(9223372036854L, 775807000),
        of(Long.MaxValue, MICROS))

    for (s <- Seq(-1, 1)) {
      expectThrows(classOf[ArithmeticException], of(106751991167301L * s, DAYS))
      expectThrows(classOf[ArithmeticException],
          of(213503982334602L * s, HALF_DAYS))
      expectThrows(classOf[ArithmeticException],
          of(2562047788015216L * s, HOURS))
      expectThrows(classOf[ArithmeticException],
          of(153722867280912931L * s, MINUTES))
    }

    for (n <- Seq(-1L, 0L, 1L)) {
      expectThrows(classOf[UnsupportedTemporalTypeException], of(n, WEEKS))
      expectThrows(classOf[UnsupportedTemporalTypeException], of(n, MONTHS))
      expectThrows(classOf[UnsupportedTemporalTypeException], of(n, YEARS))
      expectThrows(classOf[UnsupportedTemporalTypeException], of(n, DECADES))
      expectThrows(classOf[UnsupportedTemporalTypeException], of(n, CENTURIES))
      expectThrows(classOf[UnsupportedTemporalTypeException], of(n, MILLENNIA))
      expectThrows(classOf[UnsupportedTemporalTypeException], of(n, ERAS))
      expectThrows(classOf[UnsupportedTemporalTypeException], of(n, FOREVER))
    }
  }

  @Test def test_from(): Unit = {
    assertEquals(dmin, from(dmin))
    assertEquals(ZERO, from(ZERO))
    assertEquals(dmax, from(dmax))

    expectThrows(classOf[UnsupportedTemporalTypeException], from(Period.ZERO))
  }

  @Test def test_between(): Unit = {
    val MIN = LocalTime.MIN
    val MAX = LocalTime.MAX

    assertEquals(ofNanos(86399999999999L), between(MIN, MAX))
    assertEquals(ofNanos(1), between(MIN, LocalTime.of(0, 0, 0, 1)))

    expectThrows(classOf[DateTimeException],
        between(MIN, LocalDate.of(2012, 2, 29)))
    expectThrows(classOf[DateTimeException],
        between(LocalDate.of(2012, 2, 29), LocalDate.of(2012, 3, 1)))
  }
}
