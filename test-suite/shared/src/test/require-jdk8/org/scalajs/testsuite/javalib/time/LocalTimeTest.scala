package org.scalajs.testsuite.javalib.time

import java.time._
import java.time.temporal._

import org.junit.Test
import org.junit.Assert._
import org.scalajs.testsuite.utils.AssertThrows._

class LocalTimeTest extends TemporalTest[LocalTime] {

  import DateTimeTestUtil._
  import LocalTime._
  import ChronoUnit._
  import ChronoField._

  val samples = Seq(MAX, NOON, MIN)

  def isSupported(unit: ChronoUnit): Boolean = unit.isTimeBased

  def isSupported(field: ChronoField): Boolean = field.isTimeBased

  @Test def test_getLong(): Unit = {
    assertEquals(0L, MIN.getLong(NANO_OF_SECOND))
    assertEquals(0L, MIN.getLong(NANO_OF_DAY))
    assertEquals(0L, MIN.getLong(MICRO_OF_SECOND))
    assertEquals(0L, MIN.getLong(MICRO_OF_DAY))
    assertEquals(0L, MIN.getLong(MILLI_OF_SECOND))
    assertEquals(0L, MIN.getLong(MILLI_OF_DAY))
    assertEquals(0L, MIN.getLong(SECOND_OF_MINUTE))
    assertEquals(0L, MIN.getLong(SECOND_OF_DAY))
    assertEquals(0L, MIN.getLong(MINUTE_OF_HOUR))
    assertEquals(0L, MIN.getLong(MINUTE_OF_DAY))
    assertEquals(0L, MIN.getLong(HOUR_OF_AMPM))
    assertEquals(12L, MIN.getLong(CLOCK_HOUR_OF_AMPM))
    assertEquals(0L, MIN.getLong(HOUR_OF_DAY))
    assertEquals(24L, MIN.getLong(CLOCK_HOUR_OF_DAY))
    assertEquals(0L, MIN.getLong(AMPM_OF_DAY))

    assertEquals(999999999L, MAX.getLong(NANO_OF_SECOND))
    assertEquals(86399999999999L, MAX.getLong(NANO_OF_DAY))
    assertEquals(999999L, MAX.getLong(MICRO_OF_SECOND))
    assertEquals(86399999999L, MAX.getLong(MICRO_OF_DAY))
    assertEquals(999L, MAX.getLong(MILLI_OF_SECOND))
    assertEquals(86399999L, MAX.getLong(MILLI_OF_DAY))
    assertEquals(59L, MAX.getLong(SECOND_OF_MINUTE))
    assertEquals(86399L, MAX.getLong(SECOND_OF_DAY))
    assertEquals(59L, MAX.getLong(MINUTE_OF_HOUR))
    assertEquals(1439L, MAX.getLong(MINUTE_OF_DAY))
    assertEquals(11L, MAX.getLong(HOUR_OF_AMPM))
    assertEquals(11L, MAX.getLong(CLOCK_HOUR_OF_AMPM))
    assertEquals(23L, MAX.getLong(HOUR_OF_DAY))
    assertEquals(23L, MAX.getLong(CLOCK_HOUR_OF_DAY))
    assertEquals(1L, MAX.getLong(AMPM_OF_DAY))
  }

  @Test def test_getHour(): Unit = {
    assertEquals(0, MIN.getHour)
    assertEquals(12, NOON.getHour)
    assertEquals(23, MAX.getHour)
  }

  @Test def test_getMinute(): Unit = {
    assertEquals(0, MIN.getMinute)
    assertEquals(30, of(0, 30).getMinute)
    assertEquals(59, MAX.getMinute)
  }

  @Test def test_getSecond(): Unit = {
    assertEquals(0, MIN.getSecond)
    assertEquals(30, of(0, 0, 30).getSecond)
    assertEquals(59, MAX.getSecond)
  }

  @Test def test_getNano(): Unit = {
    assertEquals(0, MIN.getNano)
    assertEquals(999999999, MAX.getNano)
  }

  @Test def test_with(): Unit = {
    for (t <- samples) {
      for (n <- Seq(0, 999, 999999, 999999999))
        testDateTime(t.`with`(NANO_OF_SECOND, n))(t.withNano(n))
      for (n <- Seq(0L, 1000000000L, 86399999999999L))
        testDateTime(t.`with`(NANO_OF_DAY, n))(ofNanoOfDay(n))
      for (n <- Seq(0, 999, 999999))
        testDateTime(t.`with`(MICRO_OF_SECOND, n))(t.withNano(n * 1000))
      for (n <- Seq(0L, 1000000L, 86399999999L))
        testDateTime(t.`with`(MICRO_OF_DAY, n))(ofNanoOfDay(n * 1000))
      for (n <- Seq(0, 500, 999))
        testDateTime(t.`with`(MILLI_OF_SECOND, n))(t.withNano(n * 1000000))
      for (n <- Seq(0L, 1000L, 86399999L))
        testDateTime(t.`with`(MILLI_OF_DAY, n))(ofNanoOfDay(n * 1000000))
      for (n <- Seq(0, 30, 59))
        testDateTime(t.`with`(SECOND_OF_MINUTE, n))(t.withSecond(n))
      for (n <- Seq(0, 60, 86399))
        testDateTime(t.`with`(SECOND_OF_DAY, n))(ofSecondOfDay(n).withNano(t.getNano))
      for (n <- Seq(0, 30, 59))
        testDateTime(t.`with`(MINUTE_OF_HOUR, n))(t.withMinute(n))
      for (n <- Seq(0, 60, 1439)) {
        testDateTime(t.`with`(MINUTE_OF_DAY, n)) {
          ofSecondOfDay(n * 60).withSecond(t.getSecond).withNano(t.getNano)
        }
      }
      for (n <- Seq(0, 6, 11)) {
        val h = (t.getHour / 12) * 12 + n
        testDateTime(t.`with`(HOUR_OF_AMPM, n))(t.withHour(h))
      }
      for (n <- Seq(1, 6, 12)) {
        val h = (t.getHour / 12) * 12 + (n % 12)
        testDateTime(t.`with`(CLOCK_HOUR_OF_AMPM, n))(t.withHour(h))
      }
      for (n <- Seq(0, 12, 23))
        testDateTime(t.`with`(HOUR_OF_DAY, n))(t.withHour(n))
      for (n <- Seq(1, 12, 24))
        testDateTime(t.`with`(CLOCK_HOUR_OF_DAY, n))(t.withHour(n % 24))
      for (n <- Seq(0, 1)) {
        val h = t.getHour % 12 + n * 12
        testDateTime(t.`with`(AMPM_OF_DAY, n))(t.withHour(h))
      }

      for (n <- Seq(Long.MinValue, -1L, 1000000000L, Long.MaxValue))
        expectThrows(classOf[DateTimeException], t.`with`(NANO_OF_SECOND, n))
      for (n <- Seq(Long.MinValue, -1L, 86400000000000L, Long.MaxValue))
        expectThrows(classOf[DateTimeException], t.`with`(NANO_OF_DAY, n))
      for (n <- Seq(Long.MinValue, -1L, 1000000L, Long.MaxValue))
        expectThrows(classOf[DateTimeException], t.`with`(MICRO_OF_SECOND, n))
      for (n <- Seq(Long.MinValue, -1L, 86400000000L, Long.MaxValue))
        expectThrows(classOf[DateTimeException], t.`with`(MICRO_OF_DAY, n))
      for (n <- Seq(Long.MinValue, -1L, 1000L, Long.MaxValue))
        expectThrows(classOf[DateTimeException], t.`with`(MILLI_OF_SECOND, n))
      for (n <- Seq(Long.MinValue, -1L, 86400000L, Long.MaxValue))
        expectThrows(classOf[DateTimeException], t.`with`(MILLI_OF_DAY, n))
      for (n <- Seq(Long.MinValue, -1L, 60L, Long.MaxValue))
        expectThrows(classOf[DateTimeException], t.`with`(SECOND_OF_MINUTE, n))
      for (n <- Seq(Long.MinValue, -1L, 86400L, Long.MaxValue))
        expectThrows(classOf[DateTimeException], t.`with`(SECOND_OF_DAY, n))
      for (n <- Seq(Long.MinValue, -1L, 60L, Long.MaxValue))
        expectThrows(classOf[DateTimeException], t.`with`(MINUTE_OF_HOUR, n))
      for (n <- Seq(Long.MinValue, -1L, 1440L, Long.MaxValue))
        expectThrows(classOf[DateTimeException], t.`with`(MINUTE_OF_DAY, n))
      for (n <- Seq(Long.MinValue, -1L, 12L, Long.MaxValue))
        expectThrows(classOf[DateTimeException], t.`with`(HOUR_OF_AMPM, n))
      for (n <- Seq(Long.MinValue, 0L, 13L, Long.MaxValue))
        expectThrows(classOf[DateTimeException], t.`with`(CLOCK_HOUR_OF_AMPM, n))
      for (n <- Seq(Long.MinValue, -1L, 24L, Long.MaxValue))
        expectThrows(classOf[DateTimeException], t.`with`(HOUR_OF_DAY, n))
      for (n <- Seq(Long.MinValue, 0L, 25L, Long.MaxValue))
        expectThrows(classOf[DateTimeException], t.`with`(CLOCK_HOUR_OF_DAY, n))
      for (n <- Seq(Long.MinValue, -1L, 2L, Long.MaxValue))
        expectThrows(classOf[DateTimeException], t.`with`(AMPM_OF_DAY, n))
    }
  }

  @Test def test_withHour(): Unit = {
    testDateTime(MIN.withHour(0))(MIN)
    testDateTime(MIN.withHour(12))(NOON)
    testDateTime(MIN.withHour(23))(of(23, 0))
    testDateTime(MAX.withHour(0))(of(0, 59, 59, 999999999))
    testDateTime(MAX.withHour(23))(MAX)

    for (t <- samples) {
      expectThrows(classOf[DateTimeException], t.withHour(Int.MinValue))
      expectThrows(classOf[DateTimeException], t.withHour(-1))
      expectThrows(classOf[DateTimeException], t.withHour(24))
      expectThrows(classOf[DateTimeException], t.withHour(Int.MaxValue))
    }
  }

  @Test def test_withMinute(): Unit = {
    testDateTime(MIN.withMinute(0))(MIN)
    testDateTime(MIN.withMinute(30))(of(0, 30))
    testDateTime(MIN.withMinute(59))(of(0, 59))
    testDateTime(MAX.withMinute(0))(of(23, 0, 59, 999999999))
    testDateTime(MAX.withMinute(59))(MAX)

    for (t <- samples) {
      expectThrows(classOf[DateTimeException], t.withMinute(Int.MinValue))
      expectThrows(classOf[DateTimeException], t.withMinute(-1))
      expectThrows(classOf[DateTimeException], t.withMinute(60))
      expectThrows(classOf[DateTimeException], t.withMinute(Int.MaxValue))
    }
  }

  @Test def test_withSecond(): Unit = {
    testDateTime(MIN.withSecond(0))(MIN)
    testDateTime(MIN.withSecond(30))(of(0, 0, 30))
    testDateTime(MIN.withSecond(59))(of(0, 0, 59))
    testDateTime(MAX.withSecond(0))(of(23, 59, 0, 999999999))
    testDateTime(MAX.withSecond(59))(MAX)

    for (t <- samples) {
      expectThrows(classOf[DateTimeException], t.withSecond(Int.MinValue))
      expectThrows(classOf[DateTimeException], t.withSecond(-1))
      expectThrows(classOf[DateTimeException], t.withSecond(60))
      expectThrows(classOf[DateTimeException], t.withSecond(Int.MaxValue))
    }
  }

  @Test def test_withNano(): Unit = {
    testDateTime(MIN.withNano(0))(MIN)
    testDateTime(MIN.withNano(500000000))(of(0, 0, 0, 500000000))
    testDateTime(MIN.withNano(999999999))(of(0, 0, 0, 999999999))
    testDateTime(MAX.withNano(0))(of(23, 59, 59, 0))
    testDateTime(MAX.withNano(999999999))(MAX)

    for (t <- samples) {
      expectThrows(classOf[DateTimeException], t.withNano(Int.MinValue))
      expectThrows(classOf[DateTimeException], t.withNano(-1))
      expectThrows(classOf[DateTimeException], t.withNano(1000000000))
      expectThrows(classOf[DateTimeException], t.withNano(Int.MaxValue))
    }
  }

  @Test def test_truncatedTo(): Unit = {
    testDateTime(MIN.truncatedTo(NANOS))(MIN)
    testDateTime(MAX.truncatedTo(NANOS))(MAX)
    testDateTime(MIN.truncatedTo(MICROS))(MIN)
    testDateTime(MAX.truncatedTo(MICROS))(of(23, 59, 59, 999999000))
    testDateTime(MIN.truncatedTo(MILLIS))(MIN)
    testDateTime(MAX.truncatedTo(MILLIS))(of(23, 59, 59, 999000000))
    testDateTime(MIN.truncatedTo(SECONDS))(MIN)
    testDateTime(MAX.truncatedTo(SECONDS))(of(23, 59, 59))
    testDateTime(MIN.truncatedTo(MINUTES))(MIN)
    testDateTime(MAX.truncatedTo(MINUTES))(of(23, 59))
    testDateTime(MIN.truncatedTo(HOURS))(MIN)
    testDateTime(MAX.truncatedTo(HOURS))(of(23, 0))
    testDateTime(MIN.truncatedTo(HALF_DAYS))(MIN)
    testDateTime(MAX.truncatedTo(HALF_DAYS))(of(12, 0))
    testDateTime(MIN.truncatedTo(DAYS))(MIN)
    testDateTime(MAX.truncatedTo(DAYS))(MIN)

    val illegalUnits = dateBasedUnits.filter(_ != DAYS)
    for {
      t <- samples
      u <- illegalUnits
    } {
      expectThrows(classOf[UnsupportedTemporalTypeException], t.truncatedTo(u))
    }
  }

  @Test def test_plus(): Unit = {
    val values = Seq(Long.MinValue, -1000000000L, -86400L, -3600L, -60L, -1L, 0L,
        1L, 60L, 3600L, 86400L, 1000000000L, Long.MaxValue)

    for {
      t <- samples
      n <- values
    } {
      testDateTime(t.plus(n, NANOS))(t.plusNanos(n))
      testDateTime(t.plus(n, MICROS))(t.plusNanos((n % 86400000000L) * 1000))
      testDateTime(t.plus(n, MILLIS))(t.plusNanos((n % 86400000) * 1000000))
      testDateTime(t.plus(n, SECONDS))(t.plusSeconds(n))
      testDateTime(t.plus(n, MINUTES))(t.plusMinutes(n))
      testDateTime(t.plus(n, HOURS))(t.plusHours(n))
      testDateTime(t.plus(n, HALF_DAYS))(t.plusHours((n % 2) * 12))
    }
  }

  @Test def test_plusHours(): Unit = {
    testDateTime(MIN.plusHours(Long.MinValue))(of(16, 0))
    testDateTime(MIN.plusHours(-24))(MIN)
    testDateTime(MIN.plusHours(-1))(of(23, 0))
    testDateTime(MIN.plusHours(0))(MIN)
    testDateTime(MIN.plusHours(1))(of(1, 0))
    testDateTime(MIN.plusHours(24))(MIN)
    testDateTime(MIN.plusHours(Long.MaxValue))(of(7, 0))
    testDateTime(MAX.plusHours(Long.MinValue))(of(15, 59, 59, 999999999))
    testDateTime(MAX.plusHours(-24))(MAX)
    testDateTime(MAX.plusHours(-1))(of(22, 59, 59, 999999999))
    testDateTime(MAX.plusHours(0))(MAX)
    testDateTime(MAX.plusHours(1))(of(0, 59, 59, 999999999))
    testDateTime(MAX.plusHours(24))(MAX)
    testDateTime(MAX.plusHours(Long.MaxValue))(of(6, 59, 59, 999999999))
  }

  @Test def test_plusMinutes(): Unit = {
    testDateTime(MIN.plusMinutes(Long.MinValue))(of(5, 52))
    testDateTime(MIN.plusMinutes(-1440))(MIN)
    testDateTime(MIN.plusMinutes(-60))(of(23, 0))
    testDateTime(MIN.plusMinutes(-1))(of(23, 59))
    testDateTime(MIN.plusMinutes(0))(MIN)
    testDateTime(MIN.plusMinutes(1))(of(0, 1))
    testDateTime(MIN.plusMinutes(60))(of(1, 0))
    testDateTime(MIN.plusMinutes(1440))(MIN)
    testDateTime(MIN.plusMinutes(Long.MaxValue))(of(18, 7))
    testDateTime(MAX.plusMinutes(Long.MinValue))(of(5, 51, 59, 999999999))
    testDateTime(MAX.plusMinutes(-1440))(MAX)
    testDateTime(MAX.plusMinutes(-60))(of(22, 59, 59, 999999999))
    testDateTime(MAX.plusMinutes(-1))(of(23, 58, 59, 999999999))
    testDateTime(MAX.plusMinutes(0))(MAX)
    testDateTime(MAX.plusMinutes(1))(of(0, 0, 59, 999999999))
    testDateTime(MAX.plusMinutes(60))(of(0, 59, 59, 999999999))
    testDateTime(MAX.plusMinutes(1440))(MAX)
    testDateTime(MAX.plusMinutes(Long.MaxValue))(of(18, 6, 59, 999999999))
  }

  @Test def test_plusSeconds(): Unit = {
    testDateTime(MIN.plusSeconds(Long.MinValue))(of(8, 29, 52))
    testDateTime(MIN.plusSeconds(-86400))(MIN)
    testDateTime(MIN.plusSeconds(-60))(of(23, 59))
    testDateTime(MIN.plusSeconds(-1))(of(23, 59, 59))
    testDateTime(MIN.plusSeconds(0))(MIN)
    testDateTime(MIN.plusSeconds(1))(of(0, 0, 1))
    testDateTime(MIN.plusSeconds(60))(of(0, 1))
    testDateTime(MIN.plusSeconds(86400))(MIN)
    testDateTime(MIN.plusSeconds(Long.MaxValue))(of(15, 30, 7))
    testDateTime(MAX.plusSeconds(Long.MinValue))(of(8, 29, 51, 999999999))
    testDateTime(MAX.plusSeconds(-86400))(MAX)
    testDateTime(MAX.plusSeconds(-60))(of(23, 58, 59, 999999999))
    testDateTime(MAX.plusSeconds(-1))(of(23, 59, 58, 999999999))
    testDateTime(MAX.plusSeconds(0))(MAX)
    testDateTime(MAX.plusSeconds(1))(of(0, 0, 0, 999999999))
    testDateTime(MAX.plusSeconds(60))(of(0, 0, 59, 999999999))
    testDateTime(MAX.plusSeconds(86400))(MAX)
    testDateTime(MAX.plusSeconds(Long.MaxValue))(of(15, 30, 6, 999999999))
  }

  @Test def test_plusNanos(): Unit = {
    testDateTime(MIN.plusNanos(Long.MinValue))(of(0, 12, 43, 145224192))
    testDateTime(MIN.plusNanos(-86400000000000L))(MIN)
    testDateTime(MIN.plusNanos(-1000000000))(of(23, 59, 59))
    testDateTime(MIN.plusNanos(-1))(MAX)
    testDateTime(MIN.plusNanos(0))(MIN)
    testDateTime(MIN.plusNanos(1))(of(0, 0, 0, 1))
    testDateTime(MIN.plusNanos(1000000000))(of(0, 0, 1))
    testDateTime(MIN.plusNanos(86400000000000L))(MIN)
    testDateTime(MIN.plusNanos(Long.MaxValue))(of(23, 47, 16, 854775807))
    testDateTime(MAX.plusNanos(Long.MinValue))(of(0, 12, 43, 145224191))
    testDateTime(MAX.plusNanos(-86400000000000L))(MAX)
    testDateTime(MAX.plusNanos(-1000000000))(of(23, 59, 58, 999999999))
    testDateTime(MAX.plusNanos(-1))(of(23, 59, 59, 999999998))
    testDateTime(MAX.plusNanos(0))(MAX)
    testDateTime(MAX.plusNanos(1))(MIN)
    testDateTime(MAX.plusNanos(1000000000))(of(0, 0, 0, 999999999))
    testDateTime(MAX.plusNanos(86400000000000L))(MAX)
    testDateTime(MAX.plusNanos(Long.MaxValue))(of(23, 47, 16, 854775806))
  }

  @Test def test_minusHours(): Unit = {
    testDateTime(MIN.minusHours(Long.MinValue))(of(8, 0))
    testDateTime(MIN.minusHours(-24))(MIN)
    testDateTime(MIN.minusHours(-1))(of(1, 0))
    testDateTime(MIN.minusHours(0))(MIN)
    testDateTime(MIN.minusHours(1))(of(23, 0))
    testDateTime(MIN.minusHours(24))(MIN)
    testDateTime(MIN.minusHours(Long.MaxValue))(of(17, 0))
    testDateTime(MAX.minusHours(Long.MinValue))(of(7, 59, 59, 999999999))
    testDateTime(MAX.minusHours(-24))(MAX)
    testDateTime(MAX.minusHours(-1))(of(0, 59, 59, 999999999))
    testDateTime(MAX.minusHours(0))(MAX)
    testDateTime(MAX.minusHours(1))(of(22, 59, 59, 999999999))
    testDateTime(MAX.minusHours(24))(MAX)
    testDateTime(MAX.minusHours(Long.MaxValue))(of(16, 59, 59, 999999999))
  }

  @Test def test_minusMinutes(): Unit = {
    testDateTime(MIN.minusMinutes(Long.MinValue))(of(18, 8))
    testDateTime(MIN.minusMinutes(-1440))(MIN)
    testDateTime(MIN.minusMinutes(-60))(of(1, 0))
    testDateTime(MIN.minusMinutes(-1))(of(0, 1))
    testDateTime(MIN.minusMinutes(0))(MIN)
    testDateTime(MIN.minusMinutes(1))(of(23, 59))
    testDateTime(MIN.minusMinutes(60))(of(23, 0))
    testDateTime(MIN.minusMinutes(1440))(MIN)
    testDateTime(MIN.minusMinutes(Long.MaxValue))(of(5, 53))
    testDateTime(MAX.minusMinutes(Long.MinValue))(of(18, 7, 59, 999999999))
    testDateTime(MAX.minusMinutes(-1440))(MAX)
    testDateTime(MAX.minusMinutes(-60))(of(0, 59, 59, 999999999))
    testDateTime(MAX.minusMinutes(-1))(of(0, 0, 59, 999999999))
    testDateTime(MAX.minusMinutes(0))(MAX)
    testDateTime(MAX.minusMinutes(1))(of(23, 58, 59, 999999999))
    testDateTime(MAX.minusMinutes(60))(of(22, 59, 59, 999999999))
    testDateTime(MAX.minusMinutes(1440))(MAX)
    testDateTime(MAX.minusMinutes(Long.MaxValue))(of(5, 52, 59, 999999999))
  }

  @Test def test_minusSeconds(): Unit = {
    testDateTime(MIN.minusSeconds(Long.MinValue))(of(15, 30, 8))
    testDateTime(MIN.minusSeconds(-86400))(MIN)
    testDateTime(MIN.minusSeconds(-60))(of(0, 1))
    testDateTime(MIN.minusSeconds(-1))(of(0, 0, 1))
    testDateTime(MIN.minusSeconds(0))(MIN)
    testDateTime(MIN.minusSeconds(1))(of(23, 59, 59))
    testDateTime(MIN.minusSeconds(60))(of(23, 59))
    testDateTime(MIN.minusSeconds(86400))(MIN)
    testDateTime(MIN.minusSeconds(Long.MaxValue))(of(8, 29, 53))
    testDateTime(MAX.minusSeconds(Long.MinValue))(of(15, 30, 7, 999999999))
    testDateTime(MAX.minusSeconds(-86400))(MAX)
    testDateTime(MAX.minusSeconds(-60))(of(0, 0, 59, 999999999))
    testDateTime(MAX.minusSeconds(-1))(of(0, 0, 0, 999999999))
    testDateTime(MAX.minusSeconds(0))(MAX)
    testDateTime(MAX.minusSeconds(1))(of(23, 59, 58, 999999999))
    testDateTime(MAX.minusSeconds(60))(of(23, 58, 59, 999999999))
    testDateTime(MAX.minusSeconds(86400))(MAX)
    testDateTime(MAX.minusSeconds(Long.MaxValue))(of(8, 29, 52, 999999999))
  }

  @Test def test_minusNanos(): Unit = {
    testDateTime(MIN.minusNanos(Long.MinValue))(of(23, 47, 16, 854775808))
    testDateTime(MIN.minusNanos(-86400000000000L))(MIN)
    testDateTime(MIN.minusNanos(-1000000000))(of(0, 0, 1))
    testDateTime(MIN.minusNanos(-1))(of(0, 0, 0, 1))
    testDateTime(MIN.minusNanos(0))(MIN)
    testDateTime(MIN.minusNanos(1))(MAX)
    testDateTime(MIN.minusNanos(1000000000))(of(23, 59, 59))
    testDateTime(MIN.minusNanos(86400000000000L))(MIN)
    testDateTime(MIN.minusNanos(Long.MaxValue))(of(0, 12, 43, 145224193))
    testDateTime(MAX.minusNanos(Long.MinValue))(of(23, 47, 16, 854775807))
    testDateTime(MAX.minusNanos(-86400000000000L))(MAX)
    testDateTime(MAX.minusNanos(-1000000000))(of(0, 0, 0, 999999999))
    testDateTime(MAX.minusNanos(-1))(MIN)
    testDateTime(MAX.minusNanos(0))(MAX)
    testDateTime(MAX.minusNanos(1))(of(23, 59, 59, 999999998))
    testDateTime(MAX.minusNanos(1000000000))(of(23, 59, 58, 999999999))
    testDateTime(MAX.minusNanos(86400000000000L))(MAX)
    testDateTime(MAX.minusNanos(Long.MaxValue))(of(0, 12, 43, 145224192))
  }

  @Test def test_adjustInto(): Unit = {
    for {
      t1 <- samples
      t2 <- samples
    } {
      testDateTime(t1.adjustInto(t2))(t1)
    }

    val ds = Seq(LocalDate.MIN, LocalDate.MAX)
    for {
      t <- samples
      d <- ds
    } {
      expectThrows(classOf[DateTimeException], t.adjustInto(d))
    }
  }

  @Test def test_until(): Unit = {
    assertEquals(86399999999999L, MIN.until(MAX, NANOS))
    assertEquals(86399999999L, MIN.until(MAX, MICROS))
    assertEquals(86399999L, MIN.until(MAX, MILLIS))
    assertEquals(86399L, MIN.until(MAX, SECONDS) )
    assertEquals(1439L, MIN.until(MAX, MINUTES))
    assertEquals(23L, MIN.until(MAX, HOURS))
    assertEquals(1L, MIN.until(MAX, HALF_DAYS))

    for (u <- timeBasedUnits) {
      assertEquals(-MIN.until(MAX, u), MAX.until(MIN, u))
      assertEquals(0L, MIN.until(MIN, u))
      assertEquals(0L, MAX.until(MAX, u))
    }
  }

  @Test def test_toSecondOfDay(): Unit = {
    assertEquals(0, MIN.toSecondOfDay)
    assertEquals(86399, MAX.toSecondOfDay)
  }

  @Test def test_toNanoOfDay(): Unit = {
    assertEquals(0L, MIN.toNanoOfDay)
    assertEquals(86399999999999L, MAX.toNanoOfDay)
  }

  @Test def test_compareTo(): Unit = {
    assertEquals(0, MIN.compareTo(MIN))
    assertTrue(MIN.compareTo(MAX) < 0)
    assertTrue(MAX.compareTo(MIN) > 0)
    assertEquals(0, MAX.compareTo(MAX))
  }

  @Test def test_isAfter(): Unit = {
    assertFalse(MIN.isAfter(MIN))
    assertFalse(MIN.isAfter(MAX))
    assertTrue(MAX.isAfter(MIN))
    assertFalse(MAX.isAfter(MAX))
  }

  @Test def test_isBefore(): Unit = {
    assertFalse(MIN.isBefore(MIN))
    assertTrue(MIN.isBefore(MAX))
    assertFalse(MAX.isBefore(MIN))
    assertFalse(MAX.isBefore(MAX))
  }

  @Test def test_toString(): Unit = {
    assertEquals("00:00", MIN.toString)
    assertEquals("23:59:59.999999999", MAX.toString)
    assertEquals("01:01", of(1, 1).toString)
    assertEquals("01:01:01", of(1, 1, 1).toString)
    assertEquals("01:01:01.000000001", of(1, 1, 1, 1).toString)
    assertEquals("01:01:01.100", of(1, 1, 1, 100000000).toString)
    assertEquals("01:01:01.100100", of(1, 1, 1, 100100000).toString)
    assertEquals("01:01:01.100100100", of(1, 1, 1, 100100100).toString)
  }

  @Test def test_now(): Unit = {
    assertNotNull(now())
  }

  @Test def test_of(): Unit = {
    testDateTime(of(0, 0))(MIN)
    testDateTime(of(0, 0, 0))(MIN)
    testDateTime(of(0, 0, 0, 0))(MIN)
    testDateTime(of(23, 59))(of(23, 59, 0, 0))
    testDateTime(of(23, 59, 59))(of(23, 59, 59, 0))
    testDateTime(of(23, 59, 59, 999999999))(MAX)

    expectThrows(classOf[DateTimeException], of(-1, 0))
    expectThrows(classOf[DateTimeException], of(0, -1))
    expectThrows(classOf[DateTimeException], of(0, 0, -1))
    expectThrows(classOf[DateTimeException], of(0, 0, 0, -1))
    expectThrows(classOf[DateTimeException], of(24, 0))
    expectThrows(classOf[DateTimeException], of(0, 60))
    expectThrows(classOf[DateTimeException], of(0, 0, 60))
    expectThrows(classOf[DateTimeException], of(0, 0, 0, 1000000000))
  }

  @Test def test_ofSecondOfDay(): Unit = {
    testDateTime(ofSecondOfDay(0))(MIN)
    testDateTime(ofSecondOfDay(1))(of(0, 0, 1))
    testDateTime(ofSecondOfDay(60))(of(0, 1))
    testDateTime(ofSecondOfDay(86399))(of(23, 59, 59))

    expectThrows(classOf[DateTimeException], ofSecondOfDay(-1))
    expectThrows(classOf[DateTimeException], ofSecondOfDay(86400))
  }

  @Test def test_ofNanoOfDay(): Unit = {
    testDateTime(ofNanoOfDay(0))(MIN)
    testDateTime(ofNanoOfDay(1))(of(0, 0, 0, 1))
    testDateTime(ofNanoOfDay(1000000000))(of(0, 0, 1))
    testDateTime(ofNanoOfDay(86399999999999L))(MAX)

    expectThrows(classOf[DateTimeException], ofNanoOfDay(-1))
    expectThrows(classOf[DateTimeException], ofNanoOfDay(86400000000000L))
  }

  @Test def test_from(): Unit = {
    for (t <- samples)
      testDateTime(from(t))(t)

    expectThrows(classOf[DateTimeException], from(LocalDate.of(2012, 2, 29)))
    expectThrows(classOf[DateTimeException], from(Month.JANUARY))
    expectThrows(classOf[DateTimeException], from(DayOfWeek.MONDAY))
  }
}
