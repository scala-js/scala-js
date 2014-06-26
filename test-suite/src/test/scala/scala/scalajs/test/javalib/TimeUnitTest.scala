/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package javalib

import scala.scalajs.js
import scala.scalajs.test.JasmineTest
import java.util.concurrent.TimeUnit

object TimeUnitTest extends JasmineTest {

  describe("java.util.concurrent.TimeUnit") {

    it("should respond to `toNanos`") {
      expect(TimeUnit.NANOSECONDS.toNanos(42L)).toEqual(42L)
      expect(TimeUnit.MICROSECONDS.toNanos(42L)).toEqual(42000L)
      expect(TimeUnit.MILLISECONDS.toNanos(42L)).toEqual(42000000L)
      expect(TimeUnit.SECONDS.toNanos(42L)).toEqual(42000000000L)
      expect(TimeUnit.MINUTES.toNanos(42L)).toEqual(2520000000000L)
      expect(TimeUnit.HOURS.toNanos(42L)).toEqual(151200000000000L)
      expect(TimeUnit.DAYS.toNanos(42L)).toEqual(3628800000000000L)
    }

    it("should respond to `toMicros`") {
      expect(TimeUnit.NANOSECONDS.toMicros(42L)).toEqual(0L)
      expect(TimeUnit.NANOSECONDS.toMicros(42123L)).toEqual(42L)
      expect(TimeUnit.MICROSECONDS.toMicros(42L)).toEqual(42L)
      expect(TimeUnit.MILLISECONDS.toMicros(42L)).toEqual(42000L)
      expect(TimeUnit.SECONDS.toMicros(42L)).toEqual(42000000L)
      expect(TimeUnit.MINUTES.toMicros(42L)).toEqual(2520000000L)
      expect(TimeUnit.HOURS.toMicros(42L)).toEqual(151200000000L)
      expect(TimeUnit.DAYS.toMicros(42L)).toEqual(3628800000000L)
    }

    it("should respond to `toMillis`") {
      expect(TimeUnit.NANOSECONDS.toMillis(42L)).toEqual(0L)
      expect(TimeUnit.NANOSECONDS.toMillis(42000123L)).toEqual(42L)
      expect(TimeUnit.MICROSECONDS.toMillis(42L)).toEqual(0L)
      expect(TimeUnit.MICROSECONDS.toMillis(42123L)).toEqual(42L)
      expect(TimeUnit.MILLISECONDS.toMillis(42L)).toEqual(42L)
      expect(TimeUnit.SECONDS.toMillis(42L)).toEqual(42000L)
      expect(TimeUnit.MINUTES.toMillis(42L)).toEqual(2520000L)
      expect(TimeUnit.HOURS.toMillis(42L)).toEqual(151200000L)
      expect(TimeUnit.DAYS.toMillis(42L)).toEqual(3628800000L)
    }

    it("should respond to `toSeconds`") {
      expect(TimeUnit.NANOSECONDS.toSeconds(42L)).toEqual(0L)
      expect(TimeUnit.NANOSECONDS.toSeconds(42000000123L)).toEqual(42L)
      expect(TimeUnit.MICROSECONDS.toSeconds(42L)).toEqual(0L)
      expect(TimeUnit.MICROSECONDS.toSeconds(42000123L)).toEqual(42L)
      expect(TimeUnit.MILLISECONDS.toSeconds(42L)).toEqual(0L)
      expect(TimeUnit.MILLISECONDS.toSeconds(42123L)).toEqual(42L)
      expect(TimeUnit.SECONDS.toSeconds(42L)).toEqual(42L)
      expect(TimeUnit.MINUTES.toSeconds(42L)).toEqual(2520L)
      expect(TimeUnit.HOURS.toSeconds(42L)).toEqual(151200L)
      expect(TimeUnit.DAYS.toSeconds(42L)).toEqual(3628800L)
    }

    it("should respond to `toMinutes`") {
      expect(TimeUnit.NANOSECONDS.toMinutes(42L)).toEqual(0L)
      expect(TimeUnit.NANOSECONDS.toMinutes(2520000007380L)).toEqual(42L)
      expect(TimeUnit.MICROSECONDS.toMinutes(42L)).toEqual(0L)
      expect(TimeUnit.MICROSECONDS.toMinutes(2520007380L)).toEqual(42L)
      expect(TimeUnit.MILLISECONDS.toMinutes(42L)).toEqual(0L)
      expect(TimeUnit.MILLISECONDS.toMinutes(2520738L)).toEqual(42L)
      expect(TimeUnit.SECONDS.toMinutes(42L)).toEqual(0L)
      expect(TimeUnit.SECONDS.toMinutes(2520L)).toEqual(42L)
      expect(TimeUnit.MINUTES.toMinutes(42L)).toEqual(42L)
      expect(TimeUnit.HOURS.toMinutes(42L)).toEqual(2520L)
      expect(TimeUnit.DAYS.toMinutes(42L)).toEqual(60480L)
    }

    it("should respond to `toHours`") {
      expect(TimeUnit.NANOSECONDS.toHours(42L)).toEqual(0L)
      expect(TimeUnit.NANOSECONDS.toHours(151200000442800L)).toEqual(42L)
      expect(TimeUnit.MICROSECONDS.toHours(42L)).toEqual(0L)
      expect(TimeUnit.MICROSECONDS.toHours(151200442800L)).toEqual(42L)
      expect(TimeUnit.MILLISECONDS.toHours(42L)).toEqual(0L)
      expect(TimeUnit.MILLISECONDS.toHours(151244280L)).toEqual(42L)
      expect(TimeUnit.SECONDS.toHours(42L)).toEqual(0L)
      expect(TimeUnit.SECONDS.toHours(151200L)).toEqual(42L)
      expect(TimeUnit.MINUTES.toHours(42L)).toEqual(0L)
      expect(TimeUnit.MINUTES.toHours(2520L)).toEqual(42L)
      expect(TimeUnit.HOURS.toHours(42L)).toEqual(42L)
      expect(TimeUnit.DAYS.toHours(42L)).toEqual(1008L)
    }

    it("should respond to `toDays`") {
      expect(TimeUnit.NANOSECONDS.toDays(42L)).toEqual(0L)
      expect(TimeUnit.NANOSECONDS.toDays(3628800010627200L)).toEqual(42L)
      expect(TimeUnit.MICROSECONDS.toDays(42L)).toEqual(0L)
      expect(TimeUnit.MICROSECONDS.toDays(3628810627200L)).toEqual(42L)
      expect(TimeUnit.MILLISECONDS.toDays(42L)).toEqual(0L)
      expect(TimeUnit.MILLISECONDS.toDays(3629862720L)).toEqual(42L)
      expect(TimeUnit.SECONDS.toDays(42L)).toEqual(0L)
      expect(TimeUnit.SECONDS.toDays(3628800L)).toEqual(42L)
      expect(TimeUnit.MINUTES.toDays(42L)).toEqual(0L)
      expect(TimeUnit.MINUTES.toDays(60480L)).toEqual(42L)
      expect(TimeUnit.HOURS.toDays(42L)).toEqual(1L)
      expect(TimeUnit.HOURS.toDays(1008L)).toEqual(42L)
      expect(TimeUnit.DAYS.toDays(42L)).toEqual(42L)
    }

    it("should respond to `values`") {
      val values = TimeUnit.values()
      expect(values.length).toEqual(7)
      expectTimeUnit(values(0), TimeUnit.NANOSECONDS)
      expectTimeUnit(values(1), TimeUnit.MICROSECONDS)
      expectTimeUnit(values(2), TimeUnit.MILLISECONDS)
      expectTimeUnit(values(3), TimeUnit.SECONDS)
      expectTimeUnit(values(4), TimeUnit.MINUTES)
      expectTimeUnit(values(5), TimeUnit.HOURS)
      expectTimeUnit(values(6), TimeUnit.DAYS)
    }

    it("should respond to `valueOf`") {
      expectTimeUnit(TimeUnit.valueOf("NANOSECONDS"), TimeUnit.NANOSECONDS)
      expectTimeUnit(TimeUnit.valueOf("MICROSECONDS"), TimeUnit.MICROSECONDS)
      expectTimeUnit(TimeUnit.valueOf("MILLISECONDS"), TimeUnit.MILLISECONDS)
      expectTimeUnit(TimeUnit.valueOf("SECONDS"), TimeUnit.SECONDS)
      expectTimeUnit(TimeUnit.valueOf("MINUTES"), TimeUnit.MINUTES)
      expectTimeUnit(TimeUnit.valueOf("HOURS"), TimeUnit.HOURS)
      expectTimeUnit(TimeUnit.valueOf("DAYS"), TimeUnit.DAYS)
    }

  }

  def expectTimeUnit(actual: TimeUnit, expected: TimeUnit): Unit =
    expect(actual.asInstanceOf[js.Any]).toEqual(expected.asInstanceOf[js.Any])
}
