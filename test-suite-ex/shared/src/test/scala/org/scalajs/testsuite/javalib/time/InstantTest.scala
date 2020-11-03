/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.testsuite.javalib.time

import java.time.{DateTimeException, Instant}

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._

/** Sanity tests for the dummy implemenation of `java.time.Instant`.
 *
 *  These tests ensure that our dummy implementation of `java.time.Instant`
 *  behave in an appropriate way. We only test specific behaviors that can
 *  cause tests to "fail to fail" if they are not respected.
 */
class InstantTest {
  @Test def testOfEpochSecond(): Unit = {
    def test(epochSecond: Long, nano: Int): Unit = {
      val instant = Instant.ofEpochSecond(epochSecond, nano)
      assertEquals(epochSecond, instant.getEpochSecond())
      assertEquals(nano, instant.getNano())
    }

    test(123L, 456000000)
    test(123L, 456987654)
    test(8640000000000L, 1000000)
    test(-8640000000001L, 999000000)
    test(-8640000000001L, 999123456)
    test(-31557014167219200L, 0) // smallest allowed value
    test(31556889864403199L, 999999999) // biggest allowed value

    def testException(epochSecond: Long, nano: Int): Unit = {
      assertThrows(classOf[DateTimeException], Instant.ofEpochSecond(epochSecond, nano))
    }

    testException(-31557014167219201L, 999999999) // 1ns before the smallest allowed value
    testException(-1234567891011121314L, 123456789)
    testException(Long.MinValue, 0)
    testException(31556889864403200L, 0) // 1ns after the biggest allowed value
    testException(1234567891011121314L, 123456789)
    testException(Long.MaxValue, 0)
  }

  @Test def testToEpochMilli(): Unit = {
    /* Tests that `toEpochMilli()` behaves appropriate, and in particular that
     * it throws `ArithmeticException`s, not other kinds of exceptions.
     *
     * If it threw `IllegalArgumentException`s, a naive implementation of
     * `java.util.Date.from(Instant)` could get away with not catching the
     * `ArithmeticException` to turn it into an `IllegalArgumentException`.
     */

    def test(expected: Long, epochSecond: Long, nano: Int): Unit = {
      assertEquals(expected, Instant.ofEpochSecond(epochSecond, nano).toEpochMilli())
    }

    test(123456L, 123L, 456000000)
    test(123456L, 123L, 456987654)
    test(8640000000000001L, 8640000000000L, 1000000)
    test(-8640000000000001L, -8640000000001L, 999000000)
    test(-8640000000000001L, -8640000000001L, 999123456)
    test(Long.MinValue, -9223372036854776L, 192000000)
    test(Long.MaxValue, 9223372036854775L, 807000000)
    test(Long.MaxValue, 9223372036854775L, 807999999)

    def testException(epochSecond: Long, nano: Int): Unit = {
      val instant = Instant.ofEpochSecond(epochSecond, nano) // does not throw
      assertThrows(classOf[ArithmeticException], instant.toEpochMilli())
    }

    testException(-9223372036854776L, 191999999)
    testException(-9223372036854777L, 999999999)
    testException(9223372036854775L, 808000000)
    testException(9223372036854776L, 0)
  }
}
