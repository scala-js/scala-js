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

package org.scalajs.testsuite.javalib.util

import java.util.Date
import java.time.Instant

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._

/** Additional tests for `java.util.Date` that require javalib extension
 *  dummies.
 */
class DateTestEx {
  @Test def testToInstant(): Unit = {
    def test(expectedEpochSecond: Long, expectedNano: Int, epochMilli: Long): Unit = {
      assertEquals(Instant.ofEpochSecond(expectedEpochSecond, expectedNano),
          new Date(epochMilli).toInstant())
    }

    test(123L, 456000000, 123456L)
    test(8640000000000L, 1000000, 8640000000000001L)
    test(-8640000000001L, 999000000, -8640000000000001L)
    test(-9223372036854776L, 192000000, Long.MinValue)
    test(9223372036854775L, 807000000, Long.MaxValue)
  }

  @Test def testFromInstant(): Unit = {
    def test(expectedTime: Long, epochSecond: Long, nano: Int): Unit = {
      assertEquals(new Date(expectedTime), Date.from(Instant.ofEpochSecond(epochSecond, nano)))
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
      assertThrows(classOf[IllegalArgumentException], Date.from(instant))
    }

    testException(-9223372036854776L, 191999999)
    testException(-9223372036854777L, 999999999)
    testException(9223372036854775L, 808000000)
    testException(9223372036854776L, 0)
  }
}
