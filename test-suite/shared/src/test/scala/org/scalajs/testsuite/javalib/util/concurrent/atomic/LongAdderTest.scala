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

package org.scalajs.testsuite.javalib.util.concurrent.atomic

import org.junit.Test
import org.junit.Assert._

class LongAdderTest {

  @Test def longAdderIncrementTest(): Unit = {
    val value = new java.util.concurrent.atomic.LongAdder
    assertEquals(0L, value.sum())
    value.increment()
    assertEquals(1L, value.sum())
    value.increment()
    assertEquals(2L, value.sum())
  }

  @Test def longAdderDecrementTest(): Unit = {
    val value = new java.util.concurrent.atomic.LongAdder
    assertEquals(0L, value.sum())
    value.increment()
    value.increment()
    value.increment()
    assertEquals(3L, value.sum())
    value.decrement()
    assertEquals(2L, value.sum())
    value.decrement()
    assertEquals(1L, value.sum())
  }

  @Test def longAdderLongValue(): Unit = {
    val value = new java.util.concurrent.atomic.LongAdder
    value.add(100L)
    assertEquals(100L, value.longValue())
    value.add(100L)
    assertEquals(200L, value.longValue())
  }

  @Test def longAdderIntValue(): Unit = {
    val value = new java.util.concurrent.atomic.LongAdder
    value.add(10)
    assertEquals(10, value.intValue())
    value.add(10)
    assertEquals(20, value.intValue())
  }

  @Test def longAdderFloatValue(): Unit = {
    val value = new java.util.concurrent.atomic.LongAdder
    value.add(10)
    assertEquals(10f, value.floatValue(), 0)
    value.add(10)
    assertEquals(20f, value.floatValue(), 0)
  }

  @Test def longAdderDoubleValue(): Unit = {
    val value = new java.util.concurrent.atomic.LongAdder
    value.add(10)
    assertEquals(10d, value.doubleValue(), 0)
    value.add(10)
    assertEquals(20d, value.doubleValue(), 0)
  }

  @Test def longAdderReset(): Unit = {
    val value = new java.util.concurrent.atomic.LongAdder
    value.add(10)
    assertEquals(10, value.sum())
    value.reset()
    assertEquals(0, value.sum())
  }

  @Test def longAdderAdd(): Unit = {
    val value = new java.util.concurrent.atomic.LongAdder
    value.add(10)
    assertEquals(10, value.sum())
    value.add(0)
    assertEquals(10, value.sum())
  }

  @Test def longAdderSumThenReset(): Unit = {
    val value = new java.util.concurrent.atomic.LongAdder
    value.add(10)
    val res = value.sumThenReset()
    assertEquals(0, value.sum())
    assertEquals(10, res)
  }

  @Test def longAdderToString(): Unit = {
    val value = new java.util.concurrent.atomic.LongAdder
    value.add(10)
    assertEquals("10", value.toString())
  }
}
