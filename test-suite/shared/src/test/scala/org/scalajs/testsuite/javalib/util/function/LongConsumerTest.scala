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

package org.scalajs.testsuite.javalib.util.function

import java.util.function.LongConsumer

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows._

class LongConsumerTest {
  import LongConsumerTest._

  @Test def accept(): Unit = {
    // Side-effects
    var current = 0L
    val add: LongConsumer = makeConsumer(num => current += num)

    add.accept(1L)
    assertEquals(1L, current)

    add.accept(2L)
    assertEquals(3L, current)
  }

  @Test def andThen(): Unit = {
    // Side-effects
    var current = 0L
    val add = makeConsumer(num => current += num)
    val multiply = makeConsumer(num => current *= num)
    val addAndMultiply: LongConsumer = add.andThen(multiply)

    addAndMultiply.accept(2L)
    assertEquals(4L, current)

    addAndMultiply.accept(3L)
    assertEquals(21L, current)

    // Sequential operations
    val throwingConsumer =
      makeConsumer(x => throw new ThrowingConsumerException(x))
    val dontCallConsumer =
      makeConsumer(x => throw new AssertionError(s"dontCallConsumer.accept($x)"))

    assertThrows(classOf[ThrowingConsumerException],
        throwingConsumer.andThen(dontCallConsumer).accept(0L))

    assertThrows(classOf[ThrowingConsumerException],
        add.andThen(throwingConsumer).accept(1L))
    assertEquals(22L, current)
  }
}

object LongConsumerTest {
  final class ThrowingConsumerException(x: Any)
      extends Exception(s"throwing consumer called with $x")

  def makeConsumer(f: Long => Unit): LongConsumer = {
    new LongConsumer {
      def accept(t: Long): Unit = f(t)
    }
  }
}
