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

import java.util.function.DoubleConsumer

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows._

class DoubleConsumerTest {
  import DoubleConsumerTest._

  @Test def accept(): Unit = {
    // Side-effects
    var current = 0.0d
    val add: DoubleConsumer = makeConsumer(num => current += num)

    add.accept(1.234d)
    assertEquals(1.234d, current, 0.0d)

    add.accept(2.345d)
    assertEquals(3.579d, current, 0.0d)
  }

  @Test def andThen(): Unit = {
    // Side-effects
    var current = 0.0d
    val add = makeConsumer(num => current += num)
    val multiply = makeConsumer(num => current *= num)
    val addAndMultiply: DoubleConsumer = add.andThen(multiply)

    addAndMultiply.accept(2.0d)
    assertEquals(4.0d, current, 0.0d)

    addAndMultiply.accept(3.0d)
    assertEquals(21.0d, current, 0.0d)

    // Sequential operations
    val throwingConsumer =
      makeConsumer(x => throw new ThrowingConsumerException(x))
    val dontCallConsumer =
      makeConsumer(x => throw new AssertionError(s"dontCallConsumer.accept($x)"))

    assertThrows(classOf[ThrowingConsumerException],
        throwingConsumer.andThen(dontCallConsumer).accept(0.0d))

    assertThrows(classOf[ThrowingConsumerException],
        add.andThen(throwingConsumer).accept(1.0d))
    assertEquals(22.0d, current, 0.0d)
  }
}

object DoubleConsumerTest {
  final class ThrowingConsumerException(x: Any)
      extends Exception(s"throwing consumer called with $x")

  def makeConsumer(f: Double => Unit): DoubleConsumer = {
    new DoubleConsumer {
      def accept(t: Double): Unit = f(t)
    }
  }
}
