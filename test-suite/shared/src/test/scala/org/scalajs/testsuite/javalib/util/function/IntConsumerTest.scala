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

import java.util.function.IntConsumer

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows._

class IntConsumerTest {
  import IntConsumerTest._

  @Test def accept(): Unit = {
    // Side-effects
    var current = 0
    val add: IntConsumer = makeConsumer(num => current += num)

    add.accept(1)
    assertEquals(1, current)

    add.accept(2)
    assertEquals(3, current)
  }

  @Test def andThen(): Unit = {
    // Side-effects
    var current = 0
    val add = makeConsumer(num => current += num)
    val multiply = makeConsumer(num => current *= num)
    val addAndMultiply: IntConsumer = add.andThen(multiply)

    addAndMultiply.accept(2)
    assertEquals(4, current)

    addAndMultiply.accept(3)
    assertEquals(21, current)

    // Sequential operations
    val throwingConsumer =
      makeConsumer(x => throw new ThrowingConsumerException(x))
    val dontCallConsumer =
      makeConsumer(x => throw new AssertionError(s"dontCallConsumer.accept($x)"))

    assertThrows(classOf[ThrowingConsumerException],
        throwingConsumer.andThen(dontCallConsumer).accept(0))

    assertThrows(classOf[ThrowingConsumerException],
        add.andThen(throwingConsumer).accept(1))
    assertEquals(22, current)
  }
}

object IntConsumerTest {
  final class ThrowingConsumerException(x: Any)
      extends Exception(s"throwing consumer called with $x")

  def makeConsumer(f: Int => Unit): IntConsumer = {
    new IntConsumer {
      def accept(t: Int): Unit = f(t)
    }
  }
}
