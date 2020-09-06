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

import java.util.function.BiConsumer

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows._

class BiConsumerTest {
  import BiConsumerTest._

  @Test def accept(): Unit = {
    // Side-effects
    var left: Int = 0
    var right: Int = 0

    val add: BiConsumer[Int, Int] = makeBiConsumer { (t, u) =>
      left += t
      right += u
    }

    add.accept(1, 2)
    assertEquals(1, left)
    assertEquals(2, right)

    add.accept(2,3)
    assertEquals(3, left)
    assertEquals(5, right)
  }

  @Test def andThen(): Unit = {
    // Side-effects
    var left: Int = 0
    var right: Int = 0

    val add: BiConsumer[Int, Int] = makeBiConsumer { (t, u) =>
      left += t
      right += u
    }

    val multiply: BiConsumer[Int, Int] = makeBiConsumer { (t, u) =>
      left *= t
      right *= u
    }

    val addAndMultiply = add.andThen(multiply)

    addAndMultiply.accept(2, 4)
    assertEquals(4, left)
    assertEquals(16, right)

    addAndMultiply.accept(3, 6)
    assertEquals(21, left) // (4+3) * 3
    assertEquals(132, right) // (16+6) * 6

    // Consume, then throw
    assertThrows(classOf[ThrowingConsumerException], add.andThen(throwingConsumer).accept(1,2))
    assertEquals(22, left)
    assertEquals(134, right)

    assertThrows(classOf[ThrowingConsumerException], throwingConsumer.andThen(dontCallConsumer).accept(0, 0))

  }
}

object BiConsumerTest {
  final class ThrowingConsumerException(x: Any, y: Any) extends Exception(s"throwing consumer called with ($x, $y)")

  private val throwingConsumer: BiConsumer[Int, Int] = makeBiConsumer { (t, u) =>
    throw new ThrowingConsumerException(t, u)
  }

  private val dontCallConsumer: BiConsumer[Int, Int] = makeBiConsumer { (t, u) =>
    throw new AssertionError(s"dontCallConsumer.accept($t, $u)")
  }

  def makeBiConsumer[T, U](f: (T, U) => Unit): BiConsumer[T, U] = {
    new BiConsumer[T, U] {
      def accept(t: T, u: U): Unit = f(t, u)
    }
  }
}
