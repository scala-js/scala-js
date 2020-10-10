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

import java.util.function.IntPredicate

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows._

class IntPredicateTest {
  import IntPredicateTest._

  private val largerThan10 = makePredicate(_ > 10)
  private val even = makePredicate(_ % 2 == 0)

  private val throwingPredicate =
    makePredicate(x => throw new ThrowingPredicateException(x))

  private val dontCallPredicate =
    makePredicate(x => throw new AssertionError(s"dontCallPredicate.test($x)"))

  @Test def and(): Unit = {
    // Truth table
    val evenAndLargerThan10 = largerThan10.and(even)
    assertTrue(evenAndLargerThan10.test(22))
    assertFalse(evenAndLargerThan10.test(21))
    assertFalse(evenAndLargerThan10.test(6))
    assertFalse(evenAndLargerThan10.test(5))

    // Short-circuit
    assertFalse(largerThan10.and(dontCallPredicate).test(5))
    assertThrows(classOf[ThrowingPredicateException],
        throwingPredicate.and(dontCallPredicate).test(5))
  }

  @Test def negate(): Unit = {
    // Truth table
    val notLargerThan10 = largerThan10.negate()
    assertTrue(notLargerThan10.test(5))
    assertFalse(notLargerThan10.test(15))
  }

  @Test def or(): Unit = {
    // Truth table
    val evenOrLargerThan10 = largerThan10.or(even)
    assertTrue(evenOrLargerThan10.test(22))
    assertTrue(evenOrLargerThan10.test(21))
    assertTrue(evenOrLargerThan10.test(6))
    assertFalse(evenOrLargerThan10.test(5))

    // Short-circuit
    assertTrue(largerThan10.or(dontCallPredicate).test(15))
    assertThrows(classOf[ThrowingPredicateException],
        throwingPredicate.or(dontCallPredicate).test(15))
  }
}

object IntPredicateTest {
  final class ThrowingPredicateException(x: Any)
      extends Exception(s"throwing predicate called with $x")

  def makePredicate(f: Int => Boolean): IntPredicate = {
    new IntPredicate {
      def test(value: Int): Boolean = f(value)
    }
  }
}
