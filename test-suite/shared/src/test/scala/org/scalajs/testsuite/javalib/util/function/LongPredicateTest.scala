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

import java.util.function.LongPredicate

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows.assertThrows

class LongPredicateTest {
  import LongPredicateTest._

  private val largerThan10 = makePredicate(_ > 10L)
  private val even = makePredicate(_ % 2 == 0L)

  private val throwingPredicate =
    makePredicate(x => throw new ThrowingPredicateException(x))

  private val dontCallPredicate =
    makePredicate(x => throw new AssertionError(s"dontCallPredicate.test($x)"))

  @Test def and(): Unit = {
    // Truth table
    val evenAndLargerThan10 = largerThan10.and(even)
    assertTrue(evenAndLargerThan10.test(22L))
    assertFalse(evenAndLargerThan10.test(21L))
    assertFalse(evenAndLargerThan10.test(6L))
    assertFalse(evenAndLargerThan10.test(5L))

    // Short-circuit
    assertFalse(largerThan10.and(dontCallPredicate).test(5L))
    assertThrows(classOf[ThrowingPredicateException],
        throwingPredicate.and(dontCallPredicate).test(5L))
  }

  @Test def negate(): Unit = {
    // Truth table
    val notLargerThan10 = largerThan10.negate()
    assertTrue(notLargerThan10.test(5L))
    assertFalse(notLargerThan10.test(15L))
  }

  @Test def or(): Unit = {
    // Truth table
    val evenOrLargerThan10 = largerThan10.or(even)
    assertTrue(evenOrLargerThan10.test(22L))
    assertTrue(evenOrLargerThan10.test(21L))
    assertTrue(evenOrLargerThan10.test(6L))
    assertFalse(evenOrLargerThan10.test(5L))

    // Short-circuit
    assertTrue(largerThan10.or(dontCallPredicate).test(15L))
    assertThrows(classOf[ThrowingPredicateException],
        throwingPredicate.or(dontCallPredicate).test(15L))
  }
}

object LongPredicateTest {
  final class ThrowingPredicateException(x: Any)
      extends Exception(s"throwing predicate called with $x")

  def makePredicate(f: Long => Boolean): LongPredicate = {
    new LongPredicate {
      def test(value: Long): Boolean = f(value)
    }
  }
}
