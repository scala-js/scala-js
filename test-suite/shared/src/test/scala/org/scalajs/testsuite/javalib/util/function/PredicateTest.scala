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

import java.util.function.Predicate

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows._

class PredicateTest {
  import PredicateTest._

  private val largerThan10 = makePredicate[Int](_ > 10)
  private val even = makePredicate[Int](_ % 2 == 0)

  private val throwingPredicate =
    makePredicate[Any](x => throw new ThrowingPredicateException(x))

  private val dontCallPredicate =
    makePredicate[Any](x => throw new AssertionError(s"dontCallPredicate.test($x)"))

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

  @Test def isEqual(): Unit = {
    val isEqualToPair = Predicate.isEqual[Any]((3, 4))
    assertTrue(isEqualToPair.test((3, 4)))
    assertFalse(isEqualToPair.test((5, 6)))
    assertFalse(isEqualToPair.test("foo"))
    assertFalse(isEqualToPair.test(null))

    val isEqualToNull = Predicate.isEqual[Any](null)
    assertFalse(isEqualToNull.test((3, 4)))
    assertFalse(isEqualToNull.test((5, 6)))
    assertFalse(isEqualToNull.test("foo"))
    assertTrue(isEqualToNull.test(null))
  }
}

object PredicateTest {
  final class ThrowingPredicateException(x: Any)
      extends Exception(s"throwing predicate called with $x")

  def makePredicate[T](f: T => Boolean): Predicate[T] = {
    new Predicate[T] {
      def test(t: T): Boolean = f(t)
    }
  }
}
