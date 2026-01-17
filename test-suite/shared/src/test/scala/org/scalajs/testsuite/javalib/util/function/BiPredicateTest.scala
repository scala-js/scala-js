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

import java.util.function.BiPredicate

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows.assertThrows

class BiPredicateTest {
  import BiPredicateTest._

  @Test def and(): Unit = {
    val evenAndInRange = largerThan10LessThan30.and(even)

    // Truth table
    assertTrue("First true, Second true", evenAndInRange.test(22, 22))
    assertFalse("First true, Second false", evenAndInRange.test(41, 22))
    assertFalse("First false, Second true", evenAndInRange.test(6, 6))
    assertFalse("First false, Second false", evenAndInRange.test(21, 42))

    assertFalse(
        "Short-circuit: First false", largerThan10LessThan30.and(dontCallPredicate).test(5, 5))
    assertThrows(
        classOf[ThrowingPredicateException], throwingPredicate.and(dontCallPredicate).test(5, 5))
  }

  @Test def negate(): Unit = {
    val notLeftLargerThan10AndRightLessThan30 = largerThan10LessThan30.negate()

    // Truth table
    assertFalse("First true, Second true", notLeftLargerThan10AndRightLessThan30.test(40, 20))
    assertTrue("First true, Second false", notLeftLargerThan10AndRightLessThan30.test(20, 40))
    assertTrue("First false, Second true", notLeftLargerThan10AndRightLessThan30.test(5, 20))
    assertTrue("First false, Second false", notLeftLargerThan10AndRightLessThan30.test(5, 40))

    assertThrows(classOf[ThrowingPredicateException], throwingPredicate.negate().test(5, 5))
  }

  @Test def or(): Unit = {
    val evenOrLargerThan10 = largerThan10LessThan30.or(even)

    // Truth table
    assertTrue("First true, Second true", evenOrLargerThan10.test(40, 20))
    assertTrue("First true, Second false", evenOrLargerThan10.test(42, 21))
    assertTrue("First false, Second true", evenOrLargerThan10.test(6, 42))
    assertFalse("First false, Second false", evenOrLargerThan10.test(5, 21))

    assertTrue(
        "Short-circuit: First false", largerThan10LessThan30.or(dontCallPredicate).test(42, 22))
    assertThrows(
        classOf[ThrowingPredicateException], throwingPredicate.or(dontCallPredicate).test(42, 22))
  }
}

object BiPredicateTest {
  final class ThrowingPredicateException(x: Any)
      extends Exception(s"throwing predicate called with $x")

  private val largerThan10LessThan30: BiPredicate[Int, Int] = makeBiPredicate { (t, u) =>
    t > 10 && u < 30
  }

  private val even: BiPredicate[Int, Int] = makeBiPredicate((t, u) => isEven(t) && isEven(u))

  private val throwingPredicate: BiPredicate[Int, Int] = makeBiPredicate { (t, _) =>
    throw new ThrowingPredicateException(t)
  }

  private val dontCallPredicate: BiPredicate[Int, Int] = makeBiPredicate { (t, u) =>
    throw new AssertionError(s"dontCallPredicate.test($t, $u)")
  }

  private[this] def makeBiPredicate[T, U](f: (T, U) => Boolean): BiPredicate[T, U] = {
    new BiPredicate[T, U] {
      def test(t: T, u: U): Boolean = f(t, u)
    }
  }

  @inline private def isEven(i: Int): Boolean = i % 2 == 0
}
