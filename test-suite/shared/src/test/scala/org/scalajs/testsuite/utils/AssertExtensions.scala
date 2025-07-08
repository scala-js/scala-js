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

package org.scalajs.testsuite.utils

import org.junit.Assert._

object AssertExtensions {

  /** Asserts that two Double values are exactly equal.
   *
   *  Positive and negative zeros compare as *not* equal. `NaN` compares
   *  *equal* to itself.
   */
  @noinline
  def assertExactEquals(expected: Double, actual: Double): Unit =
    assertTrue(s"expected: $expected but was: $actual", expected.equals(actual))

  /** Asserts that two Double values are exactly equal.
   *
   *  Positive and negative zeros compare as *not* equal. `NaN` compares
   *  *equal* to itself.
   */
  @noinline
  def assertExactEquals(msg: String, expected: Double, actual: Double): Unit =
    assertTrue(s"$msg; expected: $expected but was: $actual", expected.equals(actual))

  /** Asserts that two Float values are exactly equal.
   *
   *  Positive and negative zeros compare as *not* equal. `NaN` compares
   *  *equal* to itself.
   */
  @noinline
  def assertExactEquals(expected: Float, actual: Float): Unit =
    assertTrue(s"expected: $expected but was: $actual", expected.equals(actual))

  /** Asserts that two Float values are exactly equal.
   *
   *  Positive and negative zeros compare as *not* equal. `NaN` compares
   *  *equal* to itself.
   */
  @noinline
  def assertExactEquals(msg: String, expected: Float, actual: Float): Unit =
    assertTrue(s"$msg; expected: $expected but was: $actual", expected.equals(actual))

}
