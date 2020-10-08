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

package java.util.function

import scala.scalajs.js.annotation.JavaDefaultMethod

@FunctionalInterface
trait IntPredicate { self =>
  def test(t: Int): Boolean

  @JavaDefaultMethod
  def and(other: IntPredicate): IntPredicate = {
    new IntPredicate {
      def test(value: Int): Boolean =
        self.test(value) && other.test(value) // the order and short-circuit are by-spec
    }
  }

  @JavaDefaultMethod
  def negate(): IntPredicate = {
    new IntPredicate {
      def test(value: Int): Boolean =
        !self.test(value)
    }
  }

  @JavaDefaultMethod
  def or(other: IntPredicate): IntPredicate = {
    new IntPredicate {
      def test(value: Int): Boolean =
        self.test(value) || other.test(value) // the order and short-circuit are by-spec
    }
  }
}
