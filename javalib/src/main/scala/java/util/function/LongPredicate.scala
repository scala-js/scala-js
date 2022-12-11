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

@FunctionalInterface
trait LongPredicate { self =>
  def test(t: Long): Boolean

  def and(other: LongPredicate): LongPredicate = {
    new LongPredicate {
      def test(value: Long): Boolean =
        self.test(value) && other.test(value) // the order and short-circuit are by-spec
    }
  }

  def negate(): LongPredicate = {
    new LongPredicate {
      def test(value: Long): Boolean =
        !self.test(value)
    }
  }

  def or(other: LongPredicate): LongPredicate = {
    new LongPredicate {
      def test(value: Long): Boolean =
        self.test(value) || other.test(value) // the order and short-circuit are by-spec
    }
  }
}
