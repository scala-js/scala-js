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
trait LongUnaryOperator {
  def applyAsLong(operand: Long): Long

  def andThen(after: LongUnaryOperator): LongUnaryOperator = { (l: Long) =>
    after.applyAsLong(applyAsLong(l))
  }

  def compose(before: LongUnaryOperator): LongUnaryOperator = { (l: Long) =>
    applyAsLong(before.applyAsLong(l))
  }
}

object LongUnaryOperator {
  def identity(): LongUnaryOperator = (l: Long) => l
}
