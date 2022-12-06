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
trait DoubleUnaryOperator {
  def applyAsDouble(operand: Double): Double

  def andThen(after: DoubleUnaryOperator): DoubleUnaryOperator = { (d: Double) =>
    after.applyAsDouble(applyAsDouble(d))
  }

  def compose(before: DoubleUnaryOperator): DoubleUnaryOperator = { (d: Double) =>
    applyAsDouble(before.applyAsDouble(d))
  }
}

object DoubleUnaryOperator {
  def identity(): DoubleUnaryOperator = (d: Double) => d
}
