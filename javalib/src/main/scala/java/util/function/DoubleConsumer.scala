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
trait DoubleConsumer {
  def accept(value: Double): Unit

  def andThen(after: DoubleConsumer): DoubleConsumer = { (value: Double) =>
    this.accept(value)
    after.accept(value)
  }
}
