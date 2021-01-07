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
trait IntUnaryOperator {
  def applyAsInt(operand: Int): Int

  @JavaDefaultMethod
  def andThen(after: IntUnaryOperator): IntUnaryOperator = { (i: Int) =>
    after.applyAsInt(applyAsInt(i))
  }

  @JavaDefaultMethod
  def compose(before: IntUnaryOperator): IntUnaryOperator = { (i: Int) =>
    applyAsInt(before.applyAsInt(i))
  }
}

object IntUnaryOperator {
  def identity(): IntUnaryOperator = (i: Int) => i
}
