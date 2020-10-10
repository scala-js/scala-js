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
trait DoubleConsumer { self =>
  def accept(value: Double): Unit

  @JavaDefaultMethod
  def andThen(after: DoubleConsumer): DoubleConsumer = {
    new DoubleConsumer {
      def accept(value: Double): Unit = {
        self.accept(value)
        after.accept(value)
      }
    }
  }
}
