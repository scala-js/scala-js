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
trait LongConsumer { self =>
  def accept(value: Long): Unit

  @JavaDefaultMethod
  def andThen(after: LongConsumer): LongConsumer = {
    new LongConsumer {
      def accept(value: Long): Unit = {
        self.accept(value)
        after.accept(value)
      }
    }
  }
}
