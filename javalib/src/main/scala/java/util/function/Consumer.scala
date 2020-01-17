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
trait Consumer[T] { self =>
  def accept(t: T): Unit

  @JavaDefaultMethod
  def andThen(after: Consumer[_ >: T]): Consumer[T] = {
    new Consumer[T] {
      def accept(t: T): Unit = {
        self.accept(t)
        after.accept(t)
      }
    }
  }
}
