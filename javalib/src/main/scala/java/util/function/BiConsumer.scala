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

trait BiConsumer[T, U] {
  def accept(t: T, u: U): Unit

  @JavaDefaultMethod
  def andThen(after: BiConsumer[T, U]): BiConsumer[T, U] = { (t: T, u: U) =>
    accept(t, u)
    after.accept(t, u)
  }
}
