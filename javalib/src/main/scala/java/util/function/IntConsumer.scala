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
trait IntConsumer {
  def accept(value: Int): Unit

  @JavaDefaultMethod
  def andThen(after: IntConsumer): IntConsumer = { (value: Int) =>
    this.accept(value)
    after.accept(value)
  }
}
