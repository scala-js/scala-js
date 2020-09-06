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

trait BiPredicate[T, U] {
  def test(t: T, u: U): Boolean

  @JavaDefaultMethod
  def and(other: BiPredicate[_ >: T, _ >: U]): BiPredicate[T, U] = { (t: T, u: U) =>
    test(t, u) && other.test(t, u)
  }

  @JavaDefaultMethod
  def negate(): BiPredicate[T, U] = (t: T, u: U) => !test(t, u)

  @JavaDefaultMethod
  def or(other: BiPredicate[_ >: T, _ >: U]): BiPredicate[T, U] = { (t: T, u: U) =>
    test(t, u) || other.test(t, u)
  }
}
