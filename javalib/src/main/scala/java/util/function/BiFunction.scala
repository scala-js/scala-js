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

trait BiFunction[T, U, R] {
  def apply(t: T, u: U): R

  def andThen[V](after: Function[_ >: R, _ <: V]): BiFunction[T, U, V] = { (t: T, u: U) =>
    after.apply(this.apply(t, u))
  }
}
