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

trait Function[T, R] {
  def apply(t: T): R

  def andThen[V](after: Function[_ >: R, _ <: V]): Function[T, V] = { (t: T) =>
    after.apply(apply(t))
  }

  def compose[V](before: Function[_ >: V, _ <: T]): Function[V, R] = { (v: V) =>
    apply(before.apply(v))
  }
}

object Function {
  def identity[T](): Function[T, T] = (t: T) => t
}
