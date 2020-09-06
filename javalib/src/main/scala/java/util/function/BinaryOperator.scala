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

import java.util.Comparator

trait BinaryOperator[T] extends BiFunction[T, T, T]

object BinaryOperator {
  def minBy[T](comparator: Comparator[_ >: T]): BinaryOperator[T] = { (a: T, b: T) =>
    if (comparator.compare(a, b) <= 0) a
    else b
  }

  def maxBy[T](comparator: Comparator[_ >: T]): BinaryOperator[T] = { (a: T, b: T) =>
    if (comparator.compare(a, b) >= 0) a
    else b
  }
}
