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

package java.util

// scalastyle:off equals.hash.code

trait Comparator[A] {
  def compare(o1: A, o2: A): Int
  def equals(obj: Any): Boolean

  def reversed(): Comparator[A] =
    Collections.reverseOrder(this)
}
