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

/** A universal `Comparator` using the *natural ordering* of the elements.
 *
 *  A number of JDK APIs accept a possibly-null `Comparator`, and use the
 *  *natural ordering* of elements when it is `null`. This universal comparator
 *  can be used internally instead of systematically testing for `null`,
 *  simplifying code paths.
 *
 *  The `compare()` method of this comparator throws `ClassCastException`s when
 *  used with values that cannot be compared using natural ordering, assuming
 *  Scala.js is configured with compliant `asInstanceOf`s. The behavior is
 *  otherwise undefined.
 */
private[util] object NaturalComparator extends Comparator[Any] {
  def compare(o1: Any, o2: Any): Int =
    o1.asInstanceOf[Comparable[Any]].compareTo(o2)

  /** Selects the given comparator if it is non-null, otherwise the natural
   *  comparator.
   */
  def select[A](comparator: Comparator[A]): Comparator[_ >: A] =
    if (comparator eq null) this
    else comparator

  /** Unselects the given comparator, returning `null` if it was the natural
   *  comparator.
   *
   *  This method is useful to re-expose to a public API an internal comparator
   *  that was obtained with `select()`.
   */
  def unselect[A](comparator: Comparator[A]): Comparator[A] =
    if (comparator eq this) null
    else comparator
}
