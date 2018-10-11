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

import scala.collection.mutable

/** Make some Scala 2.13 APIs available in older Scala versions. */
private[util] object Compat {

  /** Adds methods from 2.13 to `SortedSet`.
   *
   *  The `to` operation has been renamed to `rangeTo` in 2.13, to not
   *  conflict with the `to` operation in `Iterable`.
   */
  implicit class SortedSetCompat[A](val __private_self: mutable.SortedSet[A])
      extends AnyVal {

    @inline private def self: mutable.SortedSet[A] = __private_self

    /* Note: the double implicit conversion trick does not work here because
     * there *is* a `to` method in 2.13 (but it takes a `Factory` as parameter)
     * so the second implicit conversion is never triggered.
     */
    def rangeTo(to: A): mutable.SortedSet[A] = {
      // Implementation copied from 2.12's implementation
      val i = self.rangeFrom(to).iterator
      if (i.isEmpty) {
        self
      } else {
        val next = i.next()
        if (self.ordering.compare(next, to) == 0) {
          if (i.isEmpty) self
          else self.rangeUntil(i.next())
        } else {
          self.rangeUntil(next)
        }
      }
    }

    /* Note: the double implicit conversion trick does not work here either
     * because the `from` and `until` methods still exist on 2.13 but they
     * are deprecated.
     */
    def rangeFrom(a: A): mutable.SortedSet[A] = self.rangeImpl(Some(a), None)
    def rangeUntil(a: A): mutable.SortedSet[A] = self.rangeImpl(None, Some(a))

  }

}
