package java.util

import scala.collection.mutable

/**
  * Provides implicit enrichments to make some Scala 2.13 APIs available in older Scala versions
  */
private[util] object Compat {

  /**
    * In 2.13, the `remove` operation of mutable Sets returns an `Option[V]`,
    * whereas in older Scala versions it returned a `Boolean`.
    * However, we sometimes use the result of `remove` in expression position,
    * and this change makes things incompatible between Scala versions.
    * This implicit enrichment adds a `nonEmpty` operation to `Boolean` values
    * so that the result of `remove` always has a `nonEmpty` operation, whatever
    * is the underlying Scala version.
    */
  implicit class BooleanNonEmpty(val boolean: Boolean) extends AnyVal {
    def nonEmpty: Boolean = boolean
  }

  /** Adds methods from 2.13 to SortedSet.
   *
   *  The `to` operation has been renamed to `rangeTo` in 2.13, to not
   *  conflict with the `to` operation in `Iterable`.
   */
  implicit class SortedSetRangeTo[A](val __private_self: mutable.SortedSet[A]) extends AnyVal {
    // Note: the double implicit conversion trick does not work here because
    // there *is* a `to` method in 2.13 (but it takes a `Factory` as parameter)
    // so the second implicit conversion is never triggered
    def rangeTo(to: A): mutable.SortedSet[A] = {
      // Implementation copied from 2.12’s implementation
      val i = __private_self.from(to).iterator
      if (i.isEmpty) {
        __private_self
      } else {
        val next = i.next()
        if (__private_self.ordering.compare(next, to) == 0) {
          if (i.isEmpty) __private_self
          else __private_self.until(i.next())
        } else {
          __private_self.until(next)
        }
      }
    }
  }

}
