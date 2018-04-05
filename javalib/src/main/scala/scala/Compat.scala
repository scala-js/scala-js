package scala

/**
  * Provides implicit enrichments to make some Scala 2.13 APIs available in older Scala versions
  */
object Compat {

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

  /**
    * The `to` operation has been renamed to `rangeTo` in 2.13, to not
    * conflict with the `to` operation in `Iterable`.
    */
  implicit class SortedSetRangeTo[A](val sortedSet: scala.collection.mutable.SortedSet[A]) extends AnyVal {
    def rangeTo(to: A): scala.collection.mutable.SortedSet[A] = {
      // Implementation copied from 2.12’s implementation
      val i = sortedSet.from(to).iterator
      if (i.isEmpty) sortedSet else {
        val next = i.next()
        if (sortedSet.ordering.compare(next, to) == 0)
          if (i.isEmpty) sortedSet
          else sortedSet.until(i.next())
        else
          sortedSet.until(next)
      }
    }
  }

}
