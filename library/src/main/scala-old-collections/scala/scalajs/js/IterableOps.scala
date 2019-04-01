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

package scala.scalajs.js

/** Adapts a JavaScript Iterable to a Scala Iterable */
@inline
final class IterableOps[+A](self: Iterable[A])
    extends scala.collection.Iterable[A] {
  @inline
  def iterator: scala.collection.Iterator[A] = self.jsIterator().toIterator
}
