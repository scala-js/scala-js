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

import scala.scalajs.js
import js.annotation._

/** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
 *  JavaScript Iterator.
 */
trait Iterator[+A] extends js.Object {
  def next(): js.Iterator.Entry[A]
}

object Iterator {

  /** Return value of [[Iterator.next]]. */
  trait Entry[+A] extends js.Object {

    /** Whether the iterator has completed. */
    def done: Boolean

    /** The current value. Reading this value is only valid if done is false. */
    def value: A
  }

  @inline
  private final class WrappedIterator[+A](self: js.Iterator[A])
      extends scala.collection.Iterator[A] {
    private[this] var lastEntry = self.next()

    @inline
    def hasNext: Boolean = !lastEntry.done

    @inline
    def next(): A = {
      val value = lastEntry.value
      lastEntry = self.next()
      value
    }
  }

  final implicit class IteratorOps[A] private[Iterator] (
      private val self: js.Iterator[A])
      extends AnyVal {

    @inline
    def toIterator: scala.collection.Iterator[A] = new WrappedIterator(self)
  }
}
