/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.js

import scala.scalajs.js
import js.annotation._

/** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
 *  JavaScript Iterator.
 */
trait Iterator[+A] extends js.Object {
  def next(): Iterator.Entry[A]
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
  private final class WrappedIterator[+A](self: Iterator[A])
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

  final implicit class IteratorOps[A](val __self: Iterator[A]) extends AnyVal {
    @inline
    def toIterator: scala.collection.Iterator[A] = new WrappedIterator(__self)
  }
}
