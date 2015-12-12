/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.js

import scala.language.implicitConversions

import scala.annotation.implicitNotFound
import scala.annotation.unchecked.uncheckedVariance

import scala.scalajs.js
import js.annotation._

/** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
 *  JavaScript Generator, such as the result of generator function.
 *
 *  @tparam V The type of elements returned by `next()`
 *  @tparam I The type of values expected by `next()` as parameter
 *  @tparam R The type of the result/done value.
 */
@ScalaJSDefined
trait Generator[+V, -I, +R] extends js.Object {
  def next(value: I): Generator.Entry[V, R]

  def `return`[S](result: S): Generator.Entry[Nothing, S]

  def `throw`(ex: scala.Any): Generator.Entry[V, R]
}

object Generator {
  /** The type of a simple Generator that takes no input in `next()` and
   *  returns no value.
   */
  type Iter[+V] = Generator[V, Unit, Unit]

  implicit def asIterator[V](g: Generator[V, Unit, _]): js.Iterator[V] =
    g.asInstanceOf[js.Iterator[V]]

  implicit def toIteratorOps[V](g: Generator[V, Unit, _]): js.Iterator.IteratorOps[V] =
    asIterator(g)

  @ScalaJSDefined
  trait Entry[+V, +R] extends js.Object {
    def value: V @uncheckedVariance | R @uncheckedVariance
    def done: Boolean
  }

  object Entry {
    implicit class EntryOps[+V, +R](val self: Entry[V, R]) extends AnyVal {
      @inline def iterValue: V = self.value.asInstanceOf[V]
      @inline def doneValue: R = self.value.asInstanceOf[R]
    }
  }
}
