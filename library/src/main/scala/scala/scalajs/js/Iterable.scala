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
 *  JavaScript Iterable.
 *
 *  An Iterable must define a method `[Symbol.iterator]`.
 */
@ScalaJSDefined
trait Iterable[+A] extends js.Object

object Iterable {
  @js.native
  private trait IteratorCallAccess[+A] extends js.Object {
    @JSBracketCall
    def iteratorCall(sym: Symbol)(): Iterator[A]
  }

  @inline
  implicit final class IterableOps[+A](val self: Iterable[A])
      extends scala.collection.Iterable[A] {

    @inline
    def jsIterator(): Iterator[A] =
      self.asInstanceOf[IteratorCallAccess[A]].iteratorCall(Symbol.iterator)()

    @inline
    def iterator: scala.collection.Iterator[A] = self.jsIterator()
  }
}
