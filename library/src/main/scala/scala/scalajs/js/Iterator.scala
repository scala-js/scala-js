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

@ScalaJSDefined
trait Iterator[+A] extends js.Object {
  def next(): Iterator.Entry[A]
}

object Iterator {
  @ScalaJSDefined
  trait Entry[+A] extends js.Object {
    def done: Boolean
    def value: A
  }

  @inline
  final implicit class IteratorOps[+A](val self: Iterator[A])
      extends scala.collection.Iterator[A] {

    private[this] var lastEntry = self.next()

    def hasNext: Boolean = !lastEntry.done

    def next(): A = {
      val value = lastEntry.value
      lastEntry = self.next()
      value
    }
  }
}
