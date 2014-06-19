/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package scala.scalajs.js

import scala.collection.mutable
import mutable.Builder

/** Equivalent of scm.ArrayOps for js.Array */
class ArrayOps[A](private[this] val array: Array[A])
    extends mutable.ArrayLike[A, Array[A]] {

  def apply(index: Int): A = array(index)
  def length: Int = array.length
  def update(index: Int, element: A): Unit = array(index) = element

  def seq: IndexedSeq[A] = new WrappedArray(array)

  protected[this] def newBuilder: Builder[A, Array[A]] =
    new ArrayOps.ArrayBuilder

}

object ArrayOps {

  class ArrayBuilder[A] extends Builder[A, Array[A]] {
    private[this] var array: Array[A] = new Array
    def +=(elem: A): this.type = {
      array.push(elem)
      this
    }
    def clear(): Unit =
      array = new Array
    def result(): Array[A] =
      array
  }

}
