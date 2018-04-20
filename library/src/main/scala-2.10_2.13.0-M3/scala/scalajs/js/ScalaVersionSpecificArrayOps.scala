/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package scala.scalajs.js

import scala.collection.mutable

trait ScalaVersionSpecificArrayOps[A]
    extends mutable.ArrayLike[A, Array[A]] {
  this: ArrayOps[A] =>

  protected[this] def newBuilder: mutable.Builder[A, Array[A]] =
    new ArrayOps[A]

  override def repr: Array[A] = array

  override protected[this] def thisCollection: mutable.IndexedSeq[A] =
    toCollection(array)
  override protected[this] def toCollection(
      repr: Array[A]): mutable.IndexedSeq[A] = new WrappedArray(repr)

  @inline def +=(elem: A): this.type = {
    array.push(elem)
    this
  }

}
