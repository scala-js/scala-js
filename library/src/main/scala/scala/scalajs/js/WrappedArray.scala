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

import scala.collection.generic.CanBuildFrom

/** Equivalent of scm.WrappedArray for js.Array */
@deprecatedInheritance("Will be made final in 0.6", "0.5.6")
class WrappedArray[A](val array: Array[A])
    extends mutable.AbstractSeq[A]
       with mutable.IndexedSeq[A]
       with mutable.ArrayLike[A, WrappedArray[A]] {

  def update(index: Int, elem: A): Unit = array(index) = elem
  def apply(index: Int): A = array(index)
  def length: Int = array.length

  override protected[this] def newBuilder: Builder[A, WrappedArray[A]] =
    new WrappedArray.WrappedArrayBuilder[A]

}

object WrappedArray {

  def empty[A]: WrappedArray[A] = new WrappedArray[A](Array())

  implicit def canBuildFrom[A]: CanBuildFrom[WrappedArray[_], A, WrappedArray[A]] = {
    new CanBuildFrom[WrappedArray[_], A, WrappedArray[A]] {
      def apply(from: WrappedArray[_]): Builder[A, WrappedArray[A]] =
        new WrappedArrayBuilder[A]
      def apply: Builder[A, WrappedArray[A]] =
        new WrappedArrayBuilder[A]
    }
  }

  class WrappedArrayBuilder[A] extends Builder[A, WrappedArray[A]] {
    private[this] var array: Array[A] = new Array
    def +=(elem: A): this.type = {
      array.push(elem)
      this
    }
    def clear(): Unit =
      array = new Array
    def result(): WrappedArray[A] =
      new WrappedArray(array)
  }

}
