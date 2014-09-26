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
final class WrappedArray[A](val array: Array[A])
    extends mutable.AbstractSeq[A]
       with mutable.IndexedSeq[A]
       with mutable.ArrayLike[A, WrappedArray[A]]
       with Builder[A, WrappedArray[A]] {

  /** Creates a new empty [[WrappedArray]]. */
  def this() = this(Array())

  // IndexedSeq interface

  def update(index: Int, elem: A): Unit = array(index) = elem
  def apply(index: Int): A = array(index)
  def length: Int = array.length

  override protected[this] def newBuilder: Builder[A, WrappedArray[A]] =
    new WrappedArray[A]

  // Builder interface

  @inline def +=(elem: A): this.type = {
    array.push(elem)
    this
  }

  @inline def clear(): Unit =
    array.length = 0

  @inline def result(): WrappedArray[A] = this

}

object WrappedArray {

  def empty[A]: WrappedArray[A] = new WrappedArray[A]

  implicit def canBuildFrom[A]: CanBuildFrom[WrappedArray[_], A, WrappedArray[A]] = {
    new CanBuildFrom[WrappedArray[_], A, WrappedArray[A]] {
      def apply(from: WrappedArray[_]): Builder[A, WrappedArray[A]] =
        new WrappedArray[A]
      def apply: Builder[A, WrappedArray[A]] =
        new WrappedArray[A]
    }
  }

}
