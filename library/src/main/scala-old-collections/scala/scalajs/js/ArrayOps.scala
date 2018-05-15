/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package scala.scalajs.js

import scala.annotation.tailrec

import scala.collection.mutable
import mutable.Builder

/** Equivalent of scm.ArrayOps for js.Array */
@inline
final class ArrayOps[A](private[this] val array: Array[A])
    extends mutable.ArrayLike[A, Array[A]]
       with Builder[A, Array[A]] {

  /** Creates a new empty [[ArrayOps]]. */
  def this() = this(Array())

  // Implementation of ArrayLike

  @inline def apply(index: Int): A = array(index)
  @inline def length: Int = array.length
  @inline def update(index: Int, element: A): Unit = array(index) = element

  def seq: IndexedSeq[A] = new WrappedArray(array)

  override def repr: Array[A] = array

  override protected[this] def thisCollection: mutable.IndexedSeq[A] =
    toCollection(array)
  override protected[this] def toCollection(
      repr: Array[A]): mutable.IndexedSeq[A] = new WrappedArray(repr)

  protected[this] def newBuilder: Builder[A, Array[A]] =
    new ArrayOps[A]

  // Implementation of Builder

  @inline def +=(elem: A): this.type = {
    array.push(elem)
    this
  }

  @inline def clear(): Unit =
    array.length = 0

  @inline def result(): Array[A] = array

  // Scala notation for a fast concat()

  @inline def ++[B >: A](that: Array[_ <: B]): Array[B] =
    ArrayOpsCommon.concat(array, that)

  // Methods whose inherited implementations do not play nice with the optimizer

  override def reduceLeft[B >: A](op: (B, A) => B): B =
    ArrayOpsCommon.reduceLeft(array, op)

  override def reduceRight[B >: A](op: (A, B) => B): B =
    ArrayOpsCommon.reduceRight(array, op)

}

@deprecated("Kept only for binary compatibility", "0.6.23")
object ArrayOps
