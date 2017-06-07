/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package scala.scalajs.js

import scala.annotation.tailrec

import scala.scalajs.js

import scala.collection.mutable
import mutable.Builder

/** Equivalent of `scm.ArrayOps` for [[js.Array]]. */
@inline
final class ArrayOps[A](private[this] val array: js.Array[A])
    extends mutable.ArrayLike[A, js.Array[A]]
       with Builder[A, js.Array[A]] {

  import ArrayOps._

  /** Creates a new empty [[js.ArrayOps]]. */
  def this() = this(js.Array())

  // Implementation of ArrayLike

  @inline def apply(index: Int): A = array(index)
  @inline def length: Int = array.length
  @inline def update(index: Int, element: A): Unit = array(index) = element

  def seq: IndexedSeq[A] = new js.WrappedArray(array)

  override def repr: js.Array[A] = array

  override protected[this] def thisCollection: mutable.IndexedSeq[A] =
    toCollection(array)

  override protected[this] def toCollection(
      repr: js.Array[A]): mutable.IndexedSeq[A] = {
    new js.WrappedArray(repr)
  }

  protected[this] def newBuilder: Builder[A, js.Array[A]] =
    new js.ArrayOps[A]

  // Implementation of Builder

  @inline def +=(elem: A): this.type = {
    array.push(elem)
    this
  }

  @inline def clear(): Unit =
    array.length = 0

  @inline def result(): js.Array[A] = array

  // Scala notation for a fast concat()

  @inline def ++[B >: A](that: js.Array[_ <: B]): js.Array[B] =
    concat(array, that)

  // Methods whose inherited implementations do not play nice with the optimizer

  override def reduceLeft[B >: A](op: (B, A) => B): B = {
    val length = this.length
    if (length <= 0)
      throwUnsupported("empty.reduceLeft")

    @inline
    @tailrec
    def loop(start: Int, z: B): B =
      if (start == length) z
      else loop(start+1, op(z, this(start)))

    loop(1, this(0))
  }

  override def reduceRight[B >: A](op: (A, B) => B): B = {
    val length = this.length
    if (length <= 0)
      throwUnsupported("empty.reduceRight")

    @inline
    @tailrec
    def loop(end: Int, z: B): B =
      if (end == 0) z
      else loop(end-1, op(this(end-1), z))

    loop(length-1, this(length-1))
  }

}

object ArrayOps {

  /** Extract the throw in a separate, non-inlineable method. */
  private def throwUnsupported(msg: String): Nothing =
    throw new UnsupportedOperationException(msg)

  /** Non-inlined implementation of [[ArrayOps.++]]. */
  private def concat[A](left: js.Array[_ <: A],
      right: js.Array[_ <: A]): js.Array[A] = {

    val leftLength = left.length
    val rightLength = right.length
    val result = new js.Array[A](leftLength + rightLength)

    @inline
    @tailrec
    def loop(src: js.Array[_ <: A], i: Int, len: Int, offset: Int): Unit = {
      if (i != len) {
        result(i + offset) = src(i)
        loop(src, i + 1, len, offset)
      }
    }

    loop(left, 0, leftLength, 0)
    loop(right, 0, rightLength, leftLength)
    result
  }

}
