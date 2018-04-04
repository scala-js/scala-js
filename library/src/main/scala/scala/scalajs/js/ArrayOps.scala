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
final class ArrayOps[A](protected[this] val array: Array[A])
    extends ScalaVersionSpecificArrayOps[A]
       with Builder[A, Array[A]] {

  import ArrayOps._

  /** Creates a new empty [[ArrayOps]]. */
  def this() = this(Array())

  // Implementation of ArrayLike

  @inline def apply(index: Int): A = array(index)
  @inline def length: Int = array.length
  @inline def update(index: Int, element: A): Unit = array(index) = element

  def seq: collection.IndexedSeq[A] = new WrappedArray(array)

  // Implementation of Builder

  @inline def clear(): Unit =
    array.length = 0

  @inline def result(): Array[A] = array

  // Scala notation for a fast scalajsConcat()

  @inline def ++[B >: A](that: Array[_ <: B]): Array[B] =
    scalajsConcat(array, that)

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
  private def scalajsConcat[A](left: Array[_ <: A], right: Array[_ <: A]): Array[A] = {
    val leftLength = left.length
    val rightLength = right.length
    val result = new Array[A](leftLength + rightLength)

    @inline
    @tailrec
    def loop(src: Array[_ <: A], i: Int, len: Int, offset: Int): Unit =
      if (i != len) {
        result(i+offset) = src(i)
        loop(src, i+1, len, offset)
      }

    loop(left, 0, leftLength, 0)
    loop(right, 0, rightLength, leftLength)
    result
  }

}
