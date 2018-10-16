/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.scalajs.js

import scala.annotation.tailrec

import scala.scalajs.js

private[js] object ArrayOpsCommon {

  /** Efficient concatenation of two JS arrays. */
  def concat[A](left: js.Array[_ <: A], right: js.Array[_ <: A]): js.Array[A] = {
    val leftLength = left.length
    val rightLength = right.length
    val result = new js.Array[A](leftLength + rightLength)

    @inline
    @tailrec
    def loop(src: js.Array[_ <: A], i: Int, len: Int, offset: Int): Unit = {
      if (i != len) {
        result(i+offset) = src(i)
        loop(src, i+1, len, offset)
      }
    }

    loop(left, 0, leftLength, 0)
    loop(right, 0, rightLength, leftLength)
    result
  }

  def reduceLeft[A, B >: A](array: js.Array[A], op: (B, A) => B): B = {
    val length = array.length
    if (length <= 0)
      throwUnsupported("empty.reduceLeft")

    @inline
    @tailrec
    def loop(start: Int, z: B): B =
      if (start == length) z
      else loop(start+1, op(z, array(start)))

    loop(1, array(0))
  }

  def reduceRight[A, B >: A](array: js.Array[A], op: (A, B) => B): B = {
    val length = array.length
    if (length <= 0)
      throwUnsupported("empty.reduceRight")

    @inline
    @tailrec
    def loop(end: Int, z: B): B =
      if (end == 0) z
      else loop(end-1, op(array(end-1), z))

    loop(length-1, array(length-1))
  }

  /** Extract the throw in a separate, non-inlineable method. */
  private def throwUnsupported(msg: String): Nothing =
    throw new UnsupportedOperationException(msg)

}
