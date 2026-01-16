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

import scala.collection.mutable
import mutable.Builder

/** Equivalent of `scm.ArrayOps` for [[js.Array]]. */
@inline
final class ArrayOps[A](private[this] val array: js.Array[A])
    extends mutable.ArrayLike[A, js.Array[A]]
    with Builder[A, js.Array[A]] {

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
    ArrayOpsCommon.concat(array, that)

  // Methods whose inherited implementations do not play nice with the optimizer

  override def reduceLeft[B >: A](op: (B, A) => B): B =
    ArrayOpsCommon.reduceLeft(array, op)

  override def reduceRight[B >: A](op: (A, B) => B): B =
    ArrayOpsCommon.reduceRight(array, op)

}
