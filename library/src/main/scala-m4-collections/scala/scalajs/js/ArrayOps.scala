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

import scala.collection.mutable
import scala.collection.{IterableOnce, SeqFactory, StrictOptimizedSeqOps}

import scala.scalajs.js

/** Equivalent of scm.ArrayOps for js.Array */
@inline
final class ArrayOps[A](private[this] val array: js.Array[A])
    extends StrictOptimizedSeqOps[A, js.Array, js.Array[A]]
    with mutable.IndexedSeqOps[A, js.Array, js.Array[A]]
    with mutable.Builder[A, js.Array[A]] {

  import ArrayOps._

  /** Creates a new empty [[ArrayOps]]. */
  def this() = this(js.Array())

  // Implementation of IndexedSeqOps

  @inline def apply(index: Int): A = array(index)
  @inline def length: Int = array.length
  @inline def update(index: Int, element: A): Unit = array(index) = element

  def seq: scala.collection.IndexedSeq[A] = new js.WrappedArray(array)

  def repr: js.Array[A] = array

  def iterableFactory: scala.collection.SeqFactory[js.Array] = ArrayFactory

  protected[this] def coll: js.Array[A] = array

  protected[this] def fromSpecificIterable(
      coll: scala.collection.Iterable[A]): js.Array[A] = {
    iterableFactory.from(coll)
  }

  protected[this] def newSpecificBuilder: mutable.Builder[A, js.Array[A]] =
    iterableFactory.newBuilder[A]

  def toIterable: scala.collection.Iterable[A] = js.Any.wrapArray(array)

  // Implementation of Builder

  @inline final def addOne(elem: A): this.type = {
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

  @inline override def reduceLeft[B >: A](op: (B, A) => B): B =
    ArrayOpsCommon.reduceLeft(array, op)

  @inline override def reduceRight[B >: A](op: (A, B) => B): B =
    ArrayOpsCommon.reduceRight(array, op)

  // Inspired by mutable.IndexedOptimizedSeq

  def mapInPlace(f: A => A): this.type = {
    var i = 0
    val size = this.size
    while (i < size) {
      this(i) = f(this(i))
      i += 1
    }
    this
  }

}

private object ArrayOps {
  private object ArrayFactory extends SeqFactory[js.Array] {
    @inline
    def empty[A]: js.Array[A] = js.Array()

    @inline
    def from[A](source: IterableOnce[A]): js.Array[A] =
      (newBuilder[A] ++= source).result()

    @inline
    def newBuilder[A]: mutable.Builder[A, js.Array[A]] =
      new ArrayOps[A]
  }
}
