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

import scala.language.implicitConversions

import scala.collection.mutable
import scala.collection.{SeqFactory, StrictOptimizedSeqFactory, StrictOptimizedSeqOps}

import scala.scalajs.js

/** Equivalent of scm.WrappedArray for js.Array */
@inline
final class WrappedArray[A](val array: js.Array[A])
    extends mutable.AbstractBuffer[A]
    with StrictOptimizedSeqOps[A, js.WrappedArray, js.WrappedArray[A]]
    with mutable.IndexedSeq[A]
    with mutable.IndexedSeqOps[A, js.WrappedArray, js.WrappedArray[A]]
    with mutable.IndexedOptimizedBuffer[A]
    with mutable.Builder[A, js.WrappedArray[A]]
    with Serializable {

  /** Creates a new empty [[WrappedArray]]. */
  def this() = this(js.Array())

  override def iterableFactory: SeqFactory[js.WrappedArray] = js.WrappedArray

  // IndexedSeq interface

  @inline def update(index: Int, elem: A): Unit = array(index) = elem
  @inline def apply(index: Int): A = array(index)
  @inline def length: Int = array.length

  // Builder interface

  @inline def addOne(elem: A): this.type = {
    array.push(elem)
    this
  }

  @inline def clear(): Unit =
    array.length = 0

  @inline def result(): js.WrappedArray[A] = this

  // Rest of Buffer interface

  @inline def prepend(elem: A): this.type = {
    array.unshift(elem)
    this
  }

  @inline override def prependAll(xs: IterableOnce[A]): this.type = {
    array.unshift(xs.iterator.toSeq: _*)
    this
  }

  @inline def subtractOne(elem: A): this.type = {
    val i = indexOf(elem)
    if (i != -1) remove(i)
    this
  }

  def insert(idx: Int, elem: A): Unit = {
    if (idx < 0 || idx > array.length)
      throw new IndexOutOfBoundsException
    array.splice(idx, 0, elem)
  }

  def insertAll(n: Int, elems: scala.collection.IterableOnce[A]): Unit = {
    if (n < 0 || n > array.length)
      throw new IndexOutOfBoundsException
    array.splice(n, 0, elems.iterator.toSeq: _*)
  }

  def remove(n: Int): A = {
    if (n < 0 || n >= array.length)
      throw new IndexOutOfBoundsException
    array.splice(n, 1)(0)
  }

  override def remove(n: Int, count: Int): Unit = {
    if (count < 0)
      throw new IllegalArgumentException
    if (n < 0 || (count > 0 && n + count > array.length))
      throw new IndexOutOfBoundsException
    array.splice(n, count)
  }

  @inline override def className: String = "WrappedArray"

}

/** Factory for [[WrappedArray]]. Provides implicit conversion to [[Array]].
 */
object WrappedArray extends StrictOptimizedSeqFactory[js.WrappedArray] {

  def empty[A]: js.WrappedArray[A] = new js.WrappedArray[A]()

  def newBuilder[A]: mutable.Builder[A, js.WrappedArray[A]] =
    new js.WrappedArray[A]

  def from[A](source: IterableOnce[A]): js.WrappedArray[A] =
    (newBuilder[A] ++= source).result()

  implicit def toJSArray[A](wrappedArray: js.WrappedArray[A]): js.Array[A] =
    wrappedArray.array

}
