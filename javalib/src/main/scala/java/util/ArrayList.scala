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

package java.util

import java.lang.Cloneable
import java.lang.Utils._

import scala.scalajs._

class ArrayList[E] private (private[ArrayList] val inner: js.Array[E])
    extends AbstractList[E] with RandomAccess with Cloneable with Serializable {
  self =>

  def this(initialCapacity: Int) = {
    this(new js.Array[E])
    if (initialCapacity < 0)
      throw new IllegalArgumentException
  }

  def this() =
    this(new js.Array[E])

  def this(c: Collection[_ <: E]) = {
    this()
    addAll(c)
  }

  def trimToSize(): Unit = {
    // We ignore this as js.Array doesn't support explicit pre-allocation
  }

  def ensureCapacity(minCapacity: Int): Unit = {
    // We ignore this as js.Array doesn't support explicit pre-allocation
  }

  def size(): Int =
    inner.length

  override def clone(): AnyRef =
    new ArrayList(inner.jsSlice(0))

  def get(index: Int): E = {
    checkIndexInBounds(index)
    inner(index)
  }

  override def set(index: Int, element: E): E = {
    val e = get(index)
    inner(index) = element
    e
  }

  override def add(e: E): Boolean = {
    inner.push(e)
    true
  }

  override def add(index: Int, element: E): Unit = {
    checkIndexOnBounds(index)
    inner.splice(index, 0, element)
  }

  override def remove(index: Int): E = {
    checkIndexInBounds(index)
    arrayRemoveAndGet(inner, index)
  }

  override def clear(): Unit =
    inner.length = 0

  override def addAll(index: Int, c: Collection[_ <: E]): Boolean = {
    c match {
      case other: ArrayList[_] =>
        checkIndexOnBounds(index)
        inner.splice(index, 0, other.inner.toSeq: _*)
        other.size() > 0
      case _ => super.addAll(index, c)
    }
  }

  override protected def removeRange(fromIndex: Int, toIndex: Int): Unit = {
    if (fromIndex < 0 || toIndex > size() || toIndex < fromIndex)
      throw new IndexOutOfBoundsException()
    inner.splice(fromIndex, toIndex - fromIndex)
  }
}
