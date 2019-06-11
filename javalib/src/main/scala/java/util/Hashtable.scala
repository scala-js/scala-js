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

import java.{util => ju}

import scala.collection.mutable

import ScalaOps._

class Hashtable[K, V] private (inner: mutable.HashMap[Box[Any], V])
    extends ju.Dictionary[K,V] with ju.Map[K, V] with Cloneable with Serializable {

  def this() =
    this(mutable.HashMap.empty[Box[Any], V])

  def this(initialCapacity: Int) = this()

  def this(initialCapacity: Int, loadFactor: Float) = this()

  def this(t: ju.Map[_ <: K, _ <: V]) = {
    this()
    putAll(t)
  }

  def size(): Int =
    inner.size

  def isEmpty: Boolean =
    inner.isEmpty

  def keys(): ju.Enumeration[K] =
    inner.keysIterator.map(_.inner.asInstanceOf[K]).asJavaEnumeration

  def elements(): ju.Enumeration[V] =
    inner.valuesIterator.asJavaEnumeration

  def contains(value: Any): Boolean =
    containsValue(value)

  def containsValue(value: Any): Boolean =
    inner.valuesIterator.contains(value)

  def containsKey(key: Any): Boolean =
    inner.contains(Box(key))

  def get(key: Any): V = {
    if (key == null)
      throw new NullPointerException
    inner.getOrElse(Box(key), null.asInstanceOf[V])
  }

  // Not implemented
  // protected def rehash(): Unit

  def put(key: K, value: V): V =
    inner.put(Box(key.asInstanceOf[AnyRef]), value).getOrElse(null.asInstanceOf[V])

  def remove(key: Any): V = {
    if (key == null)
      throw new NullPointerException
    inner.remove(Box(key)).getOrElse(null.asInstanceOf[V])
  }

  def putAll(m: ju.Map[_ <: K, _ <: V]): Unit = {
    m.entrySet.scalaOps.foreach {
      kv => inner.put(Box(kv.getKey.asInstanceOf[AnyRef]), kv.getValue)
    }
  }

  def clear(): Unit =
    inner.clear()

  override def clone(): AnyRef =
    new ju.Hashtable[K, V](this)

  override def toString(): String =
    inner.iterator.map(kv => "" + kv._1.inner + "=" + kv._2).mkString("{", ", ", "}")

  def keySet(): ju.Set[K] = {
    new AbstractSet[K] {
      def iterator(): Iterator[K] =
        new EntrySetIterator().scalaOps.map(_.getKey())

      def size(): Int = inner.size
    }
  }

  def entrySet(): ju.Set[ju.Map.Entry[K, V]] = {
    new AbstractSet[Map.Entry[K, V]] {
      def iterator(): Iterator[Map.Entry[K, V]] =
        new EntrySetIterator

      def size(): Int = inner.size
    }
  }

  /* Inspired by the implementation of
   * scala.collection.convert.JavaCollectionWrappers.MapWrapper.entrySet
   * as found in version 2.13.0, with two changes:
   *
   * - accommodate the fact that our keys are boxed, and
   * - explicitly snapshot the underlying contents right before any mutation of
   *   the underlying Map, as we do not have any guarantee that mutations
   *   preserve the state of existing iterators.
   */
  private class EntrySetIterator extends Iterator[Map.Entry[K, V]] {
    private var underlying: scala.collection.Iterator[(Box[Any], V)] =
      Hashtable.this.inner.iterator

    private var isSnapshot: Boolean = false

    private var prev: Box[Any] = null

    private def ensureSnapshot(): Unit = {
      if (!isSnapshot) {
        underlying = underlying.toList.iterator
        isSnapshot = true
      }
    }

    def hasNext(): Boolean = underlying.hasNext

    def next(): Map.Entry[K, V] = {
      val (boxedKey, initialValue) = underlying.next()
      prev = boxedKey

      new Map.Entry[K, V] {
        private var value = initialValue

        def getKey(): K = boxedKey.inner.asInstanceOf[K]

        def getValue(): V = value

        def setValue(v: V): V = {
          ensureSnapshot()
          val oldValue = value
          inner.put(boxedKey, v)
          value = v
          oldValue
        }

        override def equals(that: Any): Boolean = that match {
          case that: Map.Entry[_, _] =>
            getKey() === that.getKey() && getValue() === that.getValue()
          case _ =>
            false
        }

        override def hashCode(): Int =
          boxedKey.hashCode ^ (if (value == null) 0 else value.hashCode())
      }
    }

    def remove(): Unit = {
      if (prev == null)
        throw new IllegalStateException("next must be called at least once before remove")
      ensureSnapshot()
      inner -= prev
      prev = null
    }
  }

  def values(): ju.Collection[V] = {
    new AbstractCollection[V] {
      def iterator(): Iterator[V] =
        new EntrySetIterator().scalaOps.map(_.getValue())

      def size(): Int = inner.size
    }
  }
}
