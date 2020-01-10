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

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.collection.mutable.Builder
import scala.scalajs.js

/** Wrapper to use a js.Map as a scala.mutable.Map */
@inline
final class WrappedMap[K, V](private val underlying: js.Map[K, V])
    extends mutable.AbstractMap[K, V]
       with mutable.Map[K, V]
       with mutable.MapLike[K, V, js.WrappedMap[K, V]] {

  import WrappedMap._

  def get(key: K): Option[V] = {
    if (contains(key))
      Some(underlying.get(key).asInstanceOf[V])
    else
      None
  }

  override def apply(key: K): V = {
    if (contains(key))
      underlying.get(key).asInstanceOf[V]
    else
      throw new NoSuchElementException("key not found: " + key)
  }

  override def size(): Int =
    underlying.size

  override def contains(key: K): Boolean = {
    underlying.has(key)
  }

  def -=(key: K): this.type = {
    underlying.delete(key)
    this
  }

  override def update(key: K, value: V): Unit =
    underlying.set(key, value)

  def +=(kv: (K, V)): this.type = {
    underlying.set(kv._1, kv._2)
    this
  }

  def iterator: scala.collection.Iterator[(K, V)] =
    new MapIterator(underlying)

  @inline
  override def keys: scala.collection.Iterable[K] =
    js.Array.from(underlying.keys())

  override def empty: js.WrappedMap[K, V] =
    new js.WrappedMap(js.Map.empty[K, V])

}

object WrappedMap {

  private final class MapIterator[K, +V](underlying: js.Map[K, V])
      extends scala.collection.Iterator[(K, V)] {

    private[this] val keys = js.Array.from(underlying.keys())
    private[this] var index: Int = 0

    def hasNext(): Boolean = index < keys.length

    def next(): (K, V) = {
      val key = keys(index)
      index += 1
      (key, underlying.get(key).asInstanceOf[V])
    }
  }

  def empty[K, A]: js.WrappedMap[K, A] =
    new js.WrappedMap[K, A](js.Map.empty)

  implicit def canBuildFrom[K, A]: CanBuildFrom[js.WrappedMap[K, A], (K, A), js.WrappedMap[K, A]] = {
    new CanBuildFrom[js.WrappedMap[K, A], (K, A), js.WrappedMap[K, A]] {
      def apply(from: js.WrappedMap[K, A]): Builder[(K, A), js.WrappedMap[K, A]] =
        new WrappedMapBuilder[K, A]
      def apply(): Builder[(K, A), js.WrappedMap[K, A]] =
        new WrappedMapBuilder[K, A]
    }
  }

  private final class WrappedMapBuilder[K, A]
    extends Builder[(K, A), js.WrappedMap[K, A]] {

    private[this] var map: js.Map[K, A] = js.Map.empty

    def +=(elem: (K, A)): this.type = {
      map.set(elem._1, elem._2)
      this
    }

    def clear(): Unit =
      map = js.Map.empty

    def result(): js.WrappedMap[K, A] =
      new js.WrappedMap(map)
  }

}

