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
import scala.collection.mutable.Builder
import scala.collection.View

import scala.scalajs.js
import scala.scalajs.js.annotation._

/** Wrapper to use a js.Map as a scala.mutable.Map */
@inline
final class WrappedMap[K, V](private val underlying: js.Map[K, V])
    extends mutable.AbstractMap[K, V]
       with mutable.MapOps[K, V, mutable.Map, js.WrappedMap[K, V]] {

  import WrappedMap._

  protected[this] override def fromSpecific(
      coll: scala.collection.IterableOnce[(K, V)]
  ): js.WrappedMap[K, V] = {
    val d = js.WrappedMap.empty[K, V]
    d ++= coll
    d
  }

  protected[this] override def newSpecificBuilder: Builder[(K, V), js.WrappedMap[K, V]] =
    new WrappedMapBuilder[K, V]

  def get(key: K): Option[V] =
    if (contains(key))
      Some(underlying.asInstanceOf[js.Map.Raw[K, V]].get(key))
    else
      None

  override def apply(key: K): V =
    if (contains(key))
      underlying.asInstanceOf[js.Map.Raw[K, V]].get(key)
    else
      throw new NoSuchElementException("key not found: " + key)

  override def size(): Int =
    underlying.size

  override def contains(key: K): Boolean =
    underlying.asInstanceOf[js.Map.Raw[K, V]].has(key)

  def subtractOne(key: K): this.type = {
    underlying.delete(key)
    this
  }

  override def update(key: K, value: V): Unit =
    underlying.asInstanceOf[js.Map.Raw[K, V]].set(key, value)

  def addOne(kv: (K, V)): this.type = {
    underlying.update(kv._1, kv._2)
    this
  }

  def iterator: scala.collection.Iterator[(K, V)] =
    new MapIterator(underlying)

  @inline
  override def keys: scala.collection.Iterable[K] =
    js.Array.from(underlying.asInstanceOf[js.Map.Raw[K, V]].keys().asInstanceOf[js.Iterable[K]])

  override def empty: js.WrappedMap[K, V] =
    new js.WrappedMap(js.Map.empty)

  // Overloads to return more precise types

  def map[B](f: ((K, V)) => (K, B)): js.WrappedMap[K, B] = {
    val b = new js.WrappedMap.WrappedMapBuilder[K, B]
    val it = this.iterator
    while (it.hasNext) {
      b += f(it.next())
    }
    b.result()
  }

  def flatMap[B](
      f: ((K, V)) => IterableOnce[(K, B)]): js.WrappedMap[K, B] = {
    val b = new js.WrappedMap.WrappedMapBuilder[K, B]
    val it = this.iterator
    while (it.hasNext) {
      b ++= f(it.next())
    }
    b.result()
  }

  def collect[B](
      pf: PartialFunction[(K, V), (K, B)]): js.WrappedMap[K, B] = {
    flatMap { a =>
      if (pf.isDefinedAt(a)) new View.Single(pf(a))
      else View.Empty
    }
  }

}

object WrappedMap {

  private final class MapIterator[K, +V](underlying: js.Map[K, V])
      extends scala.collection.Iterator[(K, V)] {

    private[this] val keys = js.Array.from(underlying.asInstanceOf[js.Map.Raw[K, V]].keys().asInstanceOf[js.Iterable[K]])
    private[this] var index: Int = 0

    def hasNext(): Boolean = index < keys.length

    def next(): (K, V) = {
      val key = keys(index)
      index += 1
      (key, underlying.asInstanceOf[js.Map.Raw[K, V]].get(key).asInstanceOf[V])
    }
  }

  def empty[K, V]: js.WrappedMap[K, V] = new js.WrappedMap(js.Map.empty)

  private final class WrappedMapBuilder[K, V]
      extends Builder[(K, V), js.WrappedMap[K, V]] {

    private[this] var map: js.Map[K, V] = js.Map.empty

    def addOne(elem: (K, V)): this.type = {
      map.asInstanceOf[js.Map.Raw[K, V]].set(elem._1, elem._2)
      this
    }

    def clear(): Unit = map = js.Map.empty

    def result(): js.WrappedMap[K, V] = new js.WrappedMap(map)
  }

}
