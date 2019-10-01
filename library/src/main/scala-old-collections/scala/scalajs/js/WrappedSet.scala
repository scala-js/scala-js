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

/** Wrapper to use a js.Set as a scala.mutable.Set */
@inline
class WrappedSet[T](val underlying: js.Set[T])
    extends mutable.AbstractSet[T]
       with mutable.Set[T]
       with mutable.SetLike[T, js.WrappedSet[T]] {

  import WrappedSet._

  override def apply(key: T): Boolean =
    contains(key)

  override def size(): Int =
    underlying.size

  override def contains(value: T): Boolean = {
    underlying.has(value)
  }

  override def add(elem: T): Boolean = {
    if (underlying.has(elem)) false
    else {
      underlying.add(elem)
      true
    }
  }

  override def remove(elem: T): Boolean =
    underlying.delete(elem)

  def -=(key: T): this.type = {
    underlying.delete(key)
    this
  }

  def +=(value: T): this.type = {
    underlying.add(value)
    this
  }

  def iterator: scala.collection.Iterator[T] =
    new SetIterator(underlying)

  override def empty: js.WrappedSet[T] =
    new js.WrappedSet(js.Set.empty[T])
}

object WrappedSet {

  private final class SetIterator[+T](dict: js.Set[T])
    extends scala.collection.Iterator[T] {

    private[this] val values = js.Array.from(dict.values())
    private[this] var index: Int = 0

    def hasNext(): Boolean = index < values.length

    def next(): T = {
      val value = values(index)
      index += 1
      value
    }
  }

  def empty[A]: js.WrappedSet[A] =
    new js.WrappedSet[A](js.Set.empty)

  implicit def canBuildFrom[A]: CanBuildFrom[js.WrappedSet[A], A, js.WrappedSet[A]] = {
    new CanBuildFrom[js.WrappedSet[A], A, js.WrappedSet[A]] {
      def apply(from: js.WrappedSet[A]): Builder[A, js.WrappedSet[A]] =
        new WrappedSetBuilder[A]
      def apply(): Builder[A, js.WrappedSet[A]] =
        new WrappedSetBuilder[A]
    }
  }

  private final class WrappedSetBuilder[A]
    extends Builder[A, js.WrappedSet[A]] {

    private[this] var set: js.Set[A] = js.Set.empty

    def +=(elem: A): this.type = {
      set.add(elem)
      this
    }

    def clear(): Unit =
      set = js.Set.empty

    def result(): js.WrappedSet[A] =
      new js.WrappedSet(set)
  }

}


