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
import java.{util => ju}

import scala.annotation.tailrec

import ScalaOps._

/* The additional `internal` parameter works around
 * https://github.com/scala/bug/issues/11755
 */
class IdentityHashMap[K, V] private (
    inner: HashMap[IdentityHashMap.IdentityBox[K], V], internal: Boolean)
    extends AbstractMap[K, V] with Map[K, V] with Serializable with Cloneable {
  self =>

  import IdentityHashMap._

  def this(expectedMaxSize: Int) = {
    this(new HashMap[IdentityHashMap.IdentityBox[K], V](
        expectedMaxSize, HashMap.DEFAULT_LOAD_FACTOR), internal = true)
  }

  def this() =
    this(HashMap.DEFAULT_INITIAL_CAPACITY)

  def this(initialMap: java.util.Map[_ <: K, _ <: V]) = {
    this(initialMap.size())
    putAll(initialMap)
  }

  override def clear(): Unit = inner.clear()

  override def clone(): AnyRef = {
    new IdentityHashMap(
        inner.clone().asInstanceOf[HashMap[IdentityBox[K], V]], internal = true)
  }

  override def containsKey(key: Any): Boolean =
    inner.containsKey(new IdentityBox(key))

  override def containsValue(value: Any): Boolean =
    inner.valueIterator().scalaOps.exists(same(_, value))

  override def get(key: Any): V =
    inner.get(new IdentityBox(key))

  override def isEmpty(): Boolean = inner.isEmpty()

  override def put(key: K, value: V): V =
    inner.put(new IdentityBox(key), value)

  override def remove(key: Any): V =
    inner.remove(new IdentityBox(key))

  override def size(): Int = inner.size()

  override def values(): Collection[V] = new Values

  override def keySet(): ju.Set[K] = new KeySet

  override def entrySet(): Set[Map.Entry[K, V]] = new EntrySet

  // Views

  private final class Values extends AbstractCollection[V] {
    def iterator(): ju.Iterator[V] =
      inner.valueIterator()

    def size(): Int =
      self.size()

    override def contains(o: Any): Boolean =
      containsValue(o)

    override def remove(o: Any): Boolean = {
      @tailrec
      def findAndRemove(iter: Iterator[V]): Boolean = {
        if (iter.hasNext()) {
          if (same(iter.next(), o)) {
            iter.remove()
            true
          } else {
            findAndRemove(iter)
          }
        } else {
          false
        }
      }
      findAndRemove(iterator())
    }

    override def removeAll(c: Collection[_]): Boolean =
      c.scalaOps.foldLeft(false)((prev, elem) => this.remove(elem) || prev)

    override def retainAll(c: Collection[_]): Boolean = {
      val iter = iterator()
      var changed = false
      while (iter.hasNext()) {
        val elem = iter.next()
        if (!findSame(elem, c)) {
          iter.remove()
          changed = true
        }
      }
      changed
    }

    override def clear(): Unit =
      self.clear()
  }

  private final class KeySet extends AbstractSet[K] {
    def iterator(): Iterator[K] = {
      new ju.Iterator[K] {
        private val iter = inner.keyIterator()

        def hasNext(): Boolean =
          iter.hasNext()

        def next(): K =
          iter.next().inner

        override def remove(): Unit =
          iter.remove()
      }
    }

    def size(): Int =
      self.size()

    override def contains(o: Any): Boolean =
      containsKey(o)

    override def remove(o: Any): Boolean = {
      val hasKey = contains(o)
      if (hasKey)
        self.remove(o)
      hasKey
    }

    override def removeAll(c: Collection[_]): Boolean = {
      if (size() > c.size()) {
        c.scalaOps.foldLeft(false)((prev, elem) => this.remove(elem) || prev)
      } else {
        @tailrec
        def removeAll(iter: Iterator[K], modified: Boolean): Boolean = {
          if (iter.hasNext()) {
            if (findSame(iter.next(), c)) {
              iter.remove()
              removeAll(iter, true)
            } else {
              removeAll(iter, modified)
            }
          } else {
            modified
          }
        }
        removeAll(this.iterator(), false)
      }
    }

    override def retainAll(c: Collection[_]): Boolean = {
      val iter = iterator()
      var changed = false
      while (iter.hasNext()) {
        val elem = iter.next()
        if (!findSame(elem, c)) {
          iter.remove()
          changed = true
        }
      }
      changed
    }

    override def clear(): Unit =
      self.clear()
  }

  private final class EntrySet extends AbstractSet[Map.Entry[K, V]] {
    def iterator(): Iterator[Map.Entry[K, V]] = {
      new ju.Iterator[Map.Entry[K, V]] {
        private val iter = inner.entrySetIterator()

        def hasNext(): Boolean =
          iter.hasNext()

        def next(): Map.Entry[K, V] =
          new MapEntry(iter.next())

        override def remove(): Unit =
          iter.remove()
      }
    }

    def size(): Int =
      inner.size()

    override def contains(value: Any): Boolean = {
      value match {
        case value: Map.Entry[_, _] =>
          val thatKey = value.getKey()
          self.containsKey(thatKey) && same(self.get(thatKey), value.getValue())
        case _ =>
          false
      }
    }

    override def remove(value: Any): Boolean = {
      value match {
        case value: Map.Entry[_, _] =>
          val thatKey = value.getKey()
          val thatValue = value.getValue()
          if (self.containsKey(thatKey) && same(self.get(thatKey), thatValue)) {
            self.remove(thatKey)
            true
          } else {
            false
          }
        case _ =>
          false
      }
    }

    override def clear(): Unit =
      inner.clear()
  }
}

object IdentityHashMap {
  private final class IdentityBox[+K](val inner: K) {
    override def equals(o: Any): Boolean = {
      o match {
        case o: IdentityBox[_] =>
          same(inner, o.inner)
        case _ =>
          false
      }
    }

    override def hashCode(): Int =
      System.identityHashCode(inner)
  }

  @inline private def same(v1: Any, v2: Any): Boolean =
    v1.asInstanceOf[AnyRef] eq v2.asInstanceOf[AnyRef]

  private def findSame[K](elem: K, c: Collection[_]): Boolean = {
    // scalastyle:off return
    val iter = c.iterator()
    while (iter.hasNext()) {
      if (same(elem, iter.next()))
        return true
    }
    false
    // scalastyle:on return
  }

  private final class MapEntry[K, V](entry: Map.Entry[IdentityBox[K], V])
      extends Map.Entry[K, V] {

    override def equals(other: Any): Boolean =
      other match {
        case other: Map.Entry[_, _] =>
          same(this.getKey(), other.getKey()) &&
          same(this.getValue(), other.getValue())
        case _ =>
          false
      }

    def getKey(): K =
      entry.getKey().inner

    def getValue(): V =
      entry.getValue()

    override def hashCode(): Int =
      entry.getKey().hashCode() ^ System.identityHashCode(entry.getValue())

    def setValue(value: V): V =
      entry.setValue(value)

    override def toString(): String =
      "" + this.getKey() + "=" + this.getValue()
  }
}
