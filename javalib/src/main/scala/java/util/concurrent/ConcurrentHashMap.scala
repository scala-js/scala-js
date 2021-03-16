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

package java.util.concurrent

import java.io.Serializable
import java.util._

class ConcurrentHashMap[K, V] private (initialCapacity: Int, loadFactor: Float)
    extends AbstractMap[K, V] with ConcurrentMap[K, V] with Serializable {

  import ConcurrentHashMap._

  def this() =
    this(HashMap.DEFAULT_INITIAL_CAPACITY, HashMap.DEFAULT_LOAD_FACTOR)

  def this(initialCapacity: Int) =
    this(initialCapacity, HashMap.DEFAULT_LOAD_FACTOR)

  def this(initialMap: java.util.Map[_ <: K, _ <: V]) = {
    this(initialMap.size())
    putAll(initialMap)
  }

  def this(initialCapacity: Int, loadFactor: Float, concurrencyLevel: Int) =
    this(initialCapacity, loadFactor) // ignore concurrencyLevel

  private[this] val inner: InnerHashMap[K, V] =
    new InnerHashMap[K, V](initialCapacity, loadFactor)

  override def size(): Int =
    inner.size()

  override def isEmpty(): Boolean =
    inner.isEmpty()

  override def get(key: Any): V =
    inner.get(key)

  override def containsKey(key: Any): Boolean =
    inner.containsKey(key)

  override def containsValue(value: Any): Boolean =
    inner.containsValue(value)

  override def put(key: K, value: V): V =
    inner.put(key, value)

  override def remove(key: Any): V =
    inner.remove(key)

  override def clear(): Unit =
    inner.clear()

  override def keySet(): ConcurrentHashMap.KeySetView[K, V] = {
    // Allow null as sentinel
    new ConcurrentHashMap.KeySetView[K, V](this.inner, null.asInstanceOf[V])
  }

  def keySet(mappedValue: V): ConcurrentHashMap.KeySetView[K, V] = {
    if (mappedValue == null)
      throw new NullPointerException()
    new ConcurrentHashMap.KeySetView[K, V](this.inner, mappedValue)
  }

  override def values(): Collection[V] =
    inner.values()

  override def entrySet(): Set[Map.Entry[K, V]] =
    inner.entrySet()

  override def hashCode(): Int =
    inner.hashCode()

  override def toString(): String =
    inner.toString()

  override def equals(o: Any): Boolean =
    inner.equals(o)

  override def putIfAbsent(key: K, value: V): V = {
    if (value == null)
      throw new NullPointerException()
    val old = inner.get(key) // throws if `key` is null
    if (old == null)
      inner.put(key, value)
    old
  }

  override def remove(key: Any, value: Any): Boolean = {
    val old = inner.get(key) // throws if `key` is null
    if (old != null && old.equals(value)) { // false if `value` is null
      inner.remove(key)
      true
    } else {
      false
    }
  }

  override def replace(key: K, oldValue: V, newValue: V): Boolean = {
    if (oldValue == null || newValue == null)
      throw new NullPointerException()
    val old = inner.get(key) // throws if `key` is null
    if (oldValue.equals(old)) { // false if `old` is null
      inner.put(key, newValue)
      true
    } else {
      false
    }
  }

  override def replace(key: K, value: V): V = {
    if (value == null)
      throw new NullPointerException()
    val old = inner.get(key) // throws if `key` is null
    if (old != null)
      inner.put(key, value)
    old
  }

  def contains(value: Any): Boolean =
    containsValue(value)

  def keys(): Enumeration[K] =
    Collections.enumeration(inner.keySet())

  def elements(): Enumeration[V] =
    Collections.enumeration(values())
}

object ConcurrentHashMap {
  import HashMap.Node

  /** Inner HashMap that contains the real implementation of a
   *  ConcurrentHashMap.
   *
   *  It is a null-rejecting hash map because some algorithms rely on the fact
   *  that `get(key) == null` means the key was not in the map.
   *
   *  It also has snapshotting iterators to make sure they are *weakly
   *  consistent*.
   */
  private final class InnerHashMap[K, V](initialCapacity: Int, loadFactor: Float)
      extends NullRejectingHashMap[K, V](initialCapacity, loadFactor) {

    override private[util] def nodeIterator(): Iterator[HashMap.Node[K, V]] =
      new NodeIterator

    override private[util] def keyIterator(): Iterator[K] =
      new KeyIterator

    override private[util] def valueIterator(): Iterator[V] =
      new ValueIterator

    private def makeSnapshot(): ArrayList[Node[K, V]] = {
      val snapshot = new ArrayList[Node[K, V]](size())
      val iter = super.nodeIterator()
      while (iter.hasNext())
        snapshot.add(iter.next())
      snapshot
    }

    private final class NodeIterator extends AbstractCHMIterator[Node[K, V]] {
      protected[this] def extract(node: Node[K, V]): Node[K, V] = node
    }

    private final class KeyIterator extends AbstractCHMIterator[K] {
      protected[this] def extract(node: Node[K, V]): K = node.key
    }

    private final class ValueIterator extends AbstractCHMIterator[V] {
      protected[this] def extract(node: Node[K, V]): V = node.value
    }

    private abstract class AbstractCHMIterator[A] extends Iterator[A] {
      private[this] val innerIter = makeSnapshot().iterator()
      private[this] var lastNode: Node[K, V] = _ // null

      protected[this] def extract(node: Node[K, V]): A

      def hasNext(): Boolean =
        innerIter.hasNext()

      def next(): A = {
        val node = innerIter.next()
        lastNode = node
        extract(node)
      }

      override def remove(): Unit = {
        val last = lastNode
        if (last eq null)
          throw new IllegalStateException("next must be called at least once before remove")
        removeNode(last)
        lastNode = null
      }
    }
  }

  class KeySetView[K, V] private[ConcurrentHashMap] (innerMap: InnerHashMap[K, V], defaultValue: V)
      extends Set[K] with Serializable {

    def getMappedValue(): V = defaultValue

    def contains(o: Any): Boolean = innerMap.containsKey(o)

    def remove(o: Any): Boolean = innerMap.remove(o) != null

    def iterator(): Iterator[K] = innerMap.keySet().iterator()

    def size(): Int = innerMap.size()

    def isEmpty(): Boolean = innerMap.isEmpty()

    def toArray(): Array[AnyRef] = innerMap.keySet().toArray()

    def toArray[T <: AnyRef](a: Array[T]): Array[T] = innerMap.keySet().toArray(a)

    def add(e: K): Boolean = {
      if (defaultValue == null) {
        throw new UnsupportedOperationException()
      }
      innerMap.putIfAbsent(e, defaultValue) == null
    }

    def containsAll(c: Collection[_]): Boolean = innerMap.keySet().containsAll(c)

    def addAll(c: Collection[_ <: K]): Boolean = {
      if (defaultValue == null) {
        throw new UnsupportedOperationException()
      }
      val iter = c.iterator()
      var changed = false
      while (iter.hasNext())
        changed = innerMap.putIfAbsent(iter.next(), defaultValue) == null || changed
      changed
    }

    def removeAll(c: Collection[_]): Boolean = innerMap.keySet().removeAll(c)

    def retainAll(c: Collection[_]): Boolean = innerMap.keySet().retainAll(c)

    def clear(): Unit = innerMap.clear()
  }

  def newKeySet[K](): KeySetView[K, Boolean] = newKeySet[K](HashMap.DEFAULT_INITIAL_CAPACITY)

  def newKeySet[K](initialCapacity: Int): KeySetView[K, Boolean] = {
    val inner = new InnerHashMap[K, Boolean](initialCapacity, HashMap.DEFAULT_LOAD_FACTOR)
    new KeySetView[K, Boolean](inner, true)
  }
}
