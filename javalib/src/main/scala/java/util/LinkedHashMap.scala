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
import java.util.function.BiConsumer

class LinkedHashMap[K, V](initialCapacity: Int, loadFactor: Float,
    accessOrder: Boolean)
    extends HashMap[K, V](initialCapacity, loadFactor) {
  self =>

  import LinkedHashMap._

  /** Node that was least recently created (or accessed under access-order). */
  private var eldest: Node[K, V] = _

  /** Node that was most recently created (or accessed under access-order). */
  private var youngest: Node[K, V] = _

  def this(initialCapacity: Int, loadFactor: Float) =
    this(initialCapacity, loadFactor, false)

  def this(initialCapacity: Int) =
    this(initialCapacity, HashMap.DEFAULT_LOAD_FACTOR)

  def this() =
    this(HashMap.DEFAULT_INITIAL_CAPACITY)

  def this(m: Map[_ <: K, _ <: V]) = {
    this(m.size())
    putAll(m)
  }

  private def asMyNode(node: HashMap.Node[K, V]): Node[K, V] =
    node.asInstanceOf[Node[K, V]]

  private[util] override def newNode(key: K, hash: Int, value: V,
      previous: HashMap.Node[K, V],
      next: HashMap.Node[K, V]): HashMap.Node[K, V] = {
    new Node(key, hash, value, previous, next, null, null)
  }

  private[util] override def nodeWasAccessed(node: HashMap.Node[K, V]): Unit = {
    if (accessOrder) {
      val myNode = asMyNode(node)
      if (myNode.younger ne null) {
        removeFromOrderedList(myNode)
        appendToOrderedList(myNode)
      }
    }
  }

  private[util] override def nodeWasAdded(node: HashMap.Node[K, V]): Unit = {
    appendToOrderedList(asMyNode(node))
    if (removeEldestEntry(eldest))
      removeNode(eldest)
  }

  private[util] override def nodeWasRemoved(node: HashMap.Node[K, V]): Unit =
    removeFromOrderedList(asMyNode(node))

  private def appendToOrderedList(node: Node[K, V]): Unit = {
    val older = youngest
    if (older ne null)
      older.younger = node
    else
      eldest = node
    node.older = older
    node.younger = null
    youngest = node
  }

  private def removeFromOrderedList(node: Node[K, V]): Unit = {
    val older = node.older
    val younger = node.younger
    if (older eq null)
      eldest = younger
    else
      older.younger = younger
    if (younger eq null)
      youngest = older
    else
      younger.older = older
  }

  override def clear(): Unit = {
    super.clear()

    /* #4195 HashMap.clear() won't call `nodeWasRemoved` for every node, which
     * would be inefficient, so `eldest` and `yougest` are not automatically
     * updated. We must explicitly set them to `null` here.
     */
    eldest = null
    youngest = null
  }

  protected def removeEldestEntry(eldest: Map.Entry[K, V]): Boolean = false

  override def forEach(action: BiConsumer[_ >: K, _ >: V]): Unit = {
    var node = eldest
    while (node ne null) {
      action.accept(node.key, node.value)
      node = node.younger
    }
  }

  private[util] override def nodeIterator(): ju.Iterator[HashMap.Node[K, V]] =
    new NodeIterator

  private[util] override def keyIterator(): ju.Iterator[K] =
    new KeyIterator

  private[util] override def valueIterator(): ju.Iterator[V] =
    new ValueIterator

  private final class NodeIterator
      extends AbstractLinkedHashMapIterator[HashMap.Node[K, V]] {
    protected[this] def extract(node: Node[K, V]): Node[K, V] = node
  }

  private final class KeyIterator extends AbstractLinkedHashMapIterator[K] {
    protected[this] def extract(node: Node[K, V]): K = node.key
  }

  private final class ValueIterator extends AbstractLinkedHashMapIterator[V] {
    protected[this] def extract(node: Node[K, V]): V = node.value
  }

  private abstract class AbstractLinkedHashMapIterator[A] extends ju.Iterator[A] {
    private[this] var nextNode: Node[K, V] = eldest
    private[this] var lastNode: Node[K, V] = _

    protected[this] def extract(node: Node[K, V]): A

    def hasNext(): Boolean =
      nextNode ne null

    def next(): A = {
      if (!hasNext())
        throw new NoSuchElementException("next on empty iterator")
      val node = nextNode
      lastNode = node
      nextNode = node.younger
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

  override def clone(): AnyRef = {
    val result = new LinkedHashMap[K, V](size(), loadFactor, accessOrder)
    result.putAll(this)
    result
  }
}

object LinkedHashMap {

  private final class Node[K, V](key: K, hash: Int, value: V,
      previous: HashMap.Node[K, V], next: HashMap.Node[K, V],
      var older: Node[K, V], var younger: Node[K, V])
      extends HashMap.Node[K, V](key, hash, value, previous, next)

}
