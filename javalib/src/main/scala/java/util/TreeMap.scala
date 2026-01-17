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
import java.util.{RedBlackTree => RB}
import java.util.function.{Function, BiFunction}

class TreeMap[K, V] private (tree: RB.Tree[K, V])(
    implicit comp: Comparator[_ >: K])
    extends AbstractMap[K, V] with NavigableMap[K, V] with Cloneable with Serializable {

  def this() = this(RB.Tree.empty[K, V])(NaturalComparator)

  def this(comparator: Comparator[_ >: K]) =
    this(RB.Tree.empty[K, V])(NaturalComparator.select(comparator))

  def this(m: Map[K, V]) = {
    this()
    putAll(m)
  }

  def this(m: SortedMap[K, V]) = {
    this(RB.fromOrderedEntries(m.entrySet().iterator(), m.size()))(
        NaturalComparator.select(m.comparator()))
  }

  override def size(): Int = RB.size(tree)

  override def containsKey(key: Any): Boolean = RB.contains(tree, key)

  override def containsValue(value: Any): Boolean = {
    // scalastyle:off return
    val iter = RB.valuesIterator(tree)
    while (iter.hasNext()) {
      if (Objects.equals(value, iter.next()))
        return true
    }
    false
    // scalastyle:on return
  }

  override def get(key: Any): V = RB.get(tree, key)

  def comparator(): Comparator[_ >: K] =
    NaturalComparator.unselect(comp)

  def firstKey(): K = {
    if (isEmpty())
      throw new NoSuchElementException()
    RB.minKey(tree)
  }

  def lastKey(): K = {
    if (isEmpty())
      throw new NoSuchElementException()
    RB.maxKey(tree)
  }

  override def putAll(map: Map[_ <: K, _ <: V]): Unit =
    map.forEach((k, v) => put(k, v))

  override def put(key: K, value: V): V =
    RB.insert(tree, key, value)

  override def computeIfAbsent(key: K, mappingFunction: Function[_ >: K, _ <: V]): V = {
    val node = RB.getNode(tree, key)

    if (node eq null) {
      val newValue = mappingFunction(key)
      if (newValue != null)
        put(key, newValue)
      newValue
    } else if (node.getValue() == null) {
      val newValue = mappingFunction(key)
      if (newValue != null)
        updateNodeValue(node, newValue)
      newValue
    } else {
      node.getValue()
    }
  }

  override def computeIfPresent(key: K,
      remappingFunction: BiFunction[_ >: K, _ >: V, _ <: V]): V = {
    val node = RB.getNode(tree, key)
    if ((node ne null) && node.getValue() != null)
      updateNodeValue(node, remappingFunction(key, node.getValue()))
    else
      null.asInstanceOf[V]
  }

  override def compute(key: K, remappingFunction: BiFunction[_ >: K, _ >: V, _ <: V]): V = {
    val node = RB.getNode(tree, key)
    if (node eq null) {
      val newValue = remappingFunction(key, null.asInstanceOf[V])
      if (newValue != null)
        put(key, newValue)
      newValue
    } else {
      updateNodeValue(node, remappingFunction(key, node.getValue()))
    }
  }

  override def merge(key: K, value: V, remappingFunction: BiFunction[_ >: V, _ >: V, _ <: V]): V = {
    value.getClass() // null check

    val node = RB.getNode(tree, key)
    if (node eq null) {
      put(key, value)
      value
    } else {
      val oldValue = node.getValue()
      val newValue =
        if (oldValue == null) value
        else remappingFunction(oldValue, value)

      updateNodeValue(node, newValue)
    }
  }

  /** Common code for functions above.
   *
   *  - Sets value to newValue if it is non-null
   *  - deletes the node if newValue is null.
   *
   *  @returns newValue
   */
  private def updateNodeValue(node: RB.Node[K, V], newValue: V): V = {
    if (newValue == null)
      RB.deleteNode(tree, node)
    else
      node.setValue(newValue)
    newValue
  }

  override def remove(key: Any): V =
    RB.nullableNodeValue(RB.delete(tree, key))

  override def clear(): Unit = RB.clear(tree)

  override def clone(): Object = new TreeMap(tree.treeCopy())(comp)

  def firstEntry(): Map.Entry[K, V] = RB.minNode(tree)

  def lastEntry(): Map.Entry[K, V] = RB.maxNode(tree)

  def pollFirstEntry(): Map.Entry[K, V] = {
    val node = RB.minNode(tree)
    if (node ne null)
      RB.deleteNode(tree, node)
    node
  }

  def pollLastEntry(): Map.Entry[K, V] = {
    val node = RB.maxNode(tree)
    if (node ne null)
      RB.deleteNode(tree, node)
    node
  }

  def lowerEntry(key: K): Map.Entry[K, V] =
    RB.maxNodeBefore(tree, key, RB.ExclusiveBound)

  def lowerKey(key: K): K =
    RB.maxKeyBefore(tree, key, RB.ExclusiveBound)

  def floorEntry(key: K): Map.Entry[K, V] =
    RB.maxNodeBefore(tree, key, RB.InclusiveBound)

  def floorKey(key: K): K =
    RB.maxKeyBefore(tree, key, RB.InclusiveBound)

  def ceilingEntry(key: K): Map.Entry[K, V] =
    RB.minNodeAfter(tree, key, RB.InclusiveBound)

  def ceilingKey(key: K): K =
    RB.minKeyAfter(tree, key, RB.InclusiveBound)

  def higherEntry(key: K): Map.Entry[K, V] =
    RB.minNodeAfter(tree, key, RB.ExclusiveBound)

  def higherKey(key: K): K =
    RB.minKeyAfter(tree, key, RB.ExclusiveBound)

  override def keySet(): Set[K] = navigableKeySet()

  def navigableKeySet(): NavigableSet[K] = {
    new TreeSet.Projection(tree, null.asInstanceOf[K], RB.NoBound,
        null.asInstanceOf[K], RB.NoBound, null.asInstanceOf[V])
  }

  def descendingKeySet(): NavigableSet[K] = {
    new TreeSet.DescendingProjection(tree, null.asInstanceOf[K], RB.NoBound,
        null.asInstanceOf[K], RB.NoBound, null.asInstanceOf[V])
  }

  override def values(): Collection[V] = new AbstractCollection[V] {
    def iterator(): Iterator[V] = RB.valuesIterator(tree)

    def size(): Int = RB.size(tree)

    override def contains(o: Any): Boolean = containsValue(o)

    override def clear(): Unit = RB.clear(tree)
  }

  def entrySet(): Set[Map.Entry[K, V]] = {
    new TreeMap.ProjectedEntrySet(tree, null.asInstanceOf[K], RB.NoBound,
        null.asInstanceOf[K], RB.NoBound)
  }

  def descendingMap(): NavigableMap[K, V] = {
    new TreeMap.DescendingProjection(tree, null.asInstanceOf[K], RB.NoBound,
        null.asInstanceOf[K], RB.NoBound)
  }

  def subMap(fromKey: K, fromInclusive: Boolean, toKey: K, toInclusive: Boolean): NavigableMap[K,
      V] = {
    new TreeMap.Projection(tree,
        fromKey, RB.boundKindFromIsInclusive(fromInclusive),
        toKey, RB.boundKindFromIsInclusive(toInclusive))
  }

  def headMap(toKey: K, inclusive: Boolean): NavigableMap[K, V] = {
    new TreeMap.Projection(tree,
        null.asInstanceOf[K], RB.NoBound,
        toKey, RB.boundKindFromIsInclusive(inclusive))
  }

  def tailMap(fromKey: K, inclusive: Boolean): NavigableMap[K, V] = {
    new TreeMap.Projection(tree,
        fromKey, RB.boundKindFromIsInclusive(inclusive),
        null.asInstanceOf[K], RB.NoBound)
  }

  def subMap(fromKey: K, toKey: K): SortedMap[K, V] =
    subMap(fromKey, true, toKey, false)

  def headMap(toKey: K): SortedMap[K, V] =
    headMap(toKey, false)

  def tailMap(fromKey: K): SortedMap[K, V] =
    tailMap(fromKey, true)
}

private object TreeMap {
  private class ProjectedEntrySet[K, V](tree: RB.Tree[K, V],
      lowerBound: K, lowerKind: RB.BoundKind, upperBound: K, upperKind: RB.BoundKind)(
      implicit protected val comp: Comparator[_ >: K])
      extends AbstractSet[Map.Entry[K, V]] {

    def iterator(): Iterator[Map.Entry[K, V]] =
      RB.projectionIterator(tree, lowerBound, lowerKind, upperBound, upperKind)

    def size(): Int =
      RB.projectionSize(tree, lowerBound, lowerKind, upperBound, upperKind)

    override def contains(o: Any): Boolean = o match {
      case o: Map.Entry[_, _] if isWithinBounds(o.getKey()) =>
        val node = RB.getNode(tree, o.getKey())
        (node ne null) && Objects.equals(node.getValue(), o.getValue())
      case _ =>
        false
    }

    override def remove(o: Any): Boolean = o match {
      case o: Map.Entry[_, _] if isWithinBounds(o.getKey()) =>
        val node = RB.getNode(tree, o.getKey())
        if ((node ne null) && Objects.equals(node.getValue(), o.getValue())) {
          RB.deleteNode(tree, node)
          true
        } else {
          false
        }
      case _ =>
        false
    }

    private def isWithinBounds(key: Any): Boolean = {
      RB.isWithinLowerBound(key, lowerBound, lowerKind) && RB.isWithinUpperBound(
          key, upperBound, upperKind)
    }
  }

  private abstract class AbstractProjection[K, V](
      protected val tree: RB.Tree[K, V],
      protected val lowerBound: K,
      protected val lowerKind: RB.BoundKind,
      protected val upperBound: K,
      protected val upperKind: RB.BoundKind
  )(
      implicit protected val comp: Comparator[_ >: K])
      extends AbstractMap[K, V] with NavigableMap[K, V] {

    // To be implemented by the two concrete subclasses, depending on the order

    protected def nextNode(key: K, boundKind: RB.BoundKind): RB.Node[K, V]
    protected def previousNode(key: K, boundKind: RB.BoundKind): RB.Node[K, V]

    protected def subMapGeneric(newFromKey: K = null.asInstanceOf[K],
        newFromBoundKind: RB.BoundKind = RB.NoBound,
        newToKey: K = null.asInstanceOf[K],
        newToBoundKind: RB.BoundKind = RB.NoBound): NavigableMap[K, V]

    // Implementation of most of the NavigableMap API

    override def size(): Int =
      RB.projectionSize(tree, lowerBound, lowerKind, upperBound, upperKind)

    override def isEmpty(): Boolean =
      RB.projectionIsEmpty(tree, lowerBound, lowerKind, upperBound, upperKind)

    override def containsKey(key: Any): Boolean =
      isWithinBounds(key) && RB.contains(tree, key)

    override def get(key: Any): V = {
      if (!isWithinBounds(key))
        null.asInstanceOf[V]
      else
        RB.get(tree, key)
    }

    override def put(key: K, value: V): V = {
      if (!isWithinBounds(key))
        throw new IllegalArgumentException
      RB.insert(tree, key, value)
    }

    override def remove(key: Any): V = {
      val oldNode =
        if (isWithinBounds(key)) RB.delete(tree, key)
        else null
      RB.nullableNodeValue(oldNode)
    }

    def entrySet(): Set[Map.Entry[K, V]] =
      new ProjectedEntrySet(tree, lowerBound, lowerKind, upperBound, upperKind)

    def lowerEntry(key: K): Map.Entry[K, V] =
      previousNode(key, RB.ExclusiveBound)

    def lowerKey(key: K): K =
      RB.nullableNodeKey(previousNode(key, RB.ExclusiveBound))

    def floorEntry(key: K): Map.Entry[K, V] =
      previousNode(key, RB.InclusiveBound)

    def floorKey(key: K): K =
      RB.nullableNodeKey(previousNode(key, RB.InclusiveBound))

    def ceilingEntry(key: K): Map.Entry[K, V] =
      nextNode(key, RB.InclusiveBound)

    def ceilingKey(key: K): K =
      RB.nullableNodeKey(nextNode(key, RB.InclusiveBound))

    def higherEntry(key: K): Map.Entry[K, V] =
      nextNode(key, RB.ExclusiveBound)

    def higherKey(key: K): K =
      RB.nullableNodeKey(nextNode(key, RB.ExclusiveBound))

    def firstKey(): K = {
      val e = firstEntry()
      if (e eq null)
        throw new NoSuchElementException
      e.getKey()
    }

    def lastKey(): K = {
      val e = lastEntry()
      if (e eq null)
        throw new NoSuchElementException
      e.getKey()
    }

    def subMap(fromKey: K, fromInclusive: Boolean, toKey: K,
        toInclusive: Boolean): NavigableMap[K, V] = {
      subMapGeneric(
          fromKey, RB.boundKindFromIsInclusive(fromInclusive),
          toKey, RB.boundKindFromIsInclusive(toInclusive))
    }

    def headMap(toKey: K, inclusive: Boolean): NavigableMap[K, V] = {
      subMapGeneric(newToKey = toKey,
          newToBoundKind = RB.boundKindFromIsInclusive(inclusive))
    }

    def tailMap(fromKey: K, inclusive: Boolean): NavigableMap[K, V] = {
      subMapGeneric(newFromKey = fromKey,
          newFromBoundKind = RB.boundKindFromIsInclusive(inclusive))
    }

    def subMap(fromKey: K, toKey: K): SortedMap[K, V] =
      subMap(fromKey, true, toKey, false)

    def headMap(toKey: K): SortedMap[K, V] =
      headMap(toKey, false)

    def tailMap(fromKey: K): SortedMap[K, V] =
      tailMap(fromKey, true)

    // Common implementation of pollFirstEntry() and pollLastEntry()

    @inline
    protected final def pollLowerEntry(): Map.Entry[K, V] = {
      val node = RB.minNodeAfter(tree, lowerBound, lowerKind)
      if (node ne null) {
        if (isWithinUpperBound(node.key)) {
          RB.deleteNode(tree, node)
          node
        } else {
          null
        }
      } else {
        null
      }
    }

    @inline
    protected final def pollUpperEntry(): Map.Entry[K, V] = {
      val node = RB.maxNodeBefore(tree, upperBound, upperKind)
      if (node ne null) {
        if (isWithinLowerBound(node.key)) {
          RB.deleteNode(tree, node)
          node
        } else {
          null
        }
      } else {
        null
      }
    }

    // Helpers

    protected final def isWithinBounds(key: Any): Boolean =
      isWithinLowerBound(key) && isWithinUpperBound(key)

    protected final def isWithinLowerBound(key: Any): Boolean =
      RB.isWithinLowerBound(key, lowerBound, lowerKind)

    protected final def isWithinUpperBound(key: Any): Boolean =
      RB.isWithinUpperBound(key, upperBound, upperKind)

    protected final def ifWithinLowerBound(node: RB.Node[K, V]): RB.Node[K, V] =
      if (node != null && isWithinLowerBound(node.key)) node
      else null

    protected final def ifWithinUpperBound(node: RB.Node[K, V]): RB.Node[K, V] =
      if (node != null && isWithinUpperBound(node.key)) node
      else null
  }

  private final class Projection[K, V](
      tree0: RB.Tree[K, V], fromKey0: K, fromBoundKind0: RB.BoundKind,
      toKey0: K, toBoundKind0: RB.BoundKind)(
      implicit comp: Comparator[_ >: K])
      extends AbstractProjection[K, V](tree0, fromKey0, fromBoundKind0,
          toKey0, toBoundKind0) {

    // Access fields under a different name, more appropriate for some uses

    @inline private def fromKey: K = lowerBound
    @inline private def fromBoundKind: RB.BoundKind = lowerKind
    @inline private def toKey: K = upperBound
    @inline private def toBoundKind: RB.BoundKind = upperKind

    /* Implementation of the abstract methods from AbstractProjection
     * Some are marked `@inline` for the likely case where
     * `DescendingProjection` is not reachable at all and hence
     * dead-code-eliminated.
     */

    @inline
    protected def nextNode(key: K, boundKind: RB.BoundKind): RB.Node[K, V] =
      ifWithinUpperBound(RB.minNodeAfter(tree, key, boundKind))

    @inline
    protected def previousNode(key: K, boundKind: RB.BoundKind): RB.Node[K, V] =
      ifWithinLowerBound(RB.maxNodeBefore(tree, key, boundKind))

    protected def subMapGeneric(
        newFromKey: K, newFromBoundKind: RB.BoundKind,
        newToKey: K, newToBoundKind: RB.BoundKind): NavigableMap[K, V] = {
      val intersectedFromBound = RB.intersectLowerBounds(
          new RB.Bound(fromKey, fromBoundKind),
          new RB.Bound(newFromKey, newFromBoundKind))
      val intersectedToBound = RB.intersectUpperBounds(
          new RB.Bound(toKey, toBoundKind),
          new RB.Bound(newToKey, newToBoundKind))
      new Projection(tree,
          intersectedFromBound.bound, intersectedFromBound.kind,
          intersectedToBound.bound, intersectedToBound.kind)
    }

    // Methods of the NavigableMap API that are not implemented in AbstractProjection

    def comparator(): Comparator[_ >: K] =
      NaturalComparator.unselect(comp)

    def firstEntry(): Map.Entry[K, V] =
      nextNode(fromKey, fromBoundKind)

    def lastEntry(): Map.Entry[K, V] =
      previousNode(toKey, toBoundKind)

    @noinline
    def pollFirstEntry(): Map.Entry[K, V] =
      pollLowerEntry()

    @noinline
    def pollLastEntry(): Map.Entry[K, V] =
      pollUpperEntry()

    def navigableKeySet(): NavigableSet[K] = {
      new TreeSet.Projection(tree, fromKey, fromBoundKind,
          toKey, toBoundKind, null.asInstanceOf[V])
    }

    def descendingKeySet(): NavigableSet[K] = {
      new TreeSet.DescendingProjection(tree, toKey, toBoundKind,
          fromKey, fromBoundKind, null.asInstanceOf[V])
    }

    def descendingMap(): NavigableMap[K, V] = {
      new DescendingProjection(tree, toKey, toBoundKind,
          fromKey, fromBoundKind)
    }
  }

  private final class DescendingProjection[K, V](
      tree0: RB.Tree[K, V], fromKey0: K, fromBoundKind0: RB.BoundKind,
      toKey0: K, toBoundKind0: RB.BoundKind)(
      implicit comp: Comparator[_ >: K])
      extends AbstractProjection[K, V](tree0, toKey0, toBoundKind0,
          fromKey0, fromBoundKind0) {

    // Access fields under a different name, more appropriate for some uses

    @inline private def fromKey: K = upperBound
    @inline private def fromBoundKind: RB.BoundKind = upperKind
    @inline private def toKey: K = lowerBound
    @inline private def toBoundKind: RB.BoundKind = lowerKind

    // Implementation of the abstract methods from AbstractProjection

    protected def nextNode(key: K, boundKind: RB.BoundKind): RB.Node[K, V] =
      ifWithinLowerBound(RB.maxNodeBefore(tree, key, boundKind))

    protected def previousNode(key: K, boundKind: RB.BoundKind): RB.Node[K, V] =
      ifWithinUpperBound(RB.minNodeAfter(tree, key, boundKind))

    protected def subMapGeneric(
        newFromKey: K, newFromBoundKind: RB.BoundKind,
        newToKey: K, newToBoundKind: RB.BoundKind): NavigableMap[K, V] = {
      val intersectedFromBound = RB.intersectUpperBounds(
          new RB.Bound(fromKey, fromBoundKind),
          new RB.Bound(newFromKey, newFromBoundKind))
      val intersectedToBound = RB.intersectLowerBounds(
          new RB.Bound(toKey, toBoundKind),
          new RB.Bound(newToKey, newToBoundKind))
      new Projection(tree,
          intersectedFromBound.bound, intersectedFromBound.kind,
          intersectedToBound.bound, intersectedToBound.kind)
    }

    // Methods of the NavigableMap API that are not implemented in AbstractProjection

    def comparator(): Comparator[_ >: K] =
      Collections.reverseOrder(NaturalComparator.unselect(comp))

    def firstEntry(): Map.Entry[K, V] =
      nextNode(fromKey, fromBoundKind)

    def lastEntry(): Map.Entry[K, V] =
      previousNode(toKey, toBoundKind)

    @noinline
    def pollFirstEntry(): Map.Entry[K, V] =
      pollUpperEntry()

    @noinline
    def pollLastEntry(): Map.Entry[K, V] =
      pollLowerEntry()

    def navigableKeySet(): NavigableSet[K] = {
      new TreeSet.DescendingProjection(tree, fromKey, fromBoundKind,
          toKey, toBoundKind, null.asInstanceOf[V])
    }

    def descendingKeySet(): NavigableSet[K] = {
      new TreeSet.Projection(tree, toKey, toBoundKind,
          fromKey, fromBoundKind, null.asInstanceOf[V])
    }

    def descendingMap(): NavigableMap[K, V] =
      new Projection(tree, toKey, toBoundKind, fromKey, fromBoundKind)
  }
}
