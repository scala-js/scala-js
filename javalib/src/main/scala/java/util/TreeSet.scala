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

class TreeSet[E] private (tree: RB.Tree[E, Any])(
    implicit comp: Comparator[_ >: E])
    extends AbstractSet[E] with NavigableSet[E] with Cloneable with Serializable {

  import TreeSet._

  /* Note: in practice, the values of `tree` are always `()` (aka `undefined`).
   * We use `Any` because we need to deal with `null`s, and referencing
   * `scala.runtime.BoxedUnit` in this code would be really ugly.
   */

  def this() =
    this(RB.Tree.empty[E, Any])(NaturalComparator)

  def this(comparator: Comparator[_ >: E]) =
    this(RB.Tree.empty[E, Any])(NaturalComparator.select(comparator))

  def this(collection: Collection[_ <: E]) = {
    this()
    addAll(collection)
  }

  def this(sortedSet: SortedSet[E]) = {
    this(RB.fromOrderedKeys(sortedSet.iterator(), sortedSet.size()))(
        NaturalComparator.select(sortedSet.comparator()))
  }

  def iterator(): Iterator[E] =
    RB.keysIterator(tree)

  def descendingIterator(): Iterator[E] =
    RB.descendingKeysIterator(tree)

  def descendingSet(): NavigableSet[E] = {
    new DescendingProjection(tree, null.asInstanceOf[E], RB.NoBound,
        null.asInstanceOf[E], RB.NoBound, ())
  }

  def size(): Int =
    RB.size(tree)

  override def isEmpty(): Boolean =
    RB.isEmpty(tree)

  override def contains(o: Any): Boolean =
    RB.contains(tree, o)

  override def add(e: E): Boolean =
    RB.insert(tree, e, ()) == null

  override def remove(o: Any): Boolean =
    RB.delete(tree, o) != null

  override def clear(): Unit =
    RB.clear(tree)

  def subSet(fromElement: E, fromInclusive: Boolean, toElement: E,
      toInclusive: Boolean): NavigableSet[E] = {
    new Projection(tree,
        fromElement, RB.boundKindFromIsInclusive(fromInclusive),
        toElement, RB.boundKindFromIsInclusive(toInclusive), ())
  }

  def headSet(toElement: E, inclusive: Boolean): NavigableSet[E] = {
    new Projection(tree,
        null.asInstanceOf[E], RB.NoBound,
        toElement, RB.boundKindFromIsInclusive(inclusive), ())
  }

  def tailSet(fromElement: E, inclusive: Boolean): NavigableSet[E] = {
    new Projection(tree,
        fromElement, RB.boundKindFromIsInclusive(inclusive),
        null.asInstanceOf[E], RB.NoBound, ())
  }

  def subSet(fromElement: E, toElement: E): SortedSet[E] =
    subSet(fromElement, true, toElement, false)

  def headSet(toElement: E): SortedSet[E] =
    headSet(toElement, false)

  def tailSet(fromElement: E): SortedSet[E] =
    tailSet(fromElement, true)

  def comparator(): Comparator[_ >: E] =
    NaturalComparator.unselect(comp)

  def first(): E = {
    if (isEmpty())
      throw new NoSuchElementException()
    RB.minKey(tree)
  }

  def last(): E = {
    if (isEmpty())
      throw new NoSuchElementException()
    RB.maxKey(tree)
  }

  def lower(e: E): E =
    RB.maxKeyBefore(tree, e, RB.ExclusiveBound)

  def floor(e: E): E =
    RB.maxKeyBefore(tree, e, RB.InclusiveBound)

  def ceiling(e: E): E =
    RB.minKeyAfter(tree, e, RB.InclusiveBound)

  def higher(e: E): E =
    RB.minKeyAfter(tree, e, RB.ExclusiveBound)

  def pollFirst(): E = {
    val node = RB.minNode(tree)
    if (node ne null) {
      RB.deleteNode(tree, node)
      node.key
    } else {
      null.asInstanceOf[E]
    }
  }

  def pollLast(): E = {
    val node = RB.maxNode(tree)
    if (node ne null) {
      RB.deleteNode(tree, node)
      node.key
    } else {
      null.asInstanceOf[E]
    }
  }

  override def clone(): TreeSet[E] =
    new TreeSet(tree.treeCopy())(comp)
}

private[util] object TreeSet {
  private[util] abstract class AbstractProjection[E, V](
      protected val tree: RB.Tree[E, V],
      protected val lowerBound: E, protected val lowerKind: RB.BoundKind,
      protected val upperBound: E, protected val upperKind: RB.BoundKind,
      private val valueForAdd: V
  )(
      implicit protected val comp: Comparator[_ >: E])
      extends AbstractSet[E] with NavigableSet[E] {

    // To be implemented by the two concrete subclasses, depending on the order

    protected def nextKey(key: E, boundKind: RB.BoundKind): E
    protected def previousKey(key: E, boundKind: RB.BoundKind): E

    protected def subSetGeneric(newFromElement: E = null.asInstanceOf[E],
        newFromBoundKind: RB.BoundKind = RB.NoBound,
        newToElement: E = null.asInstanceOf[E],
        newToBoundKind: RB.BoundKind = RB.NoBound): NavigableSet[E]

    // Implementation of most of the NavigableSet API

    def size(): Int =
      RB.projectionSize(tree, lowerBound, lowerKind, upperBound, upperKind)

    override def isEmpty(): Boolean =
      RB.projectionIsEmpty(tree, lowerBound, lowerKind, upperBound, upperKind)

    override def contains(o: Any): Boolean =
      isWithinBounds(o) && RB.contains(tree, o)

    override def add(e: E): Boolean = {
      if (valueForAdd == null)
        throw new UnsupportedOperationException
      if (!isWithinBounds(e))
        throw new IllegalArgumentException
      RB.insert(tree, e, valueForAdd) == null
    }

    override def remove(o: Any): Boolean =
      isWithinBounds(o) && RB.delete(tree, o) != null

    def lower(e: E): E =
      previousKey(e, RB.ExclusiveBound)

    def floor(e: E): E =
      previousKey(e, RB.InclusiveBound)

    def ceiling(e: E): E =
      nextKey(e, RB.InclusiveBound)

    def higher(e: E): E =
      nextKey(e, RB.ExclusiveBound)

    def subSet(fromElement: E, fromInclusive: Boolean, toElement: E,
        toInclusive: Boolean): NavigableSet[E] = {
      subSetGeneric(
          fromElement, RB.boundKindFromIsInclusive(fromInclusive),
          toElement, RB.boundKindFromIsInclusive(toInclusive))
    }

    def headSet(toElement: E, inclusive: Boolean): NavigableSet[E] = {
      subSetGeneric(newToElement = toElement,
          newToBoundKind = RB.boundKindFromIsInclusive(inclusive))
    }

    def tailSet(fromElement: E, inclusive: Boolean): NavigableSet[E] = {
      subSetGeneric(newFromElement = fromElement,
          newFromBoundKind = RB.boundKindFromIsInclusive(inclusive))
    }

    def subSet(fromElement: E, toElement: E): SortedSet[E] =
      subSet(fromElement, true, toElement, false)

    def headSet(toElement: E): SortedSet[E] =
      headSet(toElement, false)

    def tailSet(fromElement: E): SortedSet[E] =
      tailSet(fromElement, true)

    // Common implementation of pollFirst() and pollLast()

    @inline
    protected final def pollLower(): E = {
      val node = RB.minNodeAfter(tree, lowerBound, lowerKind)
      if (node ne null) {
        val key = node.key
        if (isWithinUpperBound(key)) {
          RB.deleteNode(tree, node)
          key
        } else {
          null.asInstanceOf[E]
        }
      } else {
        null.asInstanceOf[E]
      }
    }

    @inline
    protected final def pollUpper(): E = {
      val node = RB.maxNodeBefore(tree, upperBound, upperKind)
      if (node ne null) {
        val key = node.key
        if (isWithinLowerBound(key)) {
          RB.deleteNode(tree, node)
          key
        } else {
          null.asInstanceOf[E]
        }
      } else {
        null.asInstanceOf[E]
      }
    }

    // Helpers

    protected final def isWithinBounds(key: Any): Boolean =
      isWithinLowerBound(key) && isWithinUpperBound(key)

    protected final def isWithinLowerBound(key: Any): Boolean =
      RB.isWithinLowerBound(key, lowerBound, lowerKind)

    protected final def isWithinUpperBound(key: Any): Boolean =
      RB.isWithinUpperBound(key, upperBound, upperKind)

    protected final def ifWithinLowerBound(e: E): E =
      if (e != null && isWithinLowerBound(e)) e
      else null.asInstanceOf[E]

    protected final def ifWithinUpperBound(e: E): E =
      if (e != null && isWithinUpperBound(e)) e
      else null.asInstanceOf[E]
  }

  private[util] final class Projection[E, V](
      tree0: RB.Tree[E, V], fromElement0: E, fromBoundKind0: RB.BoundKind,
      toElement0: E, toBoundKind0: RB.BoundKind, valueForAdd: V)(
      implicit comp: Comparator[_ >: E])
      extends AbstractProjection[E, V](tree0, fromElement0, fromBoundKind0,
          toElement0, toBoundKind0, valueForAdd) {

    // Access fields under a different name, more appropriate for some uses

    @inline private def fromElement: E = lowerBound
    @inline private def fromBoundKind: RB.BoundKind = lowerKind
    @inline private def toElement: E = upperBound
    @inline private def toBoundKind: RB.BoundKind = upperKind

    /* Implementation of the abstract methods from AbstractProjection
     * Some are marked `@inline` for the likely case where
     * `DescendingProjection` is not reachable at all and hence
     * dead-code-eliminated.
     */

    @inline
    protected def nextKey(key: E, boundKind: RB.BoundKind): E =
      ifWithinUpperBound(RB.minKeyAfter(tree, key, boundKind))

    @inline
    protected def previousKey(key: E, boundKind: RB.BoundKind): E =
      ifWithinLowerBound(RB.maxKeyBefore(tree, key, boundKind))

    protected def subSetGeneric(
        newFromElement: E, newFromBoundKind: RB.BoundKind,
        newToElement: E, newToBoundKind: RB.BoundKind): NavigableSet[E] = {
      val intersectedFromBound = RB.intersectLowerBounds(
          new RB.Bound(fromElement, fromBoundKind),
          new RB.Bound(newFromElement, newFromBoundKind))
      val intersectedToBound = RB.intersectUpperBounds(
          new RB.Bound(toElement, toBoundKind),
          new RB.Bound(newToElement, newToBoundKind))
      new Projection(tree,
          intersectedFromBound.bound, intersectedFromBound.kind,
          intersectedToBound.bound, intersectedToBound.kind, valueForAdd)
    }

    // Methods of the NavigableSet API that are not implemented in AbstractProjection

    def iterator(): Iterator[E] =
      RB.projectionKeysIterator(tree, fromElement, fromBoundKind, toElement, toBoundKind)

    def comparator(): Comparator[_ >: E] =
      NaturalComparator.unselect(comp)

    def first(): E = {
      val key = nextKey(fromElement, fromBoundKind)
      if (key == null)
        throw new NoSuchElementException()
      key
    }

    def last(): E = {
      val key = previousKey(toElement, toBoundKind)
      if (key == null)
        throw new NoSuchElementException()
      key
    }

    @noinline
    def pollFirst(): E =
      pollLower()

    @noinline
    def pollLast(): E =
      pollUpper()

    def descendingSet(): NavigableSet[E] =
      new DescendingProjection(tree, toElement, toBoundKind, fromElement, fromBoundKind, valueForAdd)

    def descendingIterator(): Iterator[E] =
      RB.descendingKeysIterator(tree, toElement, toBoundKind, fromElement, fromBoundKind)
  }

  private[util] final class DescendingProjection[E, V](
      tree0: RB.Tree[E, V], fromElement0: E, fromBoundKind0: RB.BoundKind,
      toElement0: E, toBoundKind0: RB.BoundKind, valueForAdd: V)(
      implicit comp: Comparator[_ >: E])
      extends AbstractProjection[E, V](tree0, toElement0, toBoundKind0,
          fromElement0, fromBoundKind0, valueForAdd) {

    // Access fields under a different name, more appropriate for some uses

    @inline private def fromElement: E = upperBound
    @inline private def fromBoundKind: RB.BoundKind = upperKind
    @inline private def toElement: E = lowerBound
    @inline private def toBoundKind: RB.BoundKind = lowerKind

    // Implementation of the abstract methods from AbstractProjection

    protected def nextKey(key: E, boundKind: RB.BoundKind): E =
      ifWithinLowerBound(RB.maxKeyBefore(tree, key, boundKind))

    protected def previousKey(key: E, boundKind: RB.BoundKind): E =
      ifWithinUpperBound(RB.minKeyAfter(tree, key, boundKind))

    protected def subSetGeneric(
        newFromElement: E, newFromBoundKind: RB.BoundKind,
        newToElement: E, newToBoundKind: RB.BoundKind): NavigableSet[E] = {
      val intersectedFromBound = RB.intersectUpperBounds(
          new RB.Bound(fromElement, fromBoundKind),
          new RB.Bound(newFromElement, newFromBoundKind))
      val intersectedToBound = RB.intersectLowerBounds(
          new RB.Bound(toElement, toBoundKind),
          new RB.Bound(newToElement, newToBoundKind))
      new Projection(tree,
          intersectedFromBound.bound, intersectedFromBound.kind,
          intersectedToBound.bound, intersectedToBound.kind, valueForAdd)
    }

    // Methods of the NavigableSet API that are not implemented in AbstractProjection

    def iterator(): Iterator[E] =
      RB.descendingKeysIterator(tree, fromElement, fromBoundKind, toElement, toBoundKind)

    def comparator(): Comparator[_ >: E] =
      Collections.reverseOrder(NaturalComparator.unselect(comp))

    def first(): E = {
      val key = nextKey(fromElement, fromBoundKind)
      if (key == null)
        throw new NoSuchElementException()
      key
    }

    def last(): E = {
      val key = previousKey(toElement, toBoundKind)
      if (key == null)
        throw new NoSuchElementException()
      key
    }

    @noinline
    def pollFirst(): E =
      pollUpper()

    @noinline
    def pollLast(): E =
      pollLower()

    def descendingSet(): NavigableSet[E] =
      new Projection(tree, toElement, toBoundKind, fromElement, fromBoundKind, valueForAdd)

    def descendingIterator(): Iterator[E] =
      RB.projectionKeysIterator(tree, toElement, toBoundKind, fromElement, fromBoundKind)
  }
}
