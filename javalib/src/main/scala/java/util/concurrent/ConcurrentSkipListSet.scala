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

import java.util._

class ConcurrentSkipListSet[E] private (inner: TreeSet[E])
    extends AbstractSet[E]
    with NavigableSet[E]
    with Cloneable
    with Serializable {

  def this(collection: Collection[_ <: E]) =
    this(new TreeSet[E](collection))

  def this() =
    this(new TreeSet[E]())

  def this(comparator: Comparator[_ >: E]) =
    this(new TreeSet[E](comparator))

  def this(sortedSet: SortedSet[E]) =
    this(new TreeSet[E](sortedSet))

  override def clone(): ConcurrentSkipListSet[E] =
    new ConcurrentSkipListSet(this)

  def size(): Int =
    inner.size

  override def isEmpty(): Boolean =
    inner.isEmpty

  override def contains(o: Any): Boolean =
    if (o == null) false
    else inner.contains(o)

  override def add(e: E): Boolean =
    if (e == null) throw new NullPointerException()
    else inner.add(e)

  override def remove(o: Any): Boolean =
    if (o == null) throw new NullPointerException()
    else inner.remove(o)

  override def clear(): Unit =
    inner.clear()

  def iterator(): Iterator[E] =
    inner.iterator()

  def descendingIterator(): Iterator[E] =
    inner.descendingIterator()

  override def removeAll(c: Collection[_]): Boolean =
    inner.removeAll(c)

  def lower(e: E): E =
    inner.lower(e)

  def floor(e: E): E =
    inner.floor(e)

  def ceiling(e: E): E =
    inner.ceiling(e)

  def higher(e: E): E =
    inner.higher(e)

  def pollFirst(): E =
    inner.pollFirst()

  def pollLast(): E =
    inner.pollLast()

  def comparator(): Comparator[_ >: E] =
    inner.comparator()

  def first(): E =
    inner.first

  def last(): E =
    inner.last

  def subSet(fromElement: E, fromInclusive: Boolean, toElement: E,
      toInclusive: Boolean): NavigableSet[E] =
    inner.subSet(fromElement, fromInclusive, toElement, toInclusive)

  def headSet(toElement: E, inclusive: Boolean): NavigableSet[E] =
    inner.headSet(toElement, inclusive)

  def tailSet(fromElement: E, inclusive: Boolean): NavigableSet[E] =
    inner.tailSet(fromElement, inclusive)

  def subSet(fromElement: E, toElement: E): NavigableSet[E] =
    inner.subSet(fromElement, true, toElement, false)

  def headSet(toElement: E): NavigableSet[E] =
    inner.headSet(toElement, false)

  def tailSet(fromElement: E): NavigableSet[E] =
    inner.tailSet(fromElement, true)

  def descendingSet(): NavigableSet[E] =
    inner.descendingSet()
}
