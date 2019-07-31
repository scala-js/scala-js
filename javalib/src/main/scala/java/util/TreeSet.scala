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

import java.lang.Comparable

import scala.math.Ordering

import scala.collection.mutable

import Compat.SortedSetCompat
import ScalaOps._

class TreeSet[E] (_comparator: Comparator[_ >: E])
    extends AbstractSet[E]
    with NavigableSet[E]
    with Cloneable
    with Serializable { self =>

  def this() =
    this(null.asInstanceOf[Comparator[_ >: E]])

  def this(collection: Collection[_ <: E]) = {
    this(null.asInstanceOf[Comparator[E]])
    addAll(collection)
  }

  def this(sortedSet: SortedSet[E]) = {
    this(sortedSet.comparator())
    addAll(sortedSet)
  }

  private implicit object BoxOrdering extends Ordering[Box[E]] {

    val cmp = {
      if (_comparator ne null) _comparator
      else defaultOrdering[E]
    }

    def compare(a: Box[E], b: Box[E]): Int = cmp.compare(a.inner, b.inner)

  }

  private val inner: mutable.TreeSet[Box[E]] = new mutable.TreeSet[Box[E]]()

  def iterator(): Iterator[E] = {
    new Iterator[E] {
      private val iter = inner.clone.iterator

      private var last: Option[E] = None

      def hasNext(): Boolean = iter.hasNext

      def next(): E = {
        last = Some(iter.next().inner)
        last.get
      }

      def remove(): Unit = {
        if (last.isEmpty) {
          throw new IllegalStateException()
        } else {
          last.foreach(self.remove(_))
          last = None
        }
      }
    }
  }

  def descendingIterator(): Iterator[E] = {
    new Iterator[E] {
      private val iter = inner.iterator.toList.reverse.iterator

      private var last: Option[E] = None

      def hasNext(): Boolean = iter.hasNext

      def next(): E = {
        val nxt = iter.next().inner
        last = Some(nxt)
        nxt
      }

      def remove(): Unit = {
        if (last.isEmpty) {
          throw new IllegalStateException()
        } else {
          last.foreach(self.remove(_))
          last = None
        }
      }
    }
  }

  def descendingSet(): NavigableSet[E] = {
    val descSetFun = { () =>
      val retSet = new mutable.TreeSet[Box[E]]()(BoxOrdering.reverse)
      retSet ++= inner
      retSet
    }
    new NavigableView(this, descSetFun, None, true, None, true)
  }

  def size(): Int =
    inner.size

  override def isEmpty(): Boolean =
    inner.headOption.isEmpty

  override def contains(o: Any): Boolean =
    inner.contains(Box(o.asInstanceOf[E]))

  override def add(e: E): Boolean = {
    val boxed = Box(e)

    if (isEmpty)
      BoxOrdering.compare(boxed, boxed)

    inner.add(boxed)
  }

  override def remove(o: Any): Boolean =
    inner.remove(Box(o.asInstanceOf[E]))

  override def clear(): Unit =
    inner.clear()

  override def addAll(c: Collection[_ <: E]): Boolean = {
    val iter = c.iterator()
    var changed = false
    while (iter.hasNext)
      changed = add(iter.next()) || changed
    changed
  }

  override def removeAll(c: Collection[_]): Boolean = {
    val iter = c.iterator()
    var changed = false
    while (iter.hasNext)
      changed = inner.remove(Box(iter.next).asInstanceOf[Box[E]]) || changed
    changed
  }

  def subSet(fromElement: E, fromInclusive: Boolean, toElement: E,
      toInclusive: Boolean): NavigableSet[E] = {
    val boxedFrom = Box(fromElement)
    val boxedTo = Box(toElement)

    val subSetFun = { () =>
      val base = new mutable.TreeSet[Box[E]]
      base ++= inner.range(boxedFrom, boxedTo)
      if (!fromInclusive)
        base -= boxedFrom
      if (toInclusive && inner.contains(boxedTo))
        base += boxedTo
      base
    }

    new NavigableView(this, subSetFun,
        Some(fromElement), fromInclusive,
        Some(toElement), toInclusive)
  }

  def headSet(toElement: E, inclusive: Boolean): NavigableSet[E] = {
    val boxed = Box(toElement)

    val headSetFun = { () =>
      val base = new mutable.TreeSet[Box[E]]
      if (inclusive)
        base ++= inner.rangeTo(boxed)
      else
        base ++= inner.rangeUntil(boxed)
      base
    }

    new NavigableView(this, headSetFun,
        None, true,
        Some(toElement), inclusive)
  }

  def tailSet(fromElement: E, inclusive: Boolean): NavigableSet[E] = {
    val boxed = Box(fromElement)

    val tailSetFun = { () =>
      val base = new mutable.TreeSet[Box[E]]
      base ++= inner.rangeFrom(boxed)
      if (!inclusive)
        base -= boxed
      base
    }

    new NavigableView(this, tailSetFun,
        Some(fromElement), inclusive,
        None, true)
  }

  def subSet(fromElement: E, toElement: E): SortedSet[E] =
    subSet(fromElement, true, toElement, false)

  def headSet(toElement: E): SortedSet[E] =
    headSet(toElement, false)

  def tailSet(fromElement: E): SortedSet[E] =
    tailSet(fromElement, true)

  def comparator(): Comparator[_ >: E] = _comparator

  def first(): E =
    inner.head.inner

  def last(): E =
    inner.last.inner

  def lower(e: E): E =
    headSet(e, false).last()

  def floor(e: E): E =
    headSet(e, true).last()

  def ceiling(e: E): E =
    tailSet(e, true).first()

  def higher(e: E): E =
    tailSet(e, false).first()

  def pollFirst(): E = {
    val polled = inner.headOption
    if (polled.isDefined) {
      val elem = polled.get.inner
      remove(elem)
      elem
    } else null.asInstanceOf[E]
  }

  def pollLast(): E = {
    val polled = inner.lastOption
    if (polled.isDefined) {
      val elem = polled.get.inner
      remove(elem)
      elem
    } else null.asInstanceOf[E]
  }

  override def clone(): TreeSet[E] =
    new TreeSet(this)
}
