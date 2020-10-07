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

import scala.annotation.tailrec

import scala.scalajs.js

class PriorityQueue[E] private (
    private val comp: Comparator[_ >: E], internal: Boolean)
    extends AbstractQueue[E] with Serializable {

  def this() =
    this(NaturalComparator, internal = true)

  def this(initialCapacity: Int) = {
    this()
    if (initialCapacity < 1)
      throw new IllegalArgumentException()
  }

  def this(comparator: Comparator[_ >: E]) = {
    this(NaturalComparator.select(comparator), internal = true)
  }

  def this(initialCapacity: Int, comparator: Comparator[_ >: E]) = {
    this(comparator)
    if (initialCapacity < 1)
      throw new IllegalArgumentException()
  }

  def this(c: Collection[_ <: E]) = {
    this(c match {
      case c: PriorityQueue[_] =>
        c.comp.asInstanceOf[Comparator[_ >: E]]
      case c: SortedSet[_] =>
        NaturalComparator.select(c.comparator().asInstanceOf[Comparator[_ >: E]])
      case _ =>
        NaturalComparator
    }, internal = true)
    addAll(c)
  }

  def this(c: PriorityQueue[_ <: E]) = {
    this(c.comp.asInstanceOf[Comparator[_ >: E]], internal = true)
    addAll(c)
  }

  def this(sortedSet: SortedSet[_ <: E]) = {
    this(NaturalComparator.select(
        sortedSet.comparator().asInstanceOf[Comparator[_ >: E]]),
        internal = true)
    addAll(sortedSet)
  }

  // The index 0 is not used; the root is at index 1.
  // This is standard practice in binary heaps, to simplify arithmetics.
  private[this] val inner = js.Array[E](null.asInstanceOf[E])

  override def add(e: E): Boolean = {
    if (e == null)
      throw new NullPointerException()
    inner.push(e)
    fixUp(inner.length - 1)
    true
  }

  def offer(e: E): Boolean = add(e)

  def peek(): E =
    if (inner.length > 1) inner(1)
    else null.asInstanceOf[E]

  override def remove(o: Any): Boolean = {
    if (o == null) {
      false
    } else {
      val len = inner.length
      var i = 1
      while (i != len && !o.equals(inner(i))) {
        i += 1
      }

      if (i != len) {
        removeAt(i)
        true
      } else {
        false
      }
    }
  }

  private def removeExact(o: Any): Unit = {
    val len = inner.length
    var i = 1
    while (i != len && (o.asInstanceOf[AnyRef] ne inner(i).asInstanceOf[AnyRef])) {
      i += 1
    }
    if (i == len)
      throw new ConcurrentModificationException()
    removeAt(i)
  }

  private def removeAt(i: Int): Unit = {
    val newLength = inner.length - 1
    if (i == newLength) {
      inner.length = newLength
    } else {
      inner(i) = inner(newLength)
      inner.length = newLength
      fixUpOrDown(i)
    }
  }

  override def contains(o: Any): Boolean = {
    if (o == null) {
      false
    } else {
      val len = inner.length
      var i = 1
      while (i != len && !o.equals(inner(i))) {
        i += 1
      }
      i != len
    }
  }

  def iterator(): Iterator[E] = {
    new Iterator[E] {
      private[this] var inner: js.Array[E] = PriorityQueue.this.inner
      private[this] var nextIdx: Int = 1
      private[this] var last: E = _ // null

      def hasNext(): Boolean = nextIdx < inner.length

      def next(): E = {
        if (!hasNext())
          throw new NoSuchElementException("empty iterator")
        last = inner(nextIdx)
        nextIdx += 1
        last
      }

      override def remove(): Unit = {
        /* Once we start removing elements, the inner array of the enclosing
         * PriorityQueue will be modified in arbitrary ways. In particular,
         * entries yet to be iterated can be moved before `nextIdx` if the
         * removal requires a `fixUp()`.
         *
         * Therefore, at the first removal, we take a snapshot of the remainder
         * of the inner array yet to be iterated, and continue iterating over
         * the snapshot.
         *
         * We use a linear lookup based on reference equality to precisely
         * remove the entries that we are still iterating over (in
         * `removeExact()`).
         *
         * This means that this method is O(n), contrary to typical
         * expectations for `Iterator.remove()`. I could not come up with a
         * better algorithm.
         */

        if (last == null)
          throw new IllegalStateException()
        if (inner eq PriorityQueue.this.inner) {
          inner = inner.jsSlice(nextIdx)
          nextIdx = 0
        }
        removeExact(last)
        last = null.asInstanceOf[E]
      }
    }
  }

  def size(): Int = inner.length - 1

  override def clear(): Unit =
    inner.length = 1

  def poll(): E = {
    val inner = this.inner // local copy
    if (inner.length > 1) {
      val newSize = inner.length - 1
      val result = inner(1)
      inner(1) = inner(newSize)
      inner.length = newSize
      fixDown(1)
      result
    } else {
      null.asInstanceOf[E]
    }
  }

  def comparator(): Comparator[_ >: E] =
    NaturalComparator.unselect(comp)

  // Heavy lifting: heap fixup

  /** Fixes the heap property around the child at index `m`, either up the
   *  tree or down the tree, depending on which side is found to violate the
   *  heap property.
   */
  private[this] def fixUpOrDown(m: Int): Unit = {
    val inner = this.inner // local copy
    if (m > 1 && comp.compare(inner(m >> 1), inner(m)) > 0)
      fixUp(m)
    else
      fixDown(m)
  }

  /** Fixes the heap property from the child at index `m` up the tree, towards
   *  the root.
   */
  private[this] def fixUp(m: Int): Unit = {
    val inner = this.inner // local copy

    /* At each step, even though `m` changes, the element moves with it, and
     * hence inner(m) is always the same initial `innerAtM`.
     */
    val innerAtM = inner(m)

    @inline @tailrec
    def loop(m: Int): Unit = {
      if (m > 1) {
        val parent = m >> 1
        val innerAtParent = inner(parent)
        if (comp.compare(innerAtParent, innerAtM) > 0) {
          inner(parent) = innerAtM
          inner(m) = innerAtParent
          loop(parent)
        }
      }
    }

    loop(m)
  }

  /** Fixes the heap property from the child at index `m` down the tree,
   *  towards the leaves.
   */
  private[this] def fixDown(m: Int): Unit = {
    val inner = this.inner // local copy
    val size = inner.length - 1

    /* At each step, even though `m` changes, the element moves with it, and
     * hence inner(m) is always the same initial `innerAtM`.
     */
    val innerAtM = inner(m)

    @inline @tailrec
    def loop(m: Int): Unit = {
      var j = 2 * m // left child of `m`
      if (j <= size) {
        var innerAtJ = inner(j)

        // if the left child is greater than the right child, switch to the right child
        if (j < size) {
          val innerAtJPlus1 = inner(j + 1)
          if (comp.compare(innerAtJ, innerAtJPlus1) > 0) {
            j += 1
            innerAtJ = innerAtJPlus1
          }
        }

        // if the node `m` is greater than the selected child, swap and recurse
        if (comp.compare(innerAtM, innerAtJ) > 0) {
          inner(m) = innerAtJ
          inner(j) = innerAtM
          loop(j)
        }
      }
    }

    loop(m)
  }
}
