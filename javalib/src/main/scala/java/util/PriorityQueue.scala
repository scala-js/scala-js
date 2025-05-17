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

import java.lang.Utils.roundUpToPowerOfTwo

import scala.scalajs.LinkingInfo

class PriorityQueue[E] private (
    private val comp: Comparator[_ >: E], internal: Boolean, initialCapacity: Int)
    extends AbstractQueue[E] with Serializable {

  import PriorityQueue._

  def this() =
    this(NaturalComparator, internal = true, initialCapacity = 16)

  def this(initialCapacity: Int) = {
    this(
      NaturalComparator,
      internal = true,
      {
        if (initialCapacity < 1)
          throw new IllegalArgumentException
        initialCapacity + 1 // index 0 is unused
      }
    )
  }

  def this(comparator: Comparator[_ >: E]) = {
    this(NaturalComparator.select(comparator), internal = true, initialCapacity = 16)
  }

  def this(initialCapacity: Int, comparator: Comparator[_ >: E]) = {
    this(
      NaturalComparator.select(comparator),
      internal = true,
      {
        if (initialCapacity < 1)
          throw new IllegalArgumentException()
        initialCapacity + 1 // index 0 is unused
      }
    )
  }

  def this(c: Collection[_ <: E]) = {
    this(c match {
      case c: PriorityQueue[_] =>
        c.comp.asInstanceOf[Comparator[_ >: E]]
      case c: SortedSet[_] =>
        NaturalComparator.select(c.comparator().asInstanceOf[Comparator[_ >: E]])
      case _ =>
        NaturalComparator
    }, internal = true, roundUpToPowerOfTwo(c.size() + 1)) // index 0 is unused
    addAll(c)
  }

  def this(c: PriorityQueue[_ <: E]) = {
    this(c.comp.asInstanceOf[Comparator[_ >: E]], internal = true,
        roundUpToPowerOfTwo(c.size() + 1)) // index 0 is unused
    addAll(c)
  }

  def this(sortedSet: SortedSet[_ <: E]) = {
    this(NaturalComparator.select(
        sortedSet.comparator().asInstanceOf[Comparator[_ >: E]]),
        internal = true,
        roundUpToPowerOfTwo(sortedSet.size() + 1)) // index 0 is unused
    addAll(sortedSet)
  }

  // The index 0 is not used; the root is at index 1.
  // This is standard practice in binary heaps, to simplify arithmetics.
  private var inner: innerImpl.Repr = innerImpl.make(initialCapacity)

  override def add(e: E): Boolean = {
    if (e == null)
      throw new NullPointerException()

    if (LinkingInfo.isWebAssembly) {
      val minCapacity = innerImpl.length(inner) + 1
      if (innerImpl.capacity(inner) < minCapacity)
        inner = innerImpl.resized(inner, minCapacity)
    }
    innerImpl.push(inner, e.asInstanceOf[AnyRef])
    fixUp(innerImpl.length(inner) - 1)
    true
  }

  def offer(e: E): Boolean = add(e)

  def peek(): E =
    if (innerImpl.length(inner) > 1) innerImpl.get(inner, 1).asInstanceOf[E]
    else null.asInstanceOf[E]

  override def remove(o: Any): Boolean = {
    if (o == null) {
      false
    } else {
      val len = innerImpl.length(inner)
      var i = 1
      while (i != len && !o.equals(innerImpl.get(inner, i))) {
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
    val len = innerImpl.length(inner)
    var i = 1
    while (i != len && (o.asInstanceOf[AnyRef] ne innerImpl.get(inner, i).asInstanceOf[AnyRef])) {
      i += 1
    }
    if (i == len)
      throw new ConcurrentModificationException()
    removeAt(i)
  }

  private def removeAt(i: Int): Unit = {
    val newLength = innerImpl.length(inner) - 1
    if (i == newLength) {
      innerImpl.setLength(inner, newLength)
    } else {
      innerImpl.set(inner, i, innerImpl.get(inner, newLength))
      innerImpl.setLength(inner, newLength)
      fixUpOrDown(i)
    }
    if (LinkingInfo.isWebAssembly)
      innerImpl.set(inner, innerImpl.length(inner), null) // free reference for GC
  }

  override def contains(o: Any): Boolean = {
    if (o == null) {
      false
    } else {
      val len = innerImpl.length(inner)
      var i = 1
      while (i != len && !o.equals(innerImpl.get(inner, i))) {
        i += 1
      }
      i != len
    }
  }

  def iterator(): Iterator[E] = {
    new Iterator[E] {
      private[this] var inner: innerImpl.Repr = PriorityQueue.this.inner
      private[this] var nextIdx: Int = 1
      private[this] var last: E = _ // null

      def hasNext(): Boolean = nextIdx < innerImpl.length(inner)

      def next(): E = {
        if (!hasNext())
          throw new NoSuchElementException("empty iterator")
        last = innerImpl.get(inner, nextIdx).asInstanceOf[E]
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
          inner = innerImpl.copyFrom(inner, nextIdx)
          nextIdx = 1
        }
        removeExact(last)
        last = null.asInstanceOf[E]
      }
    }
  }

  def size(): Int = innerImpl.length(inner) - 1

  override def clear(): Unit =
    innerImpl.clear(inner)

  def poll(): E = {
    val inner = this.inner // local copy
    if (innerImpl.length(inner) > 1) {
      val newSize = innerImpl.length(inner) - 1
      val result = innerImpl.get(inner, 1)
      innerImpl.set(inner, 1, innerImpl.get(inner, newSize))
      innerImpl.setLength(inner, newSize)
      fixDown(1)
      if (LinkingInfo.isWebAssembly)
        innerImpl.set(inner, newSize, null) // free reference for GC
      result.asInstanceOf[E]
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
    if (m > 1 && comp.compare(innerImpl.get(inner, m >> 1).asInstanceOf[E],
        innerImpl.get(inner, m).asInstanceOf[E]) > 0)
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
    val innerAtM = innerImpl.get(inner, m).asInstanceOf[E]

    @inline @tailrec
    def loop(m: Int): Unit = {
      if (m > 1) {
        val parent = m >> 1
        val innerAtParent = innerImpl.get(inner, parent)
        if (comp.compare(innerAtParent.asInstanceOf[E], innerAtM) > 0) {
          innerImpl.set(inner, parent, innerAtM.asInstanceOf[AnyRef])
          innerImpl.set(inner, m, innerAtParent)
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
    val size = innerImpl.length(inner) - 1

    /* At each step, even though `m` changes, the element moves with it, and
     * hence inner(m) is always the same initial `innerAtM`.
     */
    val innerAtM = innerImpl.get(inner, m)

    @inline @tailrec
    def loop(m: Int): Unit = {
      var j = 2 * m // left child of `m`
      if (j <= size) {
        var innerAtJ = innerImpl.get(inner, j)

        // if the left child is greater than the right child, switch to the right child
        if (j < size) {
          val innerAtJPlus1 = innerImpl.get(inner, j + 1)
          if (comp.compare(innerAtJ.asInstanceOf[E], innerAtJPlus1.asInstanceOf[E]) > 0) {
            j += 1
            innerAtJ = innerAtJPlus1
          }
        }

        // if the node `m` is greater than the selected child, swap and recurse
        if (comp.compare(innerAtM.asInstanceOf[E], innerAtJ.asInstanceOf[E]) > 0) {
          innerImpl.set(inner, m, innerAtJ)
          innerImpl.set(inner, j, innerAtM)
          loop(j)
        }
      }
    }

    loop(m)
  }

}

object PriorityQueue {

  /* Get the best available implementation of inner array for the given platform.
   *
   * Use Array[AnyRef] in WebAssembly to avoid JS-interop. In JS, use js.Array.
   * It is resizable by nature, so manual resizing is not needed.
   *
   * `linkTimeIf` is needed here to ensure the optimizer knows
   * there is only one implementation of `InnerArrayImpl`, and de-virtualize/inline
   * the function calls.
   */

  private val innerImpl: InnerArrayImpl =
    LinkingInfo.linkTimeIf[InnerArrayImpl](LinkingInfo.isWebAssembly) {
      InnerArrayImpl.JArrayImpl
    } {
      InnerArrayImpl.JSArrayImpl
    }


  private sealed abstract class InnerArrayImpl {
    type Repr <: AnyRef

    def make(initialCapacity: Int): Repr
    def length(v: Repr): Int
    /** Set the length of innerArray.
      *
      * In WebAssembly, freeing the reference for GC is needed
      * when we shrink the inner array.
      */
    def setLength(v: Repr, newLength: Int): Unit
    def get(v: Repr, index: Int): AnyRef
    def set(v: Repr, index: Int, e: AnyRef): Unit
    def push(v: Repr, e: AnyRef): Unit
    /** Wasm only. */
    def resized(v: Repr, minCapacity: Int): Repr
    /** Wasm only. */
    def capacity(v: Repr): Int
    def copyFrom(v: Repr, from: Int): Repr
    def clear(v: Repr): Unit
  }

  private object InnerArrayImpl {
    object JSArrayImpl extends InnerArrayImpl {
      import scala.scalajs.js

      type Repr = js.Array[AnyRef]

      @inline def make(_initialCapacity: Int): Repr = js.Array[AnyRef](null.asInstanceOf[AnyRef])
      @inline def length(v: Repr): Int = v.length
      @inline def setLength(v: Repr, newLength: Int): Unit =
        v.length = newLength
      @inline def get(v: Repr, index: Int): AnyRef = v(index)
      @inline def set(v: Repr, index: Int, e: AnyRef): Unit =
        v(index) = e
      @inline def push(v: Repr, e: AnyRef): Unit =
        v.push(e)
      @inline def resized(v: Repr, minCapacity: Int): Repr = v // no used
      @inline def capacity(v: Repr): Int = 0 // no used
      @inline def copyFrom(v: Repr, from: Int): Repr = {
        val arr = v.jsSlice(from)
        arr.unshift(null.asInstanceOf[AnyRef])
        arr
      }
      @inline def clear(v: Repr): Unit =
        v.length = 1
    }

    // We store the effective length in the index 0 of the array,
    // where is unused slot both in JSArrayImpl and this impl.
    object JArrayImpl extends InnerArrayImpl {
      type Repr = Array[AnyRef]

      @inline def make(initialCapacity: Int): Repr = {
        val v = new Array[AnyRef](initialCapacity)
        v(0) = 1.asInstanceOf[AnyRef]
        v
      }
      @inline def length(v: Repr): Int = v(0).asInstanceOf[Int]
      @inline def setLength(v: Repr, newLength: Int): Unit =
        v(0) = newLength.asInstanceOf[AnyRef]
      @inline def get(v: Repr, index: Int): AnyRef = v(index)
      @inline def set(v: Repr, index: Int, e: AnyRef): Unit =
        v(index) = e.asInstanceOf[AnyRef]
      @inline def push(v: Repr, e: AnyRef): Unit = {
        val l = length(v)
        v(l) = e.asInstanceOf[AnyRef]
        v(0) = (l + 1).asInstanceOf[AnyRef]
      }
      @inline def resized(v: Repr, minCapacity: Int): Repr =
        Arrays.copyOf(v, roundUpToPowerOfTwo(minCapacity))
      @inline def capacity(v: Repr): Int = v.length
      @inline def copyFrom(v: Repr, from: Int): Repr = {
        val elemLength = length(v) - from
        val newArr = make(roundUpToPowerOfTwo(elemLength + 1))
        newArr(0) = (elemLength + 1).asInstanceOf[AnyRef]
        System.arraycopy(v, from, newArr, 1, elemLength)
        newArr
      }
      @inline def clear(v: Repr): Unit = {
        Arrays.fill(v, null)
        v(0) = 1.asInstanceOf[AnyRef]
      }
    }
  }

}
