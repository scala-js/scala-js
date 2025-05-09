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

import java.lang.Utils.nextPowerOfTwo

import scala.annotation.tailrec

import scala.scalajs.js
import scala.scalajs.LinkingInfo

class PriorityQueue[E] private (
    private val comp: Comparator[_ >: E], internal: Boolean, initialCapacity: Int)
    extends AbstractQueue[E] with Serializable {

  def this() =
    this(NaturalComparator, internal = true, initialCapacity = 16)

  def this(initialCapacity: Int) = {
    this(
      NaturalComparator,
      internal = true,
      {
        if (initialCapacity < 1)
          throw new IllegalArgumentException
        initialCapacity
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
        initialCapacity
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
    }, internal = true, nextPowerOfTwo(c.size()))
    addAll(c)
  }

  def this(c: PriorityQueue[_ <: E]) = {
    this(c.comp.asInstanceOf[Comparator[_ >: E]], internal = true, nextPowerOfTwo(c.size()))
    addAll(c)
  }

  def this(sortedSet: SortedSet[_ <: E]) = {
    this(NaturalComparator.select(
        sortedSet.comparator().asInstanceOf[Comparator[_ >: E]]),
        internal = true,
        nextPowerOfTwo(sortedSet.size()))
    addAll(sortedSet)
  }

  /* This class has two different implementations for the internal storage,
   * depending on whether we are on Wasm or JS.
   * On JS, we utilize `js.Array`. On Wasm, for performance reasons,
   * we use a scala.Array to avoid JS interop.
   */

  // The index 0 is not used; the root is at index 1.
  // This is standard practice in binary heaps, to simplify arithmetics.
  private val innerJS: js.Array[E] =
    if (LinkingInfo.isWebAssembly) null
    else js.Array[E](null.asInstanceOf[E])

  private var innerWasm: Array[AnyRef] =
    if (LinkingInfo.isWebAssembly) new Array[AnyRef](initialCapacity)
    else null

  // Wasm only: size of the objects stored in the inner array
  private var innerSize = 1

  override def add(e: E): Boolean = {
    if (e == null)
      throw new NullPointerException()
    if (LinkingInfo.isWebAssembly) {
      ensureCapacity(innerSize + 1)
      innerWasm(innerSize) = e.asInstanceOf[AnyRef]
      innerSize += 1
    } else {
      innerJS.push(e)
    }
    fixUp(length() - 1)
    true
  }

  def offer(e: E): Boolean = add(e)

  def peek(): E = {
    if (LinkingInfo.isWebAssembly) {
      if (innerSize > 1) innerWasm(1).asInstanceOf[E]
      else null.asInstanceOf[E]
    } else {
      if (innerJS.length > 1) innerJS(1)
      else null.asInstanceOf[E]
    }
  }

  override def remove(o: Any): Boolean = {
    if (o == null) {
      false
    } else {
      val len = length()
      var i = 1
      while ({
        i != len && {
          val obj =
            if (LinkingInfo.isWebAssembly) innerWasm(i).asInstanceOf[E]
            else innerJS(i)
          !o.equals(obj)
        }
      }) {
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
    val len = length()
    var i = 1
    while ({
      i != len && {
        val obj =
          if (LinkingInfo.isWebAssembly) innerWasm(i)
          else innerJS(i).asInstanceOf[AnyRef]
        o.asInstanceOf[AnyRef] ne obj
      }
    }) {
      i += 1
    }
    if (i == len)
      throw new ConcurrentModificationException()
    removeAt(i)
  }

  private def removeAt(i: Int): Unit = {
    val newLength = length() - 1
    if (LinkingInfo.isWebAssembly) {
      if (i == newLength) {
        innerWasm(innerSize - 1) = null // free reference for GC
      } else {
        innerWasm(i) = innerWasm(newLength)
        fixUpOrDown(i)
        innerWasm(innerSize - 1) = null // free reference for GC
      }
      innerSize = newLength
    } else {
      if (i == newLength) {
        innerJS.length = newLength
      } else {
        innerJS(i) = innerJS(newLength)
        innerJS.length = newLength
        fixUpOrDown(i)
      }
    }
  }

  override def contains(o: Any): Boolean = {
    if (o == null) {
      false
    } else {
      val len = length()
      var i = 1
      while (i != len && {
        val obj =
          if (LinkingInfo.isWebAssembly) innerWasm(i).asInstanceOf[E]
          else innerJS(i)
        !o.equals(obj)
      }) {
        i += 1
      }
      i != len
    }
  }

  def iterator(): Iterator[E] = {
    new Iterator[E] {
      private[this] var innerJS: js.Array[E] = PriorityQueue.this.innerJS
      private[this] var innerWasm: Array[AnyRef] = PriorityQueue.this.innerWasm
      private[this] var innerIterSize: Int = PriorityQueue.this.innerSize
      private[this] var nextIdx: Int = 1
      private[this] var last: E = _ // null

      def hasNext(): Boolean =
        if (LinkingInfo.isWebAssembly) nextIdx < innerIterSize
        else nextIdx < innerJS.length

      def next(): E = {
        if (!hasNext())
          throw new NoSuchElementException("empty iterator")
        last = {
          if (LinkingInfo.isWebAssembly) innerWasm(nextIdx).asInstanceOf[E]
          else innerJS(nextIdx)
        }
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
        if (LinkingInfo.isWebAssembly) {
          if (innerWasm eq PriorityQueue.this.innerWasm) {
            innerIterSize = innerSize - nextIdx
            innerWasm = Arrays.copyOfRange(innerWasm, nextIdx, innerSize)
            nextIdx = 0
          }
        } else {
          if (innerJS eq PriorityQueue.this.innerJS) {
            innerJS = innerJS.jsSlice(nextIdx)
            nextIdx = 0
          }
        }
        removeExact(last)
        last = null.asInstanceOf[E]
      }
    }
  }

  def size(): Int =
    if (LinkingInfo.isWebAssembly) innerSize - 1
    else innerJS.length - 1

  override def clear(): Unit = {
    if (LinkingInfo.isWebAssembly) {
      Arrays.fill(innerWasm, null)
      innerSize = 1
    } else {
      innerJS.length = 1
    }
  }

  def poll(): E = {
    if (LinkingInfo.isWebAssembly) {
      val innerWasm = this.innerWasm // local copy
      if (innerSize > 1) {
        val newSize = innerSize - 1
        val result = innerWasm(1).asInstanceOf[E]
        innerWasm(1) = innerWasm(newSize)
        innerSize = newSize
        fixDown(1)
        result
      } else {
        null.asInstanceOf[E]
      }
    } else {
      val innerJS = this.innerJS // local copy
      if (innerJS.length > 1) {
        val newSize = innerJS.length - 1
        val result = innerJS(1)
        innerJS(1) = innerJS(newSize)
        innerJS.length = newSize
        fixDown(1)
        result
      } else {
        null.asInstanceOf[E]
      }
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
    if (m > 1 && {
        if (LinkingInfo.isWebAssembly)
          comp.compare(
              innerWasm(m >> 1).asInstanceOf[E],
              innerWasm(m).asInstanceOf[E]) > 0
        else
          comp.compare(innerJS(m >> 1), innerJS(m)) > 0
    })
      fixUp(m)
    else
      fixDown(m)
  }

  /** Fixes the heap property from the child at index `m` up the tree, towards
   *  the root.
   */
  private[this] def fixUp(m: Int): Unit = {
    // local copy
    val innerWasm = this.innerWasm
    val innerJS = this.innerJS

    /* At each step, even though `m` changes, the element moves with it, and
     * hence inner(m) is always the same initial `innerAtM`.
     */
    val innerAtM =
      if (LinkingInfo.isWebAssembly) innerWasm(m).asInstanceOf[E]
      else innerJS(m)

    @inline @tailrec
    def loop(m: Int): Unit = {
      if (m > 1) {
        val parent = m >> 1
        if (LinkingInfo.isWebAssembly) {
          val innerAtParent = innerWasm(parent).asInstanceOf[E]
          if (comp.compare(innerAtParent, innerAtM) > 0) {
            innerWasm(parent) = innerAtM.asInstanceOf[AnyRef]
            innerWasm(m) = innerAtParent.asInstanceOf[AnyRef]
          }
        } else {
          val innerAtParent = innerJS(parent)
          if (comp.compare(innerAtParent, innerAtM) > 0) {
            innerJS(parent) = innerAtM
            innerJS(m) = innerAtParent
          }
        }
        loop(parent)
      }
    }

    loop(m)
  }

  /** Fixes the heap property from the child at index `m` down the tree,
   *  towards the leaves.
   */
  private[this] def fixDown(m: Int): Unit = {
    // local copy
    val innerWasm = this.innerWasm
    val innerJS = this.innerJS
    val size = length() - 1

    /* At each step, even though `m` changes, the element moves with it, and
     * hence inner(m) is always the same initial `innerAtM`.
     */
    val innerAtM =
      if (LinkingInfo.isWebAssembly) innerWasm(m).asInstanceOf[E]
      else innerJS(m)

    @inline @tailrec
    def loop(m: Int): Unit = {
      var j = 2 * m // left child of `m`
      if (j <= size) {
        var innerAtJ =
          if (LinkingInfo.isWebAssembly) innerWasm(j).asInstanceOf[E]
          else innerJS(j)

        // if the left child is greater than the right child, switch to the right child
        if (j < size) {
          val innerAtJPlus1 =
            if (LinkingInfo.isWebAssembly) innerWasm(j + 1).asInstanceOf[E]
            else innerJS(j + 1)
          if (comp.compare(innerAtJ, innerAtJPlus1) > 0) {
            j += 1
            innerAtJ = innerAtJPlus1
          }
        }

        // if the node `m` is greater than the selected child, swap and recurse
        if (comp.compare(innerAtM, innerAtJ) > 0) {
          if (LinkingInfo.isWebAssembly) {
            innerWasm(m) = innerAtJ.asInstanceOf[AnyRef]
            innerWasm(j) = innerAtM.asInstanceOf[AnyRef]
          } else {
            innerJS(m) = innerAtJ
            innerJS(j) = innerAtM
          }
          loop(j)
        }
      }
    }

    loop(m)
  }

  @inline private def length() =
    if (LinkingInfo.isWebAssembly) innerSize
    else innerJS.length

  // Wasm only
  private def ensureCapacity(minCapacity: Int): Unit =
    if (innerWasm.length < minCapacity)
      innerWasm = Arrays.copyOf(innerWasm, nextPowerOfTwo(minCapacity))
}
