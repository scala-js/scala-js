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
import java.lang.Utils._

import java.util.ScalaOps._

import scala.scalajs.js

class ArrayDeque[E] private (initialCapacity: Int)
    extends AbstractCollection[E] with Deque[E] with Cloneable with Serializable {
  self =>

  private val inner: js.Array[E] = new js.Array[E](Math.max(initialCapacity, 16))

  fillNulls(0, inner.length)

  private var status = 0
  private var startIndex = 0 // inclusive, 0 <= startIndex < inner.length
  private var endIndex = inner.length // exclusive, 0 < endIndex <= inner.length
  private var empty = true

  def this() = this(16)

  def this(c: Collection[_ <: E]) = {
    this(c.size())
    addAll(c)
  }

  @inline
  override def isEmpty(): Boolean = empty

  def addFirst(e: E): Unit =
    offerFirst(e)

  def addLast(e: E): Unit =
    offerLast(e)

  def offerFirst(e: E): Boolean = {
    if (e == null) {
      throw new NullPointerException()
    } else {
      ensureCapacityForAdd()
      startIndex -= 1
      if (startIndex < 0)
        startIndex = inner.length - 1
      inner(startIndex) = e
      status += 1
      empty = false
      true
    }
  }

  def offerLast(e: E): Boolean = {
    if (e == null) {
      throw new NullPointerException()
    } else {
      ensureCapacityForAdd()
      endIndex += 1
      if (endIndex > inner.length)
        endIndex = 1
      inner(endIndex - 1) = e
      status += 1
      empty = false
      true
    }
  }

  def removeFirst(): E = {
    if (isEmpty())
      throw new NoSuchElementException()
    else
      pollFirst()
  }

  def removeLast(): E = {
    if (isEmpty())
      throw new NoSuchElementException()
    else
      pollLast()
  }

  def pollFirst(): E = {
    if (isEmpty()) null.asInstanceOf[E]
    else {
      val res = inner(startIndex)
      inner(startIndex) = null.asInstanceOf[E] // free reference for GC
      startIndex += 1
      if (startIndex == endIndex)
        empty = true
      if (startIndex >= inner.length)
        startIndex = 0
      status += 1
      res
    }
  }

  def pollLast(): E = {
    if (isEmpty()) {
      null.asInstanceOf[E]
    } else {
      val res = inner(endIndex - 1)
      inner(endIndex - 1) = null.asInstanceOf[E] // free reference for GC
      endIndex -= 1
      if (startIndex == endIndex)
        empty = true
      if (endIndex <= 0)
        endIndex = inner.length
      status += 1
      res
    }
  }

  def getFirst(): E = {
    if (isEmpty())
      throw new NoSuchElementException()
    else
      peekFirst()
  }

  def getLast(): E = {
    if (isEmpty())
      throw new NoSuchElementException()
    else
      peekLast()
  }

  def peekFirst(): E = {
    if (isEmpty()) null.asInstanceOf[E]
    else inner(startIndex)
  }

  def peekLast(): E = {
    if (isEmpty()) null.asInstanceOf[E]
    else inner(endIndex - 1)
  }

  def removeFirstOccurrence(o: Any): Boolean = {
    val i = firstIndexOf(o)
    if (i == -1) {
      false
    } else {
      removeAt(i)
      true
    }
  }

  def removeLastOccurrence(o: Any): Boolean = {
    val i = lastIndexOf(o)
    if (i == -1) {
      false
    } else {
      removeAt(i)
      true
    }
  }

  override def add(e: E): Boolean = {
    addLast(e)
    true
  }

  def offer(e: E): Boolean = offerLast(e)

  def remove(): E = removeFirst()

  def poll(): E = pollFirst()

  def element(): E = getFirst()

  def peek(): E = peekFirst()

  def push(e: E): Unit = addFirst(e)

  def pop(): E = removeFirst()

  def size(): Int = {
    if (isEmpty()) 0
    else if (endIndex > startIndex) endIndex - startIndex
    else (endIndex + inner.length) - startIndex
  }

  def iterator(): Iterator[E] = new Iterator[E] {
    private def checkStatus() = {
      if (self.status != expectedStatus)
        throw new ConcurrentModificationException()
    }

    private var expectedStatus = self.status

    private var lastIndex: Int = -1
    private var nextIndex: Int =
      if (isEmpty()) -1
      else startIndex

    def hasNext(): Boolean = {
      checkStatus()
      nextIndex != -1
    }

    def next(): E = {
      if (!hasNext()) // also checks status
        throw new NoSuchElementException()

      lastIndex = nextIndex

      nextIndex += 1
      if (nextIndex == endIndex)
        nextIndex = -1
      else if (nextIndex >= inner.length)
        nextIndex = 0

      inner(lastIndex)
    }

    override def remove(): Unit = {
      checkStatus()
      if (lastIndex == -1)
        throw new IllegalStateException()

      val laterShifted = removeAt(lastIndex)
      lastIndex = -1
      expectedStatus = self.status

      if (laterShifted && nextIndex != -1) {
        /* assert(nextIndex != 0)
         * Why? Assume nextIndex == 0, that means the element we just removed
         * was at the end of the ring-buffer. But in this case, removeAt shifts
         * forward to avoid copying over the buffer boundary.
         * Therefore, laterShifted cannot be true.
         */
        nextIndex -= 1
      }
    }
  }

  def descendingIterator(): Iterator[E] = new Iterator[E] {
    private def checkStatus() = {
      if (self.status != expectedStatus)
        throw new ConcurrentModificationException()
    }

    private var expectedStatus = self.status

    private var lastIndex: Int = -1
    private var nextIndex: Int =
      if (isEmpty()) -1
      else endIndex - 1

    def hasNext(): Boolean = {
      checkStatus()
      nextIndex != -1
    }

    def next(): E = {
      if (!hasNext()) // also checks status
          throw new NoSuchElementException()

      lastIndex = nextIndex

      if (nextIndex == startIndex) {
        nextIndex = -1
      } else {
        nextIndex -= 1
        if (nextIndex < 0)
          nextIndex = inner.length - 1
      }

      inner(lastIndex)
    }

    override def remove(): Unit = {
      checkStatus()
      if (lastIndex == -1)
          throw new IllegalStateException()


      val laterShifted = removeAt(lastIndex)
      expectedStatus = self.status
      lastIndex = -1

      if (!laterShifted && nextIndex != -1) {
        /* assert(nextIndex < inner.length - 1)
         * Why? Assume nextIndex == inner.length - 1, that means the element we
         * just removed was at the beginning of the ring buffer (recall, this is
         * a backwards iterator). However, in this case, removeAt would shift
         * the next elements (in forward iteration order) backwards.
         * That implies laterShifted, so we would not hit this branch.
         */
        nextIndex += 1
      }
    }
  }

  override def contains(o: Any): Boolean = firstIndexOf(o) != -1

  override def remove(o: Any): Boolean = removeFirstOccurrence(o)

  override def clear(): Unit = {
    if (!isEmpty())
      status += 1
    empty = true
    startIndex = 0
    endIndex = inner.length
  }

  private def firstIndexOf(o: Any): Int = {
    // scalastyle:off return
    if (isEmpty())
      return -1
    val inner = this.inner // local copy
    val capacity = inner.length // local copy
    val endIndex = this.endIndex // local copy
    var i = startIndex
    do {
      if (i >= capacity)
        i = 0
      if (Objects.equals(inner(i), o))
        return i
      i += 1 // let i overrun so we catch endIndex == capacity
    } while (i != endIndex)
    -1
    // scalastyle:on return
  }

  private def lastIndexOf(o: Any): Int = {
    // scalastyle:off return
    if (isEmpty())
      return -1
    val inner = this.inner // local copy
    val startIndex = this.startIndex // local copy
    var i = endIndex
    do {
      i -= 1
      if (i < 0)
        i = inner.length - 1
      if (Objects.equals(inner(i), o))
        return i
    } while (i != startIndex)
    -1
    // scalastyle:on return
  }

  private def ensureCapacityForAdd(): Unit = {
    if (isEmpty()) {
      // Nothing to do (constructor ensures capacity is always non-zero).
    } else if (startIndex == 0 && endIndex == inner.length) {
      val oldCapacity = inner.length
      inner.length *= 2
      // no copying required: We just keep adding to the end.
      // However, ensure array is dense.
      fillNulls(oldCapacity, inner.length)
    } else if (startIndex == endIndex) {
      val oldCapacity = inner.length
      inner.length *= 2
      // move beginning of array to end
      for (i <- 0 until endIndex) {
        inner(i + oldCapacity) = inner(i)
        inner(i) = null.asInstanceOf[E] // free old reference for GC
      }
      // ensure rest of array is dense
      fillNulls(endIndex + oldCapacity, inner.length)
      endIndex += oldCapacity
    }
  }

  /* Removes the element at index [[target]]
   *
   * The return value indicates which end of the queue was shifted onto the
   * element to be removed.
   *
   * @returns true if elements after target were shifted onto target or target
   *     was the last element. Returns false, if elements before target were
   *     shifted onto target or target was the first element.
   */
  private def removeAt(target: Int): Boolean = {
    /* Note that if size == 1, we always take the first branch.
     * Therefore, we need not handle the empty flag in this method.
     */

    if (target == startIndex) {
      pollFirst()
      false
    } else if (target == endIndex - 1) {
      pollLast()
      true
    } else if (target < endIndex) {
      // Shift elements from endIndex towards target
      for (i <- target until endIndex - 1)
        inner(i) = inner(i + 1)
      inner(endIndex - 1) = null.asInstanceOf[E] // free reference for GC
      status += 1

      /* Note that endIndex >= 2:
       * By previous if: target < endIndex
       * ==> target <= endIndex - 1
       * By previous if: target < endIndex - 1 (non-equality)
       * ==> target <= endIndex - 2
       * By precondition: target >= 0
       * ==> 0 <= endIndex - 2
       * ==> endIndex >= 2
       *
       * Therefore we do not need to perform an underflow check.
       */
      endIndex -= 1

      true
    } else {
      // Shift elements from startIndex towards target

      /* Note that target > startIndex.
       * Why? Assume by contradiction: target <= startIndex
       * By previous if: target >= endIndex.
       * By previous if: target < startIndex (non-equality)
       * ==> endIndex <= target < startIndex.
       * ==> target is not in the active region of the ringbuffer.
       * ==> contradiction.
       */

      // for (i <- target until startIndex by -1)
      var i = target
      while (i != startIndex) {
        inner(i) = inner(i - 1)
        i -= 1
      }
      inner(startIndex) = null.asInstanceOf[E] // free reference for GC

      status += 1

      /* Note that startIndex <= inner.length - 2:
       * By previous proof: target > startIndex
       * By precondition: target <= inner.length - 1
       * ==> startIndex < inner.length - 1
       * ==> startIndex <= inner.length - 2
       *
       * Therefore we do not need to perform an overflow check.
       */
      startIndex += 1
      false
    }
  }

  private def fillNulls(from: Int, until: Int): Unit = {
    for (i <- from until until)
      inner(i) = null.asInstanceOf[E]
  }
}
