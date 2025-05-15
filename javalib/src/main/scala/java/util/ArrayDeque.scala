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
import scala.scalajs.LinkingInfo

class ArrayDeque[E] private (initialCapacity: Int)
    extends AbstractCollection[E] with Deque[E] with Cloneable with Serializable {
  self =>

  /* This class has two different implementations for the internal storage.
   * depending on whether we are on Wasm or JS.
   * On JS, we utilize `js.Array`. On Wasm, for performance reasons,
   * we use a scala.Array to avoid JS interop.
   */

  private val innerJS: js.Array[E] =
    if (LinkingInfo.isWebAssembly) null
    else new js.Array[E](Math.max(initialCapacity, 16))

  private var innerWasm: Array[AnyRef] =
    if (LinkingInfo.isWebAssembly) new Array[AnyRef](Math.max(initialCapacity, 16))
    else null

  if (!LinkingInfo.isWebAssembly)
    fillNulls(0, innerJS.length)

  private var status = 0
  private var startIndex = 0 // inclusive, 0 <= startIndex < inner.length
  private var endIndex = // exclusive, 0 < endIndex <= inner.length
    if (LinkingInfo.isWebAssembly) innerWasm.length
    else innerJS.length
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
        startIndex = length() - 1
      if (LinkingInfo.isWebAssembly) innerWasm(startIndex) = e.asInstanceOf[AnyRef]
      else innerJS(startIndex) = e
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
      if (endIndex > length())
        endIndex = 1
      if (LinkingInfo.isWebAssembly) innerWasm(endIndex - 1) = e.asInstanceOf[AnyRef]
      else innerJS(endIndex - 1) = e
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
      val res = {
        if (LinkingInfo.isWebAssembly) {
          val res = innerWasm(startIndex).asInstanceOf[E]
          innerWasm(startIndex) = null // free reference for GC
          res
        } else {
          val res = innerJS(startIndex)
          innerJS(startIndex) = null.asInstanceOf[E] // free reference for GC
          res
        }
      }
      startIndex += 1
      if (startIndex == endIndex)
        empty = true
      if (startIndex >= length())
        startIndex = 0
      status += 1
      res
    }
  }

  def pollLast(): E = {
    if (isEmpty()) {
      null.asInstanceOf[E]
    } else {
      val res = {
        if (LinkingInfo.isWebAssembly) {
          val res = innerWasm(endIndex - 1).asInstanceOf[E]
          innerWasm(endIndex - 1) = null // free reference for GC
          res
        } else {
          val res = innerJS(endIndex - 1)
          innerJS(endIndex - 1) = null.asInstanceOf[E] // free reference for GC
          res
        }
      }
      endIndex -= 1
      if (startIndex == endIndex)
        empty = true
      if (endIndex <= 0)
        endIndex = length()
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
    if (isEmpty()) {
      null.asInstanceOf[E]
    } else {
      if (LinkingInfo.isWebAssembly) innerWasm(startIndex).asInstanceOf[E]
      else innerJS(startIndex)
    }
  }

  def peekLast(): E = {
    if (isEmpty()) {
      null.asInstanceOf[E]
    } else {
      if (LinkingInfo.isWebAssembly) innerWasm(endIndex - 1).asInstanceOf[E]
      else innerJS(endIndex - 1)
    }
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
    else (endIndex + length()) - startIndex
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
      else if (nextIndex >= length())
        nextIndex = 0

      if (LinkingInfo.isWebAssembly) innerWasm(lastIndex).asInstanceOf[E]
      else innerJS(lastIndex)
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
          nextIndex = length() - 1
      }

      if (LinkingInfo.isWebAssembly) innerWasm(lastIndex).asInstanceOf[E]
      else innerJS(lastIndex)
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
    endIndex = length()
  }

  private def firstIndexOf(o: Any): Int = {
    // scalastyle:off return
    if (isEmpty())
      return -1
    val innerJS = this.innerJS // local copy
    val innerWasm = this.innerWasm // local copy
    val capacity = length() // local copy
    val endIndex = this.endIndex // local copy
    var i = startIndex
    do {
      if (i >= capacity)
        i = 0
      val obj =
        if (LinkingInfo.isWebAssembly) innerWasm(i).asInstanceOf[E]
        else innerJS(i)
      if (Objects.equals(obj, o))
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
    val innerJS = this.innerJS // local copy
    val innerWasm = this.innerWasm // local copy
    val startIndex = this.startIndex // local copy
    var i = endIndex
    do {
      i -= 1
      if (i < 0)
        i = length() - 1
      val obj =
        if (LinkingInfo.isWebAssembly) innerWasm(i).asInstanceOf[E]
        else innerJS(i)
      if (Objects.equals(obj, o))
        return i
    } while (i != startIndex)
    -1
    // scalastyle:on return
  }

  private def ensureCapacityForAdd(): Unit = {
    if (isEmpty()) {
      // Nothing to do (constructor ensures capacity is always non-zero).
    } else if (startIndex == 0 && endIndex == length()) {
      val oldCapacity = length()
      if (LinkingInfo.isWebAssembly) {
        innerWasm = Arrays.copyOf(innerWasm, oldCapacity * 2)
      } else {
        innerJS.length *= 2
        // no copying required: We just keep adding to the end.
        // However, ensure array is dense.
        fillNulls(oldCapacity, length())
      }
    } else if (startIndex == endIndex) {
      val oldCapacity = length()
      if (LinkingInfo.isWebAssembly) {
        val newArr = new Array[AnyRef](oldCapacity * 2)
        System.arraycopy(innerWasm, 0, newArr, oldCapacity, endIndex)
        innerWasm = newArr
      } else {
        innerJS.length *= 2
        // move beginning of array to end
        for (i <- 0 until endIndex) {
          innerJS(i + oldCapacity) = innerJS(i)
          innerJS(i) = null.asInstanceOf[E] // free old reference for GC
        }
        // ensure rest of array is dense
        fillNulls(endIndex + oldCapacity, length())
      }
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
      if (LinkingInfo.isWebAssembly) {
        System.arraycopy(innerWasm, target + 1, innerWasm, target, endIndex - target)
        innerWasm(endIndex - 1) = null // free reference for GC
      } else {
        // Shift elements from endIndex towards target
        for (i <- target until endIndex - 1)
          innerJS(i) = innerJS(i + 1)
        innerJS(endIndex - 1) = null.asInstanceOf[E] // free reference for GC
      }
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

      if (LinkingInfo.isWebAssembly) {
        System.arraycopy(innerWasm, startIndex, innerWasm, startIndex + 1, target - startIndex)
        innerWasm(startIndex) = null // free reference for GC
      } else {
        // for (i <- target until startIndex by -1)
        var i = target
        while (i != startIndex) {
          innerJS(i) = innerJS(i - 1)
          i -= 1
        }
        innerJS(startIndex) = null.asInstanceOf[E] // free reference for GC
      }

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

  // JS only
  private def fillNulls(from: Int, until: Int): Unit = {
    for (i <- from until until)
      innerJS(i) = null.asInstanceOf[E]
  }

  @inline
  private def length(): Int =
    if (LinkingInfo.isWebAssembly) innerWasm.length
    else innerJS.length
}
