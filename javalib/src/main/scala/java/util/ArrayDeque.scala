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

import scala.scalajs.js

class ArrayDeque[E] private (private val inner: js.Array[E])
    extends AbstractCollection[E] with Deque[E] with Cloneable with Serializable {
  self =>

  private var status = 0

  def this(initialCapacity: Int) = {
    this(new js.Array[E])

    if (initialCapacity < 0)
      throw new IllegalArgumentException
  }

  def this() =
    this(16)

  def this(c: Collection[_ <: E]) = {
    this()
    addAll(c)
  }

  @inline
  override def isEmpty(): Boolean = inner.length == 0

  def addFirst(e: E): Unit =
    offerFirst(e)

  def addLast(e: E): Unit =
    offerLast(e)

  def offerFirst(e: E): Boolean = {
    if (e == null) {
      throw new NullPointerException()
    } else {
      inner.unshift(e)
      status += 1
      true
    }
  }

  def offerLast(e: E): Boolean = {
    if (e == null) {
      throw new NullPointerException()
    } else {
      inner.push(e)
      status += 1
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
      val res = inner.shift()
      status += 1
      res
    }
  }

  def pollLast(): E = {
    if (isEmpty()) null.asInstanceOf[E]
    else inner.pop()
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
    else inner(0)
  }

  def peekLast(): E = {
    if (isEmpty()) null.asInstanceOf[E]
    else inner(inner.length - 1)
  }

  def removeFirstOccurrence(o: Any): Boolean = {
    // scalastyle:off return
    val inner = this.inner // local copy
    val len = inner.length
    var i = 0
    while (i != len) {
      if (Objects.equals(inner(i), o)) {
        arrayRemove(inner, i)
        status += 1
        return true
      }
      i += 1
    }
    false
    // scalastyle:on return
  }

  def removeLastOccurrence(o: Any): Boolean = {
    // scalastyle:off return
    val inner = this.inner // local copy
    var i = inner.length - 1
    while (i >= 0) {
      if (Objects.equals(inner(i), o)) {
        arrayRemove(inner, i)
        status += 1
        return true
      }
      i -= 1
    }
    false
    // scalastyle:on return
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

  def size(): Int = inner.length

  private def failFastIterator(startIndex: Int, nex: (Int) => Int) = {
    new Iterator[E] {
      private def checkStatus() =
        if (self.status != actualStatus)
          throw new ConcurrentModificationException()

      private val actualStatus = self.status

      private var index: Int = startIndex

      def hasNext(): Boolean = {
        checkStatus()
        val n = nex(index)
        (n >= 0) && (n < inner.length)
      }

      def next(): E = {
        checkStatus()
        index = nex(index)
        inner(index)
      }

      override def remove(): Unit = {
        checkStatus()
        if (index < 0 || index >= inner.length) {
          throw new IllegalStateException()
        } else {
          arrayRemove(inner, index)
        }
      }
    }
  }

  def iterator(): Iterator[E] =
    failFastIterator(-1, x => (x + 1))

  def descendingIterator(): Iterator[E] =
    failFastIterator(inner.length, x => (x - 1))

  override def contains(o: Any): Boolean =
    arrayExists(inner)(Objects.equals(_, o))

  override def remove(o: Any): Boolean = removeFirstOccurrence(o)

  override def clear(): Unit = {
    if (!isEmpty())
      status += 1
    inner.length = 0
  }
}
