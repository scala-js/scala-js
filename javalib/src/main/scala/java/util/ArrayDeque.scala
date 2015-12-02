package java.util

import scala.scalajs.js

class ArrayDeque[E] private (private var inner: js.Array[E])
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

  def addFirst(e: E): Unit =
    offerFirst(e)

  def addLast(e: E): Unit =
    offerLast(e)

  def offerFirst(e: E): Boolean = {
    if (e == null) {
      throw new NullPointerException()
    } else {
      inner = e +: inner
      status += 1
      true
    }
  }

  def offerLast(e: E): Boolean = {
    if (e == null) {
      throw new NullPointerException()
    } else {
      inner += e
      status += 1
      true
    }
  }

  def removeFirst(): E = {
    if (inner.isEmpty)
      throw new NoSuchElementException()
    else
      pollFirst()
  }

  def removeLast(): E = {
    if (inner.isEmpty)
      throw new NoSuchElementException()
    else
      pollLast()
  }

  def pollFirst(): E = {
    if (inner.isEmpty) null.asInstanceOf[E]
    else {
      val res = inner.remove(0)
      status += 1
      res
    }
  }

  def pollLast(): E = {
    if (inner.isEmpty) null.asInstanceOf[E]
    else inner.pop()
  }

  def getFirst(): E = {
    if (inner.isEmpty)
      throw new NoSuchElementException()
    else
      peekFirst()
  }

  def getLast(): E = {
    if (inner.isEmpty)
      throw new NoSuchElementException()
    else
      peekLast()
  }

  def peekFirst(): E = {
    if (inner.isEmpty) null.asInstanceOf[E]
    else inner.head
  }

  def peekLast(): E = {
    if (inner.isEmpty) null.asInstanceOf[E]
    else inner.last
  }

  def removeFirstOccurrence(o: Any): Boolean = {
    val index = inner.indexWhere(_ === o)
    if (index >= 0) {
      inner.remove(index)
      status += 1
      true
    } else
      false
  }

  def removeLastOccurrence(o: Any): Boolean = {
    val index = inner.lastIndexWhere(_ === o)
    if (index >= 0) {
      inner.remove(index)
      status += 1
      true
    } else
      false
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

  def size(): Int = inner.size

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
        (n >= 0) && (n < inner.size)
      }

      def next(): E = {
        checkStatus()
        index = nex(index)
        inner(index)
      }

      def remove(): Unit = {
        checkStatus()
        if (index < 0 || index >= inner.size) {
          throw new IllegalStateException()
        } else {
          inner.remove(index)
        }
      }
    }
  }

  def iterator(): Iterator[E] =
    failFastIterator(-1, x => (x + 1))

  def descendingIterator(): Iterator[E] =
    failFastIterator(inner.size, x => (x - 1))

  override def contains(o: Any): Boolean = inner.exists(_ === o)

  override def remove(o: Any): Boolean = removeFirstOccurrence(o)

  override def clear(): Unit = {
    if (!inner.isEmpty) status += 1
    inner.clear()
  }
}
