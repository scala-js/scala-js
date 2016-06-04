package java.util

import scala.annotation.tailrec
import scala.collection.JavaConverters._

class LinkedList[E]() extends AbstractSequentialList[E]
    with List[E] with Deque[E] with Cloneable with Serializable {

  def this(c: Collection[_ <: E]) = {
    this()
    addAll(c)
  }

  import LinkedList._

  private var head: Node[E] = null
  private var last: Node[E] = null

  /* Inner size is represented with a Double to satisfy Collection
   * size method requirement:
   * If this collection contains more than Integer.MAX_VALUE elements,
   * returns Integer.MAX_VALUE.
   */
  private var _size: Double = 0

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

  def removeFirst(): E = {
    if (isEmpty())
      throw new NoSuchElementException()

    val oldHead = head
    head = oldHead.next

    if (head ne null)
      head.prev = null
    else
      last = null

    _size -= 1
    oldHead.value
  }

  def removeLast(): E = {
    if (isEmpty())
      throw new NoSuchElementException()

    val oldLast = last
    last = oldLast.prev

    if (last ne null)
      last.next = null
    else
      head = null

    _size -= 1
    oldLast.value
  }

  def addFirst(e: E): Unit = {
    val oldHead = head

    head = new Node(e, next = oldHead)

    _size += 1

    if (oldHead ne null)
      oldHead.prev = head
    else
      last = head
  }

  def addLast(e: E): Unit = {
    val oldLast = last

    last = new Node(e, prev = oldLast)

    _size += 1

    if (oldLast ne null)
      oldLast.next = last
    else
      head = last
  }

  override def contains(o: Any): Boolean =
    iterator().asScala.exists(_ === o)

  override def size(): Int =
    _size.toInt

  override def add(e: E): Boolean = {
    addLast(e)
    true
  }

  override def remove(o: Any): Boolean =
    _removeOccurrence(listIterator, o)

  override def addAll(c: Collection[_ <: E]): Boolean = {
    val iter = c.iterator
    val changed = iter.hasNext()
    while (iter.hasNext())
      addLast(iter.next())

    changed
  }

  override def clear(): Unit = {
    head = null
    last = null
    _size = 0
  }

  private def getNodeAt(index: Int): Node[E] = {
    if (index == 0) head
    else if (index == size - 1) last
    else {
      var current: Node[E] = null
      if (index <= size/2) {
        current = head
        for (_ <- 0 until index)
          current = current.next
      } else {
        current = last
        for (_ <- index until (size - 1))
          current = current.prev
      }
      current
    }
  }

  override def get(index: Int): E = {
    checkIndexInBounds(index)
    getNodeAt(index).value
  }

  override def set(index: Int, element: E): E = {
    checkIndexInBounds(index)
    val node = getNodeAt(index)
    val oldValue = node.value
    node.value = element
    oldValue
  }

  private def addNode(nextNode: Node[E], e: E): Unit = {
    if (nextNode eq head) addFirst(e)
    else if (nextNode eq null) addLast(e)
    else {
      val node = new Node(e, prev = nextNode.prev, next = nextNode)
      nextNode.prev.next = node
      nextNode.prev = node

      _size += 1
    }
  }

  override def add(index: Int, element: E): Unit = {
    checkIndexOnBounds(index)
    addNode(getNodeAt(index), element)
  }

  private def removeNode(node: Node[E]): E = {
    if (node eq head) removeFirst()
    else if (node eq last) removeLast()
    else {
      node.prev.next = node.next
      node.next.prev = node.prev

      _size -= 1

      node.value
    }
  }

  override def remove(index: Int): E = {
    checkIndexInBounds(index)
    removeNode(getNodeAt(index))
  }

  def peek(): E =
    peekFirst()

  def element(): E =
    getFirst()

  def poll(): E =
    pollFirst()

  def remove(): E =
    removeFirst()

  def offer(e: E): Boolean =
    offerLast(e)

  def offerFirst(e: E): Boolean = {
    addFirst(e)
    true
  }

  def offerLast(e: E): Boolean = {
    addLast(e)
    true
  }

  def peekFirst(): E =
    if (head eq null) null.asInstanceOf[E]
    else head.value

  def peekLast(): E =
    if (last eq null) null.asInstanceOf[E]
    else last.value

  def pollFirst(): E =
    if (isEmpty()) null.asInstanceOf[E]
    else removeFirst()

  def pollLast(): E =
    if (isEmpty) null.asInstanceOf[E]
    else removeLast()

  def push(e: E): Unit =
    addFirst(e)

  def pop(): E =
    removeFirst()

  private def _removeOccurrence(iter: Iterator[E], o: Any): Boolean = {
    var changed = false
    while (iter.hasNext() && !changed) {
      if (iter.next() === o) {
        iter.remove()
        changed = true
      }
    }

    changed
  }

  def removeFirstOccurrence(o: Any): Boolean =
    _removeOccurrence(iterator(), o)

  def removeLastOccurrence(o: Any): Boolean =
    _removeOccurrence(descendingIterator(), o)

  override def listIterator(index: Int): ListIterator[E] = {
    checkIndexOnBounds(index)
    new ListIterator[E] {

      private var last: Double = -1
      private var i: Double = index

      private var currentNode: Node[E] =
        if (index == size) null else
        getNodeAt(index)

      private var lastNode: Node[E] =
        if (currentNode ne null) null else
        LinkedList.this.last

      def hasNext(): Boolean =
        i < size

      def next(): E = {
        if (i >= size)
          throw new NoSuchElementException()

        last = i
        i += 1

        lastNode = currentNode
        currentNode = currentNode.next

        lastNode.value
      }

      def hasPrevious(): Boolean =
        i > 0

      def previous(): E = {
        if (!hasPrevious)
          throw new NoSuchElementException()

        i -= 1
        last = i

        if (currentNode eq null)
          currentNode = LinkedList.this.last
        else
          currentNode = currentNode.prev

        lastNode = currentNode

        lastNode.value
      }

      def nextIndex(): Int = i.toInt

      def previousIndex(): Int = (i - 1).toInt

      def remove(): Unit = {
        checkThatHasLast()

        if (currentNode eq null) {
          removeLast()
          lastNode = LinkedList.this.last
        } else {
          removeNode(lastNode)
        }

        if (last < i) {
          i -= 1
        }

        last = -1
      }

      def set(e: E): Unit = {
        checkThatHasLast()
        lastNode.value = e
      }

      def add(e: E): Unit = {
        if (currentNode eq null) {
          addLast(e)
          lastNode = LinkedList.this.last
        } else {
          addNode(currentNode, e)
        }

        i += 1
        last = -1
      }

      private def checkThatHasLast(): Unit = {
        if (last == -1)
          throw new IllegalStateException()
      }
    }
  }

  def descendingIterator(): Iterator[E] = {
    new Iterator[E] {

      private var removeEnabled = false
      private var nextNode: Node[E] =
        LinkedList.this.last

      def hasNext(): Boolean =
        nextNode ne null

      def next(): E = {
        if (!hasNext())
          throw new NoSuchElementException()

        removeEnabled = true
        val ret = nextNode
        nextNode = nextNode.prev
        ret.value
      }

      def remove(): Unit = {
        if (!removeEnabled)
          throw new IllegalStateException()

        removeEnabled = false
        if (nextNode eq null)
          removeFirst()
        else
          removeNode(nextNode.next)
      }
    }
  }

  override def clone(): AnyRef =
    new LinkedList[E](this)

}

object LinkedList {

  protected[LinkedList] final class Node[T](
      var value: T,
      var prev: Node[T] = null,
      var next: Node[T] = null)

}
