package java.util.concurrent

import java.util._
import scala.collection.JavaConversions._

class ConcurrentLinkedQueue[E]()
    extends AbstractQueue[E] with Queue[E] with Serializable {

  def this(c: Collection[_ <: E]) = {
    this()
    addAll(c)
  }

  import ConcurrentLinkedQueue._

  private var head: Node[E] = null
  private var last: Node[E] = null

  private var _size: Double = 0

  override def add(e: E): Boolean = {
    if (e == null) {
      throw new NullPointerException()
    } else {
      val oldLast = last

      last = new Node(e)

      _size += 1

      if (oldLast ne null)
        oldLast.next = last
      else
        head = last

      true
    }
  }

  override def offer(e: E): Boolean =
    add(e)

  override def poll(): E = {
    if (isEmpty()) null.asInstanceOf[E]
    else {
      val oldHead = head
      head = oldHead.next

      if (head eq null)
        last = null

      _size -= 1
      oldHead.value
    }
  }

  override def peek(): E =
    if (isEmpty()) null.asInstanceOf[E]
    else head.value

  override def isEmpty(): Boolean =
    _size == 0

  override def size(): Int =
    _size.toInt

  private def getNodeAt(index: Int): Node[E] = {
    var current: Node[E] = head
    for (_ <- 0 until index)
      current = current.next
    current
  }

  private def removeNode(node: Node[E]): Unit = {
    if (node eq head) {
      poll()
    } else if (head ne null) {
      var prev = head
      var current: Node[E] = head.next

      while ((current ne null) && (current ne node)) {
        prev = current
        current = current.next
      }

      if (current eq null) {
        null.asInstanceOf[E]
      } else {
        _size -= 1

        prev.next = current.next
        if (current eq last)
          last = prev
      }
    }
  }

  override def iterator(): Iterator[E] = {
    new Iterator[E] {

      private var nextNode: Node[Node[E]] = {
        val originalHead: Node[Node[E]] =
          if (head ne null) new Node(head)
          else null

        var current = originalHead
        while (current ne null) {
          val newNode: Node[Node[E]] =
            if (current.value.next ne null) new Node(current.value.next)
            else null

          current.next = newNode
          current = newNode
        }

        originalHead
      }

      private var lastNode: Node[Node[E]] = null

      def hasNext(): Boolean =
        nextNode ne null

      def next(): E = {
        if (nextNode eq null)
          throw new NoSuchElementException()

        lastNode = nextNode
        nextNode = nextNode.next

        lastNode.value.value
      }

      def remove(): Unit = {
        if (lastNode eq null)
          throw new IllegalStateException()

        removeNode(lastNode.value)

        lastNode = null
      }
    }
  }

}

object ConcurrentLinkedQueue {

  private final class Node[T](
      var value: T,
      var next: Node[T] = null)

}
