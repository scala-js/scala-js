package java.util

import java.lang.Comparable
import scala.math.Ordering
import scala.collection.mutable
import scala.annotation.tailrec
import scala.language.existentials

class PriorityQueue[E] protected (ordering: Ordering[_ >: E], _comparator: Comparator[_ >: E])
    extends AbstractQueue[E] with Serializable { self =>

  def this(initialCapacity: Int) = {
    this(defaultOrdering[E], null.asInstanceOf[Comparator[_ >: E]])
    if (initialCapacity < 1)
      throw new IllegalArgumentException()
  }

  def this() =
    this(11)

  def this(initialCapacity: Int, comparator: Comparator[_ >: E]) = {
    this(PriorityQueue.safeGetOrdering[E](comparator), null.asInstanceOf[Comparator[E]])
    if (initialCapacity < 1)
      throw new IllegalArgumentException()
  }

  def this(c: Collection[_ <: E]) = {
    this(defaultOrdering[E], null.asInstanceOf[Comparator[E]])
    addAll(c)
  }

  def this(c: PriorityQueue[_ <: E]) = {
    this(PriorityQueue.safeGetOrdering[E](c.comparator()),
        c.comparator().asInstanceOf[Comparator[E]])
    addAll(c)
  }

  def this(sortedSet: SortedSet[_ <: E]) = {
    this(PriorityQueue.safeGetOrdering[E](sortedSet.comparator()),
        sortedSet.comparator().asInstanceOf[Comparator[E]])
    addAll(sortedSet)
  }

  private implicit object BoxOrdering extends Ordering[Box[E]] {
    def compare(a: Box[E], b:Box[E]): Int = ordering.compare(b.inner, a.inner)
  }

  private val inner: mutable.PriorityQueue[Box[E]] = new mutable.PriorityQueue[Box[E]]()

  override def add(e: E): Boolean = {
    if (e == null) throw new NullPointerException()
    else {
      inner += Box(e)
      true
    }
  }

  def offer(e: E): Boolean = add(e)

  def peek(): E =
    inner.headOption.fold(null.asInstanceOf[E])(_.inner)

  override def remove(o: Any): Boolean = {
    val boxed = Box(o.asInstanceOf[E])
    val initialSize = inner.size

    @tailrec
    def takeLeft(part: mutable.PriorityQueue[Box[E]]): mutable.PriorityQueue[Box[E]] = {
      if (inner.isEmpty) part
      else {
        val next = inner.dequeue
        if (boxed == next) part
        else if (BoxOrdering.compare(boxed, next) > 0)
          part += next
        else takeLeft(part += next)
      }
    }

    val left = takeLeft(new mutable.PriorityQueue[Box[E]]())
    inner ++= left

    (inner.size != initialSize)
  }

  override def contains(o: Any): Boolean =
    inner.exists(_ === Box(o))

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

  def size(): Int = inner.size

  override def clear(): Unit =
    inner.dequeueAll

  def poll(): E =
    if (inner.isEmpty) null.asInstanceOf[E]
    else inner.dequeue().inner

  def comparator(): Comparator[_ >: E] = _comparator
}

object PriorityQueue {

  def safeGetOrdering[E](_comp: => Comparator[_]): Ordering[E] = {
    val comp: Comparator[_] = _comp
    val ord =
      if (comp == null) defaultOrdering[E]
      else Ordering.comparatorToOrdering(comp)

    ord.asInstanceOf[Ordering[E]]
  }

}
