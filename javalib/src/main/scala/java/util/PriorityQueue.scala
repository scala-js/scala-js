package java.util

import java.lang.Comparable
import scala.math.Ordering
import scala.collection.mutable

class PriorityQueue[E] protected (ordering: Ordering[_ >: E], _comparator: Comparator[_ >: E])
    extends AbstractQueue[E]
    with Serializable { self =>

  def this(initialCapacity: Int) = {
    this(defaultOrdering[E], null.asInstanceOf[Comparator[_ >: E]])
    if (initialCapacity < 1) throw new IllegalArgumentException()
  }

  def this() =
    this(11)

  def this(initialCapacity: Int, comparator: Comparator[_ >: E]) = {
    this(Ordering.comparatorToOrdering(comparator), null.asInstanceOf[Comparator[E]])
    if (initialCapacity < 1) throw new IllegalArgumentException()
  }

  def this(c: Collection[_ <: E]) = {
    this(defaultOrdering[E], null.asInstanceOf[Comparator[E]])
    addAll(c)
  }

  def this(c: PriorityQueue[_ <: E]) = {
    this(Ordering.comparatorToOrdering(c.comparator()).asInstanceOf[Ordering[E]],
      c.comparator().asInstanceOf[Comparator[E]])
    addAll(c)
  }

  def this(sortedSet: SortedSet[_ <: E]) = {
    this(Ordering.comparatorToOrdering(sortedSet.comparator()).asInstanceOf[Ordering[E]],
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
    inner.headOption.map(_.inner).getOrElse(null.asInstanceOf[E])

  override def remove(o: Any): Boolean = {
    val boxed = Box(o)
    val left = new mutable.PriorityQueue[Box[E]]()
    var last: Box[E] = 
      if (inner.isEmpty) null
      else inner.dequeue
    
    while (last != null) {
      if (last === boxed){
        inner ++= left
        return true
      } else if (inner.isEmpty) {
        inner ++= left
        return false
      } else {
        val next = inner.dequeue
        if (BoxOrdering.compare(last, next) < 0) {
          left += last
          last = next
        } else {
          inner ++= left
          return false
        }
      }
    }
    false
  }

  override def contains(o: Any): Boolean =
    inner.exists(_ === Box(o))

  def iterator(): Iterator[E] = {
    new Iterator[E] {
      private val iter = inner.iterator

      var last: Option[E] = None

      def hasNext(): Boolean = iter.hasNext

      def next(): E = {
        last = Some(iter.next().inner)
        last.get
      }

      def remove(): Unit = last.map(self.remove(_))
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
