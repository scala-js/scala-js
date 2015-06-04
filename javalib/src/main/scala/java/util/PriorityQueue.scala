package java.util

import java.lang.Comparable
import scala.math.Ordering
import scala.collection.mutable

abstract class PriorityQueue[E] protected (ordering: Ordering[_ >: E], _comparator: Comparator[_ >: E])
    extends AbstractQueue[E]
    with Serializable {

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

  private val inner: mutable.Queue[E] = new mutable.Queue[E]()

  override def add(e: E): Boolean =
    inner.add(e)

  def comparator(): Comparator[_ >: E] = _comparator
}
