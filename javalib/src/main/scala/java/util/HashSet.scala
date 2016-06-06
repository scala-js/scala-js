package java.util

import scala.collection.mutable
import scala.collection.JavaConverters._

class HashSet[E] extends AbstractSet[E] with Set[E]
                                        with Cloneable
                                        with Serializable { self =>
  def this(initialCapacity: Int, loadFactor: Float) =
    this()

  def this(initialCapacity: Int) =
    this()

  def this(c: Collection[_ <: E]) = {
    this()
    addAll(c)
  }

  protected val inner: mutable.Set[Box[E]] =
    new mutable.HashSet[Box[E]]()

  override def contains(o: Any): Boolean =
    inner.contains(Box(o.asInstanceOf[E]))

  override def remove(o: Any): Boolean =
    inner.remove(Box(o.asInstanceOf[E]))

  override def containsAll(c: Collection[_]): Boolean =
    c.iterator.asScala.forall(e => contains(e))

  override def removeAll(c: Collection[_]): Boolean = {
    val iter = c.iterator
    var changed = false
    while (iter.hasNext)
      changed = remove(iter.next()) || changed
    changed
  }

  override def retainAll(c: Collection[_]): Boolean = {
    val iter = iterator
    var changed = false
    while (iter.hasNext) {
      val value = iter.next
      if (!c.contains(value))
        changed = remove(value) || changed
    }
    changed
  }

  override def add(e: E): Boolean =
    inner.add(Box(e))

  override def addAll(c: Collection[_ <: E]): Boolean = {
    val iter = c.iterator()
    var changed = false
    while (iter.hasNext)
      changed = add(iter.next()) || changed
    changed
  }

  override def clear(): Unit = inner.clear()

  override def size(): Int = inner.size

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

}
