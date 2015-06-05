package java.util.concurrent

import java.util._
import java.lang.Comparable
import scala.collection.mutable
import scala.math.Ordering

import scala.collection.JavaConversions._

class ConcurrentSkipListSet[E] protected (ordering: Ordering[_ >: E], _comparator: Comparator[_ >: E])
    extends AbstractSet[E]
    with NavigableSet[E]
    with Cloneable
    with Serializable { self =>

  def this() =
    this(defaultOrdering[E], null.asInstanceOf[Comparator[_ >: E]])

  def this(comparator: Comparator[_ >: E]) =
    this(Ordering.comparatorToOrdering(comparator), null.asInstanceOf[Comparator[E]])

  def this(collection: Collection[_ <: E]) = {
    this(defaultOrdering[E], null.asInstanceOf[Comparator[E]])
    addAll(collection)
  }

  def this(sortedSet: SortedSet[E]) = {
    this(Ordering.comparatorToOrdering(sortedSet.comparator()),
        sortedSet.comparator())
    addAll(sortedSet)
  }

  private implicit object BoxOrdering extends Ordering[Box[E]] {
    def compare(a: Box[E], b:Box[E]): Int = ordering.compare(a.inner, b.inner)
  }

  private val inner: mutable.SortedSet[Box[E]] = new mutable.TreeSet[Box[E]]()

  override def clone(): ConcurrentSkipListSet[E] =
    new ConcurrentSkipListSet(this)

  def size(): Int =
    inner.size

  override def isEmpty(): Boolean =
    inner.isEmpty

  override def contains(o: Any): Boolean =
    inner.contains(Box(o.asInstanceOf[E]))

  override def add(e: E): Boolean =
    if (e == null) throw new NullPointerException()
    else inner.add(Box(e))

  override def remove(o: Any): Boolean =
    if (o == null) throw new NullPointerException()
    else inner.remove(Box(o.asInstanceOf[E]))

  override def clear(): Unit =
    inner.clear()

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

  def descendingIterator(): Iterator[E] = {
    new Iterator[E] {
      private val iter = inner.iterator.toList.reverse.iterator

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

  override def removeAll(c: Collection[_]): Boolean = {
    val iter = c.iterator()
    var changed = false
    while (iter.hasNext)
      changed = inner.remove(iter.next) || changed

    changed
  }

  override def addAll(c: Collection[_ <: E]): Boolean = {
    val iter = c.iterator()
    var changed = false
    while (iter.hasNext)
      changed = add(iter.next()) || changed
    changed
  }

  def lower(e: E): E =
    headSet(e, false).lastOption.getOrElse(null.asInstanceOf[E])

  def floor(e: E): E =
    headSet(e, true).lastOption.getOrElse(null.asInstanceOf[E])

  def ceiling(e: E): E =
    tailSet(e, true).headOption.getOrElse(null.asInstanceOf[E])

  def higher(e: E): E =
    tailSet(e, false).headOption.getOrElse(null.asInstanceOf[E])

  def pollFirst(): E = {
    val polled = inner.headOption
    if (polled.isDefined) {
      val elem = polled.get.inner
      remove(elem)
      elem
    } else null.asInstanceOf[E]
  }

  def pollLast(): E = {
    val polled = inner.lastOption
    if (polled.isDefined) {
      val elem = polled.get.inner
      remove(elem)
      elem
    } else null.asInstanceOf[E]
  }

  def comparator(): Comparator[_ >: E] = _comparator

  def first(): E =
    if (inner.isEmpty) throw new NoSuchElementException()
    else inner.head.inner

  def last(): E =
    if (inner.isEmpty) throw new NoSuchElementException()
    else inner.last.inner

  def subSet(fromElement: E, fromInclusive: Boolean, toElement: E, toInclusive: Boolean): NavigableSet[E] = {
    val subSetFun = { () =>
      val boxedFrom = Box(fromElement)
      val boxedTo = Box(toElement)
      val ss = new mutable.TreeSet[Box[E]]()
      ss ++= inner.range(boxedFrom, boxedTo)
      if (!fromInclusive && inner.contains(boxedFrom))
        ss.remove(boxedFrom)
      if (toInclusive && inner.contains(boxedTo))
        ss += boxedTo
      ss
    }
    new ConcurrentSkipListSet.NavigableView(this, subSetFun)
  }

  def headSet(toElement: E, inclusive: Boolean): NavigableSet[E] = {
    val headSetFun = { () =>
      val boxed = Box(toElement)
      val hs = new mutable.TreeSet[Box[E]]()
      hs ++= inner.until(boxed)
      if (inclusive && inner.contains(boxed))
        hs += boxed
      hs
    }
    new ConcurrentSkipListSet.NavigableView(this, headSetFun)
  }

  def tailSet(fromElement: E, inclusive: Boolean): NavigableSet[E] =  {
    val tailSetFun = { () =>
      val boxed = Box(fromElement)
      val ts = new mutable.TreeSet[Box[E]]()
      ts ++= inner.from(boxed)
      if (!inclusive && inner.contains(boxed))
        ts.remove(boxed)
      ts
    }
    new ConcurrentSkipListSet.NavigableView(this, tailSetFun)
  }

  def subSet(fromElement: E, toElement: E): NavigableSet[E] =
    subSet(fromElement, true, toElement, false)

  def headSet(toElement: E): NavigableSet[E] =
    headSet(toElement, false)

  def tailSet(fromElement: E): NavigableSet[E] =
    tailSet(fromElement, true)

  def descendingSet(): NavigableSet[E] = {
    val descSetFun = { () =>
      val retSet = new mutable.TreeSet[Box[E]]()(BoxOrdering.reverse)
      retSet.addAll(inner)
      retSet
    }
    new ConcurrentSkipListSet.NavigableView(this, descSetFun)
  }
}

object ConcurrentSkipListSet {

  private class NavigableView[E](original: NavigableSet[E], inner: () => mutable.SortedSet[Box[E]])
      extends AbstractCollection[E] with NavigableSet[E] with SortedSet[E] {

    def size(): Int =
      iterator.size

    override def contains(o: Any): Boolean =
      inner().contains(Box(o.asInstanceOf[E]))

    override def add(e: E): Boolean =
      original.add(e)

    override def remove(o: Any): Boolean = {
      if (contains(o))
        original.remove(o)
      else false
    }

    def iterator(): Iterator[E] =
      inner().iterator.map(_.inner)

    def descendingIterator(): Iterator[E] = {
      new Iterator[E] {
        private val iter = iterator.toList.reverse.iterator

        private var last: Option[E] = None

        def hasNext(): Boolean = iter.hasNext

        def next(): E = {
          last = Some(iter.next())
          last.get
        }

        def remove(): Unit = {
          if (last.isEmpty) {
            throw new IllegalStateException()
          } else {
            last.foreach(original.remove(_))
            last = None
          }
        }
      }
    }

    override def removeAll(c: Collection[_]): Boolean = {
      val iter = c.iterator()
      var changed = false
      while (iter.hasNext)
          changed = remove(iter.next) || changed

      changed
    }

    override def addAll(c: java.util.Collection[_ <: E]): Boolean =
      original.addAll(c)

    def lower(e: E): E =
      headSet(e, false).lastOption.getOrElse(null.asInstanceOf[E])

    def floor(e: E): E =
      headSet(e, true).lastOption.getOrElse(null.asInstanceOf[E])

    def ceiling(e: E): E =
      tailSet(e, true).headOption.getOrElse(null.asInstanceOf[E])

    def higher(e: E): E =
      tailSet(e, false).headOption.getOrElse(null.asInstanceOf[E])

    def pollFirst(): E = {
      val polled = inner().headOption
      if (polled.isDefined) {
        val elem = polled.get.inner
        remove(elem)
        elem
      } else null.asInstanceOf[E]
    }

    def pollLast(): E = {
      val polled = inner().lastOption
      if (polled.isDefined) {
        val elem = polled.get.inner
        remove(elem)
        elem
      } else null.asInstanceOf[E]
    }

    def comparator(): java.util.Comparator[E] = {
      new Comparator[E] {
        val ordering = inner().ordering
        def compare(a: E, b: E): Int =
          ordering.compare(Box(a), Box(b))
      }
    }

    def first(): E = {
      val iter = iterator()
      if (iter.hasNext) iter.next
      else null.asInstanceOf[E]
    }

    def last(): E = {
      val iter = iterator()
      if (iter.hasNext) iter.toTraversable.last
      else null.asInstanceOf[E]
    }

    def subSet(fromElement: E, fromInclusive: Boolean, toElement: E, toInclusive: Boolean): NavigableSet[E] = {
      val innerNow = inner()
      val subSetFun = { () =>
        val boxedFrom = Box(fromElement)
        val boxedTo = Box(toElement)
        val ss = new mutable.TreeSet[Box[E]]()(innerNow.ordering)
        ss ++= innerNow.range(boxedFrom, boxedTo)
        if (!fromInclusive && innerNow.contains(boxedFrom))
          ss.remove(boxedFrom)
        if (toInclusive && innerNow.contains(boxedTo))
          ss += boxedTo
        ss
      }
      new ConcurrentSkipListSet.NavigableView(this, subSetFun)
    }

    def headSet(toElement: E, inclusive: Boolean): NavigableSet[E] = {
      val innerNow = inner()
      val headSetFun = { () =>
        val boxed = Box(toElement)
        val hs = new mutable.TreeSet[Box[E]]()(innerNow.ordering)
        hs ++= innerNow.until(boxed)
        if (inclusive && innerNow.contains(boxed))
          hs += boxed
        hs
      }
      new ConcurrentSkipListSet.NavigableView(this, headSetFun)
    }

    def tailSet(fromElement: E, inclusive: Boolean): NavigableSet[E] =  {
      val innerNow = inner()
      val tailSetFun = { () =>
        val boxed = Box(fromElement)
        val ts = new mutable.TreeSet[Box[E]]()(innerNow.ordering)
        ts ++= innerNow.from(boxed)
        if (!inclusive && innerNow.contains(boxed))
          ts.remove(boxed)
        ts
      }
      new ConcurrentSkipListSet.NavigableView(this, tailSetFun)
    }

    def subSet(fromElement: E, toElement: E): NavigableSet[E] =
      subSet(fromElement, true, toElement, false)

    def headSet(toElement: E): NavigableSet[E] =
      headSet(toElement, false)

    def tailSet(fromElement: E): NavigableSet[E] =
      tailSet(fromElement, true)

    def descendingSet(): NavigableSet[E] = {
      val descSetFun = { () =>
        val innerNow = inner()
        val retSet = new mutable.TreeSet[Box[E]]()(innerNow.ordering.reverse)
        retSet.addAll(innerNow)
        retSet
      }
      new NavigableView(this, descSetFun)
    }
  }

}
