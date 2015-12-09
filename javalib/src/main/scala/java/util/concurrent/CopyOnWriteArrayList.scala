package java.util.concurrent

import java.lang.{reflect => jlr}

import scala.annotation.tailrec
import scala.collection.JavaConversions._

import scala.scalajs._

import java.util._

class CopyOnWriteArrayList[E <: AnyRef] private (private var inner: js.Array[E])
    extends List[E] with RandomAccess with Cloneable with Serializable {
  self =>

  // requiresCopyOnWrite is false if and only if no other object
  // (like the iterator) may have a reference to inner
  private var requiresCopyOnWrite = false

  def this() = {
    this(new js.Array[E])
  }

  def this(c: Collection[_ <: E]) = {
    this()
    addAll(c)
  }

  def this(toCopyIn: Array[E]) = {
    this()
    toCopyIn.foreach(add)
  }

  def size(): Int =
    inner.size

  def isEmpty: Boolean =
    size == 0

  def contains(o: scala.Any): Boolean =
    iterator.exists(o === _)

  def indexOf(o: scala.Any): Int =
    indexOf(o.asInstanceOf[E], 0)

  def indexOf(e: E, index: Int): Int = {
    checkIndexInBounds(index)
    index + listIterator(index).indexWhere(_ === e)
  }

  def lastIndexOf(o: scala.Any): Int =
    lastIndexOf(o.asInstanceOf[E], 0)

  def lastIndexOf(e: E, index: Int): Int = {
    @tailrec
    def findIndex(iter: ListIterator[E]): Int = {
      if (!iter.hasPrevious) -1
      else if (iter.previous() === e) iter.nextIndex
      else findIndex(iter)
    }
    findIndex(listIterator(size))
  }

  override def clone(): AnyRef =
    new CopyOnWriteArrayList[E](this)

  def toArray(): Array[AnyRef] =
    toArray(new Array[AnyRef](size))

  def toArray[T](a: Array[T]): Array[T] = {
    val componentType = a.getClass.getComponentType
    val toFill: Array[T] =
      if (a.size >= size) a
      else jlr.Array.newInstance(componentType, size).asInstanceOf[Array[T]]

    val iter = iterator
    for (i <- 0 until size)
      toFill(i) = iter.next().asInstanceOf[T]
    if (toFill.size > size)
      toFill(size) = null.asInstanceOf[T]
    toFill
  }

  def get(index: Int): E = {
    checkIndexInBounds(index)
    innerGet(index)
  }

  def set(index: Int, element: E): E = {
    checkIndexInBounds(index)
    copyIfNeeded()
    val oldValue = innerGet(index)
    innerSet(index, element)
    oldValue
  }

  def add(e: E): Boolean = {
    copyIfNeeded()
    innerPush(e)
    true
  }

  def add(index: Int, element: E): Unit = {
    checkIndexOnBounds(index)
    copyIfNeeded()
    innerSplice(index, 0, element)
  }

  def remove(index: Int): E = {
    checkIndexInBounds(index)
    copyIfNeeded()
    innerSplice(index, 1)(0)
  }

  def remove(o: scala.Any): Boolean = {
    val index = indexOf(o)
    if (index == -1) false else {
      remove(index)
      true
    }
  }

  def addIfAbsent(e: E): Boolean = {
    if (contains(e)) false else {
      copyIfNeeded()
      innerPush(e)
      true
    }
  }

  def containsAll(c: Collection[_]): Boolean =
    c.iterator.forall(this.contains(_))

  def removeAll(c: Collection[_]): Boolean = {
    copyIfNeeded()
    c.foldLeft(false)((prev, elem) => remove(elem) || prev)
  }

  def retainAll(c: Collection[_]): Boolean = {
    val iter = iterator()
    clear()
    var modified = false
    for (elem <- iter) {
      if (c.contains(elem))
        innerPush(elem)
      else
        modified = true
    }
    modified
  }

  def addAllAbsent(c: Collection[_ <: E]): Int = {
    var added = 0
    for (e <- c.iterator()) {
      if (addIfAbsent(e))
        added += 1
    }
    added
  }

  def clear(): Unit = {
    inner = new js.Array[E]
    requiresCopyOnWrite = false
  }

  def addAll(c: Collection[_ <: E]): Boolean =
    addAll(size, c)

  def addAll(index: Int, c: Collection[_ <: E]): Boolean = {
    checkIndexOnBounds(index)
    copyIfNeeded()
    innerSplice(index, 0, c.asInstanceOf[Collection[E]].toSeq: _*)
    c.nonEmpty
  }

  override def toString: String =
    iterator().mkString("[", ",", "]")

  override def equals(obj: Any): Boolean = {
    if (obj.asInstanceOf[AnyRef] eq this) {
      true
    } else {
      obj match {
        case obj: List[_] =>
          val oIter = obj.listIterator
          this.forall(oIter.hasNext && _ === oIter.next()) && !oIter.hasNext
        case _ => false
      }
    }
  }

  override def hashCode(): Int = {
    iterator().foldLeft(1) {
      (prev, elem) => 31 * prev + (if (elem == null) 0 else elem.hashCode)
    }
  }

  def iterator(): Iterator[E] =
    listIterator()

  def listIterator(): ListIterator[E] =
    listIterator(0)

  def listIterator(index: Int): ListIterator[E] = {
    checkIndexOnBounds(index)
    new CopyOnWriteArrayListIterator[E](innerSnapshot(), index, 0, size)
  }

  def subList(fromIndex: Int, toIndex: Int): List[E] = {
    if (fromIndex < 0 || fromIndex > toIndex || toIndex > size)
      throw new IndexOutOfBoundsException
    new CopyOnWriteArrayListView(fromIndex, toIndex)
  }

  protected def innerGet(index: Int): E =
    inner(index)

  protected def innerSet(index: Int, elem: E): Unit =
    inner(index) = elem

  protected def innerPush(elem: E): Unit =
    inner.push(elem)

  protected def innerSplice(index: Int, deleteCount: Int, items: E*): js.Array[E] =
    inner.splice(index, deleteCount, items: _*)

  protected def copyIfNeeded(): Unit = {
    if (requiresCopyOnWrite) {
      inner = inner.jsSlice()
      requiresCopyOnWrite = false
    }
  }

  protected def innerSnapshot(): js.Array[E] = {
    requiresCopyOnWrite = true
    inner
  }

  private class CopyOnWriteArrayListView(fromIndex: Int, private var toIndex: Int)
      extends CopyOnWriteArrayList[E](null: js.Array[E]) {
    viewSelf =>

    override def size(): Int =
      toIndex - fromIndex

    override def clear(): Unit = {
      copyIfNeeded()
      self.innerSplice(fromIndex, size)
      changeSize(-size)
    }

    override def listIterator(index: Int): ListIterator[E] = {
      checkIndexOnBounds(index)
      new CopyOnWriteArrayListIterator[E](innerSnapshot(), fromIndex + index,
          fromIndex, toIndex) {
        override protected def onSizeChanged(delta: Int): Unit = changeSize(delta)
      }
    }

    override def subList(fromIndex: Int, toIndex: Int): List[E] = {
      if (fromIndex < 0 || fromIndex > toIndex || toIndex > size)
        throw new IndexOutOfBoundsException

      new CopyOnWriteArrayListView(viewSelf.fromIndex + fromIndex,
          viewSelf.fromIndex + toIndex) {
        override protected def changeSize(delta: Int): Unit = {
          super.changeSize(delta)
          viewSelf.changeSize(delta)
        }
      }
    }

    override def clone(): AnyRef =
      new CopyOnWriteArrayList[E](this)

    override protected def innerGet(index: Int): E =
      self.innerGet(fromIndex + index)

    override protected def innerSet(index: Int, elem: E): Unit =
      self.innerSet(fromIndex + index, elem)

    override protected def innerSplice(index: Int, deleteCount: Int,
        items: E*): js.Array[E] = {
      changeSize(items.size - deleteCount)
      self.innerSplice(fromIndex + index, deleteCount, items: _*)
    }

    override protected def innerPush(elem: E): Unit = {
      if (toIndex < self.size) {
        innerSplice(size, 0, elem)
      } else {
        changeSize(1)
        self.innerPush(elem)
      }
    }

    override protected def copyIfNeeded(): Unit =
      self.copyIfNeeded()

    override protected def innerSnapshot(): js.Array[E] =
      self.innerSnapshot()

    protected def changeSize(delta: Int): Unit =
      toIndex += delta
  }

  protected def checkIndexInBounds(index: Int): Unit = {
    if (index < 0 || index >= size)
      throw new IndexOutOfBoundsException(index.toString)
  }

  protected def checkIndexOnBounds(index: Int): Unit = {
    if (index < 0 || index > size)
      throw new IndexOutOfBoundsException(index.toString)
  }
}

private class CopyOnWriteArrayListIterator[E](arraySnapshot: js.Array[E], i: Int, start: Int, end: Int)
    extends AbstractRandomAccessListIterator[E](i, start, end) {
  override def remove(): Unit =
    throw new UnsupportedOperationException

  override def set(e: E): Unit =
    throw new UnsupportedOperationException

  override def add(e: E): Unit =
    throw new UnsupportedOperationException

  protected def get(index: Int): E =
    arraySnapshot(index)

  protected def remove(index: Int): Unit =
    throw new UnsupportedOperationException

  protected def set(index: Int, e: E): Unit =
    throw new UnsupportedOperationException

  protected def add(index: Int, e: E): Unit =
    throw new UnsupportedOperationException
}
