package java.util

import scala.collection.JavaConversions._

import scala.annotation.tailrec

abstract class AbstractList[E] protected () extends AbstractCollection[E]
                                               with List[E] {
  self =>

  override def add(element: E): Boolean = {
    add(size, element)
    true
  }

  def set(index: Int, element: E): E =
    throw new UnsupportedOperationException

  def add(index: Int, element: E): Unit =
    throw new UnsupportedOperationException

  def remove(index: Int): E =
    throw new UnsupportedOperationException

  def indexOf(o: Any): Int =
    iterator().indexWhere(_ === o)

  def lastIndexOf(o: Any): Int = {
    @tailrec
    def findIndex(iter: ListIterator[E]): Int = {
      if (!iter.hasPrevious) -1
      else if (iter.previous() === o) iter.nextIndex
      else findIndex(iter)
    }
    findIndex(listIterator(size))
  }

  override def clear(): Unit =
    removeRange(0, size)

  def addAll(index: Int, c: Collection[_ <: E]): Boolean = {
    checkIndexOnBounds(index)
    for ((elem, i) <- c.iterator().zipWithIndex)
      add(index + i, elem)
    c.nonEmpty
  }

  def iterator(): Iterator[E] =
    listIterator()

  def listIterator(): ListIterator[E] =
    listIterator(0)

  def listIterator(index: Int): ListIterator[E] = {
    checkIndexOnBounds(index)
    // By default we use RandomAccessListIterator because we only have access to
    // the get(index) operation in the API. Subclasses override this if needs
    // using their knowledge of the structure instead.
    new RandomAccessListIterator(self, index, 0, size)
  }

  def subList(fromIndex: Int, toIndex: Int): List[E] = {
    if (fromIndex < 0 || toIndex > size)
      throw new IndexOutOfBoundsException
    else if (fromIndex > toIndex)
      throw new IllegalArgumentException

    self match {
      case _: RandomAccess =>
        new AbstractListView(fromIndex, toIndex) with RandomAccess {
          override def listIterator(index: Int): ListIterator[E] = {
            checkIndexOnBounds(index)
            // Iterator that accesses the original list directly
            new RandomAccessListIterator(self, fromIndex + index, fromIndex, toIndex)
          }
        }
      case _ =>
        new AbstractListView(fromIndex, toIndex) {
          override def listIterator(index: Int): ListIterator[E] = {
            checkIndexOnBounds(index)
            // Iterator that accesses the original list using it's iterator
            new BackedUpListIterator(self, index, fromIndex)
          }
        }
    }
  }

  override def equals(o: Any): Boolean = {
    if (o.asInstanceOf[AnyRef] eq this) {
      true
    } else {
      o match {
        case o: List[_] =>
          val oIter = o.listIterator
          this.forall(oIter.hasNext && _ === oIter.next()) && !oIter.hasNext
        case _ => false
      }
    }
  }

  override def hashCode(): Int = {
    this.foldLeft(1) {
      (prev, elem) => 31 * prev + (if (elem == null) 0 else elem.hashCode)
    }
  }

  protected def removeRange(fromIndex: Int, toIndex: Int): Unit = {
    val iter = listIterator(fromIndex)
    iter.take(toIndex - fromIndex).foreach(_ => iter.remove())
  }

  protected[this] def checkIndexInBounds(index: Int): Unit = {
    if (index < 0 || index >= size)
      throw new IndexOutOfBoundsException(index.toString)
  }

  protected[this] def checkIndexOnBounds(index: Int): Unit = {
    if (index < 0 || index > size)
      throw new IndexOutOfBoundsException(index.toString)
  }

  private abstract class AbstractListView(fromIndex: Int,
      private var toIndex: Int) extends AbstractList[E] {

    override def add(index: Int, e: E): Unit = {
      checkIndexOnBounds(index)
      self.add(fromIndex + index, e)
      toIndex += 1
    }

    override def addAll(index: Int, c: Collection[_ <: E]): Boolean = {
      checkIndexOnBounds(index)
      self.addAll(fromIndex + index, c)
      val elementsAdded = c.size
      toIndex += elementsAdded
      elementsAdded != 0
    }

    override def addAll(c: Collection[_ <: E]): Boolean =
      addAll(size, c)

    def get(index: Int): E = {
      checkIndexInBounds(index)
      self.get(fromIndex + index)
    }

    override def remove(index: Int): E = {
      checkIndexInBounds(index)
      val elem = self.remove(fromIndex + index)
      toIndex -= 1
      elem
    }

    override def set(index: Int, e: E): E = {
      checkIndexInBounds(index)
      self.set(fromIndex + index, e)
    }

    def size(): Int =
      toIndex - fromIndex

    override protected def removeRange(_fromIndex: Int, _toIndex: Int): Unit = {
      super.removeRange(_fromIndex, _toIndex)
      toIndex -= _toIndex - _fromIndex
    }

  }

}

/* BackedUpListIterator implementation assumes that the underling list is not
 * necessarily on a RandomAccess list. Hence it wraps the underling list
 * iterator and assumes that this one is more efficient than accessing
 * elements by index.
 */
private class BackedUpListIterator[E](list: List[E], startIndex: Int, fromIndex: Int)
  extends ListIterator[E] {

  private val innerIterator = list.listIterator(fromIndex + startIndex)

  def hasNext(): Boolean =
    i < list.size

  def next(): E =
    innerIterator.next()

  def hasPrevious(): Boolean =
    0 < i

  def previous(): E =
    innerIterator.previous()

  def nextIndex(): Int = i

  def previousIndex(): Int = i - 1

  def remove(): Unit =
    innerIterator.remove()

  def set(e: E): Unit =
    innerIterator.set(e)

  def add(e: E): Unit =
    innerIterator.add(e)

  private def i: Int =
    innerIterator.nextIndex - fromIndex
}

/* RandomAccessListIterator implementation assumes that the has an efficient
 * .get(index) implementation.
 */
private class RandomAccessListIterator[E](list: List[E], private var i: Int,
    start: Int, private var end: Int) extends ListIterator[E] {

  private var last = -1

  def hasNext(): Boolean =
    i < end

  def next(): E = {
    last = i
    i += 1
    list.get(last)
  }

  def hasPrevious(): Boolean =
    start < i

  def previous(): E = {
    i -= 1
    last = i
    list.get(last)
  }

  def nextIndex(): Int = i

  def previousIndex(): Int = i - 1

  def remove(): Unit = {
    checkThatHasLast()
    list.remove(last)
    if (last < i)
      i -= 1
    last = -1
    end -= 1
  }

  def set(e: E): Unit = {
    checkThatHasLast()
    list.set(last, e)
  }

  def add(e: E): Unit = {
    list.add(i, e)
    end += 1
    last = -1
    i += 1
  }

  private def checkThatHasLast(): Unit = {
    if (last == -1)
      throw new IllegalStateException
  }
}
