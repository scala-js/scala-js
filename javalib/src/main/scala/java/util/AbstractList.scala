package java.util

import scala.annotation.tailrec

import scala.collection.JavaConverters._

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
    iterator.asScala.indexWhere(_ === o)

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
    for ((elem, i) <- c.iterator.asScala.zipWithIndex)
      add(index + i, elem)
    !c.isEmpty
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
    if (fromIndex < 0)
      throw new IndexOutOfBoundsException(fromIndex.toString)
    else if (toIndex > size)
      throw new IndexOutOfBoundsException(toIndex.toString)
    else if (fromIndex > toIndex)
      throw new IllegalArgumentException

    self match {
      case _: RandomAccess =>
        new AbstractListView(self, fromIndex, toIndex) with RandomAccess { selfView =>
          override def listIterator(index: Int): ListIterator[E] = {
            checkIndexOnBounds(index)
            // Iterator that accesses the original list directly
            new RandomAccessListIterator(self, fromIndex + index, fromIndex, selfView.toIndex) {
              override protected def onSizeChanged(delta: Int): Unit = changeViewSize(delta)
            }
          }
        }
      case _ =>
        new AbstractListView(self, fromIndex, toIndex) { selfView =>
          override def listIterator(index: Int): ListIterator[E] = {
            checkIndexOnBounds(index)
            // Iterator that accesses the original list using it's iterator
            new BackedUpListIterator(list.listIterator(fromIndex + index),
                fromIndex, selfView.toIndex - fromIndex) {
              override protected def onSizeChanged(delta: Int): Unit = changeViewSize(delta)
            }
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
          this.asScala.forall(oIter.hasNext && _ === oIter.next()) && !oIter.hasNext
        case _ => false
      }
    }
  }

  override def hashCode(): Int = {
    this.asScala.foldLeft(1) {
      (prev, elem) => 31 * prev + (if (elem == null) 0 else elem.hashCode)
    }
  }

  protected def removeRange(fromIndex: Int, toIndex: Int): Unit = {
    val iter = listIterator(fromIndex)
    for (_ <- fromIndex until toIndex) {
      iter.next()
      iter.remove()
    }
  }

  protected[this] def checkIndexInBounds(index: Int): Unit = {
    if (index < 0 || index >= size)
      throw new IndexOutOfBoundsException(index.toString)
  }

  protected[this] def checkIndexOnBounds(index: Int): Unit = {
    if (index < 0 || index > size)
      throw new IndexOutOfBoundsException(index.toString)
  }
}

private abstract class AbstractListView[E](protected val list: List[E],
    fromIndex: Int, protected var toIndex: Int) extends AbstractList[E] {

  override def add(index: Int, e: E): Unit = {
    checkIndexOnBounds(index)
    list.add(fromIndex + index, e)
    changeViewSize(1)
  }

  override def addAll(index: Int, c: Collection[_ <: E]): Boolean = {
    checkIndexOnBounds(index)
    list.addAll(fromIndex + index, c)
    val elementsAdded = c.size
    toIndex += elementsAdded
    elementsAdded != 0
  }

  override def addAll(c: Collection[_ <: E]): Boolean =
    addAll(size, c)

  def get(index: Int): E = {
    checkIndexInBounds(index)
    list.get(fromIndex + index)
  }

  override def remove(index: Int): E = {
    checkIndexInBounds(index)
    val elem = list.remove(fromIndex + index)
    changeViewSize(-1)
    elem
  }

  override def set(index: Int, e: E): E = {
    checkIndexInBounds(index)
    list.set(fromIndex + index, e)
  }

  def size(): Int =
    toIndex - fromIndex

  @inline
  protected def changeViewSize(delta: Int): Unit =
    toIndex += delta
}

/* BackedUpListIterator implementation assumes that the underling list is not
 * necessarily on a RandomAccess list. Hence it wraps the underling list
 * iterator and assumes that this one is more efficient than accessing
 * elements by index.
 */
private class BackedUpListIterator[E](innerIterator: ListIterator[E], fromIndex: Int,
    override protected var end: Int) extends ListIterator[E] with SizeChangeEvent {

  def hasNext(): Boolean =
    i < end

  def next(): E =
    innerIterator.next()

  def hasPrevious(): Boolean =
    0 < i

  def previous(): E =
    innerIterator.previous()

  def nextIndex(): Int = i

  def previousIndex(): Int = i - 1

  def remove(): Unit = {
    innerIterator.remove()
    changeSize(-1)
  }

  def set(e: E): Unit =
    innerIterator.set(e)

  def add(e: E): Unit = {
    innerIterator.add(e)
    changeSize(1)
  }

  private def i: Int =
    innerIterator.nextIndex - fromIndex
}

/* RandomAccessListIterator implementation assumes that the has an efficient
 * .get(index) implementation.
 */
private class RandomAccessListIterator[E](list: List[E], i: Int, start: Int, end: Int)
    extends AbstractRandomAccessListIterator[E](i, start, end) {

  protected def get(index: Int): E =
    list.get(index)

  protected def set(index: Int, e: E): Unit =
    list.set(index, e)

  protected def remove(index: Int): Unit =
    list.remove(index)

  protected def add(index: Int, e: E): Unit =
    list.add(index, e)
}
