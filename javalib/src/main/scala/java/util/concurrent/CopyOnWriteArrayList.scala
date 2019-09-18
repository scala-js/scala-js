/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package java.util.concurrent

import java.lang.{reflect => jlr}
import java.util._
import java.util.function.Predicate

import scala.annotation.tailrec

import ScalaOps._

import scala.scalajs._

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
    for (i <- 0 until toCopyIn.length)
      add(toCopyIn(i))
  }

  def size(): Int =
    inner.size

  def isEmpty: Boolean =
    size == 0

  def contains(o: scala.Any): Boolean =
    iterator.scalaOps.exists(Objects.equals(o, _))

  def indexOf(o: scala.Any): Int =
    indexOf(o.asInstanceOf[E], 0)

  def indexOf(e: E, index: Int): Int = {
    checkIndexInBounds(index)
    index + listIterator(index).scalaOps.indexWhere(Objects.equals(_, e))
  }

  def lastIndexOf(o: scala.Any): Int =
    lastIndexOf(o.asInstanceOf[E], 0)

  def lastIndexOf(e: E, index: Int): Int = {
    @tailrec
    def findIndex(iter: ListIterator[E]): Int = {
      if (!iter.hasPrevious) -1
      else if (Objects.equals(iter.previous(), e)) iter.nextIndex
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
      if (a.length >= size) a
      else jlr.Array.newInstance(componentType, size).asInstanceOf[Array[T]]

    val iter = iterator
    for (i <- 0 until size)
      toFill(i) = iter.next().asInstanceOf[T]
    if (toFill.length > size)
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
    innerInsert(index, element)
  }

  def remove(index: Int): E = {
    checkIndexInBounds(index)
    copyIfNeeded()
    innerRemove(index)
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
    c.iterator.scalaOps.forall(this.contains(_))

  def removeAll(c: Collection[_]): Boolean = {
    copyIfNeeded()
    c.scalaOps.foldLeft(false)((prev, elem) => remove(elem) || prev)
  }

  def retainAll(c: Collection[_]): Boolean = {
    val iter = iterator()
    clear()
    var modified = false
    for (elem <- iter.scalaOps) {
      if (c.contains(elem))
        innerPush(elem)
      else
        modified = true
    }
    modified
  }

  def addAllAbsent(c: Collection[_ <: E]): Int = {
    var added = 0
    for (e <- c.iterator().scalaOps) {
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
    innerInsertMany(index, c)
    !c.isEmpty
  }

  /* Override Collection.removeIf() because our iterators do not support
   * the `remove()` method.
   */
  override def removeIf(filter: Predicate[_ >: E]): Boolean = {
    // scalastyle:off return
    /* The outer loop iterates as long as no element passes the filter (and
     * hence no modification is required).
     */
    val iter = iterator()
    var index = 0
    while (iter.hasNext()) {
      if (filter.test(iter.next())) {
        /* We found the first element that needs to be removed: copy and
         * truncate at the current index.
         */
        copyIfNeeded()
        innerRemoveMany(index, size() - index)
        /* Now keep iterating, but push elements that do not pass the test.
         * `index` is useless from now on, so do not keep updating it.
         */
        while (iter.hasNext()) {
          val elem = iter.next()
          if (!filter.test(elem))
            innerPush(elem)
        }
        return true
      }
      index += 1
    }
    false // the outer loop finished without entering the inner one
    // scalastyle:on return
  }

  override def toString: String =
    iterator().scalaOps.mkString("[", ",", "]")

  override def equals(obj: Any): Boolean = {
    if (obj.asInstanceOf[AnyRef] eq this) {
      true
    } else {
      obj match {
        case obj: List[_] =>
          val oIter = obj.listIterator
          this.scalaOps.forall(elem => oIter.hasNext && Objects.equals(elem, oIter.next())) && !oIter.hasNext
        case _ => false
      }
    }
  }

  override def hashCode(): Int = {
    iterator().scalaOps.foldLeft(1) {
      (prev, elem) => 31 * prev + Objects.hashCode(elem)
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

  protected def innerInsert(index: Int, elem: E): Unit =
    inner.splice(index, 0, elem)

  protected def innerInsertMany(index: Int, items: Collection[_ <: E]): Unit = {
    val itemsArray = js.Array[E]()
    items.scalaOps.foreach(itemsArray.push(_))
    inner.splice(index, 0, itemsArray.toSeq: _*)
  }

  protected def innerRemove(index: Int): E =
    inner.splice(index, 1)(0)

  protected def innerRemoveMany(index: Int, count: Int): Unit =
    inner.splice(index, count)

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
      self.innerRemoveMany(fromIndex, size)
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

    override protected def innerPush(elem: E): Unit = {
      changeSize(1)
      self.innerInsert(toIndex - 1, elem)
    }

    override protected def innerInsert(index: Int, elem: E): Unit = {
      changeSize(1)
      self.innerInsert(fromIndex + index, elem)
    }

    override protected def innerInsertMany(index: Int,
        items: Collection[_ <: E]): Unit = {
      changeSize(items.size())
      self.innerInsertMany(fromIndex + index, items)
    }

    override protected def innerRemove(index: Int): E = {
      changeSize(-1)
      self.innerRemove(fromIndex + index)
    }

    override protected def innerRemoveMany(index: Int, count: Int): Unit = {
      changeSize(-count)
      self.innerRemoveMany(index, count)
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
