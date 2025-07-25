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

import java.lang.Cloneable
import java.lang.Utils._
import java.lang.{reflect => jlr}
import java.util._
import java.util.function.{Predicate, UnaryOperator}

import scala.annotation.tailrec

import ScalaOps._

import scala.scalajs._
import scala.scalajs.LinkingInfo._

class CopyOnWriteArrayList[E <: AnyRef] private (initialCapacity: Int)
    extends List[E] with RandomAccess with Cloneable with Serializable {
  self =>

  /* This class has two different implementations for the
   * internal data storage, depending on whether we are on Wasm or JS.
   * We use `js.Array` on JS, and `scala.Array` on Wasm.
   * The initialCapacity parameter is effective only in Wasm,
   * since js.Array doesn't support explicit pre-allocation.
   *
   * On Wasm, we store the length at the index 0 of the array.
   */

  import CopyOnWriteArrayList._

  private var inner: innerImpl.Repr = innerImpl.make(initialCapacity)

  // requiresCopyOnWrite is false if and only if no other object
  // (like the iterator) may have a reference to inner
  private var requiresCopyOnWrite = false

  def this() = {
    this(16)
  }

  def this(c: Collection[_ <: E]) = {
    this(c.size())
    addAll(c)
  }

  def this(toCopyIn: Array[E]) = {
    this(toCopyIn.length)
    for (i <- 0 until toCopyIn.length)
      add(toCopyIn(i))
  }

  def size(): Int =
    innerImpl.length(inner)

  def isEmpty(): Boolean =
    size() == 0

  def contains(o: scala.Any): Boolean =
    iterator().scalaOps.exists(Objects.equals(o, _))

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
      if (!iter.hasPrevious()) -1
      else if (Objects.equals(iter.previous(), e)) iter.nextIndex()
      else findIndex(iter)
    }
    findIndex(listIterator(size()))
  }

  override def clone(): AnyRef =
    new CopyOnWriteArrayList[E](this)

  def toArray(): Array[AnyRef] =
    toArray(new Array[AnyRef](size()))

  def toArray[T <: AnyRef](a: Array[T]): Array[T] = {
    val componentType = a.getClass().getComponentType()
    val toFill: Array[T] =
      if (a.length >= size()) a
      else jlr.Array.newInstance(componentType, size()).asInstanceOf[Array[T]]

    val iter = iterator()
    for (i <- 0 until size())
      toFill(i) = iter.next().asInstanceOf[T]
    if (toFill.length > size())
      toFill(size()) = null.asInstanceOf[T]
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
    c.iterator().scalaOps.forall(this.contains(_))

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
    inner = innerImpl.make(16)
    requiresCopyOnWrite = false
  }

  def addAll(c: Collection[_ <: E]): Boolean =
    addAll(size(), c)

  def addAll(index: Int, c: Collection[_ <: E]): Boolean = {
    checkIndexOnBounds(index)
    copyIfNeeded()
    innerInsertMany(index, c)
    !c.isEmpty()
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

  override def replaceAll(operator: UnaryOperator[E]): Unit = {
    val size = this.size()
    if (size != 0) {
      copyIfNeeded()
      var i = 0
      while (i != size) {
        innerSet(i, operator.apply(innerGet(i)))
        i += 1
      }
    }
  }

  override def toString: String =
    iterator().scalaOps.mkString("[", ", ", "]")

  override def equals(obj: Any): Boolean = {
    if (obj.asInstanceOf[AnyRef] eq this) {
      true
    } else {
      obj match {
        case obj: List[_] =>
          val oIter = obj.listIterator()
          this.scalaOps.forall(elem => oIter.hasNext() && Objects.equals(elem, oIter.next())) && !oIter.hasNext()
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
    new CopyOnWriteArrayListIterator[E](innerSnapshot(), index, 0, size())
  }

  def subList(fromIndex: Int, toIndex: Int): List[E] = {
    if (fromIndex < 0 || fromIndex > toIndex || toIndex > size())
      throw new IndexOutOfBoundsException
    new CopyOnWriteArrayListView(fromIndex, toIndex)
  }

  protected def innerGet(index: Int): E =
    innerImpl.get(inner, index).asInstanceOf[E]

  protected def innerSet(index: Int, elem: E): Unit =
    innerImpl.set(inner, index, elem)

  protected def innerPush(elem: E): Unit = {
    ensureCapacity(size() + 1)
    innerImpl.push(inner, elem)
  }

  protected def innerInsert(index: Int, elem: E): Unit = {
    ensureCapacity(size() + 1)
    innerImpl.add(inner, index, elem.asInstanceOf[AnyRef])
  }

  protected def innerInsertMany(index: Int, items: Collection[_ <: E]): Unit = {
    ensureCapacity(size() + items.size())
    innerImpl.add(inner, index, items)
  }

  protected def innerRemove(index: Int): E =
    innerImpl.remove(inner, index).asInstanceOf[E]

  protected def innerRemoveMany(index: Int, count: Int): Unit =
    innerImpl.remove(inner, index, count)

  protected def copyIfNeeded(): Unit = {
    if (requiresCopyOnWrite) {
      inner = innerImpl.clone(inner)
      requiresCopyOnWrite = false
    }
  }

  protected def innerSnapshot(): innerImpl.Repr = {
    requiresCopyOnWrite = true
    inner
  }

  private class CopyOnWriteArrayListView(fromIndex: Int, private var toIndex: Int)
      extends CopyOnWriteArrayList[E](initialCapacity) {
    viewSelf =>

    override def size(): Int =
      toIndex - fromIndex

    override def clear(): Unit = {
      copyIfNeeded()
      self.innerRemoveMany(fromIndex, size())
      changeSize(-size())
    }

    override def listIterator(index: Int): ListIterator[E] = {
      checkIndexOnBounds(index)
      new CopyOnWriteArrayListIterator[E](innerSnapshot(), fromIndex + index,
          fromIndex, toIndex) {
        override protected def onSizeChanged(delta: Int): Unit = changeSize(delta)
      }
    }

    override def subList(fromIndex: Int, toIndex: Int): List[E] = {
      if (fromIndex < 0 || fromIndex > toIndex || toIndex > size())
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

    override protected def innerSnapshot(): innerImpl.Repr =
      self.innerSnapshot()

    protected def changeSize(delta: Int): Unit =
      toIndex += delta
  }

  protected def checkIndexInBounds(index: Int): Unit = {
    if (index < 0 || index >= size())
      throw new IndexOutOfBoundsException(index.toString)
  }

  protected def checkIndexOnBounds(index: Int): Unit = {
    if (index < 0 || index > size())
      throw new IndexOutOfBoundsException(index.toString)
  }

  @inline private def ensureCapacity(minCapacity: Int): Unit = {
    if (isWebAssembly) {
      if (innerImpl.capacity(inner) < minCapacity) {
        val newCapacity = roundUpToPowerOfTwo(minCapacity + 1) // Index 0 stores the length
        inner = innerImpl.resize(inner, minCapacity)
      }
    }
    // We ignore this in JS as js.Array doesn't support explicit pre-allocation
  }
}

object CopyOnWriteArrayList {
  private class CopyOnWriteArrayListIterator[E](arraySnapshot: innerImpl.Repr,
      i: Int, start: Int, end: Int) extends AbstractRandomAccessListIterator[E](i, start, end) {

    override def remove(): Unit =
      throw new UnsupportedOperationException

    override def set(e: E): Unit =
      throw new UnsupportedOperationException

    override def add(e: E): Unit =
      throw new UnsupportedOperationException

    protected def get(index: Int): E =
      innerImpl.get(arraySnapshot, index).asInstanceOf[E]

    protected def remove(index: Int): Unit =
      throw new UnsupportedOperationException

    protected def set(index: Int, e: E): Unit =
      throw new UnsupportedOperationException

    protected def add(index: Int, e: E): Unit =
      throw new UnsupportedOperationException
  }

  /* Get the best implementation of inner array for the given platform.
   *
   * Use Array[AnyRef] in WebAssembly to avoid JS-interop. In JS, use js.Array.
   * It is resizable by nature, so manual resizing is not needed.
   *
   * `linkTimeIf` is needed here to ensure the optimizer knows
   * there is only one implementation of `InnerArrayImpl`, and de-virtualize/inline
   * the function calls.
   */

  // package private so that `protected def innerSnapshot` can access this field.
  private[concurrent] val innerImpl: InnerArrayImpl =
    LinkingInfo.linkTimeIf[InnerArrayImpl](LinkingInfo.isWebAssembly) {
      InnerArrayImpl.JArrayImpl
    } {
      InnerArrayImpl.JSArrayImpl
    }

  private[concurrent] sealed abstract class InnerArrayImpl {
    type Repr <: AnyRef

    def make(initialCapacity: Int): Repr
    def length(v: Repr): Int
    def capacity(v: Repr): Int // Wasm only
    def resize(v: Repr, newCapacity: Int): Repr // Wasm only
    def get(v: Repr, index: Int): AnyRef
    def set(v: Repr, index: Int, e: AnyRef): Unit
    def push(v: Repr, e: AnyRef): Unit
    def add(v: Repr, index: Int, e: AnyRef): Unit
    def add(v: Repr, index: Int, items: Collection[_ <: AnyRef]): Unit
    def remove(v: Repr, index: Int): AnyRef
    def remove(v: Repr, index: Int, count: Int): Unit
    def clone(v: Repr): Repr
  }

  private object InnerArrayImpl {
    object JSArrayImpl extends InnerArrayImpl {
      import scala.scalajs.js

      type Repr = js.Array[AnyRef]

      @inline def make(_initialCapacity: Int): Repr = js.Array[AnyRef]()
      @inline def length(v: Repr): Int = v.length
      @inline def capacity(v: Repr): Int =
        throw new UnsupportedOperationException
      @inline def resize(v: Repr, newCapacity: Int): Repr =
        throw new UnsupportedOperationException
      @inline def get(v: Repr, index: Int): AnyRef = v(index)
      @inline def set(v: Repr, index: Int, e: AnyRef): Unit =
        v(index) = e
      @inline def push(v: Repr, e: AnyRef): Unit =
        v.push(e)
      @inline def add(v: Repr, index: Int, e: AnyRef): Unit =
        v.splice(index, 0, e)
      @inline def add(v: Repr, index: Int, items: Collection[_ <: AnyRef]): Unit = {
        val itemsArray = js.Array[AnyRef]()
        items.scalaOps.foreach(itemsArray.push(_))
        v.splice(index, 0, itemsArray.toSeq: _*)
      }
      @inline def remove(v: Repr, index: Int): AnyRef =
        arrayRemoveAndGet(v, index)
      @inline def remove(v: Repr, index: Int, count: Int): Unit =
        v.splice(index, count)
      @inline def clone(v: Repr): Repr =
        v.jsSlice(0)
    }

    object JArrayImpl extends InnerArrayImpl {
      type Repr = Array[AnyRef]

      @inline def make(initialCapacity: Int): Repr = {
        val v = new Array[AnyRef](roundUpToPowerOfTwo(initialCapacity + 1))
        v(0) = 0.asInstanceOf[AnyRef]
        v
      }
      @inline def length(v: Repr): Int = v(0).asInstanceOf[Int]
      @inline def get(v: Repr, index: Int): AnyRef = v(index + 1) // Index 0 stores the length
      @inline def set(v: Repr, index: Int, e: AnyRef): Unit =
        v(index + 1) = e.asInstanceOf[AnyRef]
      @inline def capacity(v: Repr): Int = v.length - 1 // Index 0 stores the length
      @inline def resize(v: Repr, newCapacity: Int): Repr =
        Arrays.copyOf(v, newCapacity)
      @inline def push(v: Repr, e: AnyRef): Unit = {
        val size = length(v)
        v(size + 1) = e.asInstanceOf[AnyRef]
        v(0) = (size + 1).asInstanceOf[AnyRef]
      }
      @inline def add(v: Repr, index: Int, e: AnyRef): Unit = {
        val innerIdx = index + 1
        val size = length(v)
        System.arraycopy(v, innerIdx, v, innerIdx + 1, size + 1 - innerIdx)
        v(innerIdx) = e
        v(0) = (size + 1).asInstanceOf[AnyRef]
      }
      @inline def add(v: Repr, index: Int, items: Collection[_ <: AnyRef]): Unit = {
        val innerIdx = index + 1
        val size = length(v)
        System.arraycopy(v, innerIdx, v, innerIdx + items.size(), size + 1 - innerIdx)
        System.arraycopy(items.toArray(), 0, v, innerIdx, items.size())
        v(0) = (size + items.size()).asInstanceOf[AnyRef]
      }
      @inline def remove(v: Repr, index: Int): AnyRef = {
        val innerIdx = index + 1
        val size = length(v)
        val removed = v(innerIdx)
        System.arraycopy(v, innerIdx + 1, v, innerIdx, size - innerIdx)
        v(size) = null.asInstanceOf[AnyRef] // free reference for GC
        v(0) = (size - 1).asInstanceOf[AnyRef]
        removed
      }
      @inline def remove(v: Repr, index: Int, count: Int): Unit = {
        val innerIdx = index + 1
        val size = length(v)
        val toIndex = innerIdx + count
        System.arraycopy(v, toIndex, v, innerIdx, size + 1 - toIndex)
        val newSize = size - count
        Arrays.fill(v, newSize + 1, newSize + 1 + count, null) // free references for GC
        v(0) = newSize.asInstanceOf[AnyRef]
      }
      @inline def clone(v: Repr): Repr =
        v.clone()
    }
  }
}
