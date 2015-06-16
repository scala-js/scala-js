package java.util

import scala.scalajs._

class ArrayList[E] private (private[ArrayList] val inner: js.Array[E])
    extends AbstractList[E] with RandomAccess with Cloneable with Serializable {
  self =>

  def this(initialCapacity: Int) = {
    this(new js.Array[E])
    if (initialCapacity < 0)
      throw new IllegalArgumentException
  }

  def this() =
    this(new js.Array[E])

  def this(c: Collection[_ <: E]) = {
    this()
    addAll(c)
  }

  def trimToSize(): Unit = {
    // We ignore this as js.Array doesn't support explicit pre-allocation
  }

  def ensureCapacity(minCapacity: Int): Unit = {
    // We ignore this as js.Array doesn't support explicit pre-allocation
  }

  def size(): Int =
    inner.length

  override def clone(): AnyRef =
    new ArrayList(inner.jsSlice(0))

  def get(index: Int): E = {
    checkIndexInBounds(index)
    inner(index)
  }

  override def set(index: Int, element: E): E = {
    val e = get(index)
    inner(index) = element
    e
  }

  override def add(e: E): Boolean = {
    inner += e
    true
  }

  override def add(index: Int, element: E): Unit = {
    checkIndexOnBounds(index)
    inner.insert(index, element)
  }

  override def remove(index: Int): E = {
    checkIndexInBounds(index)
    inner.remove(index)
  }

  override def clear(): Unit =
    inner.clear()

  override def addAll(index: Int, c: Collection[_ <: E]): Boolean = {
    c match {
      case other: ArrayList[_] =>
        inner.splice(index, 0, other.inner: _*)
        other.size > 0
      case _ => super.addAll(index, c)
    }
  }

  override protected def removeRange(fromIndex: Int, toIndex: Int): Unit =
    inner.splice(fromIndex, toIndex - fromIndex)

}
