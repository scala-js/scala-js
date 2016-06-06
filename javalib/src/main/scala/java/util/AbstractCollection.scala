package java.util

import scala.annotation.tailrec

import scala.collection.JavaConverters._

import java.lang.{reflect => jlr}

abstract class AbstractCollection[E] protected () extends Collection[E] {
  def iterator(): Iterator[E]
  def size(): Int

  def isEmpty(): Boolean = size == 0

  def contains(o: Any): Boolean =
    iterator.asScala.exists(o === _)

  def toArray(): Array[AnyRef] =
    toArray(new Array[AnyRef](size))

  def toArray[T <: AnyRef](a: Array[T]): Array[T] = {
    val toFill: Array[T] =
      if (a.size >= size) a
      else jlr.Array.newInstance(a.getClass.getComponentType, size).asInstanceOf[Array[T]]

    val iter = iterator
    for (i <- 0 until size)
      toFill(i) = iter.next().asInstanceOf[T]
    if (toFill.size > size)
      toFill(size) = null.asInstanceOf[T]
    toFill
  }

  def add(e: E): Boolean =
    throw new UnsupportedOperationException()

  def remove(o: Any): Boolean = {
    @tailrec
    def findAndRemove(iter: Iterator[E]): Boolean = {
      if (iter.hasNext) {
        if (iter.next() === o) {
          iter.remove()
          true
        } else
          findAndRemove(iter)
      } else
        false
    }
    findAndRemove(iterator())
  }

  def containsAll(c: Collection[_]): Boolean =
    c.iterator.asScala.forall(this.contains(_))

  def addAll(c: Collection[_ <: E]): Boolean =
    c.asScala.foldLeft(false)((prev, elem) => add(elem) || prev)

  def removeAll(c: Collection[_]): Boolean =
    removeWhere(c.contains(_))

  def retainAll(c: Collection[_]): Boolean =
    removeWhere(!c.contains(_))

  def clear(): Unit =
    removeWhere(_ => true)

  private def removeWhere(p: Any => Boolean): Boolean = {
    val iter = iterator()
    var changed = false
    while (iter.hasNext) {
      if (p(iter.next())) {
        iter.remove()
        changed = true
      }
    }
    changed
  }

  override def toString(): String =
    iterator.asScala.mkString("[", ",", "]")
}
