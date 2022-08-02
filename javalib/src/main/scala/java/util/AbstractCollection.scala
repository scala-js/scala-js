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

package java.util

import scala.annotation.tailrec

import ScalaOps._

import java.lang.{reflect => jlr}

abstract class AbstractCollection[E] protected () extends Collection[E] {
  def iterator(): Iterator[E]
  def size(): Int

  def isEmpty(): Boolean = size() == 0

  def contains(o: Any): Boolean =
    this.scalaOps.exists(Objects.equals(o, _))

  def toArray(): Array[AnyRef] =
    toArray(new Array[AnyRef](size()))

  def toArray[T <: AnyRef](a: Array[T]): Array[T] = {
    val toFill: Array[T] =
      if (a.length >= size()) a
      else jlr.Array.newInstance(a.getClass().getComponentType(), size()).asInstanceOf[Array[T]]

    val iter = iterator()
    for (i <- 0 until size())
      toFill(i) = iter.next().asInstanceOf[T]
    if (toFill.length > size())
      toFill(size()) = null.asInstanceOf[T]
    toFill
  }

  def add(e: E): Boolean =
    throw new UnsupportedOperationException()

  def remove(o: Any): Boolean = {
    @tailrec
    def findAndRemove(iter: Iterator[E]): Boolean = {
      if (iter.hasNext()) {
        if (Objects.equals(iter.next(), o)) {
          iter.remove()
          true
        } else {
          findAndRemove(iter)
        }
      } else {
        false
      }
    }
    findAndRemove(iterator())
  }

  def containsAll(c: Collection[_]): Boolean =
    c.scalaOps.forall(this.contains(_))

  def addAll(c: Collection[_ <: E]): Boolean =
    c.scalaOps.foldLeft(false)((prev, elem) => add(elem) || prev)

  def removeAll(c: Collection[_]): Boolean =
    removeWhere(c.contains(_))

  def retainAll(c: Collection[_]): Boolean =
    removeWhere(!c.contains(_))

  def clear(): Unit =
    removeWhere(_ => true)

  private def removeWhere(p: Any => Boolean): Boolean = {
    val iter = iterator()
    var changed = false
    while (iter.hasNext()) {
      if (p(iter.next())) {
        iter.remove()
        changed = true
      }
    }
    changed
  }

  override def toString(): String =
    this.scalaOps.mkString("[", ", ", "]")
}
