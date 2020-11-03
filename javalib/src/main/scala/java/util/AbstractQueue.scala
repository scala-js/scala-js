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

abstract class AbstractQueue[E] protected () extends AbstractCollection[E] with Queue[E] {

  override def add(e: E): Boolean =
    if (offer(e)) true
    else throw new IllegalStateException()

  def remove(): E =
    if (!isEmpty()) poll()
    else throw new NoSuchElementException()

  def element(): E =
    if (!isEmpty()) peek()
    else throw new NoSuchElementException()

  override def clear(): Unit = {
    while (poll() != null) {}
  }

  override def addAll(c: Collection[_ <: E]): Boolean = {
    val iter = c.iterator
    var changed = false
    while (iter.hasNext())
      changed = add(iter.next()) || changed
    changed
  }
}
