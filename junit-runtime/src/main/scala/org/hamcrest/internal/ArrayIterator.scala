/*
 * Ported from https://github.com/hamcrest/JavaHamcrest/
 */
package org.hamcrest.internal

import java.{util => ju}

class ArrayIterator private (array: Array[_], private var currentIndex: Int = 0)
    extends ju.Iterator[AnyRef] {

  def this(array: AnyRef) = {
    this(
      array match {
        case arr: Array[_] => arr
        case _ => throw new IllegalArgumentException("not an array")
      },
      0
    )
  }

  override def hasNext: Boolean =
    currentIndex < array.length

  override def next(): AnyRef = {
    val _currentIndex = currentIndex
    currentIndex = _currentIndex + 1
    array(_currentIndex).asInstanceOf[AnyRef]
  }

  override def remove(): Unit =
    throw new UnsupportedOperationException("cannot remove items from an array")
}
