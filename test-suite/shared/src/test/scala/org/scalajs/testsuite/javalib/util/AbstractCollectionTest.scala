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

package org.scalajs.testsuite.javalib.util

import java.{util => ju}

import scala.reflect.ClassTag

class AbstractCollectionTest extends CollectionTest {
  def factory: AbstractCollectionFactory = new AbstractCollectionFactory
}

class AbstractCollectionFactory extends CollectionFactory {
  import AbstractCollectionFactory._

  override def implementationName: String =
    "java.util.AbstractCollection"

  override def empty[E: ClassTag]: ju.AbstractCollection[E] =
    new AbstractCollectionImpl[E]

}

object AbstractCollectionFactory {
  /* A mutable implementation of `java.util.Collection[E]` that relies on all
   * the default behaviors implemented in `j.u.AbstractCollection`.
   *
   * Every modification allocates a new internal `Array`. This property is used
   * to reliably detect concurrent modifications.
   */
  private final class AbstractCollectionImpl[E] extends ju.AbstractCollection[E] {
    private var inner = new Array[AnyRef](0)

    override def add(elem: E): Boolean = {
      inner = ju.Arrays.copyOf(inner, inner.length + 1)
      inner(inner.length - 1) = elem.asInstanceOf[AnyRef]
      true
    }

    def size(): Int =
      inner.length

    def iterator(): ju.Iterator[E] =
      new AbstractCollectionImplIterator(inner)

    private final class AbstractCollectionImplIterator[E](private var iterInner: Array[AnyRef])
        extends ju.Iterator[E] {

      private[this] var nextIndex: Int = 0
      private[this] var canRemove: Boolean = false

      def hasNext(): Boolean = {
        checkConcurrentModification()
        nextIndex != inner.length
      }

      def next(): E = {
        checkConcurrentModification()
        if (nextIndex == inner.length)
          throw new ju.NoSuchElementException()

        val elem = inner(nextIndex).asInstanceOf[E]
        nextIndex += 1
        canRemove = true
        elem
      }

      override def remove(): Unit = {
        checkConcurrentModification()
        if (!canRemove)
          throw new IllegalStateException("remove() called before next()")

        nextIndex -= 1
        val newInner = ju.Arrays.copyOf(iterInner, iterInner.length - 1)
        var i = nextIndex
        while (i != newInner.length) {
          newInner(i) = iterInner(i + 1)
          i += 1
        }
        inner = newInner
        iterInner = newInner
        canRemove = false
      }

      private def checkConcurrentModification(): Unit = {
        if (inner ne iterInner)
          throw new ju.ConcurrentModificationException()
      }
    }
  }
}
