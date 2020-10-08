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

/** A trivial, "obviously correct" implementation of an immutable
 *  `java.util.Collection[A]`.
 *
 *  It can be used as an argument to test other collections, notably for their
 *  bulk operations such as `addAll()`, `removeAll()`, etc.
 */
final class TrivialImmutableCollection[A] private (contents: Array[AnyRef])
    extends ju.Collection[A] {

  def size(): Int = contents.length

  def isEmpty(): Boolean = size() == 0

  def contains(o: Any): Boolean = {
    // scalastyle:off return
    var i = 0
    while (i != contents.length) {
      if (ju.Objects.equals(o, contents(i)))
        return true
      i += 1
    }
    false
    // scalastyle:on return
  }

  def iterator(): ju.Iterator[A] = {
    new ju.Iterator[A] {
      private var nextIndex: Int = 0

      def hasNext(): Boolean = nextIndex != contents.length

      def next(): A = {
        if (!hasNext())
          throw new ju.NoSuchElementException()
        val result = contents(nextIndex).asInstanceOf[A]
        nextIndex += 1
        result
      }
    }
  }

  def toArray(): Array[AnyRef] =
    contents.clone()

  def toArray[T](a: Array[T with AnyRef]): Array[T with AnyRef] =
    ju.Arrays.copyOf[T, AnyRef](contents, contents.length, a.getClass())

  def add(e: A): Boolean =
    throw new UnsupportedOperationException("TrivialImmutableCollection.add()")

  def remove(o: Any): Boolean =
    throw new UnsupportedOperationException("TrivialImmutableCollection.remove()")

  def containsAll(c: ju.Collection[_]): Boolean = {
    // scalastyle:off return
    val iter = c.iterator()
    while (iter.hasNext()) {
      if (!contains(iter.next()))
        return false
    }
    true
    // scalastyle:on return
  }

  def addAll(c: ju.Collection[_ <: A]): Boolean =
    throw new UnsupportedOperationException("TrivialImmutableCollection.addAll()")

  def removeAll(c: ju.Collection[_]): Boolean =
    throw new UnsupportedOperationException("TrivialImmutableCollection.removeAll()")

  def retainAll(c: ju.Collection[_]): Boolean =
    throw new UnsupportedOperationException("TrivialImmutableCollection.retainAll()")

  def clear(): Unit =
    throw new UnsupportedOperationException("TrivialImmutableCollection.clear()")

  /** Returns the `i`th element of this collection.
   *
   *  This method is not part of the API of `java.util.Collection`. It is made
   *  publicly available to users of `TrivialImmutableCollection` as a
   *  convenience for tests.
   */
  def apply(i: Int): A = contents(i).asInstanceOf[A]
}

object TrivialImmutableCollection {
  def apply[A](elems: A*): TrivialImmutableCollection[A] =
    new TrivialImmutableCollection(elems.asInstanceOf[Seq[AnyRef]].toArray)
}
