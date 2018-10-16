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

import org.junit.Assert._
import org.junit.Test
import org.scalajs.testsuite.utils.AssertThrows._

import scala.reflect.ClassTag

class CollectionsTestOnJDK7 {

  @Test def should_implement_emptyIterator(): Unit = {
    def freshIter: ju.Iterator[Int] = ju.Collections.emptyIterator[Int]

    assertFalse(freshIter.hasNext)
    expectThrows(classOf[NoSuchElementException], freshIter.next())
    expectThrows(classOf[IllegalStateException], freshIter.remove())
  }

  @Test def should_implement_emptyListIterator(): Unit = {
    def test[E: ClassTag](toElem: Int => E): Unit = {
      def freshIter: ju.ListIterator[E] = ju.Collections.emptyListIterator[E]

      assertFalse(freshIter.hasNext)
      assertFalse(freshIter.hasPrevious)
      expectThrows(classOf[NoSuchElementException], freshIter.next())
      expectThrows(classOf[NoSuchElementException], freshIter.previous())
      expectThrows(classOf[IllegalStateException], freshIter.remove())
      expectThrows(classOf[UnsupportedOperationException],
          freshIter.add(toElem(0)))
      expectThrows(classOf[IllegalStateException], freshIter.set(toElem(0)))
    }

    test[Int](_.toInt)
    test[Long](_.toLong)
    test[Double](_.toDouble)
  }

  @Test def should_implement_emptyEnumeration(): Unit = {
    def freshEnum: ju.Enumeration[Int] = ju.Collections.emptyEnumeration[Int]

    assertFalse(freshEnum.hasMoreElements)
    expectThrows(classOf[NoSuchElementException], freshEnum.nextElement())
  }
}
