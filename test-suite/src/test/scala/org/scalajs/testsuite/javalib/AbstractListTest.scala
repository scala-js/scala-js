/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib

import java.{util => ju}

object AbstractListTest extends AbstractListTest(new AbstractListFactory)

abstract class AbstractListTest[F <: AbstractListFactory](listFactory: F)
    extends AbstractCollectionTest(listFactory) with ListTest {

  override def testApi(): Unit = {
    super.testApi()
    testListApi(listFactory)
  }
}

class AbstractListFactory extends AbstractCollectionFactory with ListFactory {

  override def implementationName: String =
    "java.util.AbstractList"

  override def empty[E]: ju.AbstractList[E] = {
    // inefficient but simple for debugging implementation of AbstractList
    new ju.AbstractList[E] {

      private var inner = scala.collection.immutable.List.empty[E]

      override def get(index: Int): E = {
        checkIndexInBounds(index)
        inner(index)
      }

      override def size(): Int =
        inner.size

      override def add(index: Int, element: E): Unit = {
        checkIndexOnBounds(index)
        val (left, right) = inner.splitAt(index)
        inner = left ::: element :: right
      }

      override def set(index: Int, element: E): E = {
        checkIndexInBounds(index)
        val (left, right) = inner.splitAt(index)
        inner = left ::: element :: right.tail
        right.head
      }

      override def remove(index: Int): E = {
        checkIndexInBounds(index)
        val (left, right) = inner.splitAt(index)
        inner = left ::: right.tail
        right.head
      }

      override def clear(): Unit =
        inner = Nil

      private def checkIndexInBounds(index: Int): Unit = {
        if (index < 0 || index >= size)
          throw new IndexOutOfBoundsException(index.toString)
      }

      private def checkIndexOnBounds(index: Int): Unit = {
        if (index < 0 || index > size)
          throw new IndexOutOfBoundsException(index.toString)
      }
    }
  }

}
