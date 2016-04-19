/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import java.{util => ju}

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

import org.scalajs.testsuite.javalib.util.concurrent.CopyOnWriteArrayListFactory
import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform._

import scala.collection.JavaConversions._

import scala.reflect.ClassTag

trait CollectionsCheckedListTest
    extends CollectionsOnListTest {

  def originalFactory: ListFactory

  def factory: ListFactory = {
    new ListFactory {
      override def implementationName: String =
        s"checkedList(${originalFactory.implementationName})"

      override def empty[E](implicit ct: ClassTag[E]): ju.List[E] = {
        ju.Collections.checkedList(originalFactory.empty[E],
            ct.runtimeClass.asInstanceOf[Class[E]])
      }

      override def allowsMutationThroughIterator: Boolean =
        originalFactory.allowsMutationThroughIterator

      override def sortableUsingCollections: Boolean =
        originalFactory.sortableUsingCollections
    }
  }

  @Test def testCheckedList(): Unit = {
    superList().add(0, new C)
    assertTrue(superList().addAll(0, Seq(new C)))
    testOnFirstPositionOfIterator[ju.ListIterator[A]](superList().listIterator,
        _.add(new C), None)
    testOnFirstPositionOfIterator[ju.ListIterator[A]](superList().listIterator,
        _.set(new C), None)
  }

  @Test def testCheckedListBadInputs(): Unit = {
    assumeTrue("Assumed compliant asInstanceOf", hasCompliantAsInstanceOfs)

    expectThrows(classOf[ClassCastException], superList().add(0, new A))
    expectThrows(classOf[ClassCastException],
        superList().addAll(0, Seq(new A)))
    testOnFirstPositionOfIterator[ju.ListIterator[A]](
        superList().listIterator,
        _.add(new A), Some(classOf[ClassCastException]))
    testOnFirstPositionOfIterator[ju.ListIterator[A]](
        superList().listIterator,
        _.set(new A), Some(classOf[ClassCastException]))
  }

  private def superList(): ju.List[A] =
    factory.empty[B].asInstanceOf[ju.List[A]]
}

class CollectionsOnCheckedListAbstractListTest
    extends CollectionsCheckedCollectionTest {
  def originalFactory: ListFactory = new AbstractListFactory
}

class CollectionsOnCheckedListArrayListTest extends CollectionsCheckedListTest {
  def originalFactory: ListFactory = new ArrayListFactory
}

class CollectionsOnCheckedListLinkedListTest
    extends CollectionsCheckedListTest {
  def originalFactory: ListFactory = new LinkedListFactory
}

class CollectionsOnCheckedListCopyOnWriteArrayListTest
    extends CollectionsCheckedListTest {
  def originalFactory: ListFactory = new CopyOnWriteArrayListFactory
}
