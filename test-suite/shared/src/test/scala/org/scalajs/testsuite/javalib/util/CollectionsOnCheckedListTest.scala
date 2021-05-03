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
import org.junit.Assume._
import org.junit.Test

import org.scalajs.testsuite.javalib.util.concurrent.CopyOnWriteArrayListFactory
import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform._

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
    }
  }

  @Test def testCheckedList(): Unit = {
    superList().add(0, new C)
    assertTrue(superList().addAll(0, TrivialImmutableCollection(new C)))
    testOnFirstPositionOfIterator[ju.ListIterator[A]](superList().listIterator _,
        _.add(new C), None)
    testOnFirstPositionOfIterator[ju.ListIterator[A]](superList().listIterator _,
        _.set(new C), None)
  }

  @Test def testCheckedListBadInputs(): Unit = {
    assumeTrue("Assumed compliant asInstanceOf", hasCompliantAsInstanceOfs)

    assertThrows(classOf[ClassCastException], superList().add(0, new A))
    assertThrows(classOf[ClassCastException],
        superList().addAll(0, TrivialImmutableCollection(new A)))
    testOnFirstPositionOfIterator[ju.ListIterator[A]](
        superList().listIterator _,
        _.add(new A), Some(classOf[ClassCastException]))
    testOnFirstPositionOfIterator[ju.ListIterator[A]](
        superList().listIterator _,
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
