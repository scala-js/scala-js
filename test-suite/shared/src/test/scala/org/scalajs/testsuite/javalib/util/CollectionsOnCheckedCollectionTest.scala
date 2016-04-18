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

import scala.collection.JavaConversions._

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform._

import scala.reflect.ClassTag

trait CollectionsCheckedCollectionTest
    extends CollectionsOnCollectionsTest {

  def originalFactory: CollectionFactory

  def factory: CollectionFactory = {
    new CollectionFactory {
      override def implementationName: String =
        s"checkedCollection(${originalFactory.implementationName})"

      override def empty[E](implicit ct: ClassTag[E]): ju.Collection[E] = {
        ju.Collections.checkedCollection(originalFactory.empty[E],
          ct.runtimeClass.asInstanceOf[Class[E]])
      }
    }
  }

  @Test def testCheckedCollection(): Unit = {
    assertTrue(superColl().add(new C))
    assertTrue(superColl().addAll(Seq(new C)))
  }

  @Test def testCheckedCollectionBadInputs(): Unit = {
    assumeTrue("Assumed compliant asInstanceOf", hasCompliantAsInstanceOfs)

    expectThrows(classOf[ClassCastException], superColl().add(new A))
    expectThrows(classOf[ClassCastException],
        superColl().addAll(Seq(new A)))
  }

  protected def superColl(): ju.Collection[A] =
    factory.empty[B].asInstanceOf[ju.Collection[A]]
}

class CollectionsOnCheckedCollectionAbstractListTest
    extends CollectionsCheckedCollectionTest {
  def originalFactory: CollectionFactory = new AbstractListFactory
}

class CollectionsOnCheckedCollectionArrayListTest extends CollectionsCheckedCollectionTest {
  def originalFactory: CollectionFactory = new ArrayListFactory
}

class CollectionsOnCheckedCollectionLinkedListTest
    extends CollectionsCheckedCollectionTest {
  def originalFactory: CollectionFactory = new LinkedListFactory
}

class CollectionsOnCheckedCollectionCopyOnWriteArrayListTest
    extends CollectionsCheckedCollectionTest {
  def originalFactory: CollectionFactory = new CopyOnWriteArrayListFactory
}

class CollectionsOnCheckedCollectionHashSetFactoryTest
    extends CollectionsCheckedCollectionTest {
  def originalFactory: CollectionFactory = new HashSetFactory
}

class CollectionsOnCheckedCollectionLinkedHashSetTest
    extends CollectionsCheckedCollectionTest {
  def originalFactory: CollectionFactory = new LinkedHashSetFactory
}

class CollectionsOnCheckedCollectionConcurrentSkipListSetTest
    extends CollectionsCheckedCollectionTest {
  def originalFactory: CollectionFactory =
    new concurrent.ConcurrentSkipListSetFactory
}

class CollectionsOnCheckedCollectionArrayDequeTest
    extends CollectionsCheckedCollectionTest {
  def originalFactory: CollectionFactory =
    new ArrayDequeFactory
}
