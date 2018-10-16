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

import scala.collection.JavaConverters._

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
    assertTrue(superColl().addAll(Seq(new C).asJava))
  }

  @Test def testCheckedCollectionBadInputs(): Unit = {
    assumeTrue("Assumed compliant asInstanceOf", hasCompliantAsInstanceOfs)

    expectThrows(classOf[ClassCastException], superColl().add(new A))
    expectThrows(classOf[ClassCastException],
        superColl().addAll(Seq(new A).asJava))
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
