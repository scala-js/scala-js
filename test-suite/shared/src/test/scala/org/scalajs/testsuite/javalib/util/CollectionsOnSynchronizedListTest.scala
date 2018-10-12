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

import org.scalajs.testsuite.javalib.util.concurrent.CopyOnWriteArrayListFactory

import scala.reflect.ClassTag

trait CollectionsSynchronizedListTest
    extends CollectionsOnListTest {

  def originalFactory: ListFactory

  def factory: ListFactory = {
    new ListFactory {
      override def implementationName: String =
        s"synchronizedList(${originalFactory.implementationName})"

      override def empty[E: ClassTag]: ju.List[E] =
        ju.Collections.synchronizedList(originalFactory.empty[E])

      override def allowsMutationThroughIterator: Boolean =
        originalFactory.allowsMutationThroughIterator

      override def sortableUsingCollections: Boolean =
        originalFactory.sortableUsingCollections
    }
  }
}

class CollectionsOnSynchronizedListAbstractListTest
    extends CollectionsSynchronizedCollectionTest {
  def originalFactory: ListFactory = new AbstractListFactory
}

class CollectionsOnSynchronizedListArrayListTest extends CollectionsSynchronizedListTest {
  def originalFactory: ListFactory = new ArrayListFactory
}

class CollectionsOnSynchronizedListLinkedListTest
    extends CollectionsSynchronizedListTest {
  def originalFactory: ListFactory = new LinkedListFactory
}

class CollectionsOnSynchronizedListCopyOnWriteArrayListTest
    extends CollectionsSynchronizedListTest {
  def originalFactory: ListFactory = new CopyOnWriteArrayListFactory
}
