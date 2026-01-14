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

trait CollectionsSynchronizedCollectionTest extends CollectionsOnCollectionsTest {

  def originalFactory: CollectionFactory

  def factory: CollectionFactory = {
    new CollectionFactory {
      override def implementationName: String =
        s"synchronizedCollection(${originalFactory.implementationName})"

      override def empty[E: ClassTag]: ju.Collection[E] =
        ju.Collections.synchronizedCollection(originalFactory.empty[E])
    }
  }
}

class CollectionsOnSynchronizedCollectionAbstractListTest
    extends CollectionsSynchronizedCollectionTest {
  def originalFactory: CollectionFactory = new AbstractListFactory
}

class CollectionsOnSynchronizedCollectionArrayListTest extends CollectionsSynchronizedCollectionTest {
  def originalFactory: CollectionFactory = new ArrayListFactory
}

class CollectionsOnSynchronizedCollectionLinkedListTest
    extends CollectionsSynchronizedCollectionTest {
  def originalFactory: CollectionFactory = new LinkedListFactory
}

class CollectionsOnSynchronizedCollectionCopyOnWriteArrayListTest
    extends CollectionsSynchronizedCollectionTest {
  def originalFactory: CollectionFactory = new CopyOnWriteArrayListFactory
}

class CollectionsOnSynchronizedCollectionHashSetFactoryTest
    extends CollectionsSynchronizedCollectionTest {
  def originalFactory: CollectionFactory = new HashSetFactory
}

class CollectionsOnSynchronizedCollectionLinkedHashSetTest
    extends CollectionsSynchronizedCollectionTest {
  def originalFactory: CollectionFactory = new LinkedHashSetFactory
}

class CollectionsOnSynchronizedCollectionConcurrentSkipListSetTest
    extends CollectionsSynchronizedCollectionTest {
  def originalFactory: CollectionFactory =
    new concurrent.ConcurrentSkipListSetFactory
}

class CollectionsOnSynchronizedCollectionArrayDequeTest
    extends CollectionsSynchronizedCollectionTest {
  def originalFactory: CollectionFactory =
    new ArrayDequeFactory
}
