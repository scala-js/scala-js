/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import java.{util => ju}

import org.scalajs.testsuite.javalib.util.concurrent.CopyOnWriteArrayListFactory

import scala.reflect.ClassTag

trait CollectionsSynchronizedCollectionTest
    extends CollectionsOnCollectionsTest {

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

