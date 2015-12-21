/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import java.{util => ju}

trait CollectionsOnSynchronizedSetTest extends CollectionsOnSetsTest {

  def originalFactory: SetFactory

  def factory: SetFactory = {
    new SetFactory {
      override def implementationName: String =
        s"synchronizedSet(${originalFactory.implementationName})"

      override def empty[E]: ju.Set[E] =
        ju.Collections.synchronizedSet(originalFactory.empty[E])

      def allowsNullElement: Boolean =
        originalFactory.allowsNullElement
    }
  }
}

trait CollectionsOnSynchronizedSortedSetTest extends CollectionsOnSortedSetsTest {

  def originalFactory: SortedSetFactory

  def factory: SortedSetFactory = {
    new SortedSetFactory {
      override def implementationName: String =
        s"synchronizedSortedSet(${originalFactory.implementationName})"

      override def empty[E]: ju.SortedSet[E] =
        ju.Collections.synchronizedSortedSet(originalFactory.empty[E])

      def allowsNullElement: Boolean =
        originalFactory.allowsNullElement
    }
  }
}

class CollectionsOnSynchronizedSetHashSetFactoryTest
    extends CollectionsOnSynchronizedSetTest {
  def originalFactory: SetFactory = new HashSetFactory
}

class CollectionsOnSynchronizedSetCollectionLinkedHashSetFactoryTest
    extends CollectionsOnSynchronizedSetTest {
  def originalFactory: SetFactory = new LinkedHashSetFactory
}

class CollectionsOnSynchronizedSetCollectionConcurrentSkipListSetFactoryTest
    extends CollectionsOnSynchronizedSetTest {
  def originalFactory: SetFactory = new concurrent.ConcurrentSkipListSetFactory
}
