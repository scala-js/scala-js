/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import java.{util => ju}

import scala.reflect.ClassTag

trait CollectionsOnSynchronizedMapTest extends CollectionsOnMapsTest {

  def originalFactory: MapFactory

  def factory: MapFactory = {
    new MapFactory {
      override def implementationName: String =
        s"synchronizedMap(${originalFactory.implementationName})"

      def empty[K: ClassTag, V: ClassTag]: ju.Map[K, V] =
        ju.Collections.synchronizedMap(originalFactory.empty[K, V])

      override def allowsNullKeys: Boolean =
        originalFactory.allowsNullKeys

      override def allowsNullValues: Boolean =
        originalFactory.allowsNullValues
    }
  }
}

trait CollectionsOnSynchronizedSortedMapTest extends CollectionsOnSortedMapsTest {

  def originalFactory: SortedMapFactory

  def factory: SortedMapFactory = {
    new SortedMapFactory {
      override def implementationName: String =
        s"synchronizedSortedMap(${originalFactory.implementationName})"

      def empty[K: ClassTag, V: ClassTag]: ju.SortedMap[K, V] =
        ju.Collections.synchronizedSortedMap(originalFactory.empty[K, V])

      override def allowsNullKeys: Boolean =
        originalFactory.allowsNullKeys

      override def allowsNullValues: Boolean =
        originalFactory.allowsNullValues
    }
  }
}

class CollectionsOnSynchronizedMapOnHashMapTest
    extends CollectionsOnSynchronizedMapTest {
  def originalFactory: MapFactory = new HashMapFactory
}

class CollectionsOnSynchronizedMapOnLinkedHashMapInsertionOrderTest
    extends CollectionsOnSynchronizedMapTest {
  def originalFactory: MapFactory = new LinkedHashMapFactory(false, None)
}

class CollectionsOnSynchronizedMapOnLinkedHashMapInsertionOrderWithLimitTest
    extends CollectionsOnSynchronizedMapTest {
  def originalFactory: MapFactory = new LinkedHashMapFactory(false, Some(50))
}

class CollectionsOnSynchronizedMapOnLinkedHashMapAccessOrderTest
    extends CollectionsOnSynchronizedMapTest {
  def originalFactory: MapFactory = new LinkedHashMapFactory(true, None)
}

class CollectionsOnOnSynchronizedMapOnLinkedHashMapAccessOrderWithLimitTest
    extends CollectionsOnSynchronizedMapTest {
  def originalFactory: MapFactory = new LinkedHashMapFactory(true, Some(50))
}
