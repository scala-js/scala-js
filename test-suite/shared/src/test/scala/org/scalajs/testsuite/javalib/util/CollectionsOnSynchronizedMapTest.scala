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
