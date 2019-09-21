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

import java.{lang => jl, util => ju}

import scala.reflect.ClassTag

trait CollectionsOnSetFromMapTest extends SetTest {

  def mapFactory: MapFactory

  def factory: SetFactory = {
    new SetFactory {
      def implementationName: String =
        s"newSetFromMap(${mapFactory.implementationName})"

      def empty[E: ClassTag]: ju.Set[E] =
        ju.Collections.newSetFromMap[E](mapFactory.empty[E, jl.Boolean])

      override def allowsNullElement: Boolean =
        mapFactory.allowsNullKeys
    }
  }
}

class CollectionsOnSetFromMapOnHashMapTest extends CollectionsOnSetFromMapTest {
  def mapFactory: MapFactory = new HashMapFactory
}

class CollectionsOnSetFromMapOnLinkedHashMapInsertionOrderTest extends CollectionsOnSetFromMapTest {
  def mapFactory: MapFactory = new LinkedHashMapFactory(false, None)
}

class CollectionsOnSetFromMapOnLinkedHashMapInsertionOrderWithLimitTest extends CollectionsOnSetFromMapTest {
  def mapFactory: MapFactory = new LinkedHashMapFactory(false, Some(50))
}

class CollectionsOnSetFromMapOnLinkedHashMapAccessOrderTest extends CollectionsOnSetFromMapTest {
  def mapFactory: MapFactory = new LinkedHashMapFactory(true, None)
}

class CollectionsOnSetFromMapOnLinkedHashMapAccessOrderWithLimitTest extends CollectionsOnSetFromMapTest {
  def mapFactory: MapFactory = new LinkedHashMapFactory(true, Some(50))
}
