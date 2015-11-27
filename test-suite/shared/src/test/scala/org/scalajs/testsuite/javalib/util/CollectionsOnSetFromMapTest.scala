package org.scalajs.testsuite.javalib.util

import java.{lang => jl, util => ju}

trait CollectionsOnSetFromMapTest extends SetTest {

  def mapFactory: MapFactory

  def factory: SetFactory = {
    new SetFactory {
      def implementationName: String =
        s"newSetFromMap(${mapFactory.implementationName})"

      def empty[E]: ju.Set[E] =
        ju.Collections.newSetFromMap[E](mapFactory.empty[E, jl.Boolean])

      def allowsNullElement: Boolean =
        mapFactory.allowsNullKeys
    }
  }
}

class CollectionsOnSetFromMapOnHashMapTest extends CollectionsOnSetFromMapTest {
  def mapFactory: MapFactory = new HashMapFactory
}

class CollectionsOnSetFromMapOnLinkedHashMapInsertionOrderTest extends CollectionsOnSetFromMapTest {
  def mapFactory: MapFactory = new LinkedHashMapFactory(false, false)
}

class CollectionsOnSetFromMapOnLinkedHashMapInsertionOrderWithLimitTest extends CollectionsOnSetFromMapTest {
  def mapFactory: MapFactory = new LinkedHashMapFactory(false, true)
}

class CollectionsOnSetFromMapOnLinkedHashMapAccessOrderTest extends CollectionsOnSetFromMapTest {
  def mapFactory: MapFactory = new LinkedHashMapFactory(true, false)
}

class CollectionsOnSetFromMapOnLinkedHashMapAccessOrderWithLimitTest extends CollectionsOnSetFromMapTest {
  def mapFactory: MapFactory = new LinkedHashMapFactory(true, true)
}
