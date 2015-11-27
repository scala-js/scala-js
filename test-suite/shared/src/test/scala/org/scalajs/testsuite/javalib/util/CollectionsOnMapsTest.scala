package org.scalajs.testsuite.javalib.util

import java.{lang => jl, util => ju}

import org.junit.Test
import org.scalajs.testsuite.utils.CollectionsTestBase

trait CollectionsOnMapsTest extends CollectionsTestBase {

  def factory: MapFactory

  @Test def unmodifiableMap(): Unit = {
    def test[K, V](toKey: Int => K, toValue: Int => V): Unit = {
      val map = factory.empty[K, V]
      testMapImmutability(ju.Collections.unmodifiableMap[K, V](map), toKey(0),
          toValue(0))
      for (i <- range)
        map.put(toKey(i), toValue(i))
      testMapImmutability(ju.Collections.unmodifiableMap[K, V](map), toKey(0),
          toValue(0))
    }

    test[Int, Int](_.toInt, _.toInt)
    test[Long, String](_.toLong, _.toString)
    test[String, String](_.toString, _.toString)
    test[Double, Double](_.toDouble, _.toDouble)
  }
}

trait CollectionsOnSortedMapsTest extends CollectionsOnMapsTest {
  def factory: SortedMapFactory

  @Test def unmodifiableSortedMap(): Unit = {
    def test[K, V](toKey: Int => K, toValue: Int => V): Unit = {
      val sortedMap = factory.empty[K, V]
      testMapImmutability(ju.Collections.unmodifiableSortedMap[K, V](sortedMap),
        toKey(0), toValue(0))
      for (i <- range)
        sortedMap.put(toKey(i), toValue(i))
      testMapImmutability(ju.Collections.unmodifiableSortedMap[K, V](sortedMap),
        toKey(0), toValue(0))
    }

    test[Int, Int](_.toInt, _.toInt)
    test[Long, String](_.toLong, _.toString)
    test[String, String](_.toString, _.toString)
    test[Double, Double](_.toDouble, _.toDouble)
  }
}

class CollectionsOnHashMapTest extends CollectionsOnMapsTest {
  def factory: MapFactory = new HashMapFactory
}

class CollectionsOnLinkedHashMapInsertionOrderTest
    extends CollectionsOnMapsTest {
  def factory: MapFactory = new LinkedHashMapFactory(false, false)
}

class CollectionsOnLinkedHashMapInsertionOrderWithLimitTest
    extends CollectionsOnMapsTest {
  def factory: MapFactory = new LinkedHashMapFactory(false, true)
}

class CollectionsOnLinkedHashMapAccessOrderTest extends CollectionsOnMapsTest {
  def factory: MapFactory = new LinkedHashMapFactory(true, false)
}

class CollectionsOnLinkedHashMapAccessOrderWithLimitTest
    extends CollectionsOnMapsTest {
  def factory: MapFactory = new LinkedHashMapFactory(true, true)
}
