/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import org.junit.Test
import org.junit.Assert._

import scala.collection.JavaConversions._

import java.{util => ju, lang => jl}

import scala.reflect.ClassTag

class LinkedHashMapInsertionOrderTest extends LinkedHashMapTest

class LinkedHashMapInsertionOrderLimitedTest extends LinkedHashMapTest {
  override def factory: LinkedHashMapFactory =
    new LinkedHashMapFactory(accessOrder = false, withSizeLimit = Some(50))
}

class LinkedHashMapAccessOrderTest extends LinkedHashMapTest {
  override def factory: LinkedHashMapFactory =
    new LinkedHashMapFactory(accessOrder = true, withSizeLimit = None)
}

class LinkedHashMapAccessOrderLimitedTest extends LinkedHashMapTest {
  override def factory: LinkedHashMapFactory =
    new LinkedHashMapFactory(accessOrder = true, withSizeLimit = Some(50))
}

abstract class LinkedHashMapTest extends HashMapTest {

  override def factory: LinkedHashMapFactory =
    new LinkedHashMapFactory(accessOrder = false, withSizeLimit = None)

  val accessOrder = factory.accessOrder
  val withSizeLimit = factory.withSizeLimit

  @Test def should_iterate_in_insertion_order_after_building(): Unit = {
    val lhm = factory.empty[jl.Integer, String]
    (0 until 100).foreach(key => lhm.put(key, s"elem $key"))

    def expectedKey(index: Int): Int =
      withSizeLimit.getOrElse(0) + index

    def expectedValue(index: Int): String =
      s"elem ${expectedKey(index)}"

    val expectedSize = withSizeLimit.getOrElse(100)

    assertEquals(expectedSize, lhm.entrySet.size)
    for ((entry, index) <- lhm.entrySet.zipWithIndex) {
      assertEquals(expectedKey(index), entry.getKey)
      assertEquals(expectedValue(index), entry.getValue)
    }

    assertEquals(expectedSize, lhm.keySet.size)
    for ((key, index) <- lhm.keySet.zipWithIndex)
      assertEquals(expectedKey(index), key)

    assertEquals(expectedSize, lhm.entrySet.size)
    for ((value, index) <- lhm.values.zipWithIndex)
      assertEquals(expectedValue(index), value)
  }

  @Test def should_iterate_in_the_same_order_after_removal_of_elements(): Unit = {
    val lhm = factory.empty[jl.Integer, String]
    (0 until 100).foreach(key => lhm.put(key, s"elem $key"))

    (0 until 100 by 3).foreach(key => lhm.remove(key))

    val expectedKey =
      ((100 - withSizeLimit.getOrElse(100)) to 100).filter(_ % 3 != 0).toArray

    def expectedValue(index: Int): String =
      s"elem ${expectedKey(index)}"

    val expectedSize = if (withSizeLimit.isDefined) 33 else 66

    assertEquals(expectedSize, lhm.entrySet.size)
    for ((entry, index) <- lhm.entrySet.zipWithIndex) {
      assertEquals(expectedKey(index), entry.getKey)
      assertEquals(expectedValue(index), entry.getValue)
    }

    assertEquals(expectedSize, lhm.keySet.size)
    for ((key, index) <- lhm.keySet.zipWithIndex)
      assertEquals(expectedKey(index), key)

    assertEquals(expectedSize, lhm.entrySet.size)
    for ((value, index) <- lhm.values.zipWithIndex)
      assertEquals(expectedValue(index), value)
  }

  @Test def should_iterate_in_order_after_adding_elements(): Unit = {
    val lhm = factory.empty[jl.Integer, String]
    (0 until 100).foreach(key => lhm.put(key, s"elem $key"))

    lhm(0) = "new 0"
    lhm(100) = "elem 100"
    lhm(42) = "new 42"
    lhm(52) = "new 52"
    lhm(1) = "new 1"
    lhm(98) = "new 98"

    val expectedKey = {
      if (factory.accessOrder) {
        val keys = (2 until 42) ++ (43 until 52) ++ (53 until 98) ++
            List(99, 0, 100, 42, 52, 1, 98)
        keys.takeRight(withSizeLimit.getOrElse(keys.length))
      } else {
        if (withSizeLimit.isDefined) (55 until 100) ++ List(0, 100, 42, 52, 1)
        else 0 to 100
      }
    }.toArray

    def expectedElem(index: Int): String = {
      val key = expectedKey(index)
      if (key == 0 || key == 1 || key == 42 || key == 52 || key == 98)
        s"new $key"
      else
        s"elem $key"
    }

    val expectedSize = withSizeLimit.getOrElse(101)

    assertEquals(expectedSize, lhm.entrySet.size)

    for ((entry, index) <- lhm.entrySet.zipWithIndex) {
      assertEquals(expectedKey(index), entry.getKey)
      assertEquals(expectedElem(index), entry.getValue)
    }

    assertEquals(expectedSize, lhm.keySet.size)
    for ((key, index) <- lhm.keySet.zipWithIndex)
      assertEquals(expectedKey(index), key)

    assertEquals(expectedSize, lhm.entrySet.size)
    for ((value, index) <- lhm.values.zipWithIndex)
      assertEquals(expectedElem(index), value)
  }

  @Test def should_iterate_in__after_accessing_elements(): Unit = {
    val lhm = factory.empty[jl.Integer, String]
    (0 until 100).foreach(key => lhm.put(key, s"elem $key"))

    lhm.get(42)
    lhm.get(52)
    lhm.get(5)

    def expectedKey(index: Int): Int = {
      if (accessOrder) {
        // elements ordered by insertion order except for those accessed
        if (withSizeLimit.isEmpty) {
          if (index < 5) index // no elements removed in this range
          else if (index + 1 < 42) index + 1 // shifted by 1 removed element
          else if (index + 2 < 52) index + 2 // shifted by 2 removed element
          else if (index < 97) index + 3 // shifted by 3 removed element
          // elements reordered by accesses
          else if (index == 97) 42
          else if (index == 98) 52
          else 5
        } else {
          // note that 5 and 42 are not accessed because they where dropped
          // due to the size limit
          if (index < 2) index + 50 // no elements removed in this range
          else if (index < 49) index + 51 // shifted by 1 removed element
          // element reordered by accesses
          else 52
        }
      } else {
        // accesses shouldn't modify the order
        withSizeLimit.getOrElse(0) + index
      }
    }

    def expectedValue(index: Int): String =
      s"elem ${expectedKey(index)}"

    val expectedSize = withSizeLimit.getOrElse(100)

    assertEquals(expectedSize, lhm.entrySet.size)
    for ((entry, index) <- lhm.entrySet.zipWithIndex) {
      assertEquals(expectedKey(index), entry.getKey)
      assertEquals(expectedValue(index), entry.getValue)
    }

    assertEquals(expectedSize, lhm.keySet.size)
    for ((key, index) <- lhm.keySet.zipWithIndex)
      assertEquals(expectedKey(index), key)

    assertEquals(expectedSize, lhm.entrySet.size)
    for ((value, index) <- lhm.values.zipWithIndex)
      assertEquals(expectedValue(index), value)
  }

}

object LinkedHashMapFactory {
  def allFactories: Iterator[MapFactory] = {
    Iterator(new LinkedHashMapFactory(true, Some(50)), new LinkedHashMapFactory(true, None),
        new LinkedHashMapFactory(false, Some(50)), new LinkedHashMapFactory(false, None))
  }
}

class LinkedHashMapFactory(val accessOrder: Boolean, val withSizeLimit: Option[Int])
    extends HashMapFactory {
  def orderName: String =
    if (accessOrder) "access-order"
    else "insertion-order"

  override def implementationName: String = {
    val sizeLimitSting = withSizeLimit.fold("")(", maxSize=" + _)
    s"java.util.LinkedHashMap{$orderName$sizeLimitSting}"
  }

  override def empty[K: ClassTag, V: ClassTag]: ju.LinkedHashMap[K, V] = {
    withSizeLimit match {
      case Some(limit) =>
        new ju.LinkedHashMap[K, V](16, 0.75f, accessOrder) {
          override protected def removeEldestEntry(eldest: ju.Map.Entry[K, V]): Boolean =
            size > limit
        }

      case None =>
        new ju.LinkedHashMap[K, V](16, 0.75f, accessOrder)
    }
  }
}
