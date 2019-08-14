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

import org.junit.Test
import org.junit.Assert._

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
    val lhm = factory.empty[String, String]
    (0 until 100).foreach(key => lhm.put(key.toString(), s"elem $key"))

    def expectedKey(index: Int): String =
      (withSizeLimit.getOrElse(0) + index).toString()

    val expected = (0 until withSizeLimit.getOrElse(100)).map { i =>
      val key = expectedKey(i)
      key -> s"elem $key"
    }

    assertSameEntriesOrdered(expected: _*)(lhm)
  }

  @Test def should_iterate_in_the_same_order_after_removal_of_elements(): Unit = {
    val lhm = factory.empty[String, String]
    (0 until 100).foreach(key => lhm.put(key.toString(), s"elem $key"))

    (0 until 100 by 3).foreach(key => lhm.remove(key.toString()))

    val expectedKeys =
      ((100 - withSizeLimit.getOrElse(100)) until 100).filter(_ % 3 != 0).map(_.toString())

    val expected = expectedKeys.map(key => key -> s"elem $key")

    assertSameEntriesOrdered(expected: _*)(lhm)
  }

  @Test def should_iterate_in_order_after_adding_elements(): Unit = {
    val lhm = factory.empty[String, String]
    (0 until 100).foreach(key => lhm.put(key.toString(), s"elem $key"))

    lhm.put("0", "new 0")
    lhm.put("100", "elem 100")
    lhm.put("42", "new 42")
    lhm.put("52", "new 52")
    lhm.put("1", "new 1")
    lhm.put("98", "new 98")

    val expectedKeys = {
      if (factory.accessOrder) {
        val keys = (2 until 42) ++ (43 until 52) ++ (53 until 98) ++
            List(99, 0, 100, 42, 52, 1, 98)
        keys.takeRight(withSizeLimit.getOrElse(keys.length))
      } else {
        if (withSizeLimit.isDefined) (55 until 100) ++ List(0, 100, 42, 52, 1)
        else 0 to 100
      }
    }.map(_.toString())

    def expectedElem(key: String): String = {
      if (key == "0" || key == "1" || key == "42" || key == "52" || key == "98")
        s"new $key"
      else
        s"elem $key"
    }

    val expected = expectedKeys.map(key => key -> expectedElem(key))

    assertSameEntriesOrdered(expected: _*)(lhm)
  }

  @Test def should_iterate_in__after_accessing_elements(): Unit = {
    val lhm = factory.empty[String, String]
    (0 until 100).foreach(key => lhm.put(key.toString(), s"elem $key"))

    lhm.get("42")
    lhm.get("52")
    lhm.get("5")

    def expectedKey(index: Int): String = {
      val intKey = if (accessOrder) {
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
      intKey.toString()
    }

    val expected = (0 until withSizeLimit.getOrElse(100)).map { i =>
      val key = expectedKey(i)
      key -> s"elem $key"
    }

    assertSameEntriesOrdered(expected: _*)(lhm)
  }

  private def assertSameEntriesOrdered[A, B](expected: (A, B)*)(
      map: ju.Map[A, B]): Unit = {

    val expectedSize = expected.size

    val entrySet = map.entrySet()
    assertEquals(expectedSize, entrySet.size())
    val keySet = map.keySet()
    assertEquals(expectedSize, keySet.size())
    val values = map.values()
    assertEquals(expectedSize, values.size())

    val expectedIter = expected.iterator
    val entryIter = entrySet.iterator()
    val keyIter = keySet.iterator()
    val valueIter = values.iterator()

    for (_ <- 0 until expectedSize) {
      val (key, value) = expectedIter.next()

      assertTrue(entryIter.hasNext())
      val entry = entryIter.next()
      assertEquals(key, entry.getKey())
      assertEquals(value, entry.getValue())

      assertTrue(keyIter.hasNext())
      assertEquals(key, keyIter.next())

      assertTrue(valueIter.hasNext())
      assertEquals(value, valueIter.next())
    }

    assertFalse(entryIter.hasNext())
    assertFalse(keyIter.hasNext())
    assertFalse(valueIter.hasNext())
  }

}

class LinkedHashMapFactory(val accessOrder: Boolean,
    override val withSizeLimit: Option[Int])
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
