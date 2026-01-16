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
import org.junit.Assume._

import java.{util => ju, lang => jl}
import java.util.function.{BiConsumer, BiFunction, Function}

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

  @Test def iteratorOrder(): Unit = {
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

  @Test def iteratorOrderAfterRemove(): Unit = {
    val lhm = factory.empty[String, String]
    (0 until 100).foreach(key => lhm.put(key.toString(), s"elem $key"))

    (0 until 100 by 3).foreach(key => lhm.remove(key.toString()))

    val expectedKeys =
      ((100 - withSizeLimit.getOrElse(100)) until 100).filter(_ % 3 != 0).map(_.toString())

    val expected = expectedKeys.map(key => key -> s"elem $key")

    assertSameEntriesOrdered(expected: _*)(lhm)
  }

  @Test def iteratorOrderAfterPutPutIfAbsent(): Unit = {
    val lhm = factory.empty[String, String]
    (0 until 100).foreach(key => lhm.put(key.toString(), s"elem $key"))

    lhm.put("0", "new 0")
    lhm.put("100", "elem 100")
    lhm.put("42", "new 42")
    lhm.putIfAbsent("101", "elem 101")
    lhm.put("52", "new 52")
    lhm.put("1", "new 1")
    lhm.putIfAbsent("42", "ignored")
    lhm.put("98", "new 98")

    val expectedKeys = {
      if (factory.accessOrder) {
        val keys = (2 until 42) ++ (43 until 52) ++ (53 until 98) ++
          List(99, 0, 100, 101, 52, 1, 42, 98)
        keys.takeRight(withSizeLimit.getOrElse(keys.length))
      } else {
        if (withSizeLimit.isDefined) (56 until 100) ++ List(0, 100, 42, 101, 52, 1)
        else 0 to 101
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

  @Test def iteratorOrderAfterGet(): Unit = {
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

  @Test def iteratorOrderAfterUsingAllMethods(): Unit = {
    /* Relevant JavaDoc excerpt:
     *
     * > Invoking the put, putIfAbsent, get, getOrDefault, compute,
     * > computeIfAbsent, computeIfPresent, or merge methods results in an
     * > access to the corresponding entry (assuming it exists after the
     * > invocation completes).
     * >
     * > The replace methods only result in an access of the entry if the value
     * > is replaced.
     */

    assumeTrue("relevant for access-order only", factory.accessOrder)
    assumeFalse("assuming that entries are not going to be automatically removed",
        factory.withSizeLimit.isDefined)

    val initialElems = (0 until 100).map(key => key.toString() -> s"elem $key").toList
    val lhm = factory.fromKeyValuePairs[String, String](initialElems: _*)

    (0 until 100).foreach(key => lhm.put(key.toString(), s"elem $key"))

    val biFunctionReturnsNull = new BiFunction[String, String, String] {
      def apply(x: String, y: String): String = null
    }

    val computeFunKeep = new BiFunction[String, String, String] {
      def apply(key: String, value: String): String = s"$key - $value"
    }

    val computeFunDrop = biFunctionReturnsNull

    val computeIfAbsentFunAdd = new Function[String, String] {
      def apply(key: String): String = s"computed $key"
    }

    val computeIfAbsentFunNoAdd = new Function[String, String] {
      def apply(key: String): String = null
    }

    val computeIfPresentFunReplace = new BiFunction[String, String, String] {
      def apply(key: String, value: String): String = s"replaced $key - $value"
    }

    val computeIfPresentFunDrop = biFunctionReturnsNull

    val mergeFunAddOrReplace = new BiFunction[String, String, String] {
      def apply(oldValue: String, value: String): String = s"merged $oldValue - $value"
    }

    val mergeFunDrop = biFunctionReturnsNull

    lhm.put("30", "new 30")
    lhm.putIfAbsent("42", "ignored") // no-op, affects order
    lhm.putIfAbsent("123", "elem 123") // appends
    lhm.get("23") // no-op, affects order
    lhm.getOrDefault("54", "ignored") // no-op, affects order
    lhm.getOrDefault("321", "used") // does not affect order
    lhm.containsKey("25") // no-op, does not affect order
    lhm.containsValue("elem 10") // no-op, does not affect order
    lhm.compute("43", computeFunKeep)
    lhm.compute("432", computeFunKeep) // appends
    lhm.compute("65", computeFunDrop) // removes
    lhm.remove("21", "not 21") // no-op, does not affect order
    lhm.computeIfAbsent("76", computeIfAbsentFunAdd) // no-op, affects order
    lhm.computeIfAbsent("532", computeIfAbsentFunAdd)
    lhm.computeIfAbsent("567", computeIfAbsentFunNoAdd) // no-op, does not affect order
    lhm.computeIfPresent("33", computeIfPresentFunReplace)
    lhm.computeIfPresent("456", computeIfPresentFunReplace) // no-op, does not affect order
    lhm.computeIfPresent("78", computeIfPresentFunDrop) // removes
    lhm.merge("92", "append", mergeFunAddOrReplace)
    lhm.merge("987", "default", mergeFunAddOrReplace) // appends
    lhm.merge("27", "unused", mergeFunDrop) // removes
    lhm.replace("48", "new 48")
    lhm.replace("543", "unused") // no-op, does not affect order
    lhm.replace("12", "elem 12", "new 12") // affects order
    lhm.replace("14", "not elem 14", "unused") // not replaced, does not affect order!

    val latestAccesses: List[(String, String)] = List(
      "30" -> "new 30",
      "42" -> "elem 42",
      "123" -> "elem 123",
      "23" -> "elem 23",
      "54" -> "elem 54",
      "43" -> "43 - elem 43",
      "432" -> "432 - null",
      "76" -> "elem 76",
      "532" -> "computed 532",
      "33" -> "replaced 33 - elem 33",
      "92" -> "merged elem 92 - append",
      "987" -> "default",
      "48" -> "new 48",
      "12" -> "new 12"
    )

    val removedKeys = Set("65", "78", "27")

    val alteredKeys = latestAccesses.map(_._1).toSet ++ removedKeys
    val allElems = initialElems.filterNot(kv => alteredKeys(kv._1)) ::: latestAccesses

    assertSameEntriesOrdered(allElems: _*)(lhm)
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

    // Traversal with forEach should also be in the same order

    val expectedIter2 = expected.iterator
    map.forEach(new BiConsumer[A, B] {
      def accept(key: A, value: B): Unit = {
        assertTrue(expectedIter2.hasNext)
        val (expectedKey, expectedValue) = expectedIter2.next()
        assertEquals(expectedKey, key)
        assertEquals(expectedValue, value)
      }
    })
    assertFalse(expectedIter2.hasNext)
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

  override def guaranteesInsertionOrder: Boolean = !accessOrder
}
