/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib

import scala.collection.JavaConversions._

import java.{util => ju}

object LinkedHashMapInsertionOrderTest extends LinkedHashMapTest(new LinkedHashMapFactory(false, false))

object LinkedHashMapAccessOrderTest extends LinkedHashMapTest(new LinkedHashMapFactory(true, false))

object LinkedHashMapInsertionOrderLimitedTest extends LinkedHashMapTest(new LinkedHashMapFactory(false, true))

object LinkedHashMapAccessOrderLimitedTest extends LinkedHashMapTest(new LinkedHashMapFactory(true, true))

class LinkedHashMapTest[F <: LinkedHashMapFactory](mapFactory: F)
    extends HashMapTest[F](mapFactory) {

  override def testApi(): Unit = {

    super.testApi()

    val accessOrder = mapFactory.accessOrder
    val withSizeLimit = mapFactory.withSizeLimit

    describe(s"${mapFactory.implementationName} iterator order") {

      it ("should iterate in insertion order after building") {
        val lhm = mapFactory.empty[Int, String]
        (0 until 100).foreach(key => lhm.put(key, s"elem $key"))

        def expectedKey(index: Int): Int = {
          if (withSizeLimit)
            50 + index
          else
            index
        }

        def expectedValue(index: Int): String =
          s"elem ${expectedKey(index)}"

        val expectedSize = if (withSizeLimit) 50 else 100

        expect(lhm.entrySet.size).toEqual(expectedSize)
        for ((entry, index) <- lhm.entrySet.zipWithIndex) {
          expect(entry.getKey).toEqual(expectedKey(index))
          expect(entry.getValue).toEqual(expectedValue(index))
        }

        expect(lhm.keySet.size).toEqual(expectedSize)
        for ((key, index) <- lhm.keySet.zipWithIndex)
          expect(key).toEqual(expectedKey(index))

        expect(lhm.entrySet.size).toEqual(expectedSize)
        for ((value, index) <- lhm.values.zipWithIndex)
          expect(value).toEqual(expectedValue(index))

      }

      it ("should iterate in the same order after removal of elements") {
        val lhm = mapFactory.empty[Int, String]
        (0 until 100).foreach(key => lhm.put(key, s"elem $key"))

        (0 until 100 by 3).foreach(key => lhm.remove(key))

        def expectedKey(index: Int) = {
          if (!withSizeLimit) {
            index * 3 / 2 + 1
          } else if (index == 0) {
            50 // case that does not work with the 'else' closed formula
          } else {
            // compensate for dropped elements
            (index - 1) * 3 / 2 + 52
          }
        }

        def expectedValue(index: Int): String =
          s"elem ${expectedKey(index)}"

        val expectedSize = if (withSizeLimit) 33 else 66

        expect(lhm.entrySet.size).toEqual(expectedSize)
        for ((entry, index) <- lhm.entrySet.zipWithIndex) {
          expect(entry.getKey).toEqual(expectedKey(index))
          expect(entry.getValue).toEqual(expectedValue(index))
        }

        expect(lhm.keySet.size).toEqual(expectedSize)
        for ((key, index) <- lhm.keySet.zipWithIndex)
          expect(key).toEqual(expectedKey(index))

        expect(lhm.entrySet.size).toEqual(expectedSize)
        for ((value, index) <- lhm.values.zipWithIndex)
          expect(value).toEqual(expectedValue(index))

      }

      it (s"should iterate in insertion-order after adding elements") {
        val lhm = mapFactory.empty[Int, String]
        (0 until 100).foreach(key => lhm.put(key, s"elem $key"))

        lhm(0) = "new 0"
        lhm(100) = "elem 100"
        lhm(42) = "new 42"
        lhm(52) = "new 52"
        lhm(1) = "new 1"

        def expectedKey(index: Int): Int = {
          if (withSizeLimit) {
            if (index < 45)
              index + 55 // 50 from size limit + 5 from
            else if (index == 45) 0
            else if (index == 46) 100
            else if (index == 47) 42
            else if (index == 48) 52
            else 1
          } else {
            index
          }
        }

        def expectedElem(index: Int): String = {
          val key = expectedKey(index)
          if (key == 0 || key == 1 || key == 42 || key == 52)
            s"new $key"
          else
            s"elem $key"
        }

        val expectedSize = if (withSizeLimit) 50 else 101

        expect(lhm.entrySet.size).toEqual(expectedSize)
        for ((entry, index) <- lhm.entrySet.zipWithIndex) {
          expect(entry.getKey).toEqual(expectedKey(index))
          expect(entry.getValue).toEqual(expectedElem(index))
        }

        expect(lhm.keySet.size).toEqual(expectedSize)
        for ((key, index) <- lhm.keySet.zipWithIndex)
          expect(key).toEqual(expectedKey(index))

        expect(lhm.entrySet.size).toEqual(expectedSize)
        for ((value, index) <- lhm.values.zipWithIndex)
          expect(value).toEqual(expectedElem(index))
      }

      it (s"should iterate in ${mapFactory.orderName} after accessing elements") {
        val lhm = mapFactory.empty[Int, String]
        (0 until 100).foreach(key => lhm.put(key, s"elem $key"))

        lhm.get(42)
        lhm.get(52)
        lhm.get(5)

        def expectedKey(index: Int): Int = {
          if (accessOrder) {
            // elements ordered by insertion order except for those accessed
            if (!withSizeLimit) {
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
            if (withSizeLimit) 50 + index
            else index
          }
        }

        def expectedValue(index: Int): String =
          s"elem ${expectedKey(index)}"

        val expectedSize = if (withSizeLimit) 50 else 100

        expect(lhm.entrySet.size).toEqual(expectedSize)
        for ((entry, index) <- lhm.entrySet.zipWithIndex) {
          expect(entry.getKey).toEqual(expectedKey(index))
          expect(entry.getValue).toEqual(expectedValue(index))
        }

        expect(lhm.keySet.size).toEqual(expectedSize)
        for ((key, index) <- lhm.keySet.zipWithIndex)
          expect(key).toEqual(expectedKey(index))

        expect(lhm.entrySet.size).toEqual(expectedSize)
        for ((value, index) <- lhm.values.zipWithIndex)
          expect(value).toEqual(expectedValue(index))
      }

    }

  }

}

class LinkedHashMapFactory(val accessOrder: Boolean, val withSizeLimit: Boolean) extends HashMapFactory {
  def orderName: String =
    if (accessOrder) "access-order"
    else "insertion-order"

  override def implementationName: String = {
    val sizeLimitSting = if (withSizeLimit) ", maxSize=50" else ""
    s"java.util.LinkedHashMap{$orderName$sizeLimitSting}"
  }

  override def empty[K, V]: ju.LinkedHashMap[K, V] = {
    if (withSizeLimit) {
      new ju.LinkedHashMap[K, V](16, 0.75f, accessOrder) {
        override protected def removeEldestEntry(eldest: ju.Map.Entry[K, V]): Boolean =
          size > 50
      }
    } else {
      new ju.LinkedHashMap[K, V](16, 0.75f, accessOrder)
    }
  }
}
