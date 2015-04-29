/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib

import scala.language.implicitConversions

import scala.collection.JavaConversions._
import scala.collection.mutable

import org.scalajs.jasminetest.JasmineTest

import java.util.AbstractMap
import java.util.concurrent.ConcurrentHashMap

object ConcurrentHashMapTest extends JasmineTest {

  describe("java.util.concurrent.ConcurrentHashMap") {

    def expectNullPointerException(f: => Unit): Unit = {
      expect({
        try {
          f
          false
        } catch {
          case err: NullPointerException => true
        }
      }).toBeTruthy
    }

    it("should store strings") {
      val chm = new ConcurrentHashMap[String, String]()

      expect(chm.size()).toEqual(0)
      chm.put("ONE", "one")
      expect(chm.size()).toEqual(1)
      expect(chm.get("ONE")).toEqual("one")
      chm.put("TWO", "two")
      expect(chm.size()).toEqual(2)
      expect(chm.get("TWO")).toEqual("two")
    }

    it("should store integers") {
      val chm = new ConcurrentHashMap[Int, Int]()

      chm.put(100, 12345)
      expect(chm.size()).toEqual(1)
      val one = chm.get(100)
      expect(one).toEqual(12345)
    }

    it("should store doubles also in corner cases") {
      val chm = new ConcurrentHashMap[Double, Double]()

      chm.put(1.2345, 11111.0)
      expect(chm.size()).toEqual(1)
      val one = chm.get(1.2345)
      expect(one).toEqual(11111.0)

      chm.put(Double.NaN, 22222.0)
      expect(chm.size()).toEqual(2)
      val two = chm.get(Double.NaN)
      expect(two).toEqual(22222.0)

      chm.put(+0.0, 33333.0)
      expect(chm.size()).toEqual(3)
      val three = chm.get(+0.0)
      expect(three).toEqual(33333.0)

      chm.put(-0.0, 44444.0)
      expect(chm.size()).toEqual(4)
      val four = chm.get(-0.0)
      expect(four).toEqual(44444.0)
    }

    it("should store custom objects") {
      case class TestObj(num: Int)

      val chm = new ConcurrentHashMap[TestObj, TestObj]()

      chm.put(TestObj(100), TestObj(12345))
      expect(chm.size()).toEqual(1)
      val one = chm.get(TestObj(100))
      expect(one.num).toEqual(12345)
    }

    it("should remove stored elements") {
      val chm = new ConcurrentHashMap[String, String]()

      chm.put("ONE", "one")
      expect(chm.size()).toEqual(1)
      expect(chm.remove("ONE")).toEqual("one")
      val newOne = chm.get("ONE")
      expect(chm.get("ONE")).toBeNull
    }

    it("should remove stored elements on double corner cases") {
      val chm = new ConcurrentHashMap[Double, String]()

      chm.put(1.2345, "11111.0")
      chm.put(Double.NaN, "22222.0")
      chm.put(+0.0, "33333.0")
      chm.put(-0.0, "44444.0")

      expect(chm.get(1.2345)).toEqual("11111.0")
      expect(chm.get(Double.NaN)).toEqual("22222.0")
      expect(chm.get(+0.0)).toEqual("33333.0")
      expect(chm.get(-0.0)).toEqual("44444.0")

      expect(chm.remove(-0.0)).toEqual("44444.0")
      expect(chm.get(-0.0)).toBeNull

      chm.put(-0.0, "55555.0")

      expect(chm.remove(+0.0)).toEqual("33333.0")
      expect(chm.get(+0.0)).toBeNull

      chm.put(+0.0, "66666.0")

      expect(chm.remove(Double.NaN)).toEqual("22222.0")
      expect(chm.get(Double.NaN)).toBeNull

      chm.put(Double.NaN, "77777.0")

      chm.clear()

      expect(chm.isEmpty).toBeTruthy
    }

    it("should not put null keys or null values") {
      val chm = new ConcurrentHashMap[String, String]()

      expectNullPointerException(chm.put(null, "one"))
      expectNullPointerException(chm.put("ONE", null))
      expectNullPointerException(chm.put(null, null))
    }

    it("should be cleared with one operation") {
      val chm = new ConcurrentHashMap[String, String]()

      chm.put("ONE", "one")
      chm.put("TWO", "two")
      expect(chm.size()).toEqual(2)
      chm.clear()
      expect(chm.size()).toEqual(0)
    }

    it("should check contained key presence") {
      val chm = new ConcurrentHashMap[String, String]()

      chm.put("ONE", "one")
      expect(chm.containsKey("ONE")).toBeTruthy
      expect(chm.containsKey("TWO")).toBeFalsy
      expect(chm.containsKey(null)).toBeFalsy
    }

    it("should check contained value presence") {
      val chm = new ConcurrentHashMap[String, String]()

      chm.put("ONE", "one")
      expect(chm.containsValue("one")).toBeTruthy
      expect(chm.containsValue("two")).toBeFalsy
      expect(chm.containsValue(null)).toBeFalsy
    }

    it("should give proper Enumerator over elements") {
      val chm = new ConcurrentHashMap[String, String]()

      chm.put("ONE", "one")
      val elements = chm.elements
      expect(elements.hasNext).toBeTruthy
      expect(elements.nextElement).toEqual("one")
      expect(elements.hasNext).toBeFalsy
    }

    it("should give proper Collection over values") {
      val chm = new ConcurrentHashMap[String, String]()

      chm.put("ONE", "one")
      val values = chm.values
      expect(values.size).toEqual(1)
      val iter = values.iterator
      expect(iter.hasNext).toBeTruthy
      expect(iter.next).toEqual("one")
      expect(iter.hasNext).toBeFalsy
    }

    it("should give proper EntrySet over key values pairs") {
      val chm = new ConcurrentHashMap[String, String]()

      chm.put("ONE", "one")
      val entrySet = chm.entrySet

      expect(entrySet.size).toEqual(1)

      val iter = entrySet.iterator
      expect(iter.hasNext).toBeTruthy
      val next = iter.next
      expect(iter.hasNext).toBeFalsy
      expect(next.getKey).toEqual("ONE")
      expect(next.getValue).toEqual("one")
    }

    it("should give proper KeySet over keys") {
      val chm = new ConcurrentHashMap[String, String]()

      chm.put("ONE", "one")
      val keySet = chm.keySet

      expect(keySet.size).toEqual(1)

      val iter = keySet.iterator
      expect(iter.hasNext).toBeTruthy
      expect(iter.next).toEqual("ONE")
      expect(iter.hasNext).toBeFalsy
    }

    it("should put a whole map into") {
      val chm = new ConcurrentHashMap[String, String]()

      val m = mutable.Map[String, String](
          "X" -> "y")
      chm.putAll(mutableMapAsJavaMap(m))
      expect(chm.size).toEqual(1)
      expect(chm.get("X")).toEqual("y")

      val nullMap = mutable.Map[String, String](
          (null: String) -> "y",
          "X" -> "y")

      expectNullPointerException(chm.putAll(mutableMapAsJavaMap(nullMap)))
    }

    it("should replace contained items") {
      val chm = new ConcurrentHashMap[String, String]()

      chm.put("ONE", "one")
      expect(chm.replace("ONE", "two")).toEqual("one")
      expectNullPointerException(chm.replace("ONE", null))
      expectNullPointerException(chm.replace(null, "one"))
      expect(chm.get("ONE")).toEqual("two")

      expect(chm.replace("ONE", "one", "two")).toBeFalsy
      expectNullPointerException(chm.replace(null, "two", "one"))
      expectNullPointerException(chm.replace("ONE", null, "one"))
      expectNullPointerException(chm.replace("ONE", "two", null))

      expect(chm.replace("ONE", "two", "one")).toBeTruthy
      expect(chm.get("ONE")).toEqual("one")
    }

  }

  case class SimpleQuerableMap[K, V](inner: mutable.HashMap[K, V])
      extends AbstractMap[K, V] {
    def entrySet(): java.util.Set[java.util.Map.Entry[K, V]] = {
      setAsJavaSet(inner.map {
        case (k, v) => new AbstractMap.SimpleImmutableEntry(k, v)
      }.toSet)
    }
  }

  describe("java.util.AbstractMap.values") {

    it("should mirror the related map size") {
      val chm = new ConcurrentHashMap[String, String]()

      chm.put("ONE", "one")
      chm.put("TWO", "two")

      val values = chm.values
      expect(values.size).toEqual(2)

      chm.put("THREE", "three")

      expect(values.size).toEqual(3)

      chm.remove("ONE")

      expect(values.size).toEqual(2)

      expect(values.isEmpty).toBeFalsy

      chm.clear()

      expect(values.size).toEqual(0)

      expect(values.isEmpty).toBeTruthy

      val hm1 = mutable.HashMap(
        "ONE" -> "one",
        "TWO" -> "two")
      val hm2 = mutable.HashMap(
        "ONE" -> null,
        "TWO" -> "two")
      val hm3 = mutable.HashMap(
        (null: String) -> "one",
        "TWO" -> "two")
      val hm4 = mutable.HashMap(
        (null: String) -> null,
        "TWO" -> "two")

      expect(SimpleQuerableMap(hm1).values.size).toEqual(2)
      expect(SimpleQuerableMap(hm2).values.size).toEqual(2)
      expect(SimpleQuerableMap(hm3).values.size).toEqual(2)
      expect(SimpleQuerableMap(hm4).values.size).toEqual(2)
    }

    it("should check single and multiple objects presence") {
      val chm = new ConcurrentHashMap[String, String]()

      chm.put("ONE", "one")
      chm.put("TWO", "two")

      val values = chm.values
      expect(values.contains("one")).toBeTruthy
      expect(values.contains("two")).toBeTruthy
      expect(values.contains("three")).toBeFalsy
      expect(values.contains(null)).toBeFalsy

      chm.put("THREE", "three")

      expect(values.contains("three")).toBeTruthy

      val coll1 = asJavaCollection(Set("one", "two", "three"))
      expect(values.containsAll(coll1)).toBeTruthy

      val coll2 = asJavaCollection(Set("one", "two", "three", "four"))
      expect(values.containsAll(coll2)).toBeFalsy

      val coll3 = asJavaCollection(Set("one", "two", "three", null))
      expect(values.containsAll(coll2)).toBeFalsy

      val numChm = new ConcurrentHashMap[Double, Double]()

      val numValues = numChm.values
      numChm.put(1, +0.0)
      expect(numValues.contains(+0.0)).toBeTruthy
      expect(numValues.contains(-0.0)).toBeFalsy
      expect(numValues.contains(Double.NaN)).toBeFalsy

      numChm.put(2, -0.0)
      expect(numValues.contains(+0.0)).toBeTruthy
      expect(numValues.contains(-0.0)).toBeTruthy
      expect(numValues.contains(Double.NaN)).toBeFalsy

      numChm.put(3, Double.NaN)
      expect(numValues.contains(+0.0)).toBeTruthy
      expect(numValues.contains(-0.0)).toBeTruthy
      expect(numValues.contains(Double.NaN)).toBeTruthy

      val hm1 = mutable.HashMap(
        1.0 -> null,
        2.0 -> 2.0)
      val hm2 = mutable.HashMap(
        (null: Any) -> 1.0,
        2.0 -> 2.0)
      val hm3 = mutable.HashMap(
        (null: Any) -> null,
        2.0 -> 2.0)

      expect(SimpleQuerableMap(hm1).values.contains(1.0)).toBeFalsy
      expect(SimpleQuerableMap(hm2).values.contains(1.0)).toBeTruthy
      expect(SimpleQuerableMap(hm3).values.contains(1.0)).toBeFalsy

      expect(SimpleQuerableMap(hm1).values.contains(null)).toBeTruthy
      expect(SimpleQuerableMap(hm2).values.contains(null)).toBeFalsy
      expect(SimpleQuerableMap(hm3).values.contains(null)).toBeTruthy
    }

    it("should side effect clear/remove/retain on the related map") {
      val chm = new ConcurrentHashMap[String, String]()

      chm.put("ONE", "one")
      chm.put("TWO", "two")

      val values = chm.values
      expect(values.isEmpty).toBeFalsy
      expect(chm.isEmpty).toBeFalsy

      values.clear()

      expect(values.isEmpty).toBeTruthy
      expect(chm.isEmpty).toBeTruthy

      chm.put("ONE", "one")
      chm.put("TWO", "two")

      expect(chm.containsKey("ONE")).toBeTruthy

      values.remove("one")

      expect(chm.containsKey("ONE")).toBeFalsy

      chm.put("ONE", "one")
      chm.put("THREE", "three")

      expect(chm.containsKey("ONE")).toBeTruthy
      expect(chm.containsKey("TWO")).toBeTruthy
      expect(chm.containsKey("THREE")).toBeTruthy

      values.removeAll(asJavaCollection(List("one", "two")))

      expect(chm.containsKey("ONE")).toBeFalsy
      expect(chm.containsKey("TWO")).toBeFalsy
      expect(chm.containsKey("THREE")).toBeTruthy

      chm.put("ONE", "one")
      chm.put("TWO", "two")
      chm.put("THREE", "three")

      expect(chm.containsKey("ONE")).toBeTruthy
      expect(chm.containsKey("TWO")).toBeTruthy
      expect(chm.containsKey("THREE")).toBeTruthy

      values.retainAll(asJavaCollection(List("one", "two")))

      expect(chm.containsKey("ONE")).toBeTruthy
      expect(chm.containsKey("TWO")).toBeTruthy
      expect(chm.containsKey("THREE")).toBeFalsy
    }

  }

  describe("java.util.AbstractMap.keySet") {

    it("should mirror the related map size") {
      val chm = new ConcurrentHashMap[String, String]()

      chm.put("ONE", "one")
      chm.put("TWO", "two")

      val keySet = chm.keySet
      expect(keySet.size).toEqual(2)

      chm.put("THREE", "three")

      expect(keySet.size).toEqual(3)

      chm.remove("ONE")

      expect(keySet.size).toEqual(2)

      expect(keySet.isEmpty).toBeFalsy

      chm.clear()

      expect(keySet.size).toEqual(0)

      expect(keySet.isEmpty).toBeTruthy

      val hm1 = mutable.HashMap(
        "ONE" -> "one",
        "TWO" -> "two")
      val hm2 = mutable.HashMap(
        "ONE" -> null,
        "TWO" -> "two")
      val hm3 = mutable.HashMap(
        (null: String) -> "one",
        "TWO" -> "two")
      val hm4 = mutable.HashMap(
        (null: String) -> null,
        "TWO" -> "two")

      expect(SimpleQuerableMap(hm1).keySet.size).toEqual(2)
      expect(SimpleQuerableMap(hm2).keySet.size).toEqual(2)
      expect(SimpleQuerableMap(hm3).keySet.size).toEqual(2)
      expect(SimpleQuerableMap(hm4).keySet.size).toEqual(2)
    }

    it("should check single and multiple objects presence") {
      val chm = new ConcurrentHashMap[String, String]()

      chm.put("ONE", "one")
      chm.put("TWO", "two")

      val keySet = chm.keySet
      expect(keySet.contains("ONE")).toBeTruthy
      expect(keySet.contains("TWO")).toBeTruthy
      expect(keySet.contains("THREE")).toBeFalsy
      expect(keySet.contains(null)).toBeFalsy

      chm.put("THREE", "three")

      expect(keySet.contains("THREE")).toBeTruthy

      val coll1 =
          asJavaCollection(Set("ONE", "TWO", "THREE"))
      expect(keySet.containsAll(coll1)).toBeTruthy

      val coll2 =
          asJavaCollection(Set("ONE", "TWO", "THREE", "FOUR"))
      expect(keySet.containsAll(coll2)).toBeFalsy

      val coll3 =
          asJavaCollection(Set("ONE", "TWO", "THREE", null))
      expect(keySet.containsAll(coll2)).toBeFalsy

      val numChm = new ConcurrentHashMap[Double, Double]()

      val numkeySet = numChm.keySet
      numChm.put(+0.0, 1)
      expect(numkeySet.contains(+0.0)).toBeTruthy
      expect(numkeySet.contains(-0.0)).toBeFalsy
      expect(numkeySet.contains(Double.NaN)).toBeFalsy

      numChm.put(-0.0, 2)
      expect(numkeySet.contains(+0.0)).toBeTruthy
      expect(numkeySet.contains(-0.0)).toBeTruthy
      expect(numkeySet.contains(Double.NaN)).toBeFalsy

      numChm.put(Double.NaN, 3)
      expect(numkeySet.contains(+0.0)).toBeTruthy
      expect(numkeySet.contains(-0.0)).toBeTruthy
      expect(numkeySet.contains(Double.NaN)).toBeTruthy

      val hm1 = mutable.HashMap(
        1.0 -> null,
        2.0 -> 2.0)
      val hm2 = mutable.HashMap(
        (null: Any) -> 1.0,
        2.0 -> 2.0)
      val hm3 = mutable.HashMap(
        (null: Any) -> null,
        2.0 -> 2.0)

      expect(SimpleQuerableMap(hm1).keySet.contains(1.0)).toBeTruthy
      expect(SimpleQuerableMap(hm2).keySet.contains(1.0)).toBeFalsy
      expect(SimpleQuerableMap(hm3).keySet.contains(1.0)).toBeFalsy

      expect(SimpleQuerableMap(hm1).keySet.contains(null)).toBeFalsy
      expect(SimpleQuerableMap(hm2).keySet.contains(null)).toBeTruthy
      expect(SimpleQuerableMap(hm3).keySet.contains(null)).toBeTruthy
    }

    it("should side effect clear/remove/retain on the related map") {
      val chm = new ConcurrentHashMap[String, String]()

      chm.put("ONE", "one")
      chm.put("TWO", "two")

      val keySet = chm.keySet
      expect(keySet.isEmpty).toBeFalsy
      expect(chm.isEmpty).toBeFalsy

      keySet.clear()

      expect(keySet.isEmpty).toBeTruthy

      expect(chm.isEmpty).toBeTruthy

      chm.put("ONE", "one")
      chm.put("TWO", "two")

      expect(chm.containsKey("ONE")).toBeTruthy

      keySet.remove("ONE")

      expect(chm.containsKey("ONE")).toBeFalsy

      chm.put("ONE", "one")
      chm.put("THREE", "three")

      expect(chm.containsKey("ONE")).toBeTruthy
      expect(chm.containsKey("TWO")).toBeTruthy
      expect(chm.containsKey("THREE")).toBeTruthy

      keySet.removeAll(asJavaCollection(List("ONE", "TWO")))

      expect(chm.containsKey("ONE")).toBeFalsy
      expect(chm.containsKey("TWO")).toBeFalsy
      expect(chm.containsKey("THREE")).toBeTruthy

      chm.put("ONE", "one")
      chm.put("TWO", "two")
      chm.put("THREE", "three")

      expect(chm.containsKey("ONE")).toBeTruthy
      expect(chm.containsKey("TWO")).toBeTruthy
      expect(chm.containsKey("THREE")).toBeTruthy

      keySet.retainAll(asJavaCollection(List("ONE", "TWO")))

      expect(chm.containsKey("ONE")).toBeTruthy
      expect(chm.containsKey("TWO")).toBeTruthy
      expect(chm.containsKey("THREE")).toBeFalsy
    }

  }

}
