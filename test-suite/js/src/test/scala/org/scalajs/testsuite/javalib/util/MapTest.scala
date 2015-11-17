/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import java.{util => ju}

import org.scalajs.jasminetest.JasmineTest
import org.scalajs.testsuite.utils.ExpectExceptions
import org.scalajs.testsuite.javalib.util.concurrent.ConcurrentMapFactory

import scala.collection.JavaConversions._
import scala.collection.{mutable => mu}

trait MapTest extends JasmineTest with ExpectExceptions {

  def testMapApi(mapFactory: MapFactory): Unit = {

    it("should store strings") {
      val mp = mapFactory.empty[String, String]

      expect(mp.size()).toEqual(0)
      mp.put("ONE", "one")
      expect(mp.size()).toEqual(1)
      expect(mp.get("ONE")).toEqual("one")
      mp.put("TWO", "two")
      expect(mp.size()).toEqual(2)
      expect(mp.get("TWO")).toEqual("two")
    }

    it("should store integers") {
      val mp = mapFactory.empty[Int, Int]

      mp.put(100, 12345)
      expect(mp.size()).toEqual(1)
      val one = mp.get(100)
      expect(one).toEqual(12345)
    }

    it("should store doubles also in corner cases") {
      val mp = mapFactory.empty[Double, Double]

      mp.put(1.2345, 11111.0)
      expect(mp.size()).toEqual(1)
      val one = mp.get(1.2345)
      expect(one).toEqual(11111.0)

      mp.put(Double.NaN, 22222.0)
      expect(mp.size()).toEqual(2)
      val two = mp.get(Double.NaN)
      expect(two).toEqual(22222.0)

      mp.put(+0.0, 33333.0)
      expect(mp.size()).toEqual(3)
      val three = mp.get(+0.0)
      expect(three).toEqual(33333.0)

      mp.put(-0.0, 44444.0)
      expect(mp.size()).toEqual(4)
      val four = mp.get(-0.0)
      expect(four).toEqual(44444.0)
    }

    it("should store custom objects") {
      case class TestObj(num: Int)

      val mp = mapFactory.empty[TestObj, TestObj]

      mp.put(TestObj(100), TestObj(12345))
      expect(mp.size()).toEqual(1)
      val one = mp.get(TestObj(100))
      expect(one.num).toEqual(12345)
    }

    it("should remove stored elements") {
      val mp = mapFactory.empty[String, String]

      mp.put("ONE", "one")
      expect(mp.size()).toEqual(1)
      expect(mp.remove("ONE")).toEqual("one")
      val newOne = mp.get("ONE")
      expect(mp.get("ONE")).toBeNull
    }

    it("should remove stored elements on double corner cases") {
      val mp = mapFactory.empty[Double, String]

      mp.put(1.2345, "11111.0")
      mp.put(Double.NaN, "22222.0")
      mp.put(+0.0, "33333.0")
      mp.put(-0.0, "44444.0")

      expect(mp.get(1.2345)).toEqual("11111.0")
      expect(mp.get(Double.NaN)).toEqual("22222.0")
      expect(mp.get(+0.0)).toEqual("33333.0")
      expect(mp.get(-0.0)).toEqual("44444.0")

      expect(mp.remove(-0.0)).toEqual("44444.0")
      expect(mp.get(-0.0)).toBeNull

      mp.put(-0.0, "55555.0")

      expect(mp.remove(+0.0)).toEqual("33333.0")
      expect(mp.get(+0.0)).toBeNull

      mp.put(+0.0, "66666.0")

      expect(mp.remove(Double.NaN)).toEqual("22222.0")
      expect(mp.get(Double.NaN)).toBeNull

      mp.put(Double.NaN, "77777.0")

      mp.clear()

      expect(mp.isEmpty).toBeTruthy
    }

    if (mapFactory.allowsNullKeys) {
      it("should put null keys") {
        val mp = mapFactory.empty[String, String]
        mp.put(null, "one")
        expect(mp.get(null)).toEqual("one")
      }
    } else {
      it("should not put null keys") {
        val mp = mapFactory.empty[String, String]
        expectThrows[NullPointerException](mp.put(null, "one"))
      }
    }

    if (mapFactory.allowsNullValues) {
      it("should put null values") {
        val mp = mapFactory.empty[String, String]
        mp.put("one", null)
        expect(mp.get("one")).toEqual(null)
      }
    } else {
      it("should not put null values") {
        val mp = mapFactory.empty[String, String]
        expectThrows[NullPointerException](mp.put("one", null))
      }
    }

    it("should be cleared with one operation") {
      val mp = mapFactory.empty[String, String]

      mp.put("ONE", "one")
      mp.put("TWO", "two")
      expect(mp.size()).toEqual(2)
      mp.clear()
      expect(mp.size()).toEqual(0)
    }

    it("should check contained key presence") {
      val mp = mapFactory.empty[String, String]

      mp.put("ONE", "one")
      expect(mp.containsKey("ONE")).toBeTruthy
      expect(mp.containsKey("TWO")).toBeFalsy
      expect(mp.containsKey(null)).toBeFalsy
    }

    it("should check contained value presence") {
      val mp = mapFactory.empty[String, String]

      mp.put("ONE", "one")
      expect(mp.containsValue("one")).toBeTruthy
      expect(mp.containsValue("two")).toBeFalsy
      expect(mp.containsValue(null)).toBeFalsy
    }

    it("should give proper Collection over values") {
      val mp = mapFactory.empty[String, String]

      mp.put("ONE", "one")
      val values = mp.values
      expect(values.size).toEqual(1)
      val iter = values.iterator
      expect(iter.hasNext).toBeTruthy
      expect(iter.next).toEqual("one")
      expect(iter.hasNext).toBeFalsy
    }

    it("should give proper EntrySet over key values pairs") {
      val mp = mapFactory.empty[String, String]

      mp.put("ONE", "one")
      val entrySet = mp.entrySet

      expect(entrySet.size).toEqual(1)

      val iter = entrySet.iterator
      expect(iter.hasNext).toBeTruthy
      val next = iter.next
      expect(iter.hasNext).toBeFalsy
      expect(next.getKey).toEqual("ONE")
      expect(next.getValue).toEqual("one")
    }

    it("should give proper KeySet over keys") {
      val mp = mapFactory.empty[String, String]

      mp.put("ONE", "one")
      val keySet = mp.keySet

      expect(keySet.size).toEqual(1)

      val iter = keySet.iterator
      expect(iter.hasNext).toBeTruthy
      expect(iter.next).toEqual("ONE")
      expect(iter.hasNext).toBeFalsy
    }

    it("should put a whole map into") {
      val mp = mapFactory.empty[String, String]

      val m = mu.Map[String, String](
        "X" -> "y")
      mp.putAll(mutableMapAsJavaMap(m))
      expect(mp.size).toEqual(1)
      expect(mp.get("X")).toEqual("y")

      val nullMap = mu.Map[String, String](
        (null: String) -> "y",
        "X" -> "y")

      if (mapFactory.allowsNullKeys) {
        mp.putAll(mutableMapAsJavaMap(nullMap))
        expect(mp.get(null)).toEqual("y")
        expect(mp.get("X")).toEqual("y")
      } else {
        expectThrows[NullPointerException](mp.putAll(mutableMapAsJavaMap(nullMap)))
      }
    }

    class SimpleQueryableMap[K, V](inner: mu.HashMap[K, V])
        extends ju.AbstractMap[K, V] {
      def entrySet(): java.util.Set[java.util.Map.Entry[K, V]] = {
        setAsJavaSet(inner.map {
          case (k, v) => new ju.AbstractMap.SimpleImmutableEntry(k, v)
        }.toSet)
      }
    }

    it("values should mirror the related map size") {
      val mp = mapFactory.empty[String, String]

      mp.put("ONE", "one")
      mp.put("TWO", "two")

      val values = mp.values
      expect(values.size).toEqual(2)

      mp.put("THREE", "three")

      expect(values.size).toEqual(3)

      mp.remove("ONE")

      expect(values.size).toEqual(2)

      expect(values.isEmpty).toBeFalsy

      mp.clear()

      expect(values.size).toEqual(0)

      expect(values.isEmpty).toBeTruthy

      val hm1 = mu.HashMap(
        "ONE" -> "one",
        "TWO" -> "two")
      val hm2 = mu.HashMap(
        "ONE" -> null,
        "TWO" -> "two")
      val hm3 = mu.HashMap(
        (null: String) -> "one",
        "TWO" -> "two")
      val hm4 = mu.HashMap(
        (null: String) -> null,
        "TWO" -> "two")

      expect(new SimpleQueryableMap(hm1).values.size).toEqual(2)
      expect(new SimpleQueryableMap(hm2).values.size).toEqual(2)
      expect(new SimpleQueryableMap(hm3).values.size).toEqual(2)
      expect(new SimpleQueryableMap(hm4).values.size).toEqual(2)
    }

    it("values should check single and multiple objects presence") {
      val mp = mapFactory.empty[String, String]

      mp.put("ONE", "one")
      mp.put("TWO", "two")

      val values = mp.values
      expect(values.contains("one")).toBeTruthy
      expect(values.contains("two")).toBeTruthy
      expect(values.contains("three")).toBeFalsy
      expect(values.contains(null)).toBeFalsy

      mp.put("THREE", "three")

      expect(values.contains("three")).toBeTruthy

      val coll1 = asJavaCollection(Set("one", "two", "three"))
      expect(values.containsAll(coll1)).toBeTruthy

      val coll2 = asJavaCollection(Set("one", "two", "three", "four"))
      expect(values.containsAll(coll2)).toBeFalsy

      val coll3 = asJavaCollection(Set("one", "two", "three", null))
      expect(values.containsAll(coll2)).toBeFalsy

      val nummp = mapFactory.empty[Double, Double]

      val numValues = nummp.values
      nummp.put(1, +0.0)
      expect(numValues.contains(+0.0)).toBeTruthy
      expect(numValues.contains(-0.0)).toBeFalsy
      expect(numValues.contains(Double.NaN)).toBeFalsy

      nummp.put(2, -0.0)
      expect(numValues.contains(+0.0)).toBeTruthy
      expect(numValues.contains(-0.0)).toBeTruthy
      expect(numValues.contains(Double.NaN)).toBeFalsy

      nummp.put(3, Double.NaN)
      expect(numValues.contains(+0.0)).toBeTruthy
      expect(numValues.contains(-0.0)).toBeTruthy
      expect(numValues.contains(Double.NaN)).toBeTruthy

      val hm1 = mu.HashMap(
        1.0 -> null,
        2.0 -> 2.0)
      val hm2 = mu.HashMap(
        (null: Any) -> 1.0,
        2.0 -> 2.0)
      val hm3 = mu.HashMap(
        (null: Any) -> null,
        2.0 -> 2.0)

      expect(new SimpleQueryableMap(hm1).values.contains(1.0)).toBeFalsy
      expect(new SimpleQueryableMap(hm2).values.contains(1.0)).toBeTruthy
      expect(new SimpleQueryableMap(hm3).values.contains(1.0)).toBeFalsy

      expect(new SimpleQueryableMap(hm1).values.contains(null)).toBeTruthy
      expect(new SimpleQueryableMap(hm2).values.contains(null)).toBeFalsy
      expect(new SimpleQueryableMap(hm3).values.contains(null)).toBeTruthy
    }

    it("values should side effect clear/remove/retain on the related map") {
      val mp = mapFactory.empty[String, String]

      mp.put("ONE", "one")
      mp.put("TWO", "two")

      val values = mp.values
      expect(values.isEmpty).toBeFalsy
      expect(mp.isEmpty).toBeFalsy

      values.clear()

      expect(values.isEmpty).toBeTruthy
      expect(mp.isEmpty).toBeTruthy

      mp.put("ONE", "one")
      mp.put("TWO", "two")

      expect(mp.containsKey("ONE")).toBeTruthy

      values.remove("one")

      expect(mp.containsKey("ONE")).toBeFalsy

      mp.put("ONE", "one")
      mp.put("THREE", "three")

      expect(mp.containsKey("ONE")).toBeTruthy
      expect(mp.containsKey("TWO")).toBeTruthy
      expect(mp.containsKey("THREE")).toBeTruthy

      values.removeAll(asJavaCollection(List("one", "two")))

      expect(mp.containsKey("ONE")).toBeFalsy
      expect(mp.containsKey("TWO")).toBeFalsy
      expect(mp.containsKey("THREE")).toBeTruthy

      mp.put("ONE", "one")
      mp.put("TWO", "two")
      mp.put("THREE", "three")

      expect(mp.containsKey("ONE")).toBeTruthy
      expect(mp.containsKey("TWO")).toBeTruthy
      expect(mp.containsKey("THREE")).toBeTruthy

      values.retainAll(asJavaCollection(List("one", "two")))

      expect(mp.containsKey("ONE")).toBeTruthy
      expect(mp.containsKey("TWO")).toBeTruthy
      expect(mp.containsKey("THREE")).toBeFalsy
    }

    it("keySet should mirror the related map size") {
      val mp = mapFactory.empty[String, String]

      mp.put("ONE", "one")
      mp.put("TWO", "two")

      val keySet = mp.keySet
      expect(keySet.size).toEqual(2)

      mp.put("THREE", "three")

      expect(keySet.size).toEqual(3)

      mp.remove("ONE")

      expect(keySet.size).toEqual(2)

      expect(keySet.isEmpty).toBeFalsy

      mp.clear()

      expect(keySet.size).toEqual(0)

      expect(keySet.isEmpty).toBeTruthy

      val hm1 = mu.HashMap(
        "ONE" -> "one",
        "TWO" -> "two")
      val hm2 = mu.HashMap(
        "ONE" -> null,
        "TWO" -> "two")
      val hm3 = mu.HashMap(
        (null: String) -> "one",
        "TWO" -> "two")
      val hm4 = mu.HashMap(
        (null: String) -> null,
        "TWO" -> "two")

      expect(new SimpleQueryableMap(hm1).keySet.size).toEqual(2)
      expect(new SimpleQueryableMap(hm2).keySet.size).toEqual(2)
      expect(new SimpleQueryableMap(hm3).keySet.size).toEqual(2)
      expect(new SimpleQueryableMap(hm4).keySet.size).toEqual(2)
    }

    it("keySet should check single and multiple objects presence") {
      val mp = mapFactory.empty[String, String]

      mp.put("ONE", "one")
      mp.put("TWO", "two")

      val keySet = mp.keySet
      expect(keySet.contains("ONE")).toBeTruthy
      expect(keySet.contains("TWO")).toBeTruthy
      expect(keySet.contains("THREE")).toBeFalsy
      expect(keySet.contains(null)).toBeFalsy

      mp.put("THREE", "three")

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

      val nummp = mapFactory.empty[Double, Double]

      val numkeySet = nummp.keySet
      nummp.put(+0.0, 1)
      expect(numkeySet.contains(+0.0)).toBeTruthy
      expect(numkeySet.contains(-0.0)).toBeFalsy
      expect(numkeySet.contains(Double.NaN)).toBeFalsy

      nummp.put(-0.0, 2)
      expect(numkeySet.contains(+0.0)).toBeTruthy
      expect(numkeySet.contains(-0.0)).toBeTruthy
      expect(numkeySet.contains(Double.NaN)).toBeFalsy

      nummp.put(Double.NaN, 3)
      expect(numkeySet.contains(+0.0)).toBeTruthy
      expect(numkeySet.contains(-0.0)).toBeTruthy
      expect(numkeySet.contains(Double.NaN)).toBeTruthy

      val hm1 = mu.HashMap(
        1.0 -> null,
        2.0 -> 2.0)
      val hm2 = mu.HashMap(
        (null: Any) -> 1.0,
        2.0 -> 2.0)
      val hm3 = mu.HashMap(
        (null: Any) -> null,
        2.0 -> 2.0)

      expect(new SimpleQueryableMap(hm1).keySet.contains(1.0)).toBeTruthy
      expect(new SimpleQueryableMap(hm2).keySet.contains(1.0)).toBeFalsy
      expect(new SimpleQueryableMap(hm3).keySet.contains(1.0)).toBeFalsy

      expect(new SimpleQueryableMap(hm1).keySet.contains(null)).toBeFalsy
      expect(new SimpleQueryableMap(hm2).keySet.contains(null)).toBeTruthy
      expect(new SimpleQueryableMap(hm3).keySet.contains(null)).toBeTruthy
    }

    it("keySet should side effect clear/remove/retain on the related map") {
      val mp = mapFactory.empty[String, String]

      mp.put("ONE", "one")
      mp.put("TWO", "two")

      val keySet = mp.keySet
      expect(keySet.isEmpty).toBeFalsy
      expect(mp.isEmpty).toBeFalsy

      keySet.clear()

      expect(keySet.isEmpty).toBeTruthy

      expect(mp.isEmpty).toBeTruthy

      mp.put("ONE", "one")
      mp.put("TWO", "two")

      expect(mp.containsKey("ONE")).toBeTruthy

      keySet.remove("ONE")

      expect(mp.containsKey("ONE")).toBeFalsy

      mp.put("ONE", "one")
      mp.put("THREE", "three")

      expect(mp.containsKey("ONE")).toBeTruthy
      expect(mp.containsKey("TWO")).toBeTruthy
      expect(mp.containsKey("THREE")).toBeTruthy

      keySet.removeAll(asJavaCollection(List("ONE", "TWO")))

      expect(mp.containsKey("ONE")).toBeFalsy
      expect(mp.containsKey("TWO")).toBeFalsy
      expect(mp.containsKey("THREE")).toBeTruthy

      mp.put("ONE", "one")
      mp.put("TWO", "two")
      mp.put("THREE", "three")

      expect(mp.containsKey("ONE")).toBeTruthy
      expect(mp.containsKey("TWO")).toBeTruthy
      expect(mp.containsKey("THREE")).toBeTruthy

      keySet.retainAll(asJavaCollection(List("ONE", "TWO")))

      expect(mp.containsKey("ONE")).toBeTruthy
      expect(mp.containsKey("TWO")).toBeTruthy
      expect(mp.containsKey("THREE")).toBeFalsy
    }

  }
}

object MapFactory {
  def allFactories: Iterator[MapFactory] =
    HashMapFactory.allFactories ++ SortedMapFactory.allFactories ++ ConcurrentMapFactory.allFactories
}

trait MapFactory {
  def implementationName: String

  def empty[K, V]: ju.Map[K, V]

  def allowsNullKeys: Boolean

  def allowsNullValues: Boolean
}
