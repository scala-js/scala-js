/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib

import java.{util => ju}

import scala.collection.JavaConversions._
import scala.collection.mutable

abstract class AbstractMapTest[F <: AbstractMapFactory](protected val mapFactory: F) extends MapTest[F] {

  describe(mapFactory.implementationName) {
    testApi()
  }

  protected def testApi(): Unit = {

    testMapApi()

    case class SimpleQueryableMap[K, V](inner: mutable.HashMap[K, V])
        extends ju.AbstractMap[K, V] {

      def entrySet(): java.util.Set[java.util.Map.Entry[K, V]] = {
        setAsJavaSet(inner.map {
          case (k, v) => new ju.AbstractMap.SimpleImmutableEntry(k, v)
        }.toSet)
      }
    }

    describe(s"${mapFactory.implementationName}.values") {

      it("should mirror the related map size") {
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

        expect(SimpleQueryableMap(hm1).values.size).toEqual(2)
        expect(SimpleQueryableMap(hm2).values.size).toEqual(2)
        expect(SimpleQueryableMap(hm3).values.size).toEqual(2)
        expect(SimpleQueryableMap(hm4).values.size).toEqual(2)
      }

      it("should check single and multiple objects presence") {
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

        val hm1 = mutable.HashMap(
          1.0 -> null,
          2.0 -> 2.0)
        val hm2 = mutable.HashMap(
          (null: Any) -> 1.0,
          2.0 -> 2.0)
        val hm3 = mutable.HashMap(
          (null: Any) -> null,
          2.0 -> 2.0)

        expect(SimpleQueryableMap(hm1).values.contains(1.0)).toBeFalsy
        expect(SimpleQueryableMap(hm2).values.contains(1.0)).toBeTruthy
        expect(SimpleQueryableMap(hm3).values.contains(1.0)).toBeFalsy

        expect(SimpleQueryableMap(hm1).values.contains(null)).toBeTruthy
        expect(SimpleQueryableMap(hm2).values.contains(null)).toBeFalsy
        expect(SimpleQueryableMap(hm3).values.contains(null)).toBeTruthy
      }

      it("should side effect clear/remove/retain on the related map") {
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

    }

    describe(s"${mapFactory.implementationName}.keySet") {

      it("should mirror the related map size") {
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

        expect(SimpleQueryableMap(hm1).keySet.size).toEqual(2)
        expect(SimpleQueryableMap(hm2).keySet.size).toEqual(2)
        expect(SimpleQueryableMap(hm3).keySet.size).toEqual(2)
        expect(SimpleQueryableMap(hm4).keySet.size).toEqual(2)
      }

      it("should check single and multiple objects presence") {
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

        val hm1 = mutable.HashMap(
          1.0 -> null,
          2.0 -> 2.0)
        val hm2 = mutable.HashMap(
          (null: Any) -> 1.0,
          2.0 -> 2.0)
        val hm3 = mutable.HashMap(
          (null: Any) -> null,
          2.0 -> 2.0)

        expect(SimpleQueryableMap(hm1).keySet.contains(1.0)).toBeTruthy
        expect(SimpleQueryableMap(hm2).keySet.contains(1.0)).toBeFalsy
        expect(SimpleQueryableMap(hm3).keySet.contains(1.0)).toBeFalsy

        expect(SimpleQueryableMap(hm1).keySet.contains(null)).toBeFalsy
        expect(SimpleQueryableMap(hm2).keySet.contains(null)).toBeTruthy
        expect(SimpleQueryableMap(hm3).keySet.contains(null)).toBeTruthy
      }

      it("should side effect clear/remove/retain on the related map") {
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

}

abstract class AbstractMapFactory extends MapFactory {
  def implementationName: String

  def empty[K, V]: ju.AbstractMap[K, V]
}
