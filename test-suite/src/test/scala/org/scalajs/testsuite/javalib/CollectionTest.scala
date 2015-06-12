/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib

import java.{util => ju}

import org.scalajs.jasminetest.JasmineTest
import scala.collection.JavaConversions._

trait CollectionTest[F <: CollectionFactory] extends JasmineTest {

  def factory: F

  def testCollectionApi(): Unit = {
    it("should store strings") {
      val coll = factory.empty[String]

      expect(coll.size()).toEqual(0)
      coll.add("one")
      expect(coll.size()).toEqual(1)

      coll.clear()
      expect(coll.size()).toEqual(0)
      expect(coll.addAll(Seq.empty[String])).toBeFalsy
      expect(coll.size()).toEqual(0)

      expect(coll.addAll(Seq("one"))).toBeTruthy
      expect(coll.size()).toEqual(1)

      coll.clear()
      expect(coll.addAll(Seq("one", "two", "one"))).toBeTruthy
      expect(coll.size() >= 1).toBeTruthy
    }

    it("should store integers") {
      val coll = factory.empty[Int]

      expect(coll.size()).toEqual(0)
      coll.add(1)
      expect(coll.size()).toEqual(1)

      coll.clear()
      expect(coll.size()).toEqual(0)
      expect(coll.addAll(Seq.empty[Int])).toBeFalsy
      expect(coll.size()).toEqual(0)

      expect(coll.addAll(Seq(1))).toBeTruthy
      expect(coll.size()).toEqual(1)

      coll.clear()
      expect(coll.addAll(Seq(1, 2, 1))).toBeTruthy
      expect(coll.size() >= 1).toBeTruthy
    }

    it("should store doubles") {
      val coll = factory.empty[Double]

      expect(coll.size()).toEqual(0)
      coll.add(1.234)
      expect(coll.size()).toEqual(1)

      coll.clear()
      expect(coll.size()).toEqual(0)
      expect(coll.addAll(Seq.empty[Double])).toBeFalsy
      expect(coll.size()).toEqual(0)

      expect(coll.addAll(Seq(1.234))).toBeTruthy
      expect(coll.size()).toEqual(1)

      coll.clear()
      expect(coll.addAll(Seq(1.234, 2.345, 1.234))).toBeTruthy
      expect(coll.size() >= 1).toBeTruthy

      coll.clear()
      coll.add(+0.0)
      expect(coll.contains(+0.0)).toBeTruthy
      expect(coll.contains(-0.0)).toBeFalsy

      coll.clear()
      coll.add(-0.0)
      expect(coll.contains(+0.0)).toBeFalsy
      expect(coll.contains(-0.0)).toBeTruthy

      coll.clear()
      coll.add(Double.NaN)
      expect(coll.size()).toEqual(1)
      expect(coll.contains(Double.NaN)).toBeTruthy
    }

    it("should store custom objects") {
      case class TestObj(num: Int)

      val coll = factory.empty[TestObj]

      coll.add(TestObj(100))
      expect(coll.size()).toEqual(1)
      expect(coll.contains(TestObj(100))).toBeTruthy
      expect(coll.contains(TestObj(200))).toBeFalsy
    }

    it("should remove stored elements") {
      val coll = factory.empty[String]

      coll.add("one")
      coll.add("two")
      coll.add("three")
      coll.add("two")

      val initialSize = coll.size()
      expect(coll.remove("four")).toBeFalsy
      expect(coll.size()).toEqual(initialSize)
      expect(coll.remove("two")).toBeTruthy
      expect(coll.size()).toEqual(initialSize - 1)
      expect(coll.remove("one")).toBeTruthy
      expect(coll.size()).toEqual(initialSize - 2)
    }

    it("should remove stored elements on double corner cases") {
      val coll = factory.empty[Double]

      coll.add(1.234)
      coll.add(2.345)
      coll.add(Double.NaN)
      coll.add(+0.0)
      coll.add(-0.0)

      // coll == ArrayCollection(1.234, 2.345, NaN, +0.0, -0.0)
      expect(coll.remove(Double.NaN)).toBeTruthy
      // coll == ArrayCollection(1.234, 2.345, +0.0, -0.0)
      expect(coll.size()).toEqual(4)
      expect(coll.remove(2.345)).toBeTruthy
      // coll == ArrayCollection(1.234, +0.0, -0.0)
      expect(coll.size()).toEqual(3)
      expect(coll.remove(1.234)).toBeTruthy
      // coll == ArrayCollection(+0.0, -0.0)
      expect(coll.size()).toEqual(2)
      expect(coll.remove(-0.0)).toBeTruthy
      // coll == ArrayCollection(NaN, +0.0)
      expect(coll.size()).toEqual(1)

      coll.clear()

      expect(coll.isEmpty).toBeTruthy
    }

    it("should be cleared with one operation") {
      val coll = factory.empty[String]

      coll.add("one")
      coll.add("two")
      expect(coll.size).toEqual(2)
      coll.clear()
      expect(coll.size).toEqual(0)
    }

    it("should check contained presence") {
      val coll = factory.empty[String]

      coll.add("one")
      expect(coll.contains("one")).toBeTruthy
      expect(coll.contains("two")).toBeFalsy
      expect(coll.contains(null)).toBeFalsy
    }

    it("should check contained presence for double corner cases") {
      val coll = factory.empty[Double]

      coll.add(-0.0)
      expect(coll.contains(-0.0)).toBeTruthy
      expect(coll.contains(+0.0)).toBeFalsy

      coll.clear()

      coll.add(+0.0)
      expect(coll.contains(-0.0)).toBeFalsy
      expect(coll.contains(+0.0)).toBeTruthy
    }

    it("should give proper iterator over elements") {
      val coll = factory.empty[String]
      coll.add("one")
      coll.add("two")
      coll.add("three")
      coll.add("three")
      coll.add("three")

      expect(Set("one", "two", "three") == coll.iterator().toSet).toBeTruthy
    }

  }
}

trait CollectionFactory {
  def implementationName: String
  def empty[E]: ju.Collection[E]
}
