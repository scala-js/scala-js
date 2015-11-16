/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import java.util

import scala.language.implicitConversions

import scala.collection.JavaConversions._
import scala.collection.mutable

import org.scalajs.jasminetest.JasmineTest

import scala.scalajs.runtime.UndefinedBehaviorError

import java.{util => ju}
import ju.TreeSet
import ju.Comparator


object TreeSetTest extends TreeSetTest(new TreeSetFactory) {

  override def testApi(): Unit = {
    super.testApi()

    it("should check that comparator is always null") {
      val ts1 = factory.empty[Int]

      expect(ts1.comparator() == null).toBeTruthy

      val ts2 = factory.empty[String]

      expect(ts2.comparator() == null).toBeTruthy
    }
  }
}
object TreeSetWithNullTest extends TreeSetTest(new TreeSetWithNullFactory) {

  override def testApi(): Unit = {
    super.testApi()

    it("should check that comparator is never null") {
      val ts1 = factory.empty[Int]

      expect(ts1.comparator() == null).toBeFalsy

      val ts2 = factory.empty[String]

      expect(ts2.comparator() == null).toBeFalsy
    }
  }
}

class TreeSetTest[F <: TreeSetFactory](treeSetTestFactory: F)
    extends AbstractSetTest[F](treeSetTestFactory)
    with SortedSetTest
    with NavigableSetTest {

  override def testApi(): Unit = {
    super.testApi()
    testSortedSetApi(treeSetTestFactory)
    testNavigableSetApi(treeSetTestFactory)

    it("should store and remove ordered integers") {
      val ts = treeSetTestFactory.empty[Int]

      expect(ts.size()).toEqual(0)
      expect(ts.add(222)).toBeTruthy
      expect(ts.size()).toEqual(1)
      expect(ts.add(111)).toBeTruthy
      expect(ts.size()).toEqual(2)
      expect(ts.first).toEqual(111)
      expect(ts.remove(111)).toBeTruthy

      expect(ts.size()).toEqual(1)
      expect(ts.first).toEqual(222)

      expect(ts.remove(222)).toBeTruthy
      expect(ts.size()).toEqual(0)
      expect(ts.isEmpty).toBeTruthy
      expect(ts.remove(333)).toBeFalsy
      expect {
        try {
          ts.first
          false
        } catch {
          case _: NoSuchElementException => true
          case _: Throwable => false
        }
      }.toBeTruthy

      if (treeSetTestFactory.allowsNullElement) {
        expect(ts.add(null.asInstanceOf[Int])).toBeTruthy
        expect(ts.contains(null)).toBeTruthy
        expect(ts.remove(null)).toBeTruthy
        expect(ts.contains(null)).toBeFalsy
      }
    }

    it("should store and remove ordered strings") {
      val ts = treeSetTestFactory.empty[String]

      expect(ts.size()).toEqual(0)
      expect(ts.add("222")).toBeTruthy
      expect(ts.size()).toEqual(1)
      expect(ts.add("111")).toBeTruthy
      expect(ts.size()).toEqual(2)
      expect(ts.first).toEqual("111")
      expect(ts.remove("111")).toBeTruthy

      expect(ts.size()).toEqual(1)
      expect(ts.first).toEqual("222")

      expect(ts.remove("222")).toBeTruthy
      expect(ts.size()).toEqual(0)
      expect(ts.remove("333")).toBeFalsy
      expect(ts.isEmpty).toBeTruthy

      if (treeSetTestFactory.allowsNullElement) {
        expect(ts.add(null)).toBeTruthy
        expect(ts.contains(null)).toBeTruthy
        expect(ts.remove(null)).toBeTruthy
        expect(ts.contains(null)).toBeFalsy
      }
    }

    case class TestObj(num: Int)

    when("compliant-asinstanceofs").
    it("should throw exception on non comparable objects") {
      val ts1 = treeSetTestFactory.empty[TestObj]

      expect(ts1.size()).toEqual(0)
      expectThrows[ClassCastException](ts1.add(TestObj(111)))
    }

    it("should store objects with custom comparables") {
      case class Rect(x: Int, y: Int)

      val areaComp = new ju.Comparator[Rect] {
        def compare(a: Rect, b: Rect): Int = (a.x*a.y) - (b.x*b.y)
      }

      val ts = new TreeSet[Rect](areaComp)

      expect(ts.add(Rect(1,2))).toBeTruthy
      expect(ts.add(Rect(2,3))).toBeTruthy
      expect(ts.add(Rect(1,3))).toBeTruthy

      val first = ts.first()
      expect(first.x).toEqual(1)
      expect(first.y).toEqual(2)

      expect(ts.remove(first)).toBeTruthy
      expect(ts.remove(first)).toBeFalsy

      val second = ts.first()
      expect(second.x).toEqual(1)
      expect(second.y).toEqual(3)

      expect(ts.remove(second)).toBeTruthy

      val third = ts.first()
      expect(third.x).toEqual(2)
      expect(third.y).toEqual(3)

      expect(ts.remove(third)).toBeTruthy

      expect(ts.isEmpty).toBeTruthy
    }

    it("should store ordered Double even in corner cases") {
      val ts = treeSetTestFactory.empty[Double]

      expect(ts.add(1.0)).toBeTruthy
      expect(ts.add(+0.0)).toBeTruthy
      expect(ts.add(-0.0)).toBeTruthy
      expect(ts.add(Double.NaN)).toBeTruthy

      expect(ts.first.equals(-0.0)).toBeTruthy

      expect(ts.remove(-0.0)).toBeTruthy

      expect(ts.first.equals(+0.0)).toBeTruthy

      expect(ts.remove(+0.0)).toBeTruthy

      expect(ts.first.equals(1.0)).toBeTruthy

      expect(ts.remove(1.0)).toBeTruthy

      expect(ts.first.isNaN).toBeTruthy

      expect(ts.remove(Double.NaN)).toBeTruthy

      expect(ts.isEmpty).toBeTruthy
    }

    it("could be instantiated with a prepopulated Collection") {
      val l = asJavaCollection(Set(1, 5, 2, 3, 4))
      val ts = new TreeSet[Int](l)

      expect(ts.size()).toEqual(5)
      for (i <- 1 to 5) {
        expect(ts.first).toEqual(i)
        expect(ts.remove(i)).toBeTruthy
      }
      expect(ts.isEmpty).toBeTruthy
    }

    it("should be cleared in a single operation") {
      val l = asJavaCollection(Set(1, 5, 2, 3, 4))
      val ts = treeSetTestFactory.empty[Int]

      ts.addAll(l)

      expect(ts.size()).toEqual(5)
      ts.clear()
      expect(ts.size()).toEqual(0)
    }

    it("should add multiple element in one operation") {
      val l = asJavaCollection(Set(1, 5, 2, 3, 4))
      val ts = treeSetTestFactory.empty[Int]

      expect(ts.size()).toEqual(0)
      ts.addAll(l)
      expect(ts.size()).toEqual(5)
      ts.add(6)
      expect(ts.size()).toEqual(6)
    }

    it("should check contained values even in double corner cases") {
      val ts = treeSetTestFactory.empty[Double]

      expect(ts.add(11111.0)).toBeTruthy
      expect(ts.size()).toEqual(1)
      expect(ts.contains(11111.0)).toBeTruthy
      expect(ts.iterator.next()).toEqual(11111.0)

      expect(ts.add(Double.NaN)).toBeTruthy
      expect(ts.size()).toEqual(2)
      expect(ts.contains(Double.NaN)).toBeTruthy
      expect(ts.contains(+0.0)).toBeFalsy
      expect(ts.contains(-0.0)).toBeFalsy

      expect(ts.remove(Double.NaN)).toBeTruthy
      expect(ts.add(+0.0)).toBeTruthy
      expect(ts.size()).toEqual(2)
      expect(ts.contains(Double.NaN)).toBeFalsy
      expect(ts.contains(+0.0)).toBeTruthy
      expect(ts.contains(-0.0)).toBeFalsy

      expect(ts.remove(+0.0)).toBeTruthy
      expect(ts.add(-0.0)).toBeTruthy
      expect(ts.size()).toEqual(2)
      expect(ts.contains(Double.NaN)).toBeFalsy
      expect(ts.contains(+0.0)).toBeFalsy
      expect(ts.contains(-0.0)).toBeTruthy

      expect(ts.add(+0.0)).toBeTruthy
      expect(ts.add(Double.NaN)).toBeTruthy
      expect(ts.contains(Double.NaN)).toBeTruthy
      expect(ts.contains(+0.0)).toBeTruthy
      expect(ts.contains(-0.0)).toBeTruthy
    }

    when("compliant-asinstanceofs").
    it("should throw exceptions on access outside bound on views") {
      val l = asJavaCollection(Set(2, 3, 6))
      val ts = treeSetTestFactory.empty[Int]
      ts.addAll(l)

      val hs1 = ts.headSet(5, true)
      expect(hs1.add(4)).toBeTruthy
      expect(hs1.add(5)).toBeTruthy
      expectThrows[IllegalArgumentException](hs1.add(6))

      ts.clear()
      ts.addAll(l)

      val hs2 = ts.headSet(5, false)
      expect(hs2.add(4)).toBeTruthy
      expectThrows[IllegalArgumentException](hs2.add(5))

      ts.clear()
      ts.addAll(l)

      val ts1 = ts.tailSet(1, true)
      expect(ts1.add(7)).toBeTruthy
      expect(ts1.add(1)).toBeTruthy
      expectThrows[IllegalArgumentException](ts1.add(0))

      ts.clear()
      ts.addAll(l)

      val ts2 = ts.tailSet(1, false)
      expect(ts2.add(7)).toBeTruthy
      expectThrows[IllegalArgumentException](ts2.add(1))

      ts.clear()
      ts.addAll(l)

      val ss1 = ts.subSet(1, true, 5, true)
      expect(ss1.add(4)).toBeTruthy
      expect(ss1.add(1)).toBeTruthy
      expectThrows[IllegalArgumentException](ss1.add(0))
      expect(ss1.add(5)).toBeTruthy
      expectThrows[IllegalArgumentException](ss1.add(6))

      ts.clear()
      ts.addAll(l)

      val ss2 = ts.subSet(1, false, 5, false)
      expect(ss2.add(4)).toBeTruthy
      expectThrows[IllegalArgumentException](ss2.add(1))
      expectThrows[IllegalArgumentException](ss2.add(5))
    }

    it("should throws exception in case of null elements and default ordering") {
      val hs = treeSetTestFactory.empty[String]

      expect(hs.add("ONE")).toBeTruthy
      expect(hs.contains("ONE")).toBeTruthy
      expect(hs.contains("TWO")).toBeFalsy

      if (treeSetTestFactory.allowsNullElement) {
        expect(hs.add(null)).toBeTruthy
        expect(hs.contains(null)).toBeTruthy
      } else {
        expectThrows[Exception](hs.add(null))
      }
    }

    it("should not put a whole Collection with null elements into") {
      val l = List[String]("ONE", "TWO", (null: String))
      val ts1 = treeSetTestFactory.empty[String]

      if (treeSetTestFactory.allowsNullElement) {
        expect(ts1.addAll(l)).toBeTruthy
        expect(ts1.contains(null)).toBeTruthy
        expect(ts1.contains("ONE")).toBeTruthy
        expect(ts1.contains("THREE")).toBeFalsy
      } else {
        expectThrows[Exception] {
          ts1.addAll(asJavaCollection(l))
        }
      }
    }
  }
}

object TreeSetFactory extends TreeSetFactory {
  def allFactories: Iterator[TreeSetFactory] =
    Iterator(new TreeSetFactory, new TreeSetWithNullFactory)
}

class TreeSetFactory extends AbstractSetFactory with NavigableSetFactory
    with SortedSetFactory {
  def implementationName: String =
    "java.util.TreeSet"

  def empty[E]: ju.TreeSet[E] =
    new TreeSet[E]

  override def allowsNullElement: Boolean = false

  override def allowsNullElementQuery: Boolean = false
}

class TreeSetWithNullFactory extends TreeSetFactory {

  override def implementationName: String =
    super.implementationName + " {allows null}"

  case class EvenNullComp[E]() extends Comparator[E] {
    def compare(a: E, b: E): Int =
      (Option(a), Option(b)) match {
        case (Some(e1), Some(e2)) => e1.asInstanceOf[Comparable[E]].compareTo(e2)
        case (Some(e1), None) => -1
        case (None, Some(e2)) => 1
        case (None, None) => 0
      }
    }

  override def empty[E]: ju.TreeSet[E] =
    new TreeSet[E](EvenNullComp[E]())

  override def allowsNullElement: Boolean = true

  override def allowsNullElementQuery: Boolean = true
}
