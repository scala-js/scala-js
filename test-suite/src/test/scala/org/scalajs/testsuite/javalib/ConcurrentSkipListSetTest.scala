/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib

import java.util

import scala.language.implicitConversions

import scala.collection.JavaConversions._
import scala.collection.mutable

import org.scalajs.jasminetest.JasmineTest

import scala.scalajs.runtime.UndefinedBehaviorError

import java.{util => ju}
import ju.concurrent.ConcurrentSkipListSet

object ConcurrentSkipListSetTest extends JasmineTest {

  describe("java.util.concurrent.ConcurrentSkipListSet") {

    it("should store and remove ordered integers") {
      val csls = new ConcurrentSkipListSet[Int]()

      expect(csls.size()).toEqual(0)
      expect(csls.add(222)).toBeTruthy
      expect(csls.size()).toEqual(1)
      expect(csls.add(111)).toBeTruthy
      expect(csls.size()).toEqual(2)
      expect(csls.first).toEqual(111)
      expect(csls.remove(111)).toBeTruthy

      expect(csls.size()).toEqual(1)
      expect(csls.first).toEqual(222)

      expect(csls.remove(222)).toBeTruthy
      expect(csls.size()).toEqual(0)
      expect(csls.isEmpty).toBeTruthy
      expect(csls.remove(333)).toBeFalsy
      expect {
        try {
          csls.first
          false
        } catch {
          case _: NoSuchElementException => true
          case _: Throwable => false
        }
      }.toBeTruthy
    }

    it("should store and remove ordered strings") {
      val csls = new ConcurrentSkipListSet[String]()

      expect(csls.size()).toEqual(0)
      expect(csls.add("222")).toBeTruthy
      expect(csls.size()).toEqual(1)
      expect(csls.add("111")).toBeTruthy
      expect(csls.size()).toEqual(2)
      expect(csls.first).toEqual("111")
      expect(csls.remove("111")).toBeTruthy

      expect(csls.size()).toEqual(1)
      expect(csls.first).toEqual("222")

      expect(csls.remove("222")).toBeTruthy
      expect(csls.size()).toEqual(0)
      expect(csls.remove("333")).toBeFalsy
      expect(csls.isEmpty).toBeTruthy
    }

    when("compliant-asinstanceofs").
    it("should throw exception on non comparable objects") {
      case class TestObj(num: Int)

      val csls = new ConcurrentSkipListSet[TestObj]()

      expect(csls.size()).toEqual(0)
      expect(csls.add(TestObj(222))).toBeTruthy
      expect(csls.size()).toEqual(1)
      expect {
        try {
          csls.add(TestObj(111))
          false
        } catch {
          case _: ClassCastException => true
          case _: Throwable => false
        }
      }.toBeTruthy
      expect(csls.comparator eq null).toBeTruthy
    }

    it("should store objects with custom comparables") {
      case class Rect(x: Int, y: Int)

      val areaComp = new ju.Comparator[Rect] {
        def compare(a: Rect, b: Rect): Int = (a.x*a.y) - (b.x*b.y)
      }

      val csls = new ConcurrentSkipListSet[Rect](areaComp)

      expect(csls.add(Rect(1,2))).toBeTruthy
      expect(csls.add(Rect(2,3))).toBeTruthy
      expect(csls.add(Rect(1,3))).toBeTruthy

      val first = csls.first()
      expect(first.x).toEqual(1)
      expect(first.y).toEqual(2)

      expect(csls.remove(first)).toBeTruthy
      expect(csls.remove(first)).toBeFalsy

      val second = csls.first()
      expect(second.x).toEqual(1)
      expect(second.y).toEqual(3)

      expect(csls.remove(second)).toBeTruthy

      val third = csls.first()
      expect(third.x).toEqual(2)
      expect(third.y).toEqual(3)

      expect(csls.remove(third)).toBeTruthy

      expect(csls.isEmpty).toBeTruthy
    }

    it("should store ordered Double even in corner cases") {
      val csls = new ConcurrentSkipListSet[Double]()

      expect(csls.add(1.0)).toBeTruthy
      expect(csls.add(+0.0)).toBeTruthy
      expect(csls.add(-0.0)).toBeTruthy
      expect(csls.add(Double.NaN)).toBeTruthy

      expect(csls.first.equals(-0.0)).toBeTruthy

      expect(csls.remove(-0.0)).toBeTruthy

      expect(csls.first.equals(+0.0)).toBeTruthy

      expect(csls.remove(+0.0)).toBeTruthy

      expect(csls.first.equals(1.0)).toBeTruthy

      expect(csls.remove(1.0)).toBeTruthy

      expect(csls.first.isNaN).toBeTruthy

      expect(csls.remove(Double.NaN)).toBeTruthy

      expect(csls.isEmpty).toBeTruthy
    }

    it("could be instantiated with a prepopulated Collection") {
      val l = asJavaCollection(Set(1, 5, 2, 3, 4))
      val csls = new ConcurrentSkipListSet[Int](l)

      expect(csls.size()).toEqual(5)
      for (i <- 1 to 5) {
        expect(csls.first).toEqual(i)
        expect(csls.remove(i)).toBeTruthy
      }
      expect(csls.isEmpty).toBeTruthy
    }

    it("should be cleared in a single operation") {
      val l = asJavaCollection(Set(1, 5, 2, 3, 4))
      val csls = new ConcurrentSkipListSet[Int](l)

      expect(csls.size()).toEqual(5)
      csls.clear()
      expect(csls.size()).toEqual(0)
    }

    it("should add multiple elemnt in one operation") {
      val l = asJavaCollection(Set(1, 5, 2, 3, 4))
      val csls = new ConcurrentSkipListSet[Int]()

      expect(csls.size()).toEqual(0)
      csls.addAll(l)
      expect(csls.size()).toEqual(5)
      csls.add(6)
      expect(csls.size()).toEqual(6)
    }

    it("should check contained values even in double corner cases") {
      val csls = new ConcurrentSkipListSet[Double]()

      expect(csls.add(11111.0)).toBeTruthy
      expect(csls.size()).toEqual(1)
      expect(csls.contains(11111.0)).toBeTruthy
      expect(csls.iterator.next()).toEqual(11111.0)

      expect(csls.add(Double.NaN)).toBeTruthy
      expect(csls.size()).toEqual(2)
      expect(csls.contains(Double.NaN)).toBeTruthy
      expect(csls.contains(+0.0)).toBeFalsy
      expect(csls.contains(-0.0)).toBeFalsy

      expect(csls.remove(Double.NaN)).toBeTruthy
      expect(csls.add(+0.0)).toBeTruthy
      expect(csls.size()).toEqual(2)
      expect(csls.contains(Double.NaN)).toBeFalsy
      expect(csls.contains(+0.0)).toBeTruthy
      expect(csls.contains(-0.0)).toBeFalsy

      expect(csls.remove(+0.0)).toBeTruthy
      expect(csls.add(-0.0)).toBeTruthy
      expect(csls.size()).toEqual(2)
      expect(csls.contains(Double.NaN)).toBeFalsy
      expect(csls.contains(+0.0)).toBeFalsy
      expect(csls.contains(-0.0)).toBeTruthy

      expect(csls.add(+0.0)).toBeTruthy
      expect(csls.add(Double.NaN)).toBeTruthy
      expect(csls.contains(Double.NaN)).toBeTruthy
      expect(csls.contains(+0.0)).toBeTruthy
      expect(csls.contains(-0.0)).toBeTruthy
    }

    it("should retrieve the first(ordered) element") {
      val cslsInt = new ConcurrentSkipListSet[Int]()

      expect(cslsInt.add(1000)).toBeTruthy
      expect(cslsInt.add(10)).toBeTruthy
      expect(cslsInt.first).toEqual(10)

      val cslsString = new ConcurrentSkipListSet[String]()

      expect(cslsString.add("pluto")).toBeTruthy
      expect(cslsString.add("pippo")).toBeTruthy
      expect(cslsString.first).toEqual("pippo")

      val cslsDouble = new ConcurrentSkipListSet[Double]()

      expect(cslsDouble.add(+10000.987)).toBeTruthy
      expect(cslsDouble.add(-0.987)).toBeTruthy
      expect(cslsDouble.first).toEqual(-0.987)
    }

    it("should retrieve the last(ordered) element") {
      val cslsInt = new ConcurrentSkipListSet[Int]()

      expect(cslsInt.add(1000)).toBeTruthy
      expect(cslsInt.add(10)).toBeTruthy
      expect(cslsInt.last).toEqual(1000)

      val cslsString = new ConcurrentSkipListSet[String]()

      expect(cslsString.add("pluto")).toBeTruthy
      expect(cslsString.add("pippo")).toBeTruthy
      expect(cslsString.last).toEqual("pluto")

      val cslsDouble = new ConcurrentSkipListSet[Double]()

      expect(cslsDouble.add(+10000.987)).toBeTruthy
      expect(cslsDouble.add(-0.987)).toBeTruthy
      expect(cslsDouble.last).toEqual(10000.987)
    }

    it("should retrieve ceiling(ordered) elements") {
      val lInt = asJavaCollection(Set(1, 5, 2, 3, 4))
      val cslsInt = new ConcurrentSkipListSet[Int](lInt)

      expect(cslsInt.ceiling(-10)).toEqual(1)
      expect(cslsInt.ceiling(0)).toEqual(1)
      expect(cslsInt.ceiling(1)).toEqual(1)
      expect(cslsInt.ceiling(5)).toEqual(5)

      val lString = asJavaCollection(Set("a", "e", "b", "c", "d"))
      val cslsString = new ConcurrentSkipListSet[String](lString)

      expect(cslsString.ceiling("00000")).toEqual("a")
      expect(cslsString.ceiling("0")).toEqual("a")
      expect(cslsString.ceiling("a")).toEqual("a")
      expect(cslsString.ceiling("d")).toEqual("d")
      expect(cslsString.ceiling("z")).toEqual(null)
    }

    it("should retrieve floor(ordered) elements") {
      val lInt = asJavaCollection(Set(1, 5, 2, 3, 4))
      val cslsInt = new ConcurrentSkipListSet[Int](lInt)

      expect(cslsInt.floor(10)).toEqual(5)
      expect(cslsInt.floor(5)).toEqual(5)
      expect(cslsInt.floor(3)).toEqual(3)
      expect(cslsInt.floor(1)).toEqual(1)

      val lString = asJavaCollection(Set("a", "e", "b", "c", "d"))
      val cslsString = new ConcurrentSkipListSet[String](lString)

      expect(cslsString.floor("zzzzz")).toEqual("e")
      expect(cslsString.floor("d")).toEqual("d")
      expect(cslsString.floor("b")).toEqual("b")
      expect(cslsString.floor("a")).toEqual("a")
      expect(cslsString.floor("0")).toEqual(null)
    }

    it("should retrieve higher(ordered) elements") {
      val lInt = asJavaCollection(Set(1, 5, 2, 3, 4))
      val cslsInt = new ConcurrentSkipListSet[Int](lInt)

      expect(cslsInt.higher(4)).toEqual(5)
      expect(cslsInt.higher(3)).toEqual(4)
      expect(cslsInt.higher(1)).toEqual(2)
      expect(cslsInt.higher(-10)).toEqual(1)

      val lString = asJavaCollection(Set("a", "e", "b", "c", "d"))
      val cslsString = new ConcurrentSkipListSet[String](lString)

      expect(cslsString.higher("zzzzz")).toEqual(null)
      expect(cslsString.higher("d")).toEqual("e")
      expect(cslsString.higher("b")).toEqual("c")
      expect(cslsString.higher("a")).toEqual("b")
      expect(cslsString.higher("0")).toEqual("a")
    }

    it("should retrieve lower(ordered) elements") {
      val lInt = asJavaCollection(Set(1, 5, 2, 3, 4))
      val cslsInt = new ConcurrentSkipListSet[Int](lInt)

      expect(cslsInt.lower(5)).toEqual(4)
      expect(cslsInt.lower(4)).toEqual(3)
      expect(cslsInt.lower(3)).toEqual(2)
      expect(cslsInt.lower(10)).toEqual(5)

      val lString = asJavaCollection(Set("a", "e", "b", "c", "d"))
      val cslsString = new ConcurrentSkipListSet[String](lString)

      expect(cslsString.lower("zzzzz")).toEqual("e")
      expect(cslsString.lower("d")).toEqual("c")
      expect(cslsString.lower("b")).toEqual("a")
      expect(cslsString.lower("a")).toEqual(null)
      expect(cslsString.lower("0")).toEqual(null)
    }

    it("should poll first and last elements") {
      val l = asJavaCollection(Set(1, 5, 2, 3, 4))
      val csls = new ConcurrentSkipListSet[Int](l)

      expect(csls.contains(1)).toBeTruthy
      expect(csls.pollFirst()).toEqual(1)
      expect(csls.contains(1)).toBeFalsy
      expect(csls.pollLast()).toEqual(5)
      expect(csls.pollFirst()).toEqual(2)
      expect(csls.pollLast()).toEqual(4)
      expect(csls.pollFirst()).toEqual(3)
      expect(csls.isEmpty()).toBeTruthy
    }

    it("should get partial views that are backed on the original list") {
      val l = asJavaCollection(Set(1, 5, 2, 3, 4))
      val csls = new ConcurrentSkipListSet[Int](l)

      val hs1 = csls.headSet(3)
      val l1 = asJavaCollection(Set(1,2))
      expect(hs1.containsAll(l1)).toBeTruthy
      expect(hs1.removeAll(l1)).toBeTruthy
      expect(hs1.isEmpty).toBeTruthy
      expect(csls.size).toEqual(3)
      expect(csls.containsAll(asJavaCollection(Set(3,4,5)))).toBeTruthy

      csls.addAll(l)

      val hs2 = csls.headSet(3, true)
      val l2 = asJavaCollection(Set(1,2,3))
      expect(hs2.containsAll(l2)).toBeTruthy
      expect(hs2.removeAll(l2)).toBeTruthy
      expect(hs2.isEmpty).toBeTruthy
      expect(csls.size).toEqual(2)
      expect(csls.containsAll(asJavaCollection(Set(4,5)))).toBeTruthy

      csls.addAll(l)

      val ts1 = csls.tailSet(3)
      val l3 = asJavaCollection(Set(3,4,5))
      expect(ts1.containsAll(l3)).toBeTruthy
      expect(ts1.removeAll(l3)).toBeTruthy
      expect(ts1.isEmpty).toBeTruthy
      expect(csls.size).toEqual(2)
      expect(csls.containsAll(asJavaCollection(Set(1,2)))).toBeTruthy

      csls.addAll(l)

      val ts2 = csls.tailSet(3, false)
      val l4 = asJavaCollection(Set(4,5))
      expect(ts2.containsAll(l4)).toBeTruthy
      expect(ts2.removeAll(l4)).toBeTruthy
      expect(ts2.isEmpty).toBeTruthy
      expect(csls.size).toEqual(3)
      expect(csls.containsAll(asJavaCollection(Set(1,2,3)))).toBeTruthy

      csls.addAll(l)

      val ss1 = csls.subSet(2, true, 3, true)
      val l5 = asJavaCollection(Set(2,3))
      expect(ss1.containsAll(l5)).toBeTruthy
      expect(ss1.removeAll(l5)).toBeTruthy
      expect(ss1.isEmpty).toBeTruthy
      expect(csls.size).toEqual(3)
      expect(csls.containsAll(asJavaCollection(Set(1,4,5)))).toBeTruthy

      csls.addAll(l)

      val ss2 = csls.subSet(1, false, 4, false)
      expect(ss2.containsAll(l5)).toBeTruthy
      expect(ss2.removeAll(l5)).toBeTruthy
      expect(ss2.isEmpty).toBeTruthy
      expect(csls.size).toEqual(3)
      expect(csls.containsAll(asJavaCollection(Set(1,4,5)))).toBeTruthy
    }

  }

}

object ConcurrentSkipListSetFactory extends ConcurrentSkipListSetFactory {
  def allFactories: Iterator[ConcurrentSkipListSetFactory] =
    Iterator(new ConcurrentSkipListSetFactory)
}

class ConcurrentSkipListSetFactory extends NavigableSetFactory {
  def implementationName: String =
    "java.util.concurrent.ConcurrentSkipListSet"

  def empty[E]: ju.concurrent.ConcurrentSkipListSet[E] =
    new ConcurrentSkipListSet[E]

  override def allowsNullElement: Boolean = false
}
