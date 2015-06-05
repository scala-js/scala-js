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

import scala.scalajs.runtime.UndefinedBehaviorError

import java.util.PriorityQueue
import java.util.Comparator

object PriorityQueueTest extends JasmineTest {

  describe("java.util.PriorityQueue") {

    it("should store and remove ordered integers") {
      val pq = new PriorityQueue[Int]()

      expect(pq.size()).toEqual(0)
      expect(pq.add(111)).toBeTruthy
      expect(pq.size()).toEqual(1)
      expect(pq.add(222)).toBeTruthy
      expect(pq.size()).toEqual(2)
      expect(pq.poll).toEqual(111)
      expect(pq.size()).toEqual(1)
      expect(pq.poll).toEqual(222)
      expect(pq.add(222)).toBeTruthy
      expect(pq.add(222)).toBeTruthy
      expect(pq.remove(222)).toBeTruthy
      expect(pq.remove(222)).toBeTruthy
      expect(pq.remove(222)).toBeFalsy
    }

    it("should store and remove ordered strings") {
      val pq = new PriorityQueue[String]()

      expect(pq.size()).toEqual(0)
      expect(pq.add("aaa")).toBeTruthy
      expect(pq.size()).toEqual(1)
      expect(pq.add("bbb")).toBeTruthy
      expect(pq.size()).toEqual(2)
      expect(pq.poll).toEqual("aaa")
      expect(pq.size()).toEqual(1)
      expect(pq.poll).toEqual("bbb")
      expect(pq.add("bbb")).toBeTruthy
      expect(pq.add("bbb")).toBeTruthy
      expect(pq.remove("bbb")).toBeTruthy
      expect(pq.remove("bbb")).toBeTruthy
      expect(pq.remove("bbb")).toBeFalsy
      expect(pq.poll).toBeNull
    }

    it("should store objects with custom comparables") {
      case class Rect(x: Int, y: Int) 

      val areaComp = new Comparator[Rect] {
        def compare(a: Rect, b: Rect): Int = (a.x*a.y) - (b.x*b.y)
      }

      val pq = new PriorityQueue[Rect](11, areaComp)

      expect(pq.add(Rect(1,2))).toBeTruthy
      expect(pq.add(Rect(2,3))).toBeTruthy
      expect(pq.add(Rect(1,3))).toBeTruthy

      val first = pq.poll()
      expect(first.x).toEqual(1)
      expect(first.y).toEqual(2)

      expect(pq.remove(first)).toBeFalsy

      val second = pq.peek()
      expect(second.x).toEqual(1)
      expect(second.y).toEqual(3)

      expect(pq.remove(second)).toBeTruthy
      expect(pq.remove(second)).toBeFalsy

      val third = pq.peek()
      expect(third.x).toEqual(2)
      expect(third.y).toEqual(3)

      expect(pq.remove(third)).toBeTruthy
      expect(pq.remove(third)).toBeFalsy

      expect(pq.isEmpty).toBeTruthy

      expect(pq.peek() eq null).toBeTruthy
      expect(pq.poll() eq null).toBeTruthy
    }

    it("should store ordered Double even in corner cases") {
      val pq = new PriorityQueue[Double]()

      expect(pq.add(1.0)).toBeTruthy
      expect(pq.add(+0.0)).toBeTruthy
      expect(pq.add(-0.0)).toBeTruthy
      expect(pq.add(Double.NaN)).toBeTruthy

      expect(pq.poll.equals(-0.0)).toBeTruthy

      expect(pq.poll.equals(+0.0)).toBeTruthy

      expect(pq.poll.equals(1.0)).toBeTruthy

      expect(pq.peek.isNaN).toBeTruthy

      expect(pq.remove(Double.NaN)).toBeTruthy

      expect(pq.isEmpty).toBeTruthy
    }

    it("could be instantiated with a prepopulated Collection") {
      val l = asJavaCollection(Set(1, 5, 2, 3, 4))
      val pq = new PriorityQueue[Int](l)

      expect(pq.size()).toEqual(5)
      for (i <- 1 to 5) {
        expect(pq.poll).toEqual(i)
      }
      expect(pq.isEmpty).toBeTruthy
    }

    it("could be instantiated with a prepopulated PriorityQueue") {
      val l = asJavaCollection(Set(1, 5, 2, 3, 4))
      val pq1 = new PriorityQueue[Int](l)
      val pq2 = new PriorityQueue[Int](pq1)

      expect(pq1.size()).toEqual(5)
      expect(pq2.size()).toEqual(5)
      for (i <- 1 to 5) {
        expect(pq1.poll).toEqual(pq2.poll)
      }
      expect(pq1.isEmpty).toBeTruthy
      expect(pq2.isEmpty).toBeTruthy
    }

    it("could be instantiated with a prepopulated SortedSet") {
      val l = asJavaCollection(Set(1, 5, 2, 3, 4))
      val ss = new java.util.concurrent.ConcurrentSkipListSet[Int](l)
      val pq1 = new PriorityQueue[Int](l)
      val pq2 = new PriorityQueue[Int](ss)

      expect(pq1.size()).toEqual(5)
      expect(pq2.size()).toEqual(5)
      for (i <- 1 to 5) {
        expect(pq1.poll).toEqual(pq2.poll)
      }
      expect(pq1.isEmpty).toBeTruthy
      expect(pq2.isEmpty).toBeTruthy
    }

    it("should be cleared in a single operation") {
      val l = asJavaCollection(Set(1, 5, 2, 3, 4))
      val pq = new PriorityQueue[Int](l)

      expect(pq.size()).toEqual(5)
      pq.clear()
      expect(pq.size()).toEqual(0)
    }

    it("should add multiple elemnt in one operation") {
      val l = asJavaCollection(Set(1, 5, 2, 3, 4))
      val pq = new PriorityQueue[Int]()

      expect(pq.size()).toEqual(0)
      pq.addAll(l)
      expect(pq.size()).toEqual(5)
      pq.add(6)
      expect(pq.size()).toEqual(6)
    }

    it("should check contained values even in double corner cases") {
      val pq = new PriorityQueue[Double]()

      expect(pq.add(11111.0)).toBeTruthy
      expect(pq.size()).toEqual(1)
      expect(pq.contains(11111.0)).toBeTruthy
      expect(pq.iterator.next()).toEqual(11111.0)

      expect(pq.add(Double.NaN)).toBeTruthy
      expect(pq.size()).toEqual(2)
      expect(pq.contains(Double.NaN)).toBeTruthy
      expect(pq.contains(+0.0)).toBeFalsy
      expect(pq.contains(-0.0)).toBeFalsy

      expect(pq.remove(Double.NaN)).toBeTruthy
      expect(pq.add(+0.0)).toBeTruthy
      expect(pq.size()).toEqual(2)
      expect(pq.contains(Double.NaN)).toBeFalsy
      expect(pq.contains(+0.0)).toBeTruthy
      expect(pq.contains(-0.0)).toBeFalsy

      expect(pq.remove(+0.0)).toBeTruthy
      expect(pq.add(-0.0)).toBeTruthy
      expect(pq.size()).toEqual(2)
      expect(pq.contains(Double.NaN)).toBeFalsy
      expect(pq.contains(+0.0)).toBeFalsy
      expect(pq.contains(-0.0)).toBeTruthy

      expect(pq.add(+0.0)).toBeTruthy
      expect(pq.add(Double.NaN)).toBeTruthy
      expect(pq.contains(Double.NaN)).toBeTruthy
      expect(pq.contains(+0.0)).toBeTruthy
      expect(pq.contains(-0.0)).toBeTruthy
    }

    it("should retrieve the first(ordered) element") {
      val pqInt = new PriorityQueue[Int]()

      expect(pqInt.add(1000)).toBeTruthy
      expect(pqInt.add(10)).toBeTruthy
      expect(pqInt.poll).toEqual(10)

      val pqString = new PriorityQueue[String]()

      expect(pqString.add("pluto")).toBeTruthy
      expect(pqString.add("pippo")).toBeTruthy
      expect(pqString.poll).toEqual("pippo")

      val pqDouble = new PriorityQueue[Double]()

      expect(pqDouble.add(+10000.987)).toBeTruthy
      expect(pqDouble.add(-0.987)).toBeTruthy
      expect(pqDouble.poll).toEqual(-0.987)
    }
  }

}
