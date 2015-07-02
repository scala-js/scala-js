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

import java.util.LinkedList
import java.util.Comparator

object LinkedListTest extends LinkedListTest(new LinkedListFactory)

abstract class LinkedListTest[F <: LinkedListFactory](listFactory: F) extends AbstractListTest(listFactory) {

  override def testApi(): Unit = {

    super.testApi()

    it("add and remove properly in head and last positions") {
      val ll = new LinkedList[Int]()

      ll.addLast(1)
      ll.removeFirst()
      ll.addLast(2)
      expect(ll.peekFirst()).toEqual(2)

      ll.clear()

      ll.addFirst(1)
      ll.removeLast()
      ll.addFirst(2)
      expect(ll.peekLast()).toEqual(2)
    }

    it("could be instantiated with a prepopulated Collection") {
      val s = Seq(1, 5, 2, 3, 4)
      val l = asJavaCollection(s)
      val ll = new LinkedList[Int](l)

      expect(ll.size()).toEqual(5)

      for (i <- 0 until s.size)
        expect(ll.poll()).toEqual(s(i))

      expect(ll.isEmpty).toBeTruthy
    }

    it("should add multiple element in one operation") {
      val l = asJavaCollection(Set(1, 5, 2, 3, 4))
      val ll = new LinkedList[Int]()

      expect(ll.size()).toEqual(0)
      ll.addAll(l)
      expect(ll.size()).toEqual(5)
      ll.add(6)
      expect(ll.size()).toEqual(6)
    }

    it("could be instantiated with a prepopulated Collection") {
      val s = Seq(1, 5, 2, 3, 4)
      val l = asJavaCollection(s)
      val ll = new LinkedList[Int](l)

      expect(ll.size()).toEqual(5)

      for (i <- 0 until s.size)
        expect(ll.poll()).toEqual(s(i))

      expect(ll.isEmpty).toBeTruthy
    }

    it("should retrieve the last element") {
      val llInt = new LinkedList[Int]()

      expect(llInt.add(1000)).toBeTruthy
      expect(llInt.add(10)).toBeTruthy
      expect(llInt.pollLast()).toEqual(10)

      val llString = new LinkedList[String]()

      expect(llString.add("pluto")).toBeTruthy
      expect(llString.add("pippo")).toBeTruthy
      expect(llString.pollLast()).toEqual("pippo")

      val llDouble = new LinkedList[Double]()

      expect(llDouble.add(+10000.987)).toBeTruthy
      expect(llDouble.add(-0.987)).toBeTruthy
      expect(llDouble.pollLast()).toEqual(-0.987)
    }

    it("should perform as a stack with push and pop") {
      val llInt = new LinkedList[Int]()

      llInt.push(1000)
      llInt.push(10)
      expect(llInt.pop()).toEqual(10)
      expect(llInt.pop()).toEqual(1000)
      expect(llInt.isEmpty()).toBeTruthy

      val llString = new LinkedList[String]()

      llString.push("pluto")
      llString.push("pippo")
      expect(llString.pop()).toEqual("pippo")
      expect(llString.pop()).toEqual("pluto")
      expect(llString.isEmpty()).toBeTruthy

      val llDouble = new LinkedList[Double]()

      llDouble.push(+10000.987)
      llDouble.push(-0.987)
      expect(llDouble.pop()).toEqual(-0.987)
      expect(llDouble.pop()).toEqual(+10000.987)
      expect(llString.isEmpty()).toBeTruthy
    }

    it("should poll and peek elements") {
      val pq = new LinkedList[String]()

      expect(pq.add("one")).toBeTruthy
      expect(pq.add("two")).toBeTruthy
      expect(pq.add("three")).toBeTruthy

      expect(pq.peek.equals("one")).toBeTruthy
      expect(pq.poll.equals("one")).toBeTruthy

      expect(pq.peekFirst.equals("two")).toBeTruthy
      expect(pq.pollFirst.equals("two")).toBeTruthy

      expect(pq.peekLast.equals("three")).toBeTruthy
      expect(pq.pollLast.equals("three")).toBeTruthy

      expect(pq.peekFirst).toBeNull
      expect(pq.pollFirst).toBeNull

      expect(pq.peekLast).toBeNull
      expect(pq.pollLast).toBeNull
    }

    it("should remove occurrences of provided elements") {
      val l = asJavaCollection(Seq("one", "two", "three", "two", "one"))
      val ll = new LinkedList[String](l)

      expect(ll.removeFirstOccurrence("one")).toBeTruthy
      expect(ll.indexOf("one")).toEqual(3)
      expect(ll.removeLastOccurrence("two")).toBeTruthy
      expect(ll.lastIndexOf("two")).toEqual(0)
      expect(ll.removeFirstOccurrence("one")).toBeTruthy
      expect(ll.removeLastOccurrence("two")).toBeTruthy
      expect(ll.removeFirstOccurrence("three")).toBeTruthy
      expect(ll.removeLastOccurrence("three")).toBeFalsy
      expect(ll.isEmpty).toBeTruthy
    }

    it("should iterate over elements in both directions") {
      val s = Seq("one", "two", "three")
      val l = asJavaCollection(s)
      val ll = new LinkedList[String](l)

      val iter = ll.iterator()
      for (i <- 0 until l.size()) {
        expect(iter.hasNext()).toBeTruthy
        expect(iter.next()).toEqual(s(i))
      }
      expect(iter.hasNext()).toBeFalsy

      val diter = ll.descendingIterator()
      for (i <- (0 until l.size()).reverse) {
        expect(diter.hasNext()).toBeTruthy
        expect(diter.next()).toEqual(s(i))
      }
      expect(diter.hasNext()).toBeFalsy
    }
  }
}

class LinkedListFactory extends AbstractListFactory {
  override def implementationName: String =
    "java.util.LinkedList"

  override def empty[E]: LinkedList[E] =
    new LinkedList[E]()
}
