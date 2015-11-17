/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util.concurrent

import org.scalajs.testsuite.javalib.util.{AbstractCollectionFactory, AbstractCollectionTest}

import scala.language.implicitConversions

import scala.collection.JavaConversions._

import org.scalajs.jasminetest.JasmineTest

import scala.scalajs.runtime.UndefinedBehaviorError

import java.util.concurrent.ConcurrentLinkedQueue

object ConcurrentLinkedQueueTest extends ConcurrentLinkedQueueTest(new ConcurrentLinkedQueueFactory)

abstract class ConcurrentLinkedQueueTest[F <: ConcurrentLinkedQueueFactory](
    listFactory: F) extends AbstractCollectionTest(listFactory) {

  override def testApi(): Unit = {

    super.testApi()

    it("should store and remove ordered integers") {
      val pq = new ConcurrentLinkedQueue[Int]()

      expect(pq.size()).toEqual(0)
      expect(pq.add(111)).toBeTruthy
      expect(pq.size()).toEqual(1)
      expect(pq.add(222)).toBeTruthy
      expect(pq.size()).toEqual(2)
      expect(pq.poll()).toEqual(111)
      expect(pq.size()).toEqual(1)
      expect(pq.poll()).toEqual(222)
      expect(pq.add(222)).toBeTruthy
      expect(pq.add(222)).toBeTruthy
      expect(pq.remove(222)).toBeTruthy
      expect(pq.remove(222)).toBeTruthy
      expect(pq.remove(222)).toBeFalsy
    }

    it("should store and remove strings") {
      val pq = new ConcurrentLinkedQueue[String]()

      expect(pq.size()).toEqual(0)
      expect(pq.add("aaa")).toBeTruthy
      expect(pq.size()).toEqual(1)
      expect(pq.add("bbb")).toBeTruthy
      expect(pq.size()).toEqual(2)
      expect(pq.poll()).toEqual("aaa")
      expect(pq.size()).toEqual(1)
      expect(pq.poll()).toEqual("bbb")
      expect(pq.add("bbb")).toBeTruthy
      expect(pq.add("bbb")).toBeTruthy
      expect(pq.remove("bbb")).toBeTruthy
      expect(pq.remove("bbb")).toBeTruthy
      expect(pq.remove("bbb")).toBeFalsy
      expect(pq.poll()).toBeNull
    }

    it("should store Double even in corner cases") {
      val pq = new ConcurrentLinkedQueue[Double]()

      expect(pq.add(1.0)).toBeTruthy
      expect(pq.add(+0.0)).toBeTruthy
      expect(pq.add(-0.0)).toBeTruthy
      expect(pq.add(Double.NaN)).toBeTruthy

      expect(pq.poll.equals(1.0)).toBeTruthy

      expect(pq.poll.equals(+0.0)).toBeTruthy

      expect(pq.poll.equals(-0.0)).toBeTruthy

      expect(pq.peek.isNaN).toBeTruthy

      expect(pq.remove(Double.NaN)).toBeTruthy

      expect(pq.isEmpty).toBeTruthy
    }

    it("could be instantiated with a prepopulated Collection") {
      val l = asJavaCollection(Set(1, 5, 2, 3, 4))
      val pq = new ConcurrentLinkedQueue[Int](l)

      expect(pq.size()).toEqual(5)
      for (i <- l) {
        expect(pq.poll()).toEqual(i)
      }
      expect(pq.isEmpty).toBeTruthy
    }

    it("should be cleared in a single operation") {
      val l = asJavaCollection(Set(1, 5, 2, 3, 4))
      val pq = new ConcurrentLinkedQueue[Int](l)

      expect(pq.size()).toEqual(5)
      pq.clear()
      expect(pq.size()).toEqual(0)
    }

    it("should add multiple elemnt in one operation") {
      val l = asJavaCollection(Set(1, 5, 2, 3, 4))
      val pq = new ConcurrentLinkedQueue[Int]()

      expect(pq.size()).toEqual(0)
      pq.addAll(l)
      expect(pq.size()).toEqual(5)
      pq.add(6)
      expect(pq.size()).toEqual(6)
    }

    it("should check contained values even in double corner cases") {
      val pq = new ConcurrentLinkedQueue[Double]()

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

    it("should provide a weakly consistent iterator") {
      val queue = new ConcurrentLinkedQueue[Int]()
      queue.add(1)
      queue.add(2)
      val iter1 = queue.iterator()
      expect(iter1.next()).toEqual(1)
      expect(iter1.hasNext).toBeTruthy
      queue.remove(2)
      expect(iter1.hasNext).toBeTruthy
      expect(iter1.next()).toEqual(2)
      expect(iter1.hasNext).toBeFalsy

      val queue2 = new ConcurrentLinkedQueue[Int]()
      queue2.add(1)
      queue2.add(2)
      queue2.add(3)
      val iter2 = queue2.iterator()
      expect(iter2.next()).toEqual(1)
      iter2.remove()
      expect(iter2.next()).toEqual(2)
      expect(iter2.next()).toEqual(3)
    }
  }
}

class ConcurrentLinkedQueueFactory extends AbstractCollectionFactory {
  override def implementationName: String =
    "java.util.concurrent.ConcurrentLinkedQueue"

  override def empty[E]: ConcurrentLinkedQueue[E] =
    new ConcurrentLinkedQueue[E]()
}
