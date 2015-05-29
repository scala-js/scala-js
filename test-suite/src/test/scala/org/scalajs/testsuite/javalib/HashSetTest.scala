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

import java.util.{HashSet, LinkedHashSet}

object HashSetTest extends JasmineTest {

  trait HashSetGen {
    def apply[T](): HashSet[T]
  }

  def genHashSetTests(newHashSet: HashSetGen): Unit = {
    it("should check hashSet size") {
      val hs = newHashSet[String]()

      expect(hs.size()).toEqual(0)
      expect(hs.add("ONE")).toBeTruthy
      expect(hs.size()).toEqual(1)
      expect(hs.add("TWO")).toBeTruthy
      expect(hs.size()).toEqual(2)
    }

    it("should store integers") {
      val hs = newHashSet[Int]()

      expect(hs.add(100)).toBeTruthy
      expect(hs.size()).toEqual(1)
      expect(hs.contains(100)).toBeTruthy
      expect(hs.iterator.next()).toEqual(100)
    }

    it("should store objects with same HashCode but different types") {
      val hs = newHashSet[Any]()

      expect(hs.add(true)).toBeTruthy
      expect(hs.add(1231)).toBeTruthy
      expect(hs.size()).toEqual(2)
    }

    it("should store doubles also in corner cases") {
      val hs = newHashSet[Double]()

      expect(hs.add(11111.0)).toBeTruthy
      expect(hs.size()).toEqual(1)
      expect(hs.contains(11111.0)).toBeTruthy
      expect(hs.iterator.next()).toEqual(11111.0)

      expect(hs.add(Double.NaN)).toBeTruthy
      expect(hs.size()).toEqual(2)
      expect(hs.contains(Double.NaN)).toBeTruthy
      expect(hs.contains(+0.0)).toBeFalsy
      expect(hs.contains(-0.0)).toBeFalsy

      expect(hs.remove(Double.NaN)).toBeTruthy
      expect(hs.add(+0.0)).toBeTruthy
      expect(hs.size()).toEqual(2)
      expect(hs.contains(Double.NaN)).toBeFalsy
      expect(hs.contains(+0.0)).toBeTruthy
      expect(hs.contains(-0.0)).toBeFalsy

      expect(hs.remove(+0.0)).toBeTruthy
      expect(hs.add(-0.0)).toBeTruthy
      expect(hs.size()).toEqual(2)
      expect(hs.contains(Double.NaN)).toBeFalsy
      expect(hs.contains(+0.0)).toBeFalsy
      expect(hs.contains(-0.0)).toBeTruthy

      expect(hs.add(+0.0)).toBeTruthy
      expect(hs.add(Double.NaN)).toBeTruthy
      expect(hs.contains(Double.NaN)).toBeTruthy
      expect(hs.contains(+0.0)).toBeTruthy
      expect(hs.contains(-0.0)).toBeTruthy
    }

    it("should store custom objects") {
      case class TestObj(num: Int)

      val hs = newHashSet[TestObj]()

      expect(hs.add(TestObj(100))).toBeTruthy
      expect(hs.size()).toEqual(1)
      expect(hs.contains(TestObj(100))).toBeTruthy
      expect(hs.iterator.next().num).toEqual(100)
    }

    it("should remove stored elements") {
      val hs = newHashSet[String]()

      expect(hs.size()).toEqual(0)
      expect(hs.add("ONE")).toBeTruthy
      expect(hs.add("ONE")).toBeFalsy
      expect(hs.size()).toEqual(1)
      expect(hs.add("TWO")).toBeTruthy
      expect(hs.size()).toEqual(2)
      expect(hs.remove("ONE")).toBeTruthy
      expect(hs.remove("ONE")).toBeFalsy
      expect(hs.size()).toEqual(1)
      expect(hs.remove("TWO")).toBeTruthy
      expect(hs.size()).toEqual(0)

      expect(hs.add("ONE")).toBeTruthy
      expect(hs.add("TWO")).toBeTruthy
      expect(hs.size()).toEqual(2)
      val l1 = List[String]("ONE", "TWO")
      expect(hs.removeAll(asJavaCollection(l1))).toBeTruthy
      expect(hs.size()).toEqual(0)

      expect(hs.add("ONE")).toBeTruthy
      expect(hs.add("TWO")).toBeTruthy
      expect(hs.size()).toEqual(2)
      val l2 = List[String]("ONE", "THREE")
      expect(hs.retainAll(asJavaCollection(l2))).toBeTruthy
      expect(hs.size()).toEqual(1)
      expect(hs.contains("ONE")).toBeTruthy
      expect(hs.contains("TWO")).toBeFalsy
    }

    it("should be cleared with one operation") {
      val hs = newHashSet[String]()

      expect(hs.add("ONE")).toBeTruthy
      expect(hs.add("TWO")).toBeTruthy
      expect(hs.size()).toEqual(2)

      hs.clear()
      expect(hs.size()).toEqual(0)
      expect(hs.isEmpty).toBeTruthy
    }

    it("should check contained elems presence") {
      val hs = newHashSet[String]()

      expect(hs.add("ONE")).toBeTruthy
      expect(hs.contains("ONE")).toBeTruthy
      expect(hs.contains("TWO")).toBeFalsy
      expect(hs.contains(null)).toBeFalsy
      expect(hs.add(null)).toBeTruthy
      expect(hs.contains(null)).toBeTruthy
    }

    it("should put a whole Collection into") {
      val hs = newHashSet[String]()

      val l = List[String]("ONE", "TWO", (null: String))
      expect(hs.addAll(asJavaCollection(l))).toBeTruthy
      expect(hs.size).toEqual(3)
      expect(hs.contains("ONE")).toBeTruthy
      expect(hs.contains("TWO")).toBeTruthy
      expect(hs.contains(null)).toBeTruthy
    }

    it("should iterate over elements") {
      val hs = newHashSet[String]()

      val l = List[String]("ONE", "TWO", (null: String))
      expect(hs.addAll(asJavaCollection(l))).toBeTruthy
      expect(hs.size).toEqual(3)

      val iter = hs.iterator()
      val result = {
        for {
          i <- 0 until 3
        } yield {
          expect(iter.hasNext()).toBeTruthy
          iter.next()
        }
      }
      expect(iter.hasNext()).toBeFalsy
      expect(result.containsAll(l)).toBeTruthy
      expect(l.containsAll(result)).toBeTruthy
    }

  }

  describe("java.util.HashSet") {
    genHashSetTests(new HashSetGen {
      def apply[T](): HashSet[T] = new HashSet[T]()
    })
  }

  describe("java.util.LinkedHashSet") {

    genHashSetTests(new HashSetGen {
      def apply[T](): HashSet[T] = new LinkedHashSet[T]()
    })

    it("should iterate over elements in an ordered manner") {
      val hs = new LinkedHashSet[String]()

      val l1 = List[String]("ONE", "TWO", (null: String))
      expect(hs.addAll(asJavaCollection(l1))).toBeTruthy
      expect(hs.size).toEqual(3)

      val iter1 = hs.iterator()
      val result1 = {
        for {
          i <- 0 until 3
        } yield {
          expect(iter1.hasNext()).toBeTruthy
          val value = iter1.next()
          expect(value).toEqual(l1(i))
        }
      }
      expect(iter1.hasNext()).toBeFalsy
      expect(result1.equals(l1))

      val l2 = l1 :+ "THREE"
      expect(hs.add(l2(3))).toBeTruthy

      val iter2 = hs.iterator()
      val result2 = {
        for {
          i <- 0 until 4
        } yield {
          expect(iter2.hasNext()).toBeTruthy
          val value = iter2.next()
          expect(value).toEqual(l2(i))
        }
      }
      expect(iter2.hasNext()).toBeFalsy
      expect(result2.equals(l2))
    }

  }

}
