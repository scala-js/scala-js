package org.scalajs.testsuite.javalib

import scala.language.implicitConversions

import java.{util => ju, lang => jl}

import scala.collection.JavaConversions._

trait SetTest extends CollectionTest with ExpectExceptions {

  def testSetApi(setFactory: SetFactory): Unit = {
    testCollectionApi(setFactory)

    it("should check Set size") {
      val hs = setFactory.empty[String]

      expect(hs.size()).toEqual(0)
      expect(hs.add("ONE")).toBeTruthy
      expect(hs.size()).toEqual(1)
      expect(hs.add("TWO")).toBeTruthy
      expect(hs.size()).toEqual(2)
    }

    it("should store integers") {
      val hs = setFactory.empty[Int]

      expect(hs.add(100)).toBeTruthy
      expect(hs.size()).toEqual(1)
      expect(hs.contains(100)).toBeTruthy
      expect(hs.iterator.next()).toEqual(100)
    }

    it("should store objects with same hashCode but different types") {
      val hs = setFactory.empty[AnyRef]
      trait A extends Comparable[A] {
        def compareTo(o: A): Int = toString.compareTo(o.toString)
      }
      object B extends A {
        override def hashCode(): Int = 42
      }
      object C extends A {
        override def hashCode(): Int = 42
      }

      expect(hs.add(B)).toBeTruthy
      expect(hs.add(C)).toBeTruthy
      expect(hs.size()).toEqual(2)
    }

    it("should store doubles also in corner cases") {
      val hs = setFactory.empty[Double]

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
      case class TestObj(num: Int) extends jl.Comparable[TestObj] {
        override def compareTo(o: TestObj): Int = o.num - num
      }

      val hs = setFactory.empty[TestObj]

      expect(hs.add(TestObj(100))).toBeTruthy
      expect(hs.size()).toEqual(1)
      expect(hs.contains(TestObj(100))).toBeTruthy
      expect(hs.iterator.next().num).toEqual(100)
    }

    it("should remove stored elements") {
      val hs = setFactory.empty[String]

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
      val hs = setFactory.empty[String]

      expect(hs.add("ONE")).toBeTruthy
      expect(hs.add("TWO")).toBeTruthy
      expect(hs.size()).toEqual(2)

      hs.clear()
      expect(hs.size()).toEqual(0)
      expect(hs.isEmpty).toBeTruthy
    }

    it("should check contained elems presence") {
      val hs = setFactory.empty[String]

      expect(hs.add("ONE")).toBeTruthy
      expect(hs.contains("ONE")).toBeTruthy
      expect(hs.contains("TWO")).toBeFalsy
      if (setFactory.allowsNullElement) {
        expect(hs.contains(null)).toBeFalsy
        expect(hs.add(null)).toBeTruthy
        expect(hs.contains(null)).toBeTruthy
      } else {
        expectThrows[Exception](hs.add(null))
      }
    }

    it("should put a whole Collection into") {
      val hs = setFactory.empty[String]

      if (setFactory.allowsNullElement) {
        val l = List[String]("ONE", "TWO", (null: String))
        expect(hs.addAll(asJavaCollection(l))).toBeTruthy
        expect(hs.size).toEqual(3)
        expect(hs.contains("ONE")).toBeTruthy
        expect(hs.contains("TWO")).toBeTruthy
        expect(hs.contains(null)).toBeTruthy
      } else {
        expectThrows[Exception] {
          val l = List[String]("ONE", "TWO", (null: String))
          hs.addAll(asJavaCollection(l))
        }
      }
    }

    it("should iterate over elements") {
      val hs = setFactory.empty[String]

      val l = {
        if (setFactory.allowsNullElement)
          List[String]("ONE", "TWO", (null: String))
        else
          List[String]("ONE", "TWO", "THREE")
      }
      expect(hs.addAll(asJavaCollection(l))).toBeTruthy
      expect(hs.size).toEqual(3)

      val iter = hs.iterator()
      val result = {
        for (i <- 0 until 3) yield {
          expect(iter.hasNext()).toBeTruthy
          iter.next()
        }
      }
      expect(iter.hasNext()).toBeFalsy
      expect(result.containsAll(l)).toBeTruthy
      expect(l.containsAll(result)).toBeTruthy
    }
  }
}

object SetFactory {
  def allFactories: Iterator[SetFactory] =
    AbstractSetFactory.allFactories ++ SortedSetFactory.allFactories ++ NavigableSetFactory.allFactories
}

trait SetFactory extends CollectionFactory {
  def empty[E]: ju.Set[E]

  def allowsNullElement: Boolean
}
