/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib

import org.scalajs.jasminetest.JasmineTest

trait ListTest[F <: ListFactory] extends JasmineTest {

  def listFactory: F

  def expectIndexOutOfBoundsException[T](f: => T): Unit = {
    expect({
      try {
        f
        false
      } catch {
        case err: IndexOutOfBoundsException => true
      }
    }).toBeTruthy
  }

  def testListApi(): Unit = {
    it("should store strings") {
      val lst = listFactory.empty[String]

      expect(lst.size()).toEqual(0)
      lst.add("one")
      expect(lst.size()).toEqual(1)
      expect(lst.get(0)).toEqual("one")
      lst.add("two")
      expect(lst.size()).toEqual(2)
      expect(lst.get(0)).toEqual("one")
      expect(lst.get(1)).toEqual("two")

      expectIndexOutOfBoundsException(lst.get(-1))
      expectIndexOutOfBoundsException(lst.get(lst.size))
    }

    it("should store integers") {
      val lst = listFactory.empty[Int]

      lst.add(1)
      expect(lst.size()).toEqual(1)
      expect(lst.get(0)).toEqual(1)
      lst.add(2)
      expect(lst.size()).toEqual(2)
      expect(lst.get(0)).toEqual(1)
      expect(lst.get(1)).toEqual(2)

      expectIndexOutOfBoundsException(lst.get(-1))
      expectIndexOutOfBoundsException(lst.get(lst.size))
    }

    it("should store doubles") {
      val lst = listFactory.empty[Double]

      lst.add(1.234)
      expect(lst.size()).toEqual(1)
      expect(lst.get(0)).toEqual(1.234)
      lst.add(2.345)
      expect(lst.size()).toEqual(2)
      expect(lst.get(0)).toEqual(1.234)
      expect(lst.get(1)).toEqual(2.345)
      lst.add(Double.NaN)
      lst.add(+0.0)
      lst.add(-0.0)
      expect(lst.size()).toEqual(5)
      expect(lst.get(0)).toEqual(1.234)
      expect(lst.get(1)).toEqual(2.345)
      expect(lst.get(2).isNaN).toBeTruthy
      expect(lst.get(3).equals(+0.0)).toBeTruthy
      expect(lst.get(4).equals(-0.0)).toBeTruthy

      expectIndexOutOfBoundsException(lst.get(-1))
      expectIndexOutOfBoundsException(lst.get(lst.size))
    }

    it("should store custom objects") {
      case class TestObj(num: Int)

      val lst = listFactory.empty[TestObj]

      lst.add(TestObj(100))
      expect(lst.size()).toEqual(1)
      expect(lst.get(0) == TestObj(100)).toBeTruthy

      expectIndexOutOfBoundsException(lst.get(-1))
      expectIndexOutOfBoundsException(lst.get(lst.size))
    }

    it("should remove stored elements") {
      val lst = listFactory.empty[String]

      lst.add("one")
      lst.add("two")
      lst.add("three")

      expect(lst.remove("four")).toBeFalsy
      expect(lst.size()).toEqual(3)
      expect(lst.remove("two")).toBeTruthy
      expect(lst.size()).toEqual(2)
      expect(lst.remove(0)).toEqual("one")
      expect(lst.size()).toEqual(1)
      expect(lst.get(0)).toEqual("three")

      expectIndexOutOfBoundsException(lst.remove(-1))
      expectIndexOutOfBoundsException(lst.remove(lst.size))
    }

    it("should remove stored elements on double corner cases") {
      val al = listFactory.empty[Double]

      al.add(1.234)
      al.add(2.345)
      al.add(Double.NaN)
      al.add(+0.0)
      al.add(-0.0)

      // al == ArrayList(1.234, 2.345, NaN, +0.0, -0.0)
      expect(al.remove(Double.NaN)).toBeTruthy
      // al == ArrayList(1.234, 2.345, +0.0, -0.0)
      expect(al.size()).toEqual(4)
      expect(al.remove(2.345)).toBeTruthy
      // al == ArrayList(1.234, +0.0, -0.0)
      expect(al.size()).toEqual(3)
      expect(al.remove(0)).toEqual(1.234)
      // al == ArrayList(+0.0, -0.0)
      expect(al.size()).toEqual(2)
      expect(al.remove(-0.0)).toBeTruthy
      // al == ArrayList(NaN, +0.0)
      expect(al.size()).toEqual(1)

      al.clear()

      expect(al.isEmpty).toBeTruthy
    }

    it("should be cleared with one operation") {
      val al = listFactory.empty[String]

      al.add("one")
      al.add("two")
      expect(al.size).toEqual(2)
      al.clear()
      expect(al.size).toEqual(0)
    }

    it("should check contained presence") {
      val al = listFactory.empty[String]

      al.add("one")
      expect(al.contains("one")).toBeTruthy
      expect(al.contains("two")).toBeFalsy
      expect(al.contains(null)).toBeFalsy
    }

    it("should check contained presence for double corner cases") {
      val al = listFactory.empty[Double]

      al.add(-0.0)
      expect(al.contains(-0.0)).toBeTruthy
      expect(al.contains(+0.0)).toBeFalsy

      al.clear()

      al.add(+0.0)
      expect(al.contains(-0.0)).toBeFalsy
      expect(al.contains(+0.0)).toBeTruthy
    }

    it("should give a proper set operation") {
      val al = listFactory.empty[String]
      al.add("one")
      al.add("two")
      al.add("three")

      al.set(1, "four")
      expect(al.get(0)).toEqual("one")
      expect(al.get(1)).toEqual("four")
      expect(al.get(2)).toEqual("three")

      expectIndexOutOfBoundsException(al.set(-1, ""))
      expectIndexOutOfBoundsException(al.set(al.size, ""))
    }

    it("should give proper iterator over elements") {
      val al = listFactory.empty[String]
      al.add("one")
      al.add("two")
      al.add("three")

      val elements = al.iterator
      expect(elements.hasNext).toBeTruthy
      expect(elements.next()).toEqual("one")
      expect(elements.hasNext).toBeTruthy
      expect(elements.next()).toEqual("two")
      expect(elements.hasNext).toBeTruthy
      expect(elements.next()).toEqual("three")
      expect(elements.hasNext).toBeFalsy
    }

    it("should give proper list iterator over elements") {
      val lst = listFactory.empty[String]
      lst.add("one")
      lst.add("two")
      lst.add("three")

      val elements = lst.listIterator
      expect(elements.hasPrevious).toBeFalsy
      expect(elements.hasNext).toBeTruthy
      expect(elements.next()).toEqual("one")
      expect(elements.hasPrevious).toBeTruthy
      expect(elements.hasNext).toBeTruthy
      expect(elements.next()).toEqual("two")
      expect(elements.hasPrevious).toBeTruthy
      expect(elements.hasNext).toBeTruthy
      expect(elements.next()).toEqual("three")
      expect(elements.hasPrevious).toBeTruthy
      expect(elements.hasNext).toBeFalsy
      expect(elements.previous()).toEqual("three")
      expect(elements.previous()).toEqual("two")
      expect(elements.previous()).toEqual("one")
    }

    it("should add elements at a given index") {
      val al = listFactory.empty[String]
      al.add(0, "one") // ("one")
      al.add(0, "two") // ("two", "one")
      al.add(1, "three") // ("two", "three", "one")

      expect(al.get(0)).toEqual("two")
      expect(al.get(1)).toEqual("three")
      expect(al.get(2)).toEqual("one")

      expectIndexOutOfBoundsException(al.add(-1, ""))
      expectIndexOutOfBoundsException(al.add(al.size + 1, ""))
    }

    it("should give the first index of an element") {
      val al = listFactory.empty[String]
      al.add("one")
      al.add("two")
      al.add("three")
      al.add("one")
      al.add("two")
      al.add("three")

      expect(al.indexOf("one")).toEqual(0)
      expect(al.indexOf("two")).toEqual(1)
      expect(al.indexOf("three")).toEqual(2)
      expect(al.indexOf("four")).toEqual(-1)
    }

    it("should give the last index of an element") {
      val al = listFactory.empty[String]
      al.add("one")
      al.add("two")
      al.add("three")
      al.add("one")
      al.add("two")
      al.add("three")

      expect(al.lastIndexOf("one")).toEqual(3)
      expect(al.lastIndexOf("two")).toEqual(4)
      expect(al.lastIndexOf("three")).toEqual(5)
      expect(al.lastIndexOf("four")).toEqual(-1)
    }

    it("should give the first/last index of an element for double corner cases") {
      val al = listFactory.empty[Double]

      al.add(-0.0)
      al.add(+0.0)
      al.add(Double.NaN)
      al.add(+0.0)
      al.add(-0.0)
      al.add(Double.NaN)

      expect(al.indexOf(-0.0)).toEqual(0)
      expect(al.indexOf(+0.0)).toEqual(1)
      expect(al.indexOf(Double.NaN)).toEqual(2)

      expect(al.lastIndexOf(+0.0)).toEqual(3)
      expect(al.lastIndexOf(-0.0)).toEqual(4)
      expect(al.lastIndexOf(Double.NaN)).toEqual(5)
    }

    it("should give a sublist backed up by the original list") {
      val al = listFactory.empty[String]

      al.add("one")
      al.add("two")
      al.add("three")
      al.add("four")
      al.add("five")
      al.add("six")

      val al0 = al.subList(0, al.size)
      expect(al0.size).toEqual(6)
      expect(al0.size).toEqual(al.size)
      for (i <- 0 until al.size)
        expect(al0.get(i)).toEqual(al.get(i))
      al0.set(3, "zero")
      expect(al0.get(3)).toEqual("zero")
      for (i <- 0 until al.size)
        expect(al0.get(i)).toEqual(al.get(i))

      val al1 = al.subList(2, 5)
      expect(al1.size).toEqual(3)
      for (i <- 0 until 3)
        expect(al1.get(i)).toEqual(al.get(2 + i))
      al1.set(0, "nine")
      expect(al1.get(0)).toEqual("nine")
      for (i <- 0 until 3) {
        expect(al1.get(i)).toEqual(al.get(2 + i))
        expect(al1.get(i)).toEqual(al0.get(2 + i))
      }
      expect(al1.get(0)).toEqual("nine")
      expect(al1.get(1)).toEqual("zero")
      expect(al1.get(2)).toEqual("five")

      al1.clear()

      expect(al.get(0)).toEqual("one")
      expect(al.get(1)).toEqual("two")
      expect(al.get(2)).toEqual("six")

      expect(al.size).toEqual(3)
      expect(al1.size).toEqual(0)
    }

  }
}

trait ListFactory {
  def implementationName: String
  def empty[E]: java.util.List[E]
}
