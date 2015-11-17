/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import java.{util => ju}
import scala.collection.JavaConversions._

trait SortedSetTest extends SetTest {

  def testSortedSetApi(setFactory: SortedSetFactory): Unit = {

    it("should retrieve the first(ordered) element") {
      val ssInt = setFactory.empty[Int]

      expect(ssInt.add(1000)).toBeTruthy
      expect(ssInt.add(10)).toBeTruthy
      expect(ssInt.first).toEqual(10)

      val ssString = setFactory.empty[String]

      expect(ssString.add("pluto")).toBeTruthy
      expect(ssString.add("pippo")).toBeTruthy
      expect(ssString.first).toEqual("pippo")

      val ssDouble = setFactory.empty[Double]

      expect(ssDouble.add(+10000.987)).toBeTruthy
      expect(ssDouble.add(-0.987)).toBeTruthy
      expect(ssDouble.first).toEqual(-0.987)
    }

    it("should retrieve the last(ordered) element") {
      val ssInt = setFactory.empty[Int]

      expect(ssInt.add(1000)).toBeTruthy
      expect(ssInt.add(10)).toBeTruthy
      expect(ssInt.last).toEqual(1000)

      val ssString = setFactory.empty[String]

      expect(ssString.add("pluto")).toBeTruthy
      expect(ssString.add("pippo")).toBeTruthy
      expect(ssString.last).toEqual("pluto")

      val ssDouble = setFactory.empty[Double]

      expect(ssDouble.add(+10000.987)).toBeTruthy
      expect(ssDouble.add(-0.987)).toBeTruthy
      expect(ssDouble.last).toEqual(10000.987)
    }

    val l = asJavaCollection(Set(1, 5, 2, 3, 4))

    it("should return a proper headSet") {
      val ss = setFactory.empty[Int]

      ss.addAll(l)

      val hs1 = ss.headSet(3)
      val l1 = asJavaCollection(Set(1,2))
      expect(hs1.containsAll(l1)).toBeTruthy
      expect(hs1.removeAll(l1)).toBeTruthy
      expect(hs1.isEmpty).toBeTruthy
      expect(ss.size).toEqual(3)
      expect(ss.containsAll(asJavaCollection(Set(3,4,5)))).toBeTruthy

      ss.addAll(l)

      val hs2 = ss.headSet(4)
      val l2 = asJavaCollection(Set(1,2,3))
      expect(hs2.containsAll(l2)).toBeTruthy
      expect(hs2.removeAll(l2)).toBeTruthy
      expect(hs2.isEmpty).toBeTruthy
      expect(ss.size).toEqual(2)
      expect(ss.containsAll(asJavaCollection(Set(4,5)))).toBeTruthy
    }

    it("should return a proper tailSet") {
      val ss = setFactory.empty[Int]

      ss.addAll(l)

      val ts1 = ss.tailSet(3)
      val l3 = asJavaCollection(Set(3,4,5))
      expect(ts1.containsAll(l3)).toBeTruthy
      expect(ts1.removeAll(l3)).toBeTruthy
      expect(ts1.isEmpty).toBeTruthy
      expect(ss.size).toEqual(2)
      expect(ss.containsAll(asJavaCollection(Set(1,2)))).toBeTruthy

      ss.addAll(l)

      val ts2 = ss.tailSet(4)
      val l4 = asJavaCollection(Set(4,5))
      expect(ts2.containsAll(l4)).toBeTruthy
      expect(ts2.removeAll(l4)).toBeTruthy
      expect(ts2.isEmpty).toBeTruthy
      expect(ss.size).toEqual(3)
      expect(ss.containsAll(asJavaCollection(Set(1,2,3)))).toBeTruthy
    }

    it("should return a proper subSet") {
      val ss = setFactory.empty[Int]

      ss.addAll(l)

      val ss1 = ss.subSet(2, 4)
      val l5 = asJavaCollection(Set(2,3))
      expect(ss1.containsAll(l5)).toBeTruthy
      expect(ss1.removeAll(l5)).toBeTruthy
      expect(ss1.isEmpty).toBeTruthy
      expect(ss.size).toEqual(3)
      expect(ss.containsAll(asJavaCollection(Set(1,4,5)))).toBeTruthy

      ss.addAll(l)

      val ss2 = ss.subSet(1, 5)
      expect(ss2.containsAll(l5)).toBeTruthy
      expect(ss2.removeAll(l5)).toBeTruthy
      expect(ss2.isEmpty).toBeFalsy
      expect(ss.size).toEqual(3)
      expect(ss.containsAll(asJavaCollection(Set(1,4,5)))).toBeTruthy
    }
  }
}

object SortedSetFactory {
  def allFactories: Iterator[SortedSetFactory] =
    Iterator.empty
}

trait SortedSetFactory extends SetFactory {
  def empty[E]: ju.SortedSet[E]
}
