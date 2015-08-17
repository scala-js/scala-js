/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import java.{util => ju}

import org.scalajs.testsuite.javalib.util.concurrent.ConcurrentSkipListSetFactory

import scala.collection.JavaConversions._

trait NavigableSetTest extends SetTest {

  def testNavigableSetApi(setFactory: NavigableSetFactory): Unit = {
  it("should retrieve ceiling(ordered) elements") {
      val lInt = asJavaCollection(Set(1, 5, 2, 3, 4))
      val nsInt = setFactory.empty[Int]

      nsInt.addAll(lInt)

      expect(nsInt.ceiling(-10)).toEqual(1)
      expect(nsInt.ceiling(0)).toEqual(1)
      expect(nsInt.ceiling(1)).toEqual(1)
      expect(nsInt.ceiling(5)).toEqual(5)

      val lString = asJavaCollection(Set("a", "e", "b", "c", "d"))
      val nsString = setFactory.empty[String]

      nsString.addAll(lString)

      expect(nsString.ceiling("00000")).toEqual("a")
      expect(nsString.ceiling("0")).toEqual("a")
      expect(nsString.ceiling("a")).toEqual("a")
      expect(nsString.ceiling("d")).toEqual("d")
      expect(nsString.ceiling("z")).toEqual(null)
    }

    it("should retrieve floor(ordered) elements") {
      val lInt = asJavaCollection(Set(1, 5, 2, 3, 4))
      val nsInt = setFactory.empty[Int]

      nsInt.addAll(lInt)

      expect(nsInt.floor(10)).toEqual(5)
      expect(nsInt.floor(5)).toEqual(5)
      expect(nsInt.floor(3)).toEqual(3)
      expect(nsInt.floor(1)).toEqual(1)

      val lString = asJavaCollection(Set("a", "e", "b", "c", "d"))
      val nsString = setFactory.empty[String]

      nsString.addAll(lString)

      expect(nsString.floor("zzzzz")).toEqual("e")
      expect(nsString.floor("d")).toEqual("d")
      expect(nsString.floor("b")).toEqual("b")
      expect(nsString.floor("a")).toEqual("a")
      expect(nsString.floor("0")).toEqual(null)
    }

    it("should retrieve higher(ordered) elements") {
      val lInt = asJavaCollection(Set(1, 5, 2, 3, 4))
      val nsInt = setFactory.empty[Int]

      nsInt.addAll(lInt)

      expect(nsInt.higher(4)).toEqual(5)
      expect(nsInt.higher(3)).toEqual(4)
      expect(nsInt.higher(1)).toEqual(2)
      expect(nsInt.higher(-10)).toEqual(1)

      val lString = asJavaCollection(Set("a", "e", "b", "c", "d"))
      val nsString = setFactory.empty[String]

      nsString.addAll(lString)

      expect(nsString.higher("zzzzz")).toEqual(null)
      expect(nsString.higher("d")).toEqual("e")
      expect(nsString.higher("b")).toEqual("c")
      expect(nsString.higher("a")).toEqual("b")
      expect(nsString.higher("0")).toEqual("a")
    }

    it("should retrieve lower(ordered) elements") {
      val lInt = asJavaCollection(Set(1, 5, 2, 3, 4))
      val nsInt = setFactory.empty[Int]

      nsInt.addAll(lInt)

      expect(nsInt.lower(5)).toEqual(4)
      expect(nsInt.lower(4)).toEqual(3)
      expect(nsInt.lower(3)).toEqual(2)
      expect(nsInt.lower(10)).toEqual(5)

      val lString = asJavaCollection(Set("a", "e", "b", "c", "d"))
      val nsString = setFactory.empty[String]

      nsString.addAll(lString)

      expect(nsString.lower("zzzzz")).toEqual("e")
      expect(nsString.lower("d")).toEqual("c")
      expect(nsString.lower("b")).toEqual("a")
      expect(nsString.lower("a")).toEqual(null)
      expect(nsString.lower("0")).toEqual(null)
    }

    it("should poll first and last elements") {
      val lInt = asJavaCollection(Set(1, 5, 2, 3, 4))
      val ns = setFactory.empty[Int]

      ns.addAll(lInt)

      expect(ns.contains(1)).toBeTruthy
      expect(ns.pollFirst()).toEqual(1)
      expect(ns.contains(1)).toBeFalsy
      expect(ns.pollLast()).toEqual(5)
      expect(ns.pollFirst()).toEqual(2)
      expect(ns.pollLast()).toEqual(4)
      expect(ns.pollFirst()).toEqual(3)
      expect(ns.isEmpty()).toBeTruthy
    }
  }
}

object NavigableSetFactory {
  def allFactories: Iterator[NavigableSetFactory] =
    ConcurrentSkipListSetFactory.allFactories
}

trait NavigableSetFactory extends SetFactory {
  def empty[E]: ju.NavigableSet[E]
}
