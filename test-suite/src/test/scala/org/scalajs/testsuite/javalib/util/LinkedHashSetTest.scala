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

class LinkedHashSetTest[F <: LinkedHashSetFactory](linkedHashSetFactory: F)
  extends HashSetTest[F](linkedHashSetFactory) {

  override def testApi(): Unit = {
    super.testApi()

    it("should iterate over elements in an ordered manner") {
      val hs = new ju.LinkedHashSet[String]()

      val l1 = List[String]("ONE", "TWO", (null: String))
      expect(hs.addAll(asJavaCollection(l1))).toBeTruthy
      expect(hs.size).toEqual(3)

      val iter1 = hs.iterator()
      val result1 = {
        for (i <- 0 until 3) yield {
          expect(iter1.hasNext()).toBeTruthy
          val value = iter1.next()
          expect(value).toEqual(l1(i))
        }
      }
      expect(iter1.hasNext()).toBeFalsy
      expect(result1.equals(l1)).toBeTruthy

      val l2 = l1 :+ "THREE"
      expect(hs.add(l2(3))).toBeTruthy

      val iter2 = hs.iterator()
      val result2 = {
        for (i <- 0 until 4) yield {
          expect(iter2.hasNext()).toBeTruthy
          val value = iter2.next()
          expect(value).toEqual(l2(i))
          value
        }
      }
      expect(iter2.hasNext()).toBeFalsy
      expect(result2.equals(l2)).toBeTruthy
    }
  }
}

object LinkedHashSetFactory extends HashSetFactory {
  def allFactories: Iterator[LinkedHashSetFactory] =
    Iterator(new LinkedHashSetFactory)
}

class LinkedHashSetFactory extends HashSetFactory {
  override def implementationName: String =
    "java.util.LinkedHashSet"

  override def empty[E]: ju.LinkedHashSet[E] =
    new ju.LinkedHashSet[E]()
}
