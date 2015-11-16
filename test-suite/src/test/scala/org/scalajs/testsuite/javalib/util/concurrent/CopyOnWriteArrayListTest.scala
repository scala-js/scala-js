/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util.concurrent

import java.{util => ju}

import org.scalajs.testsuite.javalib.util.ListTest
import org.scalajs.testsuite.javalib.util.ListFactory

import scala.collection.JavaConversions._

object CopyOnWriteArrayListTest extends CopyOnWriteArrayListTest(new CopyOnWriteArrayListFactory)

abstract class CopyOnWriteArrayListTest[F <: CopyOnWriteArrayListFactory](listFactory: F) extends ListTest {

  describe(listFactory.implementationName) {
    testApi()
  }

  def testApi(): Unit = {
    testListApi(listFactory)

    it ("should implement addIfAbsent") {
      val list = listFactory.empty[Int]

      expect(list.addIfAbsent(0)).toBeTruthy
      expect(list.size).toEqual(1)
      expect(list.get(0)).toEqual(0)

      expect(list.addIfAbsent(0)).toBeFalsy
      expect(list.size).toEqual(1)
      expect(list.get(0)).toEqual(0)

      expect(list.addIfAbsent(1)).toBeTruthy
      expect(list.size).toEqual(2)
      expect(list.get(0)).toEqual(0)
      expect(list.get(1)).toEqual(1)
    }

    it ("should implement addAllAbsent") {
      val list = listFactory.empty[Int]

      expect(list.addAllAbsent(0 until 3)).toEqual(3)
      expect(list.size).toEqual(3)
      for (i <- 0 until 3)
        expect(list.get(i)).toEqual(i)

      expect(list.addAllAbsent(0 until 2)).toEqual(0)
      expect(list.size).toEqual(3)
      for (i <- 0 until 3)
        expect(list.get(i)).toEqual(i)

      expect(list.addAllAbsent(3 until 6)).toEqual(3)
      expect(list.size).toEqual(6)
      for (i <- 0 until 6)
        expect(list.get(i)).toEqual(i)

      expect(list.addAllAbsent(0 until 10)).toEqual(4)
      expect(list.size).toEqual(10)
      for (i <- 0 until 10)
        expect(list.get(i)).toEqual(i)

      expect(list.addAllAbsent(Seq(42, 42, 42))).toEqual(3)
      expect(list.size).toEqual(13)
      for (i <- 0 until 10)
        expect(list.get(i)).toEqual(i)
      for (i <- 10 until 13)
        expect(list.get(i)).toEqual(42)
    }

    it ("should implement a snapshot iterator") {
      val list = listFactory.empty[Int]
      list.addAll(0 to 10)

      val iter = list.iterator()
      list.clear()
      val iter2 = list.iterator()
      list.addAll(0 to 5)

      for (i <- 0 to 10) {
        expect(iter.hasNext).toBeTruthy
        if (iter.hasNext)
          expect(iter.next()).toEqual(i)
      }
      expect(iter2.hasNext).toBeFalsy
    }

    it ("should have accessible array constructor - #2023") {
      def test[T <: AnyRef](arr: Array[T]): Unit = {
        val cowal1 = new ju.concurrent.CopyOnWriteArrayList(arr)
        expect(cowal1.length).toEqual(arr.length)
        for (i <- arr.indices)
          expect(cowal1.get(i) == arr(i)).toBeTruthy
      }

      test(Array("a", "", "da", "23"))
      test(Array[Integer](1, 7, 2, 5, 3))
      test(Array[Character]('a', '3', '5', 'g', 'a'))
    }
  }
}

class CopyOnWriteArrayListFactory extends ListFactory {

  override def allowsMutationThroughIterator: Boolean = false

  override def implementationName: String =
    "java.util.concurrent.CopyOnWriteArrayList"

  override def empty[E]: ju.concurrent.CopyOnWriteArrayList[E] =
    new ju.concurrent.CopyOnWriteArrayList[E]
}
