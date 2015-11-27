/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import java.{util => ju}

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.CollectionsTestBase
import org.scalajs.testsuite.utils.Platform.executingInJVM

import scala.collection.JavaConversions._

class CollectionsTest extends CollectionsTestBase {

  @Test def emptySet(): Unit = {
    def test[E](toElem: Int => E): Unit = {
      val emptySet = ju.Collections.emptySet[E]
      assertTrue(emptySet.isEmpty)
      assertEquals(0, emptySet.size)
      assertEquals(0, emptySet.iterator.size)
      if (!executingInJVM) // Issue #2080
        testSetImmutability(emptySet, toElem(0))
    }

    test[Int](_.toInt)
    test[Long](_.toLong)
    test[Double](_.toDouble)
  }

  @Test def emptyList(): Unit = {
    def test[E](toElem: Int => E): Unit = {
      val emptyList = ju.Collections.emptyList[E]
      assertTrue(emptyList.isEmpty)
      assertEquals(0, emptyList.size)
      assertEquals(0, emptyList.iterator.size)
      if (!executingInJVM) // Issue #2080
        testListImmutability(emptyList, toElem(0))
    }

    test[Int](_.toInt)
    test[Long](_.toLong)
    test[Double](_.toDouble)
  }

  @Test def emptyMap(): Unit = {
    def test[K, V](toKey: Int => K, toValue: Int => V): Unit = {
      val emptyMap = ju.Collections.emptyMap[K, V]
      assertTrue(emptyMap.isEmpty)
      assertEquals(0, emptyMap.size)
      assertEquals(0, emptyMap.entrySet.size)
      assertEquals(0, emptyMap.keySet.size)
      assertEquals(0, emptyMap.values.size)
      if (!executingInJVM)
        testMapImmutability(emptyMap, toKey(0), toValue(0))
    }

    test[Int, Int](_.toInt, _.toInt)
    test[Long, String](_.toLong, _.toString)
    test[Double, Double](_.toDouble, _.toDouble)
  }

  @Test def singleton(): Unit = {
    def test[E](toElem: Int => E): Unit = {
      val singletonSet = ju.Collections.singleton[E](toElem(0))
      assertTrue(singletonSet.contains(toElem(0)))
      assertEquals(1, singletonSet.size)
      assertEquals(1, singletonSet.iterator.size)
      if (!executingInJVM) // Issue #2080
        testSetImmutability(singletonSet, toElem(0))
    }

    test[Int](_.toInt)
    test[Long](_.toLong)
    test[Double](_.toDouble)
  }

  @Test def singletonList(): Unit = {
    def test[E](toElem: Int => E): Unit = {
      val singletonList = ju.Collections.singletonList[E](toElem(0))
      assertTrue(singletonList.contains(toElem(0)))
      assertEquals(1, singletonList.size)
      assertEquals(1, singletonList.iterator.size)
      if (!executingInJVM) // Issue #2080
        testListImmutability(singletonList, toElem(0))
    }

    test[Int](_.toInt)
    test[Long](_.toLong)
    test[Double](_.toDouble)
  }

  @Test def singletonMap(): Unit = {
    def test[K, V](toKey: Int => K, toValue: Int => V): Unit = {
      val singletonMap = ju.Collections.singletonMap[K, V](toKey(0), toValue(1))
      assertTrue(singletonMap.get(toKey(0)) == toValue(1))
      assertEquals(1, singletonMap.size)
      assertEquals(1, singletonMap.iterator.size)
      if (!executingInJVM) // Issue #2080
        testMapImmutability(singletonMap, toKey(0), toValue(0))
    }

    test[Int, Int](_.toInt, _.toInt)
    test[Long, String](_.toLong, _.toString)
    test[Double, Double](_.toDouble, _.toDouble)
  }

  @Test def nCopies(): Unit = {
    def test[E](toElem: Int => E): Unit = {
      for (n <- Seq(1, 4, 543)) {
        val nCopies = ju.Collections.nCopies(n, toElem(0))
        assertTrue(nCopies.contains(toElem(0)))
        nCopies.forall(_ == toElem(0))
        assertEquals(n, nCopies.size)
        assertEquals(n, nCopies.iterator.size)
        if (!executingInJVM) // Issue #2080
          testListImmutability(nCopies, toElem(0))
      }

      val zeroCopies = ju.Collections.nCopies(0, toElem(0))
      assertFalse(zeroCopies.contains(toElem(0)))
      assertEquals(0, zeroCopies.size)
      assertEquals(0, zeroCopies.iterator.size)
      if (!executingInJVM) // Issue #2080
        testListImmutability(zeroCopies, toElem(0))

      for (n <- Seq(-1, -4, -543)) {
        expectThrows(classOf[IllegalArgumentException],
          ju.Collections.nCopies(n, toElem(0)))
      }
    }

    test[Int](_.toInt)
    test[Long](_.toLong)
    test[Double](_.toDouble)
  }

  @Test def reverseOrder_on_comparables(): Unit = {
    def testNumerical[E](toElem: Int => E): Unit = {
      val rCmp = ju.Collections.reverseOrder[E]
      for (i <- range) {
        assertEquals(0, rCmp.compare(toElem(i), toElem(i)))
        assertTrue(rCmp.compare(toElem(i), toElem(i - 1)) < 0)
        assertTrue(rCmp.compare(toElem(i), toElem(i + 1)) > 0)
      }
    }

    testNumerical[Int](_.toInt)
    testNumerical[Long](_.toLong)
    testNumerical[Double](_.toDouble)

    val rCmp = ju.Collections.reverseOrder[String]

    assertEquals(0, rCmp.compare("", ""))
    assertEquals(0, rCmp.compare("a", "a"))
    assertEquals(0, rCmp.compare("123", "123"))
    assertEquals(0, rCmp.compare("hello world", "hello world"))

    assertTrue(rCmp.compare("a", "b") > 0)
    assertTrue(rCmp.compare("a", "ba") > 0)
    assertTrue(rCmp.compare("a", "aa") > 0)
    assertTrue(rCmp.compare("aa", "aaa") > 0)

    assertTrue(rCmp.compare("b", "a") < 0)
    assertTrue(rCmp.compare("ba", "a") < 0)
    assertTrue(rCmp.compare("aa", "a") < 0)
    assertTrue(rCmp.compare("aaa", "aa") < 0)
  }

  @Test def reverseOrder_with_comparator(): Unit = {
    val rCmp1 = new ju.Comparator[Int] {
      override def compare(o1: Int, o2: Int): Int = o2 - o1
    }
    val rCmp2 = ju.Collections.reverseOrder(new ju.Comparator[Int] {
      override def compare(o1: Int, o2: Int): Int = o1 - o2
    })

    scala.util.Random.setSeed(42)
    for (_ <- 0 to 50) {
      val num = scala.util.Random.nextInt(10000)
      assertEquals(0, rCmp1.compare(num, num))
      assertEquals(0, rCmp2.compare(num, num))
    }

    for (i <- range) {
      for (_ <- 1 to 10) {
        val num = scala.util.Random.nextInt(10000) + 1
        assertTrue(rCmp1.compare(i, i + num) > 0)
        assertTrue(rCmp2.compare(i, i + num) > 0)
        assertTrue(rCmp1.compare(i, i - num) < 0)
        assertTrue(rCmp2.compare(i, i - num) < 0)
      }
    }

    for (_ <- 1 to 100) {
      val num1 = scala.util.Random.nextInt(10000)
      val num2 = scala.util.Random.nextInt(10000)
      assertEquals(rCmp2.compare(num1, num2), rCmp1.compare(num1, num2))
    }
  }

  @Test def enumeration(): Unit = {
    val coll = asJavaCollection(range)
    val enum = ju.Collections.enumeration(coll)
    for (elem <- coll) {
      assertTrue(enum.hasMoreElements)
      assertEquals(elem, enum.nextElement())
    }
    assertFalse(enum.hasMoreElements)
  }

  @Test def list(): Unit = {
    val enum = asJavaEnumeration(range.iterator)
    val list = ju.Collections.list(enum)
    assertEquals(range.size, list.size)
    for (i <- range)
      assertEquals(i, list.get(i))
  }

  @Test def disjoint(): Unit = {
    assertFalse(ju.Collections.disjoint(0 to 3, 0 to 3))
    assertFalse(ju.Collections.disjoint(0 to 3, 3 to 5))
    assertTrue(ju.Collections.disjoint(0 to 3, 6 to 9))
    assertTrue(ju.Collections.disjoint(0 to -1, 0 to 3))
    assertTrue(ju.Collections.disjoint(0 to 3, 0 to -1))
    assertTrue(ju.Collections.disjoint(0 to -1, 0 to -1))
  }
}
