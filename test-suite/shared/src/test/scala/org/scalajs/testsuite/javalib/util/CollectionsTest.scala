/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.testsuite.javalib.util

import java.{util => ju}

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.CollectionsTestBase

import scala.reflect.ClassTag

import Utils._

class CollectionsTest extends CollectionsTestBase {

  private def checkImmutablilityOfCollectionApi[E](coll: ju.Collection[E],
      elem: E): Unit = {
    expectThrows(classOf[UnsupportedOperationException], coll.add(elem))
    expectThrows(classOf[UnsupportedOperationException],
        coll.addAll(TrivialImmutableCollection(elem)))
    assertFalse(coll.addAll(TrivialImmutableCollection[E]()))

    if (ju.Collections.frequency(coll, elem) != coll.size)
      expectThrows(classOf[Exception], coll.retainAll(TrivialImmutableCollection(elem)))
    else
      assertFalse(coll.retainAll(TrivialImmutableCollection(elem)))

    if (coll.contains(elem)) {
      expectThrows(classOf[Exception], coll.remove(elem))
      expectThrows(classOf[Exception], coll.removeAll(TrivialImmutableCollection(elem)))
    } else {
      assertFalse(coll.remove(elem))
      assertFalse(coll.removeAll(TrivialImmutableCollection(elem)))
    }
    assertFalse(coll.removeAll(TrivialImmutableCollection[E]()))

    if (!coll.isEmpty()) {
      expectThrows(classOf[Throwable], coll.clear())
    } else {
      coll.clear() // Should not throw
    }
  }

  private def checkImmutablilityOfSetApi[E](set: ju.Set[E], elem: E): Unit =
    checkImmutablilityOfCollectionApi(set, elem)

  private def checkImmutablilityOfListApi[E](list: ju.List[E], elem: E): Unit = {
    checkImmutablilityOfCollectionApi(list, elem)
    expectThrows(classOf[UnsupportedOperationException], list.add(0, elem))
    assertFalse(list.addAll(0, TrivialImmutableCollection[E]()))
    expectThrows(classOf[UnsupportedOperationException],
        list.addAll(0, TrivialImmutableCollection(elem)))
    expectThrows(classOf[UnsupportedOperationException], list.remove(0))
  }

  private def checkImmutablilityOfMapApi[K, V](map: ju.Map[K, V], k: K,
      v: V): Unit = {
    expectThrows(classOf[UnsupportedOperationException], map.put(k, v))
    expectThrows(classOf[UnsupportedOperationException],
        map.putAll(TrivialImmutableMap(k -> v)))
    map.putAll(TrivialImmutableMap[K, V]()) // Should not throw

    if (map.containsKey(k))
      expectThrows(classOf[Throwable], map.remove(k))
    else
      assertNull(map.remove(k).asInstanceOf[AnyRef])

    if (!map.isEmpty())
      expectThrows(classOf[Throwable], map.clear())
    else
      map.clear() // Should not throw
  }

  @Test def emptyIterator(): Unit = {
    def freshIter: ju.Iterator[Int] = ju.Collections.emptyIterator[Int]

    assertFalse(freshIter.hasNext)
    expectThrows(classOf[NoSuchElementException], freshIter.next())
    expectThrows(classOf[IllegalStateException], freshIter.remove())
  }

  @Test def emptyListIterator(): Unit = {
    def test[E: ClassTag](toElem: Int => E): Unit = {
      def freshIter: ju.ListIterator[E] = ju.Collections.emptyListIterator[E]

      assertFalse(freshIter.hasNext)
      assertFalse(freshIter.hasPrevious)
      expectThrows(classOf[NoSuchElementException], freshIter.next())
      expectThrows(classOf[NoSuchElementException], freshIter.previous())
      expectThrows(classOf[IllegalStateException], freshIter.remove())
      expectThrows(classOf[UnsupportedOperationException],
          freshIter.add(toElem(0)))
      expectThrows(classOf[IllegalStateException], freshIter.set(toElem(0)))
    }

    test[Int](_.toInt)
    test[Long](_.toLong)
    test[Double](_.toDouble)
  }

  @Test def emptyEnumeration(): Unit = {
    def freshEnum: ju.Enumeration[Int] = ju.Collections.emptyEnumeration[Int]

    assertFalse(freshEnum.hasMoreElements)
    expectThrows(classOf[NoSuchElementException], freshEnum.nextElement())
  }

  @Test def emptySet(): Unit = {
    def test[E: ClassTag](toElem: Int => E): Unit = {
      val emptySet = ju.Collections.emptySet[E]
      assertTrue(emptySet.isEmpty)
      assertEquals(0, emptySet.size)
      assertTrue(iteratorIsEmpty(emptySet.iterator()))
      checkImmutablilityOfSetApi(emptySet, toElem(0))
    }

    test[Int](_.toInt)
    test[Long](_.toLong)
    test[Double](_.toDouble)
  }

  @Test def emptyList(): Unit = {
    def test[E: ClassTag](toElem: Int => E): Unit = {
      val emptyList = ju.Collections.emptyList[E]
      assertTrue(emptyList.isEmpty)
      assertEquals(0, emptyList.size)
      assertTrue(iteratorIsEmpty(emptyList.iterator()))
      checkImmutablilityOfListApi(emptyList, toElem(0))
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
      checkImmutablilityOfMapApi(emptyMap, toKey(0), toValue(0))
    }

    test[Int, Int](_.toInt, _.toInt)
    test[Long, String](_.toLong, _.toString)
    test[Double, Double](_.toDouble, _.toDouble)
  }

  @Test def singleton(): Unit = {
    def test[E: ClassTag](toElem: Int => E): Unit = {
      val singletonSet = ju.Collections.singleton[E](toElem(0))
      assertTrue(singletonSet.contains(toElem(0)))
      assertEquals(1, singletonSet.size)
      assertEquals(1, iteratorSize(singletonSet.iterator()))
      checkImmutablilityOfSetApi(singletonSet, toElem(0))
      checkImmutablilityOfSetApi(singletonSet, toElem(1))
    }

    test[Int](_.toInt)
    test[Long](_.toLong)
    test[Double](_.toDouble)
  }

  @Test def singletonList(): Unit = {
    def test[E: ClassTag](toElem: Int => E): Unit = {
      val singletonList = ju.Collections.singletonList[E](toElem(0))
      assertTrue(singletonList.contains(toElem(0)))
      assertEquals(1, singletonList.size)
      assertEquals(1, iteratorSize(singletonList.iterator()))
      checkImmutablilityOfListApi(singletonList, toElem(0))
      checkImmutablilityOfListApi(singletonList, toElem(1))
    }

    test[Int](_.toInt)
    test[Long](_.toLong)
    test[Double](_.toDouble)
  }

  @Test def singletonMap(): Unit = {
    def test[K, V](toKey: Int => K, toValue: Int => V): Unit = {
      val singletonMap = ju.Collections.singletonMap[K, V](toKey(0), toValue(1))
      assertEquals(toValue(1), singletonMap.get(toKey(0)))
      assertEquals(1, singletonMap.size)
      assertEquals(1, iteratorSize(singletonMap.entrySet().iterator()))
      assertEquals(1, iteratorSize(singletonMap.keySet().iterator()))
      assertEquals(1, iteratorSize(singletonMap.values().iterator()))
      checkImmutablilityOfMapApi(singletonMap, toKey(0), toValue(0))
      checkImmutablilityOfMapApi(singletonMap, toKey(1), toValue(1))
    }

    test[Int, Int](_.toInt, _.toInt)
    test[Long, String](_.toLong, _.toString)
    test[Double, Double](_.toDouble, _.toDouble)
  }

  @Test def nCopies(): Unit = {
    def test[E: ClassTag](toElem: Int => E): Unit = {
      for (n <- Seq(1, 4, 543)) {
        val nCopies = ju.Collections.nCopies(n, toElem(0))
        assertTrue(nCopies.contains(toElem(0)))
        assertEquals(n, ju.Collections.frequency(nCopies, toElem(0)))
        assertEquals(n, nCopies.size)
        assertEquals(n, iteratorSize(nCopies.iterator()))
        checkImmutablilityOfListApi(nCopies, toElem(0))
        checkImmutablilityOfListApi(nCopies, toElem(1))
      }

      val zeroCopies = ju.Collections.nCopies(0, toElem(0))
      assertFalse(zeroCopies.contains(toElem(0)))
      assertEquals(0, zeroCopies.size)
      assertTrue(iteratorIsEmpty(zeroCopies.iterator()))
      checkImmutablilityOfListApi(zeroCopies, toElem(0))

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

  @Test def reverseOrder_with_null_comparator(): Unit = {
    // Essentially equivalent to reverseOrder_on_comparables

    def testNumerical[E](toElem: Int => E): Unit = {
      val rCmp = ju.Collections.reverseOrder[E](null)
      for (i <- range) {
        assertEquals(0, rCmp.compare(toElem(i), toElem(i)))
        assertTrue(rCmp.compare(toElem(i), toElem(i - 1)) < 0)
        assertTrue(rCmp.compare(toElem(i), toElem(i + 1)) > 0)
      }
    }

    testNumerical[Int](_.toInt)
    testNumerical[Long](_.toLong)
    testNumerical[Double](_.toDouble)

    val rCmp = ju.Collections.reverseOrder[String](null)

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

  @Test def enumeration(): Unit = {
    val coll = TrivialImmutableCollection(range: _*)
    val enumeration = ju.Collections.enumeration(coll)
    for (elem <- range) {
      assertTrue(enumeration.hasMoreElements)
      assertEquals(elem, enumeration.nextElement())
    }
    assertFalse(enumeration.hasMoreElements)
  }

  @Test def list(): Unit = {
    val elementCount = 30

    val enumeration = new ju.Enumeration[Int] {
      private var next: Int = 0
      def hasMoreElements(): Boolean = next != elementCount
      def nextElement(): Int = {
        next += 1
        next - 1
      }
    }

    val list = ju.Collections.list(enumeration)
    assertEquals(elementCount, list.size)
    for (i <- 0 until elementCount)
      assertEquals(i, list.get(i))
  }

  @Test def frequency(): Unit = {
    val coll = TrivialImmutableCollection(5, 68, 12, 5, 5, 3, 12, 40, 56)

    assertEquals(0, ju.Collections.frequency(coll, 1))
    assertEquals(1, ju.Collections.frequency(coll, 3))
    assertEquals(3, ju.Collections.frequency(coll, 5))
    assertEquals(2, ju.Collections.frequency(coll, 12))
    assertEquals(1, ju.Collections.frequency(coll, 40))
    assertEquals(1, ju.Collections.frequency(coll, 56))
    assertEquals(1, ju.Collections.frequency(coll, 68))
  }

  @Test def disjoint(): Unit = {
    def coll(range: Range): ju.Collection[Int] =
      TrivialImmutableCollection(range: _*)

    assertFalse(ju.Collections.disjoint(coll(0 to 3), coll(0 to 3)))
    assertFalse(ju.Collections.disjoint(coll(0 to 3), coll(3 to 5)))
    assertTrue(ju.Collections.disjoint(coll(0 to 3), coll(6 to 9)))
    assertTrue(ju.Collections.disjoint(coll(0 to -1), coll(0 to 3)))
    assertTrue(ju.Collections.disjoint(coll(0 to 3), coll(0 to -1)))
    assertTrue(ju.Collections.disjoint(coll(0 to -1), coll(0 to -1)))
  }
}
