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

package org.scalajs.testsuite.library

import scala.scalajs.js

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform.scalaVersion

import scala.reflect.{ClassTag, classTag}

object ArrayOpsTest {
  @noinline
  def assertJSArrayEquals[A: ClassTag](expected: js.Array[_],
      actual: A): Unit = {
    assertEquals("wrong inferred type", classOf[js.Array[_]],
        classTag[A].runtimeClass)
    val actual1 = actual.asInstanceOf[js.Array[_]]
    assertEquals("lengths differ", expected.length, actual1.length)
    for (i <- 0 until expected.length)
      assertEquals(s"elems differ at index $i", expected(i), actual1(i))
  }

  @noinline
  def assertJSArrayEqualsNotSame[A: ClassTag](unexpected: js.Array[_],
      expected: js.Array[_], actual: A): Unit = {
    assertNotSame(unexpected, actual)
    assertJSArrayEquals(expected, actual)
  }

  @noinline
  def assertJSArrayEqualsSame[A: ClassTag](original: js.Array[_],
      expected: js.Array[_], actual: A): Unit = {
    assertSame(original, actual)
    assertJSArrayEquals(expected, actual)
  }

  @noinline
  def assertJSArrayPairEquals(expected: (js.Array[_], js.Array[_]),
      actual: (js.Array[_], js.Array[_])): Unit = {
    assertJSArrayEquals(expected._1, actual._1)
    assertJSArrayEquals(expected._2, actual._2)
  }

  object FallbackImplicits {
    implicit class JSArrayOpsFallback[A](self: js.Any) {
      def sizeCompare(otherSize: Int): Int =
        throw new AssertionError("unreachable code")

      def sizeIs: Int =
        throw new AssertionError("unreachable code")

      def lengthIs: Int =
        throw new AssertionError("unreachable code")

      def partitionMap[A1, A2](f: Any => Either[A1, A2]): (js.Array[A1], js.Array[A2]) =
        throw new AssertionError("unreachable code")
    }
  }
}

class ArrayOpsTest {
  import ArrayOpsTest._

  @Test def size(): Unit = {
    assertEquals(4, js.Array(1, 2, 5, 65).size)
  }

  @Test def isEmpty(): Unit = {
    assertTrue(js.Array[Int]().isEmpty)
    assertFalse(js.Array(5, 6).isEmpty)
  }

  @Test def nonEmpty(): Unit = {
    assertFalse(js.Array[Int]().nonEmpty)
    assertTrue(js.Array(5, 6).nonEmpty)
  }

  @Test def head(): Unit = {
    assertEquals(5, js.Array(5, 7, 10).head)

    assertThrows(classOf[NoSuchElementException], js.Array[Int]().head)
  }

  @Test def last(): Unit = {
    assertEquals(10, js.Array(5, 7, 10).last)

    assertThrows(classOf[NoSuchElementException], js.Array[Int]().last)
  }

  @Test def headOption(): Unit = {
    assertEquals(Some(5), js.Array(5, 7, 10).headOption)
    assertEquals(None, js.Array[Int]().headOption)
  }

  @Test def lastOption(): Unit = {
    assertEquals(Some(10), js.Array(5, 7, 10).lastOption)
    assertEquals(None, js.Array[Int]().lastOption)
  }

  @Test def sizeCompare(): Unit = {
    assumeFalse("sizeCompare was added in 2.13",
        scalaVersion.startsWith("2.11.") ||
        scalaVersion.startsWith("2.12."))

    import FallbackImplicits._
    import js.Any.jsArrayOps

    val array = js.Array(5, 7, 10)
    assertEquals(0, array.sizeCompare(3))
    assertTrue(array.sizeCompare(1) > 0)
    assertTrue(array.sizeCompare(6) < 0)
  }

  @Test def lengthCompare(): Unit = {
    val array = js.Array(5, 7, 10)
    assertEquals(0, array.lengthCompare(3))
    assertTrue(array.lengthCompare(1) > 0)
    assertTrue(array.lengthCompare(6) < 0)
  }

  @Test def sizeIs(): Unit = {
    assumeFalse("sizeIs was added in 2.13",
        scalaVersion.startsWith("2.11.") ||
        scalaVersion.startsWith("2.12."))

    import FallbackImplicits._
    import js.Any.jsArrayOps

    val array = js.Array(5, 7, 10)
    assertTrue(array.sizeIs == 3)
    assertTrue(array.sizeIs > 1)
    assertTrue(array.sizeIs < 6)
  }

  @Test def lengthIs(): Unit = {
    assumeFalse("lengthIs was added in 2.13",
        scalaVersion.startsWith("2.11.") ||
        scalaVersion.startsWith("2.12."))

    import FallbackImplicits._
    import js.Any.jsArrayOps

    val array = js.Array(5, 7, 10)
    assertTrue(array.lengthIs == 3)
    assertTrue(array.lengthIs > 1)
    assertTrue(array.lengthIs < 6)
  }

  @Test def slice(): Unit = {
    val array = js.Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    assertJSArrayEqualsNotSame(array, js.Array(3, 4, 5), array.slice(2, 5))
    assertJSArrayEqualsNotSame(array, js.Array(1, 2, 3, 4, 5), array.slice(-2, 5))
    assertJSArrayEqualsNotSame(array, js.Array(), array.slice(2, -5))
    assertJSArrayEqualsNotSame(array, js.Array(), array.slice(-5, -2))
    assertJSArrayEqualsNotSame(array, js.Array(8, 9, 10), array.slice(7, 15))
  }

  @Test def tail(): Unit = {
    val array = js.Array(1, 5, 7, 2)
    assertJSArrayEqualsNotSame(array, js.Array(5, 7, 2), array.tail)
    assertThrows(classOf[UnsupportedOperationException], js.Array[Int]().tail)
  }

  @Test def init(): Unit = {
    val array = js.Array(1, 5, 7, 2)
    assertJSArrayEqualsNotSame(array, js.Array(1, 5, 7), array.init)
    assertThrows(classOf[UnsupportedOperationException], js.Array[Int]().init)
  }

  @Test def tails(): Unit = {
    val array = js.Array(1, 5, 7, 2)
    val tailsIter = array.tails
    assertJSArrayEquals(array, tailsIter.next())
    assertJSArrayEqualsNotSame(array, js.Array(5, 7, 2), tailsIter.next())
    assertJSArrayEqualsNotSame(array, js.Array(7, 2), tailsIter.next())
    assertJSArrayEqualsNotSame(array, js.Array(2), tailsIter.next())
    assertJSArrayEqualsNotSame(array, js.Array(), tailsIter.next())
    assertFalse(tailsIter.hasNext)
  }

  @Test def inits(): Unit = {
    val array = js.Array(1, 5, 7, 2)
    val initsIter = array.inits
    assertJSArrayEquals(array, initsIter.next())
    assertJSArrayEqualsNotSame(array, js.Array(1, 5, 7), initsIter.next())
    assertJSArrayEqualsNotSame(array, js.Array(1, 5), initsIter.next())
    assertJSArrayEqualsNotSame(array, js.Array(1), initsIter.next())
    assertJSArrayEqualsNotSame(array, js.Array(), initsIter.next())
    assertFalse(initsIter.hasNext)
  }

  @Test def take(): Unit = {
    val array = js.Array(1, 5, 7, 2)
    assertJSArrayEqualsNotSame(array, js.Array(1, 5, 7), array.take(3))
    assertJSArrayEqualsNotSame(array, array, array.take(10))
    assertJSArrayEqualsNotSame(array, js.Array(), array.take(-3))
    assertJSArrayEqualsNotSame(array, js.Array(1, 5), array.take(2))
  }

  @Test def drop(): Unit = {
    val array = js.Array(1, 5, 7, 2)
    assertJSArrayEqualsNotSame(array, js.Array(2), array.drop(3))
    assertJSArrayEqualsNotSame(array, js.Array(), array.drop(10))
    assertJSArrayEqualsNotSame(array, array, array.drop(-3))
    assertJSArrayEqualsNotSame(array, js.Array(7, 2), array.drop(2))
  }

  @Test def takeRight(): Unit = {
    val array = js.Array(1, 5, 7, 2)
    assertJSArrayEqualsNotSame(array, js.Array(2), array.takeRight(1))
    assertJSArrayEqualsNotSame(array, js.Array(), array.takeRight(-3))
    assertJSArrayEqualsNotSame(array, array, array.takeRight(10))
    assertJSArrayEqualsNotSame(array, js.Array(7, 2), array.takeRight(2))
  }

  @Test def dropRight(): Unit = {
    val array = js.Array(1, 5, 7, 2)
    assertJSArrayEqualsNotSame(array, js.Array(1, 5, 7), array.dropRight(1))
    assertJSArrayEqualsNotSame(array, array, array.dropRight(-3))
    assertJSArrayEqualsNotSame(array, js.Array(), array.dropRight(10))
    assertJSArrayEqualsNotSame(array, js.Array(1, 5), array.dropRight(2))
  }

  @Test def takeWhile(): Unit = {
    val array = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)
    assertJSArrayEqualsNotSame(array, js.Array(), array.takeWhile(_ < 0))
    assertJSArrayEqualsNotSame(array, js.Array(1, 5, 7, 2), array.takeWhile(_ < 10))
    assertJSArrayEqualsNotSame(array, array, array.takeWhile(_ < 100))
  }

  @Test def dropWhile(): Unit = {
    val array = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)
    assertJSArrayEqualsNotSame(array, array, array.dropWhile(_ < 0))
    assertJSArrayEqualsNotSame(array, js.Array(54, 2, 78, 0, 3), array.dropWhile(_ < 10))
    assertJSArrayEqualsNotSame(array, js.Array(), array.dropWhile(_ < 100))
  }

  @Test def iterator(): Unit = {
    val array = js.Array(1, 5, 7, 2)
    val iter = array.iterator
    assertEquals(1, iter.next())
    assertEquals(5, iter.next())
    assertEquals(7, iter.next())
    assertEquals(2, iter.next())
    assertFalse(iter.hasNext)

    assertThrows(classOf[NoSuchElementException], iter.next())
  }

  @Test def grouped(): Unit = {
    val array = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)
    val iter = array.grouped(3)
    assertJSArrayEquals(js.Array(1, 5, 7), iter.next())
    assertJSArrayEquals(js.Array(2, 54, 2), iter.next())
    assertJSArrayEquals(js.Array(78, 0, 3), iter.next())
    assertFalse(iter.hasNext)
    assertThrows(classOf[NoSuchElementException], iter.next())

    val iter2 = array.grouped(4)
    assertJSArrayEquals(js.Array(1, 5, 7, 2), iter2.next())
    assertJSArrayEquals(js.Array(54, 2, 78, 0), iter2.next())
    assertJSArrayEquals(js.Array(3), iter2.next())
    assertFalse(iter2.hasNext)
    assertThrows(classOf[NoSuchElementException], iter2.next())
  }

  @Test def span(): Unit = {
    val array = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)
    assertJSArrayPairEquals((js.Array(), array), array.span(_ < 0))
    assertJSArrayPairEquals((js.Array(1, 5, 7, 2), js.Array(54, 2, 78, 0, 3)), array.span(_ < 10))
    assertJSArrayPairEquals((array, js.Array()), array.span(_ < 100))
  }

  @Test def splitAt(): Unit = {
    val array = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)
    assertJSArrayPairEquals((js.Array(), array), array.splitAt(-2))
    assertJSArrayPairEquals((js.Array(1, 5, 7, 2), js.Array(54, 2, 78, 0, 3)), array.splitAt(4))
    assertJSArrayPairEquals((array, js.Array()), array.splitAt(15))
  }

  @Test def partition(): Unit = {
    val array = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)
    assertJSArrayPairEquals((js.Array(), array), array.partition(_ < 0))
    assertJSArrayPairEquals((js.Array(1, 5, 7, 2, 2, 0, 3), js.Array(54, 78)), array.partition(_ < 10))
    assertJSArrayPairEquals((array, js.Array()), array.partition(_ < 100))
  }

  @Test def partitionMap(): Unit = {
    assumeFalse("partitionMap was added in 2.13",
        scalaVersion.startsWith("2.11.") ||
        scalaVersion.startsWith("2.12."))

    import FallbackImplicits._
    import js.Any.jsArrayOps

    val array = js.Array[Any](1, "one", 2, "two", 3, "three")
    val resultInferType = array.partitionMap {
      case x: Int    => Left(x)
      case x: String => Right(x)
    }
    val result: (js.Array[Int], js.Array[String]) = resultInferType
    assertJSArrayPairEquals((js.Array(1, 2, 3), js.Array("one", "two", "three")), result)
  }

  @Test def reverse(): Unit = {
    val array = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)
    assertJSArrayEqualsNotSame(array, js.Array(3, 0, 78, 2, 54, 2, 7, 5, 1), array.reverse)
  }

  @Test def reverseIterator(): Unit = {
    val array = js.Array(1, 5, 7, 2)
    val iter = array.reverseIterator
    assertEquals(2, iter.next())
    assertEquals(7, iter.next())
    assertEquals(5, iter.next())
    assertEquals(1, iter.next())
    assertFalse(iter.hasNext)
    assertThrows(classOf[NoSuchElementException], iter.next())
  }

  @Test def filter(): Unit = {
    val array = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)
    assertJSArrayEqualsNotSame(array, js.Array(), array.filter(_ < 0))
    assertJSArrayEqualsNotSame(array, js.Array(1, 5, 7, 2, 2, 0, 3), array.filter(_ < 10))
    assertJSArrayEqualsNotSame(array, array, array.filter(_ < 100))
  }

  @Test def filterNot(): Unit = {
    val array = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)
    assertJSArrayEqualsNotSame(array, array, array.filterNot(_ < 0))
    assertJSArrayEqualsNotSame(array, js.Array(54, 78), array.filterNot(_ < 10))
    assertJSArrayEqualsNotSame(array, js.Array(), array.filterNot(_ < 100))
  }

  @Test def sorted(): Unit = {
    val array = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)
    assertJSArrayEqualsNotSame(array, js.Array(0, 1, 2, 2, 3, 5, 7, 54, 78), array.sorted)
  }

  @Test def sortWith(): Unit = {
    val array = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)
    assertJSArrayEqualsNotSame(array, js.Array(78, 54, 7, 5, 3, 2, 2, 1, 0), array.sortWith(_ > _))
  }

  @Test def sortBy(): Unit = {
    val array = js.Array(1 -> "1", 5 -> "5", 7 -> "7", 2 -> "2", 54 -> "54",
        2 -> "2 2nd", 78 -> "78", 0 -> "0", 3 -> "3")
    assertJSArrayEqualsNotSame(array,
        js.Array(0 -> "0", 1 -> "1", 2 -> "2", 2 -> "2 2nd", 3 -> "3",
            5 -> "5", 7 -> "7", 54 -> "54", 78 -> "78"),
        array.sortBy(_._1))
  }

  @Test def indexOf(): Unit = {
    val array = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)
    assertEquals(2, array.indexOf(7))
    assertEquals(3, array.indexOf(2))
    assertEquals(6, array.indexOf(78))
    assertEquals(-1, array.indexOf(123))

    assertEquals(5, array.indexOf(2, 4))
    assertEquals(-1, array.indexOf(7, 4))
  }

  @Test def indexWhere(): Unit = {
    val array = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)
    assertEquals(2, array.indexWhere(_ > 5))
    assertEquals(-1, array.indexWhere(_ < 0))

    assertEquals(4, array.indexWhere(_ > 5, 4))
    assertEquals(-1, array.indexWhere(_ > 10, 7))
  }

  @Test def lastIndexOf(): Unit = {
    val array = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)
    assertEquals(2, array.lastIndexOf(7))
    assertEquals(5, array.lastIndexOf(2))
    assertEquals(6, array.lastIndexOf(78))
    assertEquals(-1, array.lastIndexOf(123))

    assertEquals(3, array.lastIndexOf(2, 4))
    assertEquals(-1, array.lastIndexOf(78, 4))
  }

  @Test def lastIndexWhere(): Unit = {
    val array = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)
    assertEquals(6, array.lastIndexWhere(_ > 5))
    assertEquals(-1, array.lastIndexWhere(_ < 0))

    assertEquals(2, array.lastIndexWhere(_ > 5, 3))
    assertEquals(-1, array.lastIndexWhere(_ > 10, 3))
  }

  @Test def find(): Unit = {
    val array = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)
    assertEquals(Some(7), array.find(_ > 5))
    assertEquals(Some(54), array.find(_ > 10))
    assertEquals(None, array.find(_ > 100))
  }

  @Test def exists(): Unit = {
    val array = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)
    assertTrue(array.exists(_ > 5))
    assertTrue(array.exists(_ > 10))
    assertFalse(array.exists(_ > 100))
  }

  @Test def forall(): Unit = {
    val array = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)
    assertFalse(array.forall(_ < 5))
    assertFalse(array.forall(_ < 10))
    assertTrue(array.forall(_ < 100))
  }

  @Test def foldLeft(): Unit = {
    val array = js.Array(6, 2, 56, -1)
    assertEquals(37, array.foldLeft(100)(_ - _))
  }

  @Test def scanLeft(): Unit = {
    val array = js.Array(6, 2, 56, -1)
    assertJSArrayEqualsNotSame(array, js.Array(100, 94, 92, 36, 37), array.scanLeft(100)(_ - _))
  }

  @Test def scan(): Unit = {
    val array = js.Array(6, 2, 56, -1)
    assertJSArrayEqualsNotSame(array, js.Array(0, 6, 8, 64, 63), array.scanLeft(0)(_ + _))
  }

  @Test def scanRight(): Unit = {
    val array = js.Array(6, 2, 56, -1)
    assertJSArrayEqualsNotSame(array, js.Array(161, -155, 157, -101, 100), array.scanRight(100)(_ - _))
  }

  @Test def foldRight(): Unit = {
    val array = js.Array(6, 2, 56, -1)
    assertEquals(161, array.foldRight(100)(_ - _))
  }

  @Test def fold(): Unit = {
    val array = js.Array(6, 2, 56, -1)
    assertEquals(63, array.fold(0)(_ + _))
  }

  @Test def map(): Unit = {
    val array = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)
    assertJSArrayEqualsNotSame(array, js.Array(3, 15, 21, 6, 162, 6, 234, 0, 9), array.map(_ * 3))
  }

  @Test def flatMap(): Unit = {
    val array = js.Array(6, 2, 56, -1)
    assertJSArrayEqualsNotSame(array,
        js.Array(6, 7, 8, 2, 3, 4, 56, 57, 58, -1, 0, 1),
        array.flatMap(x => js.Array(x, x + 1, x + 2)))
  }

  @Test def collect(): Unit = {
    val array = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)
    assertJSArrayEqualsNotSame(array, js.Array(2, 10, 14, 4, 4, 0, 6), array.collect {
      case x if x < 10 => x * 2
    })
  }

  @Test def collectFirst(): Unit = {
    val array = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)
    assertEquals(Some(108), array.collectFirst {
      case x if x > 10 => x * 2
    })
    assertEquals(None, array.collectFirst {
      case x if x < 0 => x * 2
    })
  }

  @Test def zip(): Unit = {
    val array1 = js.Array(1, 5, 7, 2)
    val array2 = js.Array("foo", "bar", "baz")
    assertJSArrayEquals(js.Array(1 -> "foo", 5 -> "bar", 7 -> "baz"), array1.zip(array2))
    assertJSArrayEquals(js.Array("foo" -> 1, "bar" -> 5, "baz" -> 7), array2.zip(array1))
  }

  @Test def zipAll(): Unit = {
    val array1 = js.Array(1, 5, 7, 2)
    val array2 = js.Array("foo", "bar", "baz")
    assertJSArrayEquals(js.Array(1 -> "foo", 5 -> "bar", 7 -> "baz", 2 -> "foobar"),
        array1.zipAll(array2, 10, "foobar"))
    assertJSArrayEquals(js.Array("foo" -> 1, "bar" -> 5, "baz" -> 7, "foobar" -> 2),
        array2.zipAll(array1, "foobar", 10))
  }

  @Test def zipWithIndex(): Unit = {
    val array = js.Array(1, 5, 7, 2)
    assertJSArrayEquals(js.Array(1 -> 0, 5 -> 1, 7 -> 2, 2 -> 3), array.zipWithIndex)
  }

  @Test def :+(): Unit = {
    val array = js.Array(1, 5, 7, 2)
    assertJSArrayEqualsNotSame(array, js.Array(1, 5, 7, 2, 10), array :+ 10)
  }

  @Test def +:(): Unit = {
    val array = js.Array(1, 5, 7, 2)
    assertJSArrayEqualsNotSame(array, js.Array(10, 1, 5, 7, 2), 10 +: array)
  }

  @Test def ++:(): Unit = {
    val array = js.Array(1, 5, 7, 2)
    assertJSArrayEqualsNotSame(array, js.Array(10, 11, 1, 5, 7, 2), js.Array(10, 11) ++: array)
    assertJSArrayEqualsNotSame(array, js.Array(10, 11, 1, 5, 7, 2), List(10, 11) ++: array)
  }

  @Test def ++(): Unit = {
    val array = js.Array(1, 5, 7, 2)
    assertJSArrayEqualsNotSame(array, js.Array(1, 5, 7, 2, 10, 11), array ++ js.Array(10, 11))
    assertJSArrayEqualsNotSame(array, js.Array(1, 5, 7, 2, 10, 11), array ++ List(10, 11))
  }

  @Test def contains(): Unit = {
    val array = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)
    assertTrue(array.contains(7))
    assertTrue(array.contains(2))
    assertTrue(array.contains(78))
    assertFalse(array.contains(123))
  }

  @Test def patch(): Unit = {
    val array = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)
    assertJSArrayEqualsNotSame(array,
        js.Array(1, 5, 7, 0, 3),
        array.patch(3, Nil, 4))
    assertJSArrayEqualsNotSame(array,
        js.Array(1, 5, 7, 42, 34, 0, 3),
        array.patch(3, js.Array(42, 34), 4))
    assertJSArrayEqualsNotSame(array,
        js.Array(42, 34, 54, 2, 78, 0, 3),
        array.patch(-3, js.Array(42, 34), 4))
    assertJSArrayEqualsNotSame(array,
        js.Array(1, 5, 7, 2, 54, 2, 78, 42),
        array.patch(7, js.Array(42), 6))
    assertJSArrayEqualsNotSame(array,
        js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3, 42, 34),
        array.patch(17, js.Array(42, 34), 6))
    assertJSArrayEqualsNotSame(array,
        js.Array(1, 5, 7, 2, 42, 34, 54, 2, 78, 0, 3),
        array.patch(4, js.Array(42, 34), -6))
  }

  @Test def foreach(): Unit = {
    val array = js.Array(6, 2, 56, -1)
    var i = 0
    array.foreach { elem =>
      i match {
        case 0 => assertEquals(6, elem)
        case 1 => assertEquals(2, elem)
        case 2 => assertEquals(56, elem)
        case 3 => assertEquals(-1, elem)
        case _ => fail("foreach called its lambda too many times")
      }
      i += 1
    }
    assertEquals("foreach did not call its lambda enough times", 4, i)
  }

  @Test def distinct(): Unit = {
    val array = js.Array(5, 7, 1, 34, 7, 3, 5, 9, 9)
    assertJSArrayEqualsNotSame(array, js.Array(5, 7, 1, 34, 3, 9), array.distinct)
  }

  @Test def padTo(): Unit = {
    val array = js.Array(5, 7, 1, 34)
    assertJSArrayEqualsNotSame(array, js.Array(5, 7, 1, 34), array.padTo(3, -1))
    assertJSArrayEqualsNotSame(array, js.Array(5, 7, 1, 34), array.padTo(4, -1))
    assertJSArrayEqualsNotSame(array, js.Array(5, 7, 1, 34, -1, -1, -1), array.padTo(7, -1))
    assertJSArrayEqualsNotSame(array, js.Array(5, 7, 1, 34), array.padTo(-3, -1))
  }

  @Test def indices(): Unit = {
    assertTrue(js.Array[Int]().indices.isEmpty)
    assertEquals(0 until 3, js.Array(4, 6, 8).indices)
  }

  @Test def groupBy(): Unit = {
    val array = js.Array("foo" -> 1, "bar" -> 5, "baz" -> 1, "foobar" -> 3,
        "hello" -> 7, "bonjour" -> 3)
    val groups = array.groupBy(_._2)
    assertEquals(Set(1, 3, 5, 7), groups.keySet)
    assertJSArrayEquals(js.Array("foo" -> 1, "baz" -> 1), groups(1))
    assertJSArrayEquals(js.Array("foobar" -> 3, "bonjour" -> 3), groups(3))
    assertJSArrayEquals(js.Array("bar" -> 5), groups(5))
    assertJSArrayEquals(js.Array("hello" -> 7), groups(7))
  }

  @Test def toIndexedSeq(): Unit = {
    val array = js.Array(5, 7, 1, 34)
    assertEquals(IndexedSeq(5, 7, 1, 34), array.toIndexedSeq)
  }

  @Test def toSeq(): Unit = {
    val array = js.Array(5, 7, 1, 34)
    assertEquals(Seq(5, 7, 1, 34), array.toSeq)
  }

  @Test def copyToArray(): Unit = {
    // Negative `start` argument is UB for this method.

    val array = js.Array(5, 7, 1, 34)

    var dest = new Array[Int](10)
    array.copyToArray(dest, 1)
    assertArrayEquals(Array(0, 5, 7, 1, 34, 0, 0, 0, 0, 0), dest)

    dest = new Array[Int](10)
    array.copyToArray(dest, 7)
    assertArrayEquals(Array(0, 0, 0, 0, 0, 0, 0, 5, 7, 1), dest)

    dest = new Array[Int](10)
    array.copyToArray(dest, 1, 3)
    assertArrayEquals(Array(0, 5, 7, 1, 0, 0, 0, 0, 0, 0), dest)

    dest = new Array[Int](10)
    array.copyToArray(dest, 7, 3)
    assertArrayEquals(Array(0, 0, 0, 0, 0, 0, 0, 5, 7, 1), dest)

    dest = new Array[Int](10)
    array.copyToArray(dest, 1, -3)
    assertArrayEquals(Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), dest)
  }

  @Test def toArray(): Unit = {
    // Negative `start` argument is UB for this method.

    val jsArray1: js.Array[Int] = js.Array(5, 7, 1, 34)
    val array1 = jsArray1.toArray
    assertEquals(classOf[Array[Int]], array1.getClass)
    assertArrayEquals(Array(5, 7, 1, 34), array1)

    val jsArray2: js.Array[String] = js.Array("foo", "bar", "baz")
    val array2 = jsArray2.toArray[CharSequence]
    assertEquals(classOf[Array[CharSequence]], array2.getClass)
    assertArrayEquals(Array[AnyRef]("foo", "bar", "baz"), array2.asInstanceOf[Array[AnyRef]])
  }

  @Test def count(): Unit = {
    val array = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)
    assertEquals(0, array.count(_ < 0))
    assertEquals(7, array.count(_ < 10))
    assertEquals(9, array.count(_ < 100))
  }

  @Test def startsWith(): Unit = {
    val array = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)

    val supportsNegativeStart = {
      !scalaVersion.startsWith("2.11.") &&
      !scalaVersion.startsWith("2.12.")
    }

    // js.Array

    assertTrue(array.startsWith(js.Array[Int]()))
    assertTrue(array.startsWith(js.Array(1, 5, 7, 2)))
    assertTrue(array.startsWith(js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)))

    assertFalse(array.startsWith(js.Array(1, 5, 3, 2)))
    assertFalse(array.startsWith(js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3, 6, 4)))

    assertTrue(array.startsWith(js.Array[Int](), 2))
    assertTrue(array.startsWith(js.Array(7, 2), 2))
    assertTrue(array.startsWith(js.Array(7, 2, 54, 2, 78, 0, 3), 2))
    if (supportsNegativeStart) {
      assertTrue(array.startsWith(js.Array(1, 5, 7, 2), -1))
      assertTrue(array.startsWith(js.Array(1, 5, 7, 2), Int.MinValue))
    }

    assertFalse(array.startsWith(js.Array(7, 2, 34, 2), 2))
    assertFalse(array.startsWith(js.Array(7, 2, 54, 2, 78, 0, 3, 6, 4), 2))
    if (supportsNegativeStart)
      assertFalse(array.startsWith(js.Array(1, 5, 3, 2), -1))

    // List

    assertTrue(array.startsWith(List[Int]()))
    assertTrue(array.startsWith(List(1, 5, 7, 2)))
    assertTrue(array.startsWith(List(1, 5, 7, 2, 54, 2, 78, 0, 3)))

    assertFalse(array.startsWith(List(1, 5, 3, 2)))
    assertFalse(array.startsWith(List(1, 5, 7, 2, 54, 2, 78, 0, 3, 6, 4)))

    assertTrue(array.startsWith(List[Int](), 2))
    assertTrue(array.startsWith(List(7, 2), 2))
    assertTrue(array.startsWith(List(7, 2, 54, 2, 78, 0, 3), 2))
    if (supportsNegativeStart) {
      assertTrue(array.startsWith(List(1, 5, 7, 2), -1))
      assertTrue(array.startsWith(List(1, 5, 7, 2), Int.MinValue))
    }

    assertFalse(array.startsWith(List(7, 2, 34, 2), 2))
    assertFalse(array.startsWith(List(7, 2, 54, 2, 78, 0, 3, 6, 4), 2))
    if (supportsNegativeStart)
      assertFalse(array.startsWith(List(1, 5, 3, 2), -1))
  }

  @Test def endsWith(): Unit = {
    val array = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)

    // js.Array

    assertTrue(array.endsWith(js.Array[Int]()))
    assertTrue(array.endsWith(js.Array(2, 78, 0, 3)))
    assertTrue(array.endsWith(js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)))

    assertFalse(array.endsWith(js.Array(2, 14, 0, 3)))
    assertFalse(array.endsWith(js.Array(6, 4, 1, 5, 7, 2, 54, 2, 78, 0, 3)))

    // List

    assertTrue(array.endsWith(List[Int]()))
    assertTrue(array.endsWith(List(2, 78, 0, 3)))
    assertTrue(array.endsWith(List(1, 5, 7, 2, 54, 2, 78, 0, 3)))

    assertFalse(array.endsWith(List(2, 14, 0, 3)))
    assertFalse(array.endsWith(List(6, 4, 1, 5, 7, 2, 54, 2, 78, 0, 3)))
  }

  @Test def updated(): Unit = {
    val array = js.Array(1, 5, 7, 2)
    assertJSArrayEqualsNotSame(array, js.Array(1, 3, 7, 2), array.updated(1, 3))
  }

  @Test def diff(): Unit = {
    val array = js.Array(1, 2, 1, 3, 1, 10, 9)
    assertJSArrayEqualsNotSame(array, js.Array(2, 1, 1, 10), array.diff(Seq(1, 3, 9)))
  }

  @Test def intersect(): Unit = {
    val array = js.Array(1, 2, 1, 3, 1, 10, 9)
    assertJSArrayEqualsNotSame(array, js.Array(1, 1, 3, 9), array.intersect(Seq(1, 3, 1, 9)))
  }

  @Test def sliding(): Unit = {
    val array = js.Array(1, 2, 1, 3, 1, 10, 9, 3, 0)
    val iter = array.sliding(4, 3)
    assertJSArrayEquals(js.Array(1, 2, 1, 3), iter.next())
    assertJSArrayEquals(js.Array(3, 1, 10, 9), iter.next())
    assertJSArrayEquals(js.Array(9, 3, 0), iter.next())
  }

  @Test def combinations(): Unit = {
    val array = js.Array(2, 3, 5, 7, 11)
    val iter = array.combinations(3)
    assertJSArrayEquals(js.Array(2, 3, 5), iter.next())
    assertJSArrayEquals(js.Array(2, 3, 7), iter.next())
    assertJSArrayEquals(js.Array(2, 3, 11), iter.next())
    assertJSArrayEquals(js.Array(2, 5, 7), iter.next())
    assertJSArrayEquals(js.Array(2, 5, 11), iter.next())
    assertJSArrayEquals(js.Array(2, 7, 11), iter.next())
    assertJSArrayEquals(js.Array(3, 5, 7), iter.next())
    assertJSArrayEquals(js.Array(3, 5, 11), iter.next())
    assertJSArrayEquals(js.Array(3, 7, 11), iter.next())
    assertJSArrayEquals(js.Array(5, 7, 11), iter.next())
    assertFalse(iter.hasNext)
  }

  @Test def permutations(): Unit = {
    val array = js.Array(2, 3, 5)
    val iter = array.permutations
    assertJSArrayEquals(js.Array(2, 3, 5), iter.next())
    assertJSArrayEquals(js.Array(2, 5, 3), iter.next())
    assertJSArrayEquals(js.Array(3, 2, 5), iter.next())
    assertJSArrayEquals(js.Array(3, 5, 2), iter.next())
    assertJSArrayEquals(js.Array(5, 2, 3), iter.next())
    assertJSArrayEquals(js.Array(5, 3, 2), iter.next())
    assertFalse(iter.hasNext)
  }

  @Test def clear(): Unit = {
    val array = js.Array(2, 3, 5)
    array.clear()
    assertEquals(0, array.length)
  }

  @Test def +=(): Unit = {
    val array = js.Array(2, 3, 5)
    array += 13
    assertJSArrayEquals(js.Array(2, 3, 5, 13), array)
  }

  @Test def append(): Unit = {
    val array = js.Array(2, 3, 5)
    array.append(13)
    assertJSArrayEquals(js.Array(2, 3, 5, 13), array)
  }

  @Test def ++=(): Unit = {
    val array = js.Array(2, 3, 5)
    array ++= List(13, 21, 36)
    assertJSArrayEquals(js.Array(2, 3, 5, 13, 21, 36), array)
    array ++= js.Array(51, 0, 2)
    assertJSArrayEquals(js.Array(2, 3, 5, 13, 21, 36, 51, 0, 2), array)
  }

  @Test def appendAll(): Unit = {
    val array = js.Array(2, 3, 5)
    array.appendAll(List(13, 21, 36))
    assertJSArrayEquals(js.Array(2, 3, 5, 13, 21, 36), array)
    array.appendAll(js.Array(51, 0, 2))
    assertJSArrayEquals(js.Array(2, 3, 5, 13, 21, 36, 51, 0, 2), array)
  }

  @Test def -=(): Unit = {
    val array = js.Array(2, 3, 5, 3, 24, 2)
    array -= 3
    assertJSArrayEquals(js.Array(2, 5, 3, 24, 2), array)
    array -= 42
    assertJSArrayEquals(js.Array(2, 5, 3, 24, 2), array)
  }

  @Test def --=(): Unit = {
    val array = js.Array(2, 3, 5, 3, 24, 2)
    array --= List(3, 24)
    assertJSArrayEquals(js.Array(2, 5, 3, 2), array)
    array --= js.Array(5, 2, 2)
    assertJSArrayEquals(js.Array(3), array)
  }

  @Test def +=:(): Unit = {
    val array = js.Array(2, 3, 5)
    13 +=: array
    assertJSArrayEquals(js.Array(13, 2, 3, 5), array)
  }

  @Test def prepend(): Unit = {
    val array = js.Array(2, 3, 5)
    array.prepend(13)
    assertJSArrayEquals(js.Array(13, 2, 3, 5), array)
  }

  @Test def prependAll(): Unit = {
    val array = js.Array(2, 3, 5)
    array.prependAll(List(13, 21, 36))
    assertJSArrayEquals(js.Array(13, 21, 36, 2, 3, 5), array)
    array.prependAll(js.Array(51, 0, 2))
    assertJSArrayEquals(js.Array(51, 0, 2, 13, 21, 36, 2, 3, 5), array)
  }

  @Test def ++=:(): Unit = {
    val array = js.Array(2, 3, 5)
    List(13, 21, 36) ++=: array
    assertJSArrayEquals(js.Array(13, 21, 36, 2, 3, 5), array)
    js.Array(51, 0, 2) ++=: array
    assertJSArrayEquals(js.Array(51, 0, 2, 13, 21, 36, 2, 3, 5), array)
  }

  @Test def insert(): Unit = {
    val array = js.Array(2, 3, 5, 54, 23)
    array.insert(2, 42)
    assertJSArrayEquals(js.Array(2, 3, 42, 5, 54, 23), array)
    array.insert(0, 33)
    assertJSArrayEquals(js.Array(33, 2, 3, 42, 5, 54, 23), array)
    array.insert(7, 44)
    assertJSArrayEquals(js.Array(33, 2, 3, 42, 5, 54, 23, 44), array)

    assertThrows(classOf[IndexOutOfBoundsException], array.insert(-2, 0))
    assertThrows(classOf[IndexOutOfBoundsException], array.insert(10, 0))
  }

  @Test def insertAll(): Unit = {
    val array = js.Array(2, 3, 5, 54, 23)
    array.insertAll(2, List(42, 53))
    assertJSArrayEquals(js.Array(2, 3, 42, 53, 5, 54, 23), array)
    array.insertAll(0, js.Array(33, 11))
    assertJSArrayEquals(js.Array(33, 11, 2, 3, 42, 53, 5, 54, 23), array)
    array.insertAll(9, Seq(44, 78))
    assertJSArrayEquals(js.Array(33, 11, 2, 3, 42, 53, 5, 54, 23, 44, 78), array)

    assertThrows(classOf[IndexOutOfBoundsException], array.insertAll(-2, Nil))
    assertThrows(classOf[IndexOutOfBoundsException], array.insertAll(15, Nil))
  }

  @Test def remove(): Unit = {
    val array = js.Array(33, 11, 2, 3, 42, 53, 5, 54, 23, 44, 78)

    array.remove(4)
    assertJSArrayEquals(js.Array(33, 11, 2, 3, 53, 5, 54, 23, 44, 78), array)

    assertThrows(classOf[IndexOutOfBoundsException], array.remove(-2))
    assertThrows(classOf[IndexOutOfBoundsException], array.remove(15))

    array.remove(6, 3)
    assertJSArrayEquals(js.Array(33, 11, 2, 3, 53, 5, 78), array)
    array.remove(10, 0)
    assertJSArrayEquals(js.Array(33, 11, 2, 3, 53, 5, 78), array)

    assertThrows(classOf[IndexOutOfBoundsException], array.remove(-3, 5))
    assertThrows(classOf[IndexOutOfBoundsException], array.remove(5, 4))
    assertThrows(classOf[IllegalArgumentException], array.remove(15, -2))
  }

  @Test def trimStart(): Unit = {
    val array = js.Array(33, 11, 2, 3, 42, 53, 5, 54, 23, 44, 78)
    array.trimStart(4)

    assumeFalse("the safe behavior was introduced in 2.13",
        scalaVersion.startsWith("2.11.") ||
        scalaVersion.startsWith("2.12."))
    assertJSArrayEquals(js.Array(42, 53, 5, 54, 23, 44, 78), array)
    array.trimStart(-3)
    assertJSArrayEquals(js.Array(42, 53, 5, 54, 23, 44, 78), array)
    array.trimStart(10)
    assertJSArrayEquals(js.Array[Int](), array)
  }

  @Test def trimEnd(): Unit = {
    val array = js.Array(33, 11, 2, 3, 42, 53, 5, 54, 23, 44, 78)
    array.trimEnd(4)

    assumeFalse("the safe behavior was introduced in 2.13",
        scalaVersion.startsWith("2.11.") ||
        scalaVersion.startsWith("2.12."))
    assertJSArrayEquals(js.Array(33, 11, 2, 3, 42, 53, 5), array)
    array.trimEnd(-3)
    assertJSArrayEquals(js.Array(33, 11, 2, 3, 42, 53, 5), array)
    array.trimEnd(10)
    assertJSArrayEquals(js.Array[Int](), array)
  }

  @Test def reduceLeft(): Unit = {
    val array = js.Array(100, 6, 2, 56, -1)
    assertEquals(37, array.reduceLeft(_ - _))
    assertThrows(classOf[UnsupportedOperationException],
        js.Array[Int]().reduceLeft(_ + _))
  }

  @Test def reduceRight(): Unit = {
    val array = js.Array("hello", "world")
    assertEquals("hello, world", array.reduceRight(_ + ", " + _))
    assertThrows(classOf[UnsupportedOperationException],
        js.Array[Int]().reduceRight(_ + _))
  }

  @Test def toList_Issue843(): Unit = {
    val array = js.Array(1,2,1,3,1,10,9)
    val list = array.toList
    assertArrayEquals(array.toArray, list.toArray)
  }

}
