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

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform._

import java.util.{ Arrays, Comparator }

import scala.reflect.ClassTag

object ArraysTest extends ArraysTest

/** This is also used in the typedarray package to test scala.Arrays backed
 *  by TypedArrays
 */
class ArraysTest {

  // To invoke org.junit.Assert.assertArrayEquals on Array[T]
  def assertArrayEquals[T](expected: Array[T], actual: Array[T]): Unit = {
    (expected, actual) match {
      case (expected: Array[AnyRef], actual: Array[AnyRef]) =>
        org.junit.Assert.assertArrayEquals(expected, actual)
      case _ =>
        org.junit.Assert.assertArrayEquals(
            expected.map(_.asInstanceOf[AnyRef]),
            actual.map(_.asInstanceOf[AnyRef]))
    }
  }

  /** Overridden by typedarray tests */
  def Array[T: ClassTag](v: T*): scala.Array[T] = scala.Array(v: _*)

  val stringComparator = new Comparator[String]() {
    def compare(s1: String, s2: String): Int = s1.compareTo(s2)
  }

  @Test def sortInt(): Unit =
    testSort[Int](_.toInt, new Array(_), Arrays.sort(_), Arrays.sort(_, _, _))

  @Test def sortLong(): Unit =
    testSort[Long](_.toLong, new Array(_), Arrays.sort(_), Arrays.sort(_, _, _))

  @Test def sortShort(): Unit =
    testSort[Short](_.toShort, new Array(_), Arrays.sort(_), Arrays.sort(_, _, _))

  @Test def sortByte(): Unit =
    testSort[Byte](_.toByte, new Array(_), Arrays.sort(_), Arrays.sort(_, _, _))

  @Test def sortChar(): Unit =
    testSort[Char](_.toChar, new Array(_), Arrays.sort(_), Arrays.sort(_, _, _))

  @Test def sortFloat(): Unit =
    testSort[Float](_.toFloat, new Array(_), Arrays.sort(_), Arrays.sort(_, _, _))

  @Test def sortDouble(): Unit =
    testSort[Double](_.toDouble, new Array(_), Arrays.sort(_), Arrays.sort(_, _, _))

  @Test def sortString(): Unit =
    testSort[AnyRef](_.toString, new Array(_), Arrays.sort(_), Arrays.sort(_, _, _))

  @Test def sortStringWithNullComparator(): Unit =
    testSort[AnyRef](_.toString, new Array(_), Arrays.sort(_, null), Arrays.sort(_, _, _, null))

  private def testSort[T: ClassTag](elem: Int => T, newArray: Int => Array[T],
      sort: Array[T] => Unit, sort2: (Array[T], Int, Int) => Unit): Unit = {
    val values = Array(5, 3, 6, 1, 2, 4).map(elem)
    val arr = newArray(values.length)

    for (i <- 0 until values.length)
      arr(i) = values(i)
    sort(arr)
    assertArrayEquals(arr, Array(1, 2, 3, 4, 5, 6).map(elem))

    for (i <- 0 until values.length)
      arr(i) = values(i)
    sort2(arr, 0, 3)
    assertArrayEquals(arr, Array(3, 5, 6, 1, 2, 4).map(elem))

    sort2(arr, 2, 5)
    assertArrayEquals(arr, Array(3, 5, 1, 2, 6, 4).map(elem))

    sort2(arr, 0, 6)
    assertArrayEquals(arr, Array(1, 2, 3, 4, 5, 6).map(elem))

    // check zero length doesn't fail.
    sort2(arr, 1, 1)
  }

  @Test def sortIsStable_Issue2400(): Unit = {
    case class N(i: Int)

    val cmp = new Comparator[N] {
      def compare(o1: N, o2: N): Int = 0
    }

    def isStable(a: scala.Array[N]): Boolean =
      a.zipWithIndex.forall { case (n, i) => n.i == i }

    for (size <- Seq(2, 4, 5, 15, 16, 19, 32, 33, 63, 64, 65, 4356)) {
      val sxs = scala.Array.tabulate(size)(i => N(i))
      Arrays.sort(sxs, cmp)

      assertTrue(s"Arrays.sort wasn't stable on array of size $size.'",
          isStable(sxs))
    }
  }

  @Test def sortWithComparator(): Unit = {
    val scalajs: Array[String] = Array("S", "c", "a", "l", "a", ".", "j", "s")
    val sorted = Array[String](".", "S", "a", "a", "c", "j", "l", "s")

    Arrays.sort(scalajs, stringComparator)
    assertArrayEquals(sorted, scalajs)
  }

  @Test def sortIsStable(): Unit = {
    case class A(n: Int)
    val cmp = new Comparator[A]() {
      def compare(a1: A, a2: A): Int = a1.n.compareTo(a2.n)
    }
    val scalajs: Array[A] = Array(A(1), A(2), A(2), A(3), A(1), A(2), A(3))
    val sorted = Array[A](scalajs(0), scalajs(4), scalajs(1), scalajs(2),
        scalajs(5), scalajs(3), scalajs(6))

    Arrays.sort(scalajs, cmp)
    assertArrayEquals(sorted, scalajs)
    scalajs.zip(sorted).forall(pair => pair ._1 eq pair._2)
  }

  @Test def sortIllegalArgumentException(): Unit = {
    val array = Array(0, 1, 3, 4)

    val e1 = assertThrows(classOf[IllegalArgumentException],
        Arrays.sort(array, 3, 2))
    assertEquals("fromIndex(3) > toIndex(2)", e1.getMessage)

    // start/end comparison is made before index ranges checks
    val e2 = assertThrows(classOf[IllegalArgumentException],
        Arrays.sort(array, 7, 5))
    assertEquals("fromIndex(7) > toIndex(5)", e2.getMessage)
  }

  @Test def sortArrayIndexOutOfBoundsException(): Unit = {
    assumeTrue("Assuming compliant array errors", hasCompliantArrayErrors)

    val array = Array(0, 1, 3, 4)

    assertThrows(classOf[ArrayIndexOutOfBoundsException],
        Arrays.sort(array, -1, 4))

    assertThrows(classOf[ArrayIndexOutOfBoundsException],
        Arrays.sort(array, 0, 5))
  }

  @Test def fillBoolean(): Unit = {
    val booleans = new Array[Boolean](6)
    Arrays.fill(booleans, false)
    assertArrayEquals(Array(false, false, false, false, false, false), booleans)

    Arrays.fill(booleans, true)
    assertArrayEquals(Array(true, true, true, true, true, true), booleans)
  }

  @Test def fillBooleanWithStartAndEndIndex(): Unit = {
    val booleans = new Array[Boolean](6)
    Arrays.fill(booleans, 1, 4, true)
    assertArrayEquals(Array(false, true, true, true, false, false), booleans)
  }

  @Test def fillByte(): Unit = {
    val bytes = new Array[Byte](6)
    Arrays.fill(bytes, 42.toByte)
    assertArrayEquals(Array[Byte](42, 42, 42, 42, 42, 42), bytes)

    Arrays.fill(bytes, -1.toByte)
    assertArrayEquals(Array[Byte](-1, -1, -1, -1, -1, -1), bytes)
  }

  @Test def fillByteWithStartAndEndIndex(): Unit = {
    val bytes = new Array[Byte](6)
    Arrays.fill(bytes, 1, 4, 42.toByte)
    assertArrayEquals(Array[Byte](0, 42, 42, 42, 0, 0), bytes)

    Arrays.fill(bytes, 2, 5, -1.toByte)
    assertArrayEquals(Array[Byte](0, 42, -1, -1, -1, 0), bytes)
  }

  @Test def fillShort(): Unit = {
    val shorts = new Array[Short](6)
    Arrays.fill(shorts, 42.toShort)
    assertArrayEquals(Array[Short](42, 42, 42, 42, 42, 42), shorts)

    Arrays.fill(shorts, -1.toShort)
    assertArrayEquals(Array[Short](-1, -1, -1, -1, -1, -1), shorts)
  }

  @Test def fillShortWithStartAndEndIndex(): Unit = {
    val shorts = new Array[Short](6)
    Arrays.fill(shorts, 1, 4, 42.toShort)
    assertArrayEquals(Array[Short](0, 42, 42, 42, 0, 0), shorts)

    Arrays.fill(shorts, 2, 5, -1.toShort)
    assertArrayEquals(Array[Short](0, 42, -1, -1, -1, 0), shorts)
  }

  @Test def fillInt(): Unit = {
    val ints = new Array[Int](6)
    Arrays.fill(ints, 42)
    assertArrayEquals(Array(42, 42, 42, 42, 42, 42), ints)

    Arrays.fill(ints, -1)
    assertArrayEquals(Array(-1, -1, -1, -1, -1, -1), ints)
  }

  @Test def fillIntWithStartAndEndIndex(): Unit = {
    val ints = new Array[Int](6)
    Arrays.fill(ints, 1, 4, 42)
    assertArrayEquals(Array(0, 42, 42, 42, 0, 0), ints)

    Arrays.fill(ints, 2, 5, -1)
    assertArrayEquals(Array(0, 42, -1, -1, -1, 0), ints)
  }

  @Test def fillLong(): Unit = {
    val longs = new Array[Long](6)
    Arrays.fill(longs, 42L)
    assertArrayEquals(Array(42L, 42L, 42L, 42L, 42L, 42L), longs)

    Arrays.fill(longs, -1L)
    assertArrayEquals(Array(-1L, -1L, -1L, -1L, -1L, -1L), longs)
  }

  @Test def fillLongWithStartAndEndIndex(): Unit = {
    val longs = new Array[Long](6)
    Arrays.fill(longs, 1, 4, 42L)
    assertArrayEquals(Array(0L, 42L, 42L, 42L, 0L, 0L), longs)

    Arrays.fill(longs, 2, 5, -1L)
    assertArrayEquals(Array(0L, 42L, -1L, -1L, -1L, 0L), longs)
  }

  @Test def fillFloat(): Unit = {
    val floats = new Array[Float](6)
    Arrays.fill(floats, 42.0f)
    assertArrayEquals(Array(42.0f, 42.0f, 42.0f, 42.0f, 42.0f, 42.0f), floats)

    Arrays.fill(floats, -1.0f)
    assertArrayEquals(Array(-1.0f, -1.0f, -1.0f, -1.0f, -1.0f, -1.0f), floats)
  }

  @Test def fillFloatWithStartAndEndIndex(): Unit = {
    val floats = new Array[Float](6)
    Arrays.fill(floats, 1, 4, 42.0f)
    assertArrayEquals(Array(0.0f, 42.0f, 42.0f, 42.0f, 0.0f, 0.0f), floats)

    Arrays.fill(floats, 2, 5, -1.0f)
    assertArrayEquals(Array(0.0f, 42.0f, -1.0f, -1.0f, -1.0f, 0.0f), floats)
  }

  @Test def fillDouble(): Unit = {
    val doubles = new Array[Double](6)
    Arrays.fill(doubles, 42.0)
    assertArrayEquals(Array(42.0, 42.0, 42.0, 42.0, 42.0, 42.0), doubles)

    Arrays.fill(doubles, -1.0f)
    assertArrayEquals(Array(-1.0, -1.0, -1.0, -1.0, -1.0, -1.0), doubles)
  }

  @Test def fillDoubleWithStartAndEndIndex(): Unit = {
    val doubles = new Array[Double](6)
    Arrays.fill(doubles, 1, 4, 42.0)
    assertArrayEquals(Array(0.0, 42.0, 42.0, 42.0, 0.0, 0.0), doubles)

    Arrays.fill(doubles, 2, 5, -1.0)
    assertArrayEquals(Array(0.0, 42.0, -1.0, -1.0, -1.0, 0.0), doubles)
  }

    @Test def fillAnyRef(): Unit = {
    val array = new Array[AnyRef](6)
    Arrays.fill(array, "a")
    assertArrayEquals(Array[AnyRef]("a", "a", "a", "a", "a", "a"), array)

    Arrays.fill(array, "b")
    assertArrayEquals(Array[AnyRef]("b", "b", "b", "b", "b", "b"), array)
  }

  @Test def fillAnyRefWithStartAndEndIndex(): Unit = {
    val bytes = new Array[AnyRef](6)
    Arrays.fill(bytes, 1, 4, "a")
    assertArrayEquals(Array[AnyRef](null, "a", "a", "a", null, null), bytes)

    Arrays.fill(bytes, 2, 5, "b")
    assertArrayEquals(Array[AnyRef](null, "a", "b", "b", "b", null), bytes)
  }

  @Test def binarySearchWithStartAndEndIndexOnLong(): Unit = {
    val longs: Array[Long] = Array(1, 2, 3, 5, 6, 7)
    var ret = Arrays.binarySearch(longs, 0, 6, 5)
    assertEquals(3, ret)

    ret = Arrays.binarySearch(longs, 0, 6, 0)
    assertEquals(-1, ret)

    ret = Arrays.binarySearch(longs, 0, 6, 4)
    assertEquals(-4, ret)

    ret = Arrays.binarySearch(longs, 0, 6, 8)
    assertEquals(-7, ret)
  }

  @Test def binarySearchOnLong(): Unit = {
    val longs: Array[Long] = Array(1, 2, 3, 5, 6, 7)
    var ret = Arrays.binarySearch(longs, 5)
    assertEquals(3, ret)

    ret = Arrays.binarySearch(longs, 0)
    assertEquals(-1, ret)

    ret = Arrays.binarySearch(longs, 4)
    assertEquals(-4, ret)

    ret = Arrays.binarySearch(longs, 8)
    assertEquals(-7, ret)
  }

  @Test def binarySearchWithStartAndEndIndexOnInt(): Unit = {
    val ints: Array[Int] = Array(1, 2, 3, 5, 6, 7)
    var ret = Arrays.binarySearch(ints, 0, 6, 5)
    assertEquals(3, ret)

    ret = Arrays.binarySearch(ints, 0, 6, 0)
    assertEquals(-1, ret)

    ret = Arrays.binarySearch(ints, 0, 6, 4)
    assertEquals(-4, ret)

    ret = Arrays.binarySearch(ints, 0, 6, 8)
    assertEquals(-7, ret)
  }

  @Test def binarySearchOnInt(): Unit = {
    val ints: Array[Int] = Array(1, 2, 3, 5, 6, 7)
    var ret = Arrays.binarySearch(ints, 5)
    assertEquals(3, ret)

    ret = Arrays.binarySearch(ints, 0)
    assertEquals(-1, ret)

    ret = Arrays.binarySearch(ints, 4)
    assertEquals(-4, ret)

    ret = Arrays.binarySearch(ints, 8)
    assertEquals(-7, ret)
  }

  @Test def binarySearchWithStartAndEndIndexOnShort(): Unit = {
    val shorts: Array[Short] = Array(1, 2, 3, 5, 6, 7)
    var ret = Arrays.binarySearch(shorts, 0, 6, 5.toShort)
    assertEquals(3, ret)

    ret = Arrays.binarySearch(shorts, 0, 6, 0.toShort)
    assertEquals(-1, ret)

    ret = Arrays.binarySearch(shorts, 0, 6, 4.toShort)
    assertEquals(-4, ret)

    ret = Arrays.binarySearch(shorts, 0, 6, 8.toShort)
    assertEquals(-7, ret)
  }

  @Test def binarySearchOnShort(): Unit = {
    val shorts: Array[Short] = Array(1, 2, 3, 5, 6, 7)
    var ret = Arrays.binarySearch(shorts, 5.toShort)
    assertEquals(3, ret)

    ret = Arrays.binarySearch(shorts, 0.toShort)
    assertEquals(-1, ret)

    ret = Arrays.binarySearch(shorts, 4.toShort)
    assertEquals(-4, ret)

    ret = Arrays.binarySearch(shorts, 8.toShort)
    assertEquals(-7, ret)
  }

  @Test def binarySearchWithStartAndEndIndexOnChar(): Unit = {
    val chars: Array[Char] = Array('b', 'c', 'd', 'f', 'g', 'h')
    var ret = Arrays.binarySearch(chars, 0, 6, 'f')
    assertEquals(3, ret)

    ret = Arrays.binarySearch(chars, 0, 6, 'a')
    assertEquals(-1, ret)

    ret = Arrays.binarySearch(chars, 0, 6, 'e')
    assertEquals(-4, ret)

    ret = Arrays.binarySearch(chars, 0, 6, 'i')
    assertEquals(-7, ret)
  }

  @Test def binarySearchOnChar(): Unit = {
    val chars: Array[Char] = Array('b', 'c', 'd', 'f', 'g', 'h')
    var ret = Arrays.binarySearch(chars, 'f')
    assertEquals(3, ret)

    ret = Arrays.binarySearch(chars, 'a')
    assertEquals(-1, ret)

    ret = Arrays.binarySearch(chars, 'e')
    assertEquals(-4, ret)

    ret = Arrays.binarySearch(chars, 'i')
    assertEquals(-7, ret)
  }

  @Test def binarySearchWithStartAndEndIndexOnDouble(): Unit = {
    val doubles: Array[Double] = Array(0.1, 0.2, 0.3, 0.5, 0.6, 0.7)
    var ret = Arrays.binarySearch(doubles, 0, 6, 0.5)
    assertEquals(3, ret)

    ret = Arrays.binarySearch(doubles, 0, 6, 0.0)
    assertEquals(-1, ret)

    ret = Arrays.binarySearch(doubles, 0, 6, 0.4)
    assertEquals(-4, ret)

    ret = Arrays.binarySearch(doubles, 0, 6, 0.8)
    assertEquals(-7, ret)
  }

  @Test def binarySearchOnDouble(): Unit = {
    val doubles: Array[Double] = Array(0.1, 0.2, 0.3, 0.5, 0.6, 0.7)
    var ret = Arrays.binarySearch(doubles, 0.5)
    assertEquals(3, ret)

    ret = Arrays.binarySearch(doubles, 0.0)
    assertEquals(-1, ret)

    ret = Arrays.binarySearch(doubles, 0.4)
    assertEquals(-4, ret)

    ret = Arrays.binarySearch(doubles, 0.8)
    assertEquals(-7, ret)
  }

  @Test def binarySearchWithStartAndEndIndexOnFloat(): Unit = {
    val floats: Array[Float] = Array(0.1f, 0.2f, 0.3f, 0.5f, 0.6f, 0.7f)
    var ret = Arrays.binarySearch(floats, 0, 6, 0.5f)
    assertEquals(3, ret)

    ret = Arrays.binarySearch(floats, 0, 6, 0.0f)
    assertEquals(-1, ret)

    ret = Arrays.binarySearch(floats, 0, 6, 0.4f)
    assertEquals(-4, ret)

    ret = Arrays.binarySearch(floats, 0, 6, 0.8f)
    assertEquals(-7, ret)
  }

  @Test def binarySearchOnFloat(): Unit = {
    val floats: Array[Float] = Array(0.1f, 0.2f, 0.3f, 0.5f, 0.6f, 0.7f)
    var ret = Arrays.binarySearch(floats, 0.5f)
    assertEquals(3, ret)

    ret = Arrays.binarySearch(floats, 0.0f)
    assertEquals(-1, ret)

    ret = Arrays.binarySearch(floats, 0.4f)
    assertEquals(-4, ret)

    ret = Arrays.binarySearch(floats, 0.8f)
    assertEquals(-7, ret)
  }

  @Test def binarySearchWithStartAndEndIndexOnAnyRef(): Unit = {
    val strings: Array[AnyRef] = Array("aa", "abc", "cc", "zz", "zzzs", "zzzt")
    var ret = Arrays.binarySearch(strings, 0, 6, "zz")
    assertEquals(3, ret)

    ret = Arrays.binarySearch(strings, 0, 6, "a")
    assertEquals(-1, ret)

    ret = Arrays.binarySearch(strings, 0, 6, "cd")
    assertEquals(-4, ret)

    ret = Arrays.binarySearch(strings, 0, 6, "zzzz")
    assertEquals(-7, ret)
  }

  @Test def binarySearchOnAnyRef(): Unit = {
    val strings: Array[AnyRef] = Array("aa", "abc", "cc", "zz", "zzzs", "zzzt")
    var ret = Arrays.binarySearch(strings, "zz")
    assertEquals(3, ret)

    ret = Arrays.binarySearch(strings, "a")
    assertEquals(-1, ret)

    ret = Arrays.binarySearch(strings, "cd")
    assertEquals(-4, ret)

    ret = Arrays.binarySearch(strings, "zzzz")
    assertEquals(-7, ret)
  }

  @Test def binarySearchWithStartAndEndIndexOnSpecificAnyRefWithNullComparator(): Unit = {
    val strings: Array[String] = Array("aac", "abc", "cc", "cf", "zz", "zzzs", "zzzt")

    assertEquals(2, Arrays.binarySearch(strings, 2, 6, "cc", null))
    assertEquals(5, Arrays.binarySearch(strings, 2, 6, "zzzs", null))
    assertEquals(3, Arrays.binarySearch(strings, 2, 6, "cf", null))

    assertEquals(-2 - 1, Arrays.binarySearch(strings, 2, 6, "aaa", null))
    assertEquals(-2 - 1, Arrays.binarySearch(strings, 2, 6, "aac", null))
    assertEquals(-2 - 1, Arrays.binarySearch(strings, 2, 6, "bb", null))
    assertEquals(-4 - 1, Arrays.binarySearch(strings, 2, 6, "ff", null))
    assertEquals(-5 - 1, Arrays.binarySearch(strings, 2, 6, "zza", null))
    assertEquals(-6 - 1, Arrays.binarySearch(strings, 2, 6, "zzzt", null))
    assertEquals(-6 - 1, Arrays.binarySearch(strings, 2, 6, "zzzz", null))
  }

  @Test def binarySearchOnSpecificAnyRefWithNullComparator(): Unit = {
    val strings: Array[String] = Array("aac", "abc", "cc", "cf", "zz", "zzzs", "zzzt")

    assertEquals(0, Arrays.binarySearch(strings, "aac", null))
    assertEquals(2, Arrays.binarySearch(strings, "cc", null))
    assertEquals(3, Arrays.binarySearch(strings, "cf", null))
    assertEquals(5, Arrays.binarySearch(strings, "zzzs", null))
    assertEquals(6, Arrays.binarySearch(strings, "zzzt", null))

    assertEquals(-0 - 1, Arrays.binarySearch(strings, "aaa", null))
    assertEquals(-2 - 1, Arrays.binarySearch(strings, "bb", null))
    assertEquals(-4 - 1, Arrays.binarySearch(strings, "ff", null))
    assertEquals(-5 - 1, Arrays.binarySearch(strings, "zza", null))
    assertEquals(-7 - 1, Arrays.binarySearch(strings, "zzzz", null))
  }

  @Test def binarySearchWithStartAndEndIndexOnSpecificAnyRefWithComparator(): Unit = {
    val cmp = new java.util.Comparator[(Int, Int)] {
      def compare(o1: (Int, Int), o2: (Int, Int)): Int =
        if (o1._1 != o2._1) Integer.compare(o1._1, o2._1)
        else Integer.compare(o1._2, o2._2)
    }

    val pairs: Array[(Int, Int)] = Array((5, 8), (5, 15), (6, 3), (6, 10), (6, 20), (10, -1), (10, 3))

    assertEquals(2, Arrays.binarySearch(pairs, 2, 6, (6, 3), cmp))
    assertEquals(5, Arrays.binarySearch(pairs, 2, 6, (10, -1), cmp))
    assertEquals(3, Arrays.binarySearch(pairs, 2, 6, (6, 10), cmp))

    assertEquals(-2 - 1, Arrays.binarySearch(pairs, 2, 6, (4, 50), cmp))
    assertEquals(-2 - 1, Arrays.binarySearch(pairs, 2, 6, (5, 8), cmp))
    assertEquals(-2 - 1, Arrays.binarySearch(pairs, 2, 6, (6, 0), cmp))
    assertEquals(-4 - 1, Arrays.binarySearch(pairs, 2, 6, (6, 15), cmp))
    assertEquals(-5 - 1, Arrays.binarySearch(pairs, 2, 6, (7, 3), cmp))
    assertEquals(-6 - 1, Arrays.binarySearch(pairs, 2, 6, (10, 3), cmp))
    assertEquals(-6 - 1, Arrays.binarySearch(pairs, 2, 6, (10, 7), cmp))
  }

  @Test def binarySearchOnSpecificAnyRefWithComparator(): Unit = {
    val cmp = new java.util.Comparator[(Int, Int)] {
      def compare(o1: (Int, Int), o2: (Int, Int)): Int =
        if (o1._1 != o2._1) Integer.compare(o1._1, o2._1)
        else Integer.compare(o1._2, o2._2)
    }

    val pairs: Array[(Int, Int)] = Array((5, 8), (5, 15), (6, 3), (6, 10), (6, 20), (10, -1), (10, 3))

    assertEquals(0, Arrays.binarySearch(pairs, (5, 8), cmp))
    assertEquals(2, Arrays.binarySearch(pairs, (6, 3), cmp))
    assertEquals(3, Arrays.binarySearch(pairs, (6, 10), cmp))
    assertEquals(5, Arrays.binarySearch(pairs, (10, -1), cmp))
    assertEquals(6, Arrays.binarySearch(pairs, (10, 3), cmp))

    assertEquals(-0 - 1, Arrays.binarySearch(pairs, (4, 50), cmp))
    assertEquals(-2 - 1, Arrays.binarySearch(pairs, (6, 0), cmp))
    assertEquals(-4 - 1, Arrays.binarySearch(pairs, (6, 15), cmp))
    assertEquals(-5 - 1, Arrays.binarySearch(pairs, (7, 3), cmp))
    assertEquals(-7 - 1, Arrays.binarySearch(pairs, (10, 7), cmp))
  }

  @Test def binarySearchIllegalArgumentException(): Unit = {
    val array = Array(0, 1, 3, 4)

    val e1 = assertThrows(classOf[IllegalArgumentException],
        Arrays.binarySearch(array, 3, 2, 2))
    assertEquals("fromIndex(3) > toIndex(2)", e1.getMessage)

    // start/end comparison is made before index ranges checks
    val e2 = assertThrows(classOf[IllegalArgumentException],
        Arrays.binarySearch(array, 7, 5, 2))
    assertEquals("fromIndex(7) > toIndex(5)", e2.getMessage)
  }

  @Test def binarySearchArrayIndexOutOfBoundsException(): Unit = {
    assumeTrue("Assuming compliant array errors", hasCompliantArrayErrors)

    val array = Array(0, 1, 3, 4)

    assertThrows(classOf[ArrayIndexOutOfBoundsException],
        Arrays.binarySearch(array, -1, 4, 2))

    assertThrows(classOf[ArrayIndexOutOfBoundsException],
        Arrays.binarySearch(array, 0, 5, 2))
  }

  @Test def copyOfInt(): Unit = {
    val ints: Array[Int] = Array(1, 2, 3)
    val intscopy = Arrays.copyOf(ints, 5)
    assertArrayEquals(Array(1, 2, 3, 0, 0), intscopy)
  }

  @Test def copyOfLong(): Unit = {
    val longs: Array[Long] = Array(1, 2, 3)
    val longscopy = Arrays.copyOf(longs, 5)
    assertArrayEquals(Array[Long](1, 2, 3, 0, 0), longscopy)
  }

  @Test def copyOfShort(): Unit = {
    val shorts: Array[Short] = Array(1, 2, 3)
    val shortscopy = Arrays.copyOf(shorts, 5)
    assertArrayEquals(Array[Short](1, 2, 3, 0, 0), shortscopy)
  }

  @Test def copyOfByte(): Unit = {
    val bytes: Array[Byte] = Array(42, 43, 44)
    val floatscopy = Arrays.copyOf(bytes, 5)
    assertArrayEquals(Array[Byte](42, 43, 44, 0, 0), floatscopy)
  }

  @Test def copyOfChar(): Unit = {
    val chars: Array[Char] = Array('a', 'b', '0')
    val charscopy = Arrays.copyOf(chars, 5)
    assertEquals(0.toChar, charscopy(4))
  }

  @Test def copyOfDouble(): Unit = {
    val doubles: Array[Double] = Array(0.1, 0.2, 0.3)
    val doublescopy = Arrays.copyOf(doubles, 5)
    assertArrayEquals(Array[Double](0.1, 0.2, 0.3, 0, 0), doublescopy)
  }

  @Test def copyOfFloat(): Unit = {
    val floats: Array[Float] = Array(0.1f, 0.2f, 0.3f)
    val floatscopy = Arrays.copyOf(floats, 5)
    assertArrayEquals(Array[Float](0.1f, 0.2f, 0.3f, 0f, 0f), floatscopy)
  }

  @Test def copyOfBoolean(): Unit = {
    val bools: Array[Boolean] = Array(false, true, false)
    val boolscopy = Arrays.copyOf(bools, 5)
    assertArrayEquals(Array[Boolean](false, true, false, false, false), boolscopy)
  }

  @Test def copyOfAnyRef(): Unit = {
    val anyrefs: Array[AnyRef] = Array("a", "b", "c")
    val anyrefscopy = Arrays.copyOf(anyrefs, 5)
    assertEquals(classOf[Array[AnyRef]], anyrefscopy.getClass())
    assertArrayEquals(Array[AnyRef]("a", "b", "c", null, null), anyrefscopy)

    val sequences: Array[CharSequence] = Array("a", "b", "c")
    val sequencescopy = Arrays.copyOf(sequences, 2)
    assertEquals(classOf[Array[CharSequence]], sequencescopy.getClass())
    assertArrayEquals(Array[CharSequence]("a", "b"), sequencescopy)
  }

  @Test def copyOfAnyRefWithChangeOfType(): Unit = {
    class A
    case class B(x: Int) extends A

    val bs: Array[AnyRef] = Array(B(1), B(2), B(3))
    val bscopyAsA = Arrays.copyOf(bs, 5, classOf[Array[A]])
    assertEquals(classOf[Array[A]], bscopyAsA.getClass())
    assertArrayEquals(Array[A](B(1), B(2), B(3), null, null), bscopyAsA)
  }

  @Test def copyOfRangeAnyRef(): Unit = {
    val anyrefs: Array[AnyRef] = Array("a", "b", "c", "d", "e")
    val anyrefscopy = Arrays.copyOfRange(anyrefs, 2, 4)
    assertEquals(classOf[Array[AnyRef]], anyrefscopy.getClass())
    assertArrayEquals(Array[AnyRef]("c", "d"), anyrefscopy)

    val sequences: Array[CharSequence] = Array("a", "b", "c", "d", "e")
    val sequencescopy = Arrays.copyOfRange(sequences, 1, 5)
    assertEquals(classOf[Array[CharSequence]], sequencescopy.getClass())
    assertArrayEquals(Array[CharSequence]("b", "c", "d", "e"), sequencescopy)
  }

  @Test def copyOfRangeAnyRefWithChangeOfType(): Unit = {
    class A
    case class B(x: Int) extends A
    val bs: Array[B] = Array(B(1), B(2), B(3), B(4), B(5))
    val bscopyAsA = Arrays.copyOfRange(bs, 2, 4, classOf[Array[A]])
    assertEquals(classOf[Array[A]], bscopyAsA.getClass())
    assertArrayEquals(Array[A](B(3), B(4)), bscopyAsA)
  }

  @Test def copyOfRangeAnyRefArrayIndexOutOfBoundsException(): Unit = {
    assumeTrue("Assuming compliant array errors", hasCompliantArrayErrors)

    val anyrefs: Array[AnyRef] = Array("a", "b", "c", "d", "e")
    assertThrows(classOf[ArrayIndexOutOfBoundsException],
        Arrays.copyOfRange(anyrefs, -1, 4))
    assertThrows(classOf[ArrayIndexOutOfBoundsException],
        Arrays.copyOfRange(anyrefs, 6, 8))
  }

  @Test def asList(): Unit = {
    val list = Arrays.asList(1, 2, 3)
    assertEquals(list.size(), 3)
    assertEquals(list.get(0), 1)
    assertEquals(list.get(1), 2)
    assertEquals(list.get(2), 3)
  }

  @Test def hashCodeBoolean(): Unit = {
    assertEquals(0, Arrays.hashCode(null: Array[Boolean]))
    assertEquals(1, Arrays.hashCode(Array[Boolean]()))
    assertEquals(1268, Arrays.hashCode(Array[Boolean](false)))
    assertEquals(40359, Arrays.hashCode(Array[Boolean](true, false)))
  }

  @Test def hashCodeChars(): Unit = {
    assertEquals(0, Arrays.hashCode(null: Array[Char]))
    assertEquals(1, Arrays.hashCode(Array[Char]()))
    assertEquals(128, Arrays.hashCode(Array[Char]('a')))
    assertEquals(4068, Arrays.hashCode(Array[Char]('c', '&')))
    assertEquals(74792, Arrays.hashCode(Array[Char]('-', '5', 'q')))
    assertEquals(88584920, Arrays.hashCode(Array[Char]('.', ' ', '\u4323', 'v', '~')))
  }

  @Test def hashCodeBytes(): Unit = {
    assertEquals(0, Arrays.hashCode(null: Array[Byte]))
    assertEquals(1, Arrays.hashCode(Array[Byte]()))
    assertEquals(32, Arrays.hashCode(Array[Byte](1)))
    assertEquals(1053, Arrays.hashCode(Array[Byte](7, -125)))
    assertEquals(32719, Arrays.hashCode(Array[Byte](3, 0, 45)))
    assertEquals(30065878, Arrays.hashCode(Array[Byte](0, 45, 100, 1, 1)))
  }

  @Test def hashCodeShorts(): Unit = {
    assertEquals(0, Arrays.hashCode(null: Array[Short]))
    assertEquals(1, Arrays.hashCode(Array[Short]()))
    assertEquals(32, Arrays.hashCode(Array[Short](1)))
    assertEquals(1053, Arrays.hashCode(Array[Short](7, -125)))
    assertEquals(37208, Arrays.hashCode(Array[Short](3, 0, 4534)))
    assertEquals(30065878, Arrays.hashCode(Array[Short](0, 45, 100, 1, 1)))
  }

  @Test def hashCodeInts(): Unit = {
    assertEquals(0, Arrays.hashCode(null: Array[Int]))
    assertEquals(1, Arrays.hashCode(Array[Int]()))
    assertEquals(32, Arrays.hashCode(Array[Int](1)))
    assertEquals(1053, Arrays.hashCode(Array[Int](7, -125)))
    assertEquals(37208, Arrays.hashCode(Array[Int](3, 0, 4534)))
    assertEquals(-1215441431, Arrays.hashCode(Array[Int](0, 45, 100, 1, 1, Int.MaxValue)))
  }

  @Test def hashCodeLongs(): Unit = {
    assertEquals(0, Arrays.hashCode(null: Array[Long]))
    assertEquals(1, Arrays.hashCode(Array[Long]()))
    assertEquals(32, Arrays.hashCode(Array[Long](1L)))
    assertEquals(1302, Arrays.hashCode(Array[Long](7L, -125L)))
    assertEquals(37208, Arrays.hashCode(Array[Long](3L, 0L, 4534L)))
    assertEquals(-1215441431, Arrays.hashCode(Array[Long](0L, 45L, 100L, 1L, 1L, Int.MaxValue)))
    assertEquals(-1952288964, Arrays.hashCode(Array[Long](0L, 34573566354545L, 100L, 1L, 1L, Int.MaxValue)))
  }

  @Test def hashCodeFloats(): Unit = {
    assertEquals(0, Arrays.hashCode(null: Array[Float]))
    assertEquals(1, Arrays.hashCode(Array[Float]()))
    if (!executingInJVM) {
      assertEquals(32, Arrays.hashCode(Array[Float](1f)))
      assertEquals(-2082726591, Arrays.hashCode(Array[Float](7.2f, -125.2f)))
      assertEquals(-1891539602, Arrays.hashCode(Array[Float](302.1f, 0.0f, 4534f)))
      assertEquals(-1591440133, Arrays.hashCode(Array[Float](0.0f, 45f, -100f, 1.1f, -1f, 3567f)))
    }
  }

  @Test def hashCodeDoubles(): Unit = {
    assertEquals(0, Arrays.hashCode(null: Array[Double]))
    assertEquals(1, Arrays.hashCode(Array[Double]()))
    if (!executingInJVM) {
      assertEquals(-1503133662, Arrays.hashCode(Array[Double](1.1)))
      assertEquals(-2075734168, Arrays.hashCode(Array[Double](7.3, -125.23)))
      assertEquals(-557562564, Arrays.hashCode(Array[Double](3.9, 0.2, 4534.9)))
      assertEquals(-1750344582, Arrays.hashCode(Array[Double](0.1, 45.1, -100.0, 1.1, 1.7)))
      assertEquals(-1764602991, Arrays.hashCode(Array[Double](0.0, 34573566354545.9, 100.2, 1.1, 1.2, Int.MaxValue)))
    }
  }

  @Test def hashCodeAnyRef(): Unit = {
    assertEquals(0, Arrays.hashCode(null: Array[AnyRef]))
    assertEquals(1, Arrays.hashCode(Array[AnyRef]()))
    assertEquals(961, Arrays.hashCode(Array[AnyRef](null, null)))
    assertEquals(126046, Arrays.hashCode(Array[AnyRef]("a", "b", null)))
    assertEquals(-1237252983, Arrays.hashCode(Array[AnyRef](null, "a", "b", null, "fooooo")))
  }

  @Test def deepHashCode(): Unit = {
    assertEquals(0, Arrays.deepHashCode(null: Array[AnyRef]))
    assertEquals(1, Arrays.deepHashCode(Array[AnyRef]()))
    assertEquals(961, Arrays.deepHashCode(Array[AnyRef](null, null)))
    assertEquals(126046, Arrays.deepHashCode(Array[AnyRef]("a", "b", null)))
    assertEquals(-1237252983, Arrays.deepHashCode(Array[AnyRef](null, "a", "b", null, "fooooo")))
    assertEquals(962, Arrays.deepHashCode(Array[AnyRef](null, Array[AnyRef]())))
    assertEquals(993, Arrays.deepHashCode(Array[AnyRef](Array[AnyRef](), Array[AnyRef]())))
    assertEquals(63, Arrays.deepHashCode(Array[AnyRef](Array[AnyRef](Array[AnyRef]()))))
    assertEquals(63, Arrays.deepHashCode(Array[AnyRef](Array[AnyRef](Array[Int]()))))
    assertEquals(63, Arrays.deepHashCode(Array[AnyRef](Array[AnyRef](Array[Double]()))))
    assertEquals(94, Arrays.deepHashCode(Array[AnyRef](Array[AnyRef](Array[Int](1)))))
    assertEquals(94, Arrays.deepHashCode(Array[AnyRef](Array[AnyRef](Array[AnyRef](1.asInstanceOf[AnyRef])))))
  }

  @Test def equalsBooleans(): Unit = {
    val a1 = Array(true, false)

    assertTrue(Arrays.equals(a1, a1))
    assertTrue(Arrays.equals(a1, Array(true, false)))

    assertFalse(Arrays.equals(a1, Array(true)))
    assertFalse(Arrays.equals(a1, Array(false)))
    assertFalse(Arrays.equals(a1, Array[Boolean]()))
    assertFalse(Arrays.equals(a1, Array(false, true)))
    assertFalse(Arrays.equals(a1, Array(false, true, false)))
  }

  @Test def equalsBytes(): Unit = {
    val a1 = Array[Byte](1, -7, 10)

    assertTrue(Arrays.equals(null: Array[Byte], null: Array[Byte]))
    assertTrue(Arrays.equals(a1, a1))
    assertTrue(Arrays.equals(a1, Array[Byte](1, -7, 10)))

    assertFalse(Arrays.equals(a1, null))
    assertFalse(Arrays.equals(a1, Array[Byte](3)))
    assertFalse(Arrays.equals(a1, Array[Byte](1)))
    assertFalse(Arrays.equals(a1, Array[Byte]()))
    assertFalse(Arrays.equals(a1, Array[Byte](1, -7, 11)))
    assertFalse(Arrays.equals(a1, Array[Byte](1, -7, 11, 20)))
  }

  @Test def equalsChars(): Unit = {
    val a1 = Array[Char]('a', '0', '-')

    assertTrue(Arrays.equals(null: Array[Char], null: Array[Char]))
    assertTrue(Arrays.equals(a1, a1))
    assertTrue(Arrays.equals(a1, Array[Char]('a', '0', '-')))

    assertFalse(Arrays.equals(a1, null))
    assertFalse(Arrays.equals(a1, Array[Char]('z')))
    assertFalse(Arrays.equals(a1, Array[Char]('a')))
    assertFalse(Arrays.equals(a1, Array[Char]()))
    assertFalse(Arrays.equals(a1, Array[Char]('a', '0', '+')))
    assertFalse(Arrays.equals(a1, Array[Char]('a', '0', '-', 'z')))
  }

  @Test def equalsShorts(): Unit = {
    val a1 = Array[Short](1, -7, 10)

    assertTrue(Arrays.equals(null: Array[Short], null: Array[Short]))
    assertTrue(Arrays.equals(a1, a1))
    assertTrue(Arrays.equals(a1, Array[Short](1, -7, 10)))

    assertFalse(Arrays.equals(a1, null))
    assertFalse(Arrays.equals(a1, Array[Short](3)))
    assertFalse(Arrays.equals(a1, Array[Short](1)))
    assertFalse(Arrays.equals(a1, Array[Short]()))
    assertFalse(Arrays.equals(a1, Array[Short](1, -7, 11)))
    assertFalse(Arrays.equals(a1, Array[Short](1, -7, 11, 20)))
  }

  @Test def equalsInts(): Unit = {
    val a1 = Array[Int](1, -7, 10)

    assertTrue(Arrays.equals(null: Array[Int], null: Array[Int]))
    assertTrue(Arrays.equals(a1, a1))
    assertTrue(Arrays.equals(a1, Array[Int](1, -7, 10)))

    assertFalse(Arrays.equals(a1, null))
    assertFalse(Arrays.equals(a1, Array[Int](3)))
    assertFalse(Arrays.equals(a1, Array[Int](1)))
    assertFalse(Arrays.equals(a1, Array[Int]()))
    assertFalse(Arrays.equals(a1, Array[Int](1, -7, 11)))
    assertFalse(Arrays.equals(a1, Array[Int](1, -7, 11, 20)))
  }

  @Test def equalsLongs(): Unit = {
    val a1 = Array[Long](1L, -7L, 10L)

    assertTrue(Arrays.equals(null: Array[Long], null: Array[Long]))
    assertTrue(Arrays.equals(a1, a1))
    assertTrue(Arrays.equals(a1, Array[Long](1L, -7L, 10L)))

    assertFalse(Arrays.equals(a1, null))
    assertFalse(Arrays.equals(a1, Array[Long](3L)))
    assertFalse(Arrays.equals(a1, Array[Long](1L)))
    assertFalse(Arrays.equals(a1, Array[Long]()))
    assertFalse(Arrays.equals(a1, Array[Long](1L, -7L, 11L)))
    assertFalse(Arrays.equals(a1, Array[Long](1L, -7L, 11L, 20L)))
  }

  @Test def equalsFloats(): Unit = {
    val a1 = Array[Float](1.1f, -7.4f, 10.0f)

    assertTrue(Arrays.equals(null: Array[Float], null: Array[Float]))
    assertTrue(Arrays.equals(a1, a1))
    assertTrue(Arrays.equals(a1, Array[Float](1.1f, -7.4f, 10.0f)))

    assertFalse(Arrays.equals(a1, null))
    assertFalse(Arrays.equals(a1, Array[Float](3.0f)))
    assertFalse(Arrays.equals(a1, Array[Float](1.1f)))
    assertFalse(Arrays.equals(a1, Array[Float]()))
    assertFalse(Arrays.equals(a1, Array[Float](1.1f, -7.4f, 11.0f)))
    assertFalse(Arrays.equals(a1, Array[Float](1.1f, -7.4f, 10.0f, 20.0f)))
  }

  @Test def equalsDoubles(): Unit = {
    val a1 = Array[Double](1.1, -7.4, 10.0)

    assertTrue(Arrays.equals(null: Array[Double], null: Array[Double]))
    assertTrue(Arrays.equals(a1, a1))
    assertTrue(Arrays.equals(a1, Array[Double](1.1, -7.4, 10.0)))

    assertFalse(Arrays.equals(a1, null))
    assertFalse(Arrays.equals(a1, Array[Double](3.0)))
    assertFalse(Arrays.equals(a1, Array[Double](1.1)))
    assertFalse(Arrays.equals(a1, Array[Double]()))
    assertFalse(Arrays.equals(a1, Array[Double](1.1, -7.4, 11.0)))
    assertFalse(Arrays.equals(a1, Array[Double](1.1, -7.4, 10.0, 20.0)))
  }

  @Test def equalsAnyRefs(): Unit = {
    // scalastyle:off equals.hash.code
    class A(private val x: Int) {
      override def equals(that: Any): Boolean = that match {
        case that: A => this.x == that.x
        case _ => false
      }
    }
    // scalastyle:on equals.hash.code

    def A(x: Int): A = new A(x)

    val a1 = Array[AnyRef](A(1), A(-7), null, A(10))

    assertTrue(Arrays.equals(null: Array[AnyRef], null: Array[AnyRef]))
    assertTrue(Arrays.equals(a1, a1))
    assertTrue(Arrays.equals(a1, Array[AnyRef](A(1), A(-7), null, A(10))))
    assertTrue(Arrays.equals(Array[AnyRef](A(1), A(-7), null, A(10)), a1))

    assertFalse(Arrays.equals(a1, null))
    assertFalse(Arrays.equals(a1, Array[AnyRef](A(3))))
    assertFalse(Arrays.equals(a1, Array[AnyRef](A(1))))
    assertFalse(Arrays.equals(a1, Array[AnyRef]()))
    assertFalse(Arrays.equals(a1, Array[AnyRef](A(1), null, null, A(10))))
    assertFalse(Arrays.equals(a1, Array[AnyRef](A(1), A(-7), null, A(10), A(20))))
    assertFalse(Arrays.equals(a1, Array[AnyRef](A(1), A(-7), A(11), A(10))))

    assertFalse(Arrays.equals(null, a1))
    assertFalse(Arrays.equals(Array[AnyRef](A(3)), a1))
    assertFalse(Arrays.equals(Array[AnyRef](A(1)), a1))
    assertFalse(Arrays.equals(Array[AnyRef](), a1))
    assertFalse(Arrays.equals(Array[AnyRef](A(1), null, null, A(10)), a1))
    assertFalse(Arrays.equals(Array[AnyRef](A(1), A(-7), null, A(10), A(20)), a1))
    assertFalse(Arrays.equals(Array[AnyRef](A(1), A(-7), A(11), A(10)), a1))
  }

  @Test def deepEquals(): Unit = {
    assertTrue(Arrays.deepEquals(
        null: Array[AnyRef],
        null: Array[AnyRef]))
    assertTrue(Arrays.deepEquals(
        Array[AnyRef](),
        Array[AnyRef]()))
    assertTrue(Arrays.deepEquals(
        Array[AnyRef](null, null),
        Array[AnyRef](null, null)))
    assertTrue(Arrays.deepEquals(
        Array[AnyRef]("a", "b", null),
        Array[AnyRef]("a", "b", null)))
    assertTrue(Arrays.deepEquals(
        Array[AnyRef](null, "a", "b", null, "fooooo"),
        Array[AnyRef](null, "a", "b", null, "fooooo")))
    assertTrue(Arrays.deepEquals(
        Array[AnyRef](null, Array[AnyRef]()),
        Array[AnyRef](null, Array[AnyRef]())))
    assertTrue(Arrays.deepEquals(
        Array[AnyRef](Array[AnyRef](), Array[AnyRef]()),
        Array[AnyRef](Array[AnyRef](), Array[AnyRef]())))
    assertTrue(Arrays.deepEquals(
        Array[AnyRef](Array[AnyRef](Array[AnyRef]())),
        Array[AnyRef](Array[AnyRef](Array[AnyRef]()))))
    assertTrue(Arrays.deepEquals(
        Array[AnyRef](Array[AnyRef](Array[Int]())),
        Array[AnyRef](Array[AnyRef](Array[Int]()))))
    assertTrue(Arrays.deepEquals(
        Array[AnyRef](Array[AnyRef](Array[Double]())),
        Array[AnyRef](Array[AnyRef](Array[Double]()))))
    assertTrue(Arrays.deepEquals(
        Array[AnyRef](Array[AnyRef](Array[Int](1))),
        Array[AnyRef](Array[AnyRef](Array[Int](1)))))
    assertTrue(Arrays.deepEquals(
        Array[AnyRef](Array[AnyRef](Array[AnyRef](1.asInstanceOf[AnyRef]))),
        Array[AnyRef](Array[AnyRef](Array[AnyRef](1.asInstanceOf[AnyRef])))))

    assertFalse(Arrays.deepEquals(
        null: Array[AnyRef],
        Array[AnyRef]()))
    assertFalse(Arrays.deepEquals(
        Array[AnyRef](),
        null: Array[AnyRef]))
    assertFalse(Arrays.deepEquals(
        Array[AnyRef](Array[AnyRef](), null),
        Array[AnyRef](null, null)))
    assertFalse(Arrays.deepEquals(
        Array[AnyRef](null, Array[AnyRef]()),
        Array[AnyRef](null, null)))
    assertFalse(Arrays.deepEquals(
        Array[AnyRef]("a", "b", null),
        Array[AnyRef]("a", "c", null)))
    assertFalse(Arrays.deepEquals(
        Array[AnyRef](null, "a", "b", null, "fooooo"),
        Array[AnyRef](null, "a", "b", "c", "fooooo")))
    assertFalse(Arrays.deepEquals(
        Array[AnyRef](null, Array[AnyRef]()),
        Array[AnyRef](null, Array[AnyRef](null))))
    assertFalse(Arrays.deepEquals(
        Array[AnyRef](Array[AnyRef](), Array[AnyRef]()),
        Array[AnyRef](Array[AnyRef](), Array[AnyRef](null))))
    assertFalse(Arrays.deepEquals(
        Array[AnyRef](Array[AnyRef](Array[AnyRef]())),
        Array[AnyRef](Array[AnyRef](Array[AnyRef](null)))))
    assertFalse(Arrays.deepEquals(
        Array[AnyRef](Array[AnyRef](Array[Int]())),
        Array[AnyRef](Array[AnyRef](Array[Int](1)))))
    assertFalse(Arrays.deepEquals(
        Array[AnyRef](Array[AnyRef](Array[Double]())),
        Array[AnyRef](Array[AnyRef](Array[Double](1.0)))))
    assertFalse(Arrays.deepEquals(
        Array[AnyRef](Array[AnyRef](Array[Int](1))),
        Array[AnyRef](Array[AnyRef](Array[Int](2)))))
    assertFalse(Arrays.deepEquals(
        Array[AnyRef](Array[AnyRef](Array[AnyRef](1.asInstanceOf[AnyRef]))),
        Array[AnyRef](Array[AnyRef](Array[AnyRef](2.asInstanceOf[AnyRef])))))
  }

  @Test def toStringLong(): Unit = {
    assertEquals("null", Arrays.toString(null: Array[Long]))
    assertEquals("[]", Arrays.toString(Array[Long]()))
    assertEquals("[0]", Arrays.toString(Array[Long](0L)))
    assertEquals("[1]", Arrays.toString(Array[Long](1L)))
    assertEquals("[2, 3]", Arrays.toString(Array[Long](2L, 3)))
    assertEquals("[1, 2, 3, 4, 5]", Arrays.toString(Array[Long](1L, 2L, 3L, 4L, 5L)))
    assertEquals("[1, -2, 3, 9223372036854775807]", Arrays.toString(Array[Long](1L, -2L, 3L, Long.MaxValue)))
  }

  @Test def toStringInt(): Unit = {
    assertEquals("null", Arrays.toString(null: Array[Int]))
    assertEquals("[]", Arrays.toString(Array[Int]()))
    assertEquals("[0]", Arrays.toString(Array[Int](0)))
    assertEquals("[1]", Arrays.toString(Array[Int](1)))
    assertEquals("[2, 3]", Arrays.toString(Array[Int](2, 3)))
    assertEquals("[1, 2, 3, 4, 5]", Arrays.toString(Array[Int](1, 2, 3, 4, 5)))
    assertEquals("[1, -2, 3, 2147483647]", Arrays.toString(Array[Int](1, -2, 3, Int.MaxValue)))
  }

  @Test def toStringShort(): Unit = {
    assertEquals("null", Arrays.toString(null: Array[Short]))
    assertEquals("[]", Arrays.toString(Array[Short]()))
    assertEquals("[0]", Arrays.toString(Array[Short](0)))
    assertEquals("[1]", Arrays.toString(Array[Short](1)))
    assertEquals("[2, 3]", Arrays.toString(Array[Short](2, 3)))
    assertEquals("[1, 2, 3, 4, 5]", Arrays.toString(Array[Short](1, 2, 3, 4, 5)))
    assertEquals("[1, -2, 3, 32767]", Arrays.toString(Array[Short](1, -2, 3, Short.MaxValue)))
  }

  @Test def toStringByte(): Unit = {
    assertEquals("null", Arrays.toString(null: Array[Byte]))
    assertEquals("[]", Arrays.toString(Array[Byte]()))
    assertEquals("[0]", Arrays.toString(Array[Byte](0)))
    assertEquals("[1]", Arrays.toString(Array[Byte](1)))
    assertEquals("[2, 3]", Arrays.toString(Array[Byte](2, 3)))
    assertEquals("[1, 2, 3, 4, 5]", Arrays.toString(Array[Byte](1, 2, 3, 4, 5)))
    assertEquals("[1, -2, 3, 127]", Arrays.toString(Array[Byte](1, -2, 3, Byte.MaxValue)))
  }

  @Test def toStringBoolean(): Unit = {
    assertEquals("null", Arrays.toString(null: Array[Boolean]))
    assertEquals("[]", Arrays.toString(Array[Boolean]()))
    assertEquals("[true]", Arrays.toString(Array[Boolean](true)))
    assertEquals("[false]", Arrays.toString(Array[Boolean](false)))
    assertEquals("[true, false]", Arrays.toString(Array[Boolean](true, false)))
    assertEquals("[true, true, false, false]", Arrays.toString(Array[Boolean](true, true, false, false)))
  }

  @Test def toStringFloat(): Unit = {
    assumeFalse("Assumes Float.toString JS semantics.", executingInJVM)
    assertEquals("null", Arrays.toString(null: Array[Float]))
    assertEquals("[]", Arrays.toString(Array[Float]()))
    assertEquals("[0]", Arrays.toString(Array[Float](0.0f)))
    assertEquals("[1.100000023841858]", Arrays.toString(Array[Float](1.1f)))
    assertEquals("[2.200000047683716, 3]", Arrays.toString(Array[Float](2.2f, 3f)))
    assertEquals("[1, 2, 3, 4, 5]", Arrays.toString(Array[Float](1f, 2f, 3f, 4f, 5f)))
    assertEquals("[1, -2, 3, 3.4028234663852886e+38]", Arrays.toString(Array[Float](1f, -2f, 3f, Float.MaxValue)))
  }

  @Test def toStringDouble(): Unit = {
    assumeFalse("Assumes Double.toString JS semantics.", executingInJVM)
    assertEquals("null", Arrays.toString(null: Array[Double]))
    assertEquals("[]", Arrays.toString(Array[Double]()))
    assertEquals("[0]", Arrays.toString(Array[Double](0.0d)))
    assertEquals("[1.1]", Arrays.toString(Array[Double](1.1d)))
    assertEquals("[2.2, 3]", Arrays.toString(Array[Double](2.2d, 3d)))
    assertEquals("[1, 2, 3, 4, 5]", Arrays.toString(Array[Double](1d, 2d, 3d, 4d, 5d)))
    assertEquals("[1, -2, 3, 1.7976931348623157e+308]",
        Arrays.toString(Array[Double](1d, -2d, 3d, Double.MaxValue)))
  }

  @Test def toStringAnyRef(): Unit = {
    class C(num: Int) {
      override def toString: String = s"C($num)"
    }
    assertEquals("null", Arrays.toString(null: Array[AnyRef]))
    assertEquals("[]", Arrays.toString(Array[AnyRef]()))
    assertEquals("[abc]", Arrays.toString(Array[AnyRef]("abc")))
    assertEquals("[a, b, c]", Arrays.toString(Array[AnyRef]("a", "b", "c")))
    assertEquals("[C(1)]", Arrays.toString(Array[AnyRef](new C(1))))
    assertEquals("[C(1), abc, 1, null]", Arrays.toString(Array[AnyRef](new C(1), "abc", Int.box(1), null)))
  }

  @Test def deepToString(): Unit = {
    assertEquals("null", Arrays.deepToString(null: Array[AnyRef]))
    assertEquals("[abc]", Arrays.deepToString(Array[AnyRef]("abc")))
    assertEquals("[a, b, c]", Arrays.deepToString(Array[AnyRef]("a", "b", "c")))
    assertEquals("[[1, 2, 3]]", Arrays.deepToString(Array[AnyRef](Array[Int](1, 2, 3))))
    assertEquals("[[1, 2, 3], [4, 5, 6]]",
        Arrays.deepToString(Array[AnyRef](Array[Int](1, 2, 3), Array[Int](4, 5, 6))))
    assertEquals("[[]]", Arrays.deepToString(Array[AnyRef](Array[AnyRef]())))
    assertEquals("[[[]]]", Arrays.deepToString(Array[AnyRef](Array[AnyRef](Array[AnyRef]()))))
    assertEquals("[[[[1, 2, 3]]], [4, 5, 6]]", Arrays.deepToString(
        Array[AnyRef](Array[AnyRef](Array[AnyRef](Array[Int](1, 2, 3))), Array[Int](4, 5, 6))))

    val recArr = Array[AnyRef](null, null)
    recArr(0) = recArr
    assertEquals("[[...], null]", Arrays.deepToString(recArr))
    assertEquals("[[[...], null]]", Arrays.deepToString(Array[AnyRef](recArr)))
    assertEquals("[[[...], null]]", Arrays.deepToString(Array[AnyRef](recArr)))
    recArr(1) = Array[AnyRef](null, Array[AnyRef](null, recArr, Array[AnyRef](recArr)))
    assertEquals("[[...], [null, [null, [...], [[...]]]]]", Arrays.deepToString(recArr))
  }
}
