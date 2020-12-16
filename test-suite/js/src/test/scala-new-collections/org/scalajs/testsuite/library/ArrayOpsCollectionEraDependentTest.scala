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
import org.junit.Test

class ArrayOpsCollectionEraDependentTest {
  import ArrayOpsTest._

  @Test def knownSize(): Unit = {
    assertEquals(4, js.Array(1, 2, 5, 65).knownSize)
  }

  @Test def mapInPlace(): Unit = {
    val array = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)
    assertJSArrayEqualsSame(array, js.Array(3, 15, 21, 6, 162, 6, 234, 0, 9), array.mapInPlace(_ * 3))
  }

  @Test def flatten(): Unit = {
    val array = js.Array(js.Array(6, 2), js.Array(56), js.Array(), js.Array(-1, 4, 2))
    assertJSArrayEqualsNotSame(array, js.Array(6, 2, 56, -1, 4, 2), array.flatten)
  }

  @Test def appended(): Unit = {
    val array = js.Array(1, 5, 7, 2)
    assertJSArrayEqualsNotSame(array, js.Array(1, 5, 7, 2, 10), array.appended(10))
  }

  @Test def prepended(): Unit = {
    val array = js.Array(1, 5, 7, 2)
    assertJSArrayEqualsNotSame(array, js.Array(10, 1, 5, 7, 2), array.prepended(10))
  }

  @Test def prependedAll(): Unit = {
    val array = js.Array(1, 5, 7, 2)
    assertJSArrayEqualsNotSame(array, js.Array(10, 11, 1, 5, 7, 2), array.prependedAll(js.Array(10, 11)))
    assertJSArrayEqualsNotSame(array, js.Array(10, 11, 1, 5, 7, 2), array.prependedAll(List(10, 11)))
  }

  @Test def :++(): Unit = {
    val array = js.Array(1, 5, 7, 2)
    assertJSArrayEqualsNotSame(array, js.Array(1, 5, 7, 2, 10, 11), array :++ js.Array(10, 11))
    assertJSArrayEqualsNotSame(array, js.Array(1, 5, 7, 2, 10, 11), array :++ List(10, 11))
  }

  @Test def appendedAll(): Unit = {
    val array = js.Array(1, 5, 7, 2)
    assertJSArrayEqualsNotSame(array, js.Array(1, 5, 7, 2, 10, 11), array.appendedAll(js.Array(10, 11)))
    assertJSArrayEqualsNotSame(array, js.Array(1, 5, 7, 2, 10, 11), array.appendedAll(List(10, 11)))
  }

  @Test def unzip(): Unit = {
    val array = js.Array(1 -> "foo", 5 -> "bar", 7 -> "baz")
    val unzipped = array.unzip // do not influence type inference
    val typed: (js.Array[Int], js.Array[String]) = unzipped
    assertJSArrayPairEquals((js.Array(1, 5, 7), js.Array("foo", "bar", "baz")), typed)
  }

  @Test def unzip3(): Unit = {
    val array = js.Array((1, "foo", Nil), (5, "bar", List(54)), (7, "baz", List(32, 12)))
    val unzipped = array.unzip3 // do not influence type inference
    val typed: (js.Array[Int], js.Array[String], js.Array[List[Int]]) = unzipped
    assertJSArrayEquals(js.Array(1, 5, 7), typed._1)
    assertJSArrayEquals(js.Array("foo", "bar", "baz"), typed._2)
    assertJSArrayEquals(js.Array(Nil, List(54), List(32, 12)), typed._3)
  }

  @Test def transpose(): Unit = {
    val array = js.Array(
        js.Array(1, 2, 3, 4),
        js.Array(5, 6, 7, 8),
        js.Array(9, 10, 11, 12))
    val transposed = array.transpose // do not influence type inference
    assertNotSame(array, transposed)
    val typed: js.Array[js.Array[Int]] = transposed

    assertEquals(4, typed.length)
    assertJSArrayEquals(js.Array(1, 5, 9), typed(0))
    assertJSArrayEquals(js.Array(2, 6, 10), typed(1))
    assertJSArrayEquals(js.Array(3, 7, 11), typed(2))
    assertJSArrayEquals(js.Array(4, 8, 12), typed(3))

    val emptyArray = js.Array[js.Array[Int]]()
    assertJSArrayEqualsNotSame(emptyArray, emptyArray, emptyArray.transpose)
  }

  @Test def distinctBy(): Unit = {
    val array = js.Array(5, 7, 1, 34, 7, 3, 5, 9, 9)
    assertJSArrayEqualsNotSame(array, js.Array(5, 7, 34), array.distinctBy(_ % 4))
  }

  @Test def groupMap(): Unit = {
    val array = js.Array("foo" -> 1, "bar" -> 5, "baz" -> 1, "foobar" -> 3,
        "hello" -> 7, "bonjour" -> 3)
    val groups = array.groupMap(_._2)(_._1)
    assertEquals(Set(1, 3, 5, 7), groups.keySet)
    assertJSArrayEquals(js.Array("foo", "baz"), groups(1))
    assertJSArrayEquals(js.Array("foobar", "bonjour"), groups(3))
    assertJSArrayEquals(js.Array("bar"), groups(5))
    assertJSArrayEquals(js.Array("hello"), groups(7))
  }

  @Test def addOne(): Unit = {
    val array = js.Array(2, 3, 5)
    assertJSArrayEqualsSame(array, js.Array(2, 3, 5, 13), array.addOne(13))
  }

  @Test def addAll(): Unit = {
    val array = js.Array(2, 3, 5)
    array.addAll(List(13, 21, 36))
    assertJSArrayEquals(js.Array(2, 3, 5, 13, 21, 36), array)
    array.addAll(js.Array(51, 0, 2))
    assertJSArrayEquals(js.Array(2, 3, 5, 13, 21, 36, 51, 0, 2), array)
  }

  @Test def subtractOne(): Unit = {
    val array = js.Array(2, 3, 5, 3, 24, 2)
    array.subtractOne(3)
    assertJSArrayEquals(js.Array(2, 5, 3, 24, 2), array)
    array.subtractOne(42)
    assertJSArrayEquals(js.Array(2, 5, 3, 24, 2), array)
  }

  @Test def subtractAll(): Unit = {
    val array = js.Array(2, 3, 5, 3, 24, 2)
    array.subtractAll(List(3, 24))
    assertJSArrayEquals(js.Array(2, 5, 3, 2), array)
    array.subtractAll(js.Array(5, 2, 2))
    assertJSArrayEquals(js.Array(3), array)
  }

  @Test def prepend(): Unit = {
    val array = js.Array(2, 3, 5)
    assertJSArrayEqualsSame(array, js.Array(13, 2, 3, 5), array.prepend(13))
  }

  @Test def patchInPlace(): Unit = {
    val arrayBase = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)
    var array = arrayBase.jsSlice()
    assertJSArrayEqualsSame(array,
        js.Array(1, 5, 7, 0, 3),
        array.patchInPlace(3, Nil, 4))
    array = arrayBase.jsSlice()
    assertJSArrayEqualsSame(array,
        js.Array(1, 5, 7, 42, 34, 0, 3),
        array.patchInPlace(3, js.Array(42, 34), 4))
    array = arrayBase.jsSlice()
    assertJSArrayEqualsSame(array,
        js.Array(42, 34, 54, 2, 78, 0, 3),
        array.patchInPlace(-3, js.Array(42, 34), 4))
    array = arrayBase.jsSlice()
    assertJSArrayEqualsSame(array,
        js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3, 42, 34),
        array.patchInPlace(17, js.Array(42, 34), 6))
    array = arrayBase.jsSlice()
    assertJSArrayEqualsSame(array,
        js.Array(1, 5, 7, 2, 42, 34, 54, 2, 78, 0, 3),
        array.patchInPlace(4, js.Array(42, 34), -6))

    /* Unclear what the semantics of the following one should be.
     * See https://github.com/scala/bug/issues/11114
     */
    /*array = arrayBase.jsSlice()
    assertJSArrayEqualsSame(array,
        js.Array(1, 5, 7, 2, 54, 2, 78, 42),
        array.patchInPlace(7, js.Array(42), 6))*/
  }

  @Test def takeInPlace(): Unit = {
    val array = js.Array(1, 5, 7, 2, 42, 34, 54, 2, 78, 0, 3)
    assertJSArrayEqualsSame(array, js.Array(1, 5, 7, 2, 42, 34, 54), array.takeInPlace(7))
    assertJSArrayEqualsSame(array, js.Array(1, 5, 7, 2), array.takeInPlace(4))
    assertJSArrayEqualsSame(array, js.Array(1, 5, 7, 2), array.takeInPlace(10))
    assertJSArrayEqualsSame(array, js.Array(), array.takeInPlace(-3))
  }

  @Test def dropInPlace(): Unit = {
    val array = js.Array(1, 5, 7, 2, 42, 34, 54, 2, 78, 0, 3)
    assertJSArrayEqualsSame(array, js.Array(5, 7, 2, 42, 34, 54, 2, 78, 0, 3), array.dropInPlace(1))
    assertJSArrayEqualsSame(array, js.Array(34, 54, 2, 78, 0, 3), array.dropInPlace(4))
    assertJSArrayEqualsSame(array, js.Array(34, 54, 2, 78, 0, 3), array.dropInPlace(-3))
    assertJSArrayEqualsSame(array, js.Array(), array.dropInPlace(10))
  }

  @Test def takeRightInPlace(): Unit = {
    val array = js.Array(1, 5, 7, 2, 42, 34, 54, 2, 78, 0, 3)
    assertJSArrayEqualsSame(array, js.Array(42, 34, 54, 2, 78, 0, 3), array.takeRightInPlace(7))
    assertJSArrayEqualsSame(array, js.Array(2, 78, 0, 3), array.takeRightInPlace(4))
    assertJSArrayEqualsSame(array, js.Array(2, 78, 0, 3), array.takeRightInPlace(10))
    assertJSArrayEqualsSame(array, js.Array(), array.takeRightInPlace(-3))
  }

  @Test def dropRightInPlace(): Unit = {
    val array = js.Array(1, 5, 7, 2, 42, 34, 54, 2, 78, 0, 3)
    assertJSArrayEqualsSame(array, js.Array(1, 5, 7, 2, 42, 34, 54, 2, 78, 0), array.dropRightInPlace(1))
    assertJSArrayEqualsSame(array, js.Array(1, 5, 7, 2, 42, 34), array.dropRightInPlace(4))
    assertJSArrayEqualsSame(array, js.Array(1, 5, 7, 2, 42, 34), array.dropRightInPlace(-3))
    assertJSArrayEqualsSame(array, js.Array(), array.dropRightInPlace(10))
  }

  @Test def sliceInPlace(): Unit = {
    val array = js.Array(1, 5, 7, 2, 42, 34, 54, 2, 78, 0, 3)
    assertJSArrayEqualsSame(array, js.Array(2, 42, 34, 54, 2, 78), array.sliceInPlace(3, 9))
    assertJSArrayEqualsSame(array, js.Array(2, 42, 34, 54, 2), array.sliceInPlace(-1, 5))
    assertJSArrayEqualsSame(array, js.Array(34, 54, 2), array.sliceInPlace(2, 10))
    assertJSArrayEqualsSame(array, js.Array(), array.sliceInPlace(2, 1))
  }

  @Test def dropWhileInPlace(): Unit = {
    val array = js.Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    assertJSArrayEqualsSame(array, js.Array(4, 5, 6, 7, 8, 9, 10, 11, 12), array.dropWhileInPlace(_ < 4))
    assertJSArrayEqualsSame(array, js.Array(8, 9, 10, 11, 12), array.dropWhileInPlace(_ < 8))
    assertJSArrayEqualsSame(array, js.Array(8, 9, 10, 11, 12), array.dropWhileInPlace(_ < 5))
    assertJSArrayEqualsSame(array, js.Array(), array.dropWhileInPlace(_ < 20))
  }

  @Test def takeWhileInPlace(): Unit = {
    val array = js.Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    assertJSArrayEqualsSame(array, js.Array(1, 2, 3, 4, 5, 6, 7, 8, 9), array.takeWhileInPlace(_ < 10))
    assertJSArrayEqualsSame(array, js.Array(1, 2, 3, 4), array.takeWhileInPlace(_ < 5))
    assertJSArrayEqualsSame(array, js.Array(1, 2, 3, 4), array.takeWhileInPlace(_ < 8))
    assertJSArrayEqualsSame(array, js.Array(), array.takeWhileInPlace(_ < 0))
  }

  @Test def padToInPlace(): Unit = {
    val array = js.Array(1, 5, 7, 2, 42)
    assertJSArrayEqualsSame(array, js.Array(1, 5, 7, 2, 42, 5, 5), array.padToInPlace(7, 5))
    assertJSArrayEqualsSame(array, js.Array(1, 5, 7, 2, 42, 5, 5), array.padToInPlace(3, 55))
    assertJSArrayEqualsSame(array, js.Array(1, 5, 7, 2, 42, 5, 5), array.padToInPlace(-2, 555))
  }

  @Test def flatMapInPlace(): Unit = {
    val array = js.Array(6, 2, 56, -1)
    assertJSArrayEqualsSame(array,
        js.Array(6, 7, 8, 2, 3, 4, 56, 57, 58, -1, 0, 1),
        array.flatMapInPlace(x => js.Array(x, x + 1, x + 2)))
  }

  @Test def filterInPlace(): Unit = {
    val array = js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3)
    assertJSArrayEqualsSame(array,
        js.Array(1, 5, 7, 2, 54, 2, 78, 0, 3), array.filterInPlace(_ < 100))
    assertJSArrayEqualsSame(array,
        js.Array(1, 5, 7, 2, 2, 0, 3), array.filterInPlace(_ < 10))
    assertJSArrayEqualsSame(array,
        js.Array(), array.filterInPlace(_ < 0))
  }

  @Test def toT_Issue843(): Unit = {
    val array = js.Array(1, 2, 1, 3, 1, 10, 9)
    val list = array.to(List)
    assertArrayEquals(array.toArray, list.toArray)
  }

}
