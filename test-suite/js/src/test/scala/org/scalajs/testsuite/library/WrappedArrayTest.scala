/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.library

import scala.scalajs.js

import org.junit.Assert._
import org.junit.Test

import scala.collection.mutable

class WrappedArrayTest {

  // Methods we actually implement

  @Test def apply(): Unit = {
    val array = js.Array(3,4,5,6,3,4)
    val seq: Seq[Int] = array

    assertEquals(3, seq(0))
    assertEquals(6, seq(3))

    array(0) = 4
    assertEquals(4, seq(0))
  }

  @Test def update(): Unit = {
    val array = js.Array(3,4,5,6,3,4)
    val seq: mutable.Seq[Int] = array

    assertEquals(4, array(1))
    seq(1) = 5
    assertEquals(5, array(1))

    seq(5) = 10
    assertEquals(10, array(5))
  }

  @Test def length(): Unit = {
    val array = js.Array(3,4,5,6,3,4)
    val seq: Seq[Int] = array

    assertEquals(6, seq.length)
    array.push(1)
    assertEquals(7, seq.length)
  }

  @Test def +=:(): Unit = {
    val array = js.Array(5, 8, 9)
    3 +=: array
    assertArrayEquals(Array(3, 5, 8, 9), array.toArray)
  }

  @Test def ++=:(): Unit = {
    val array = js.Array(5, 8, 9)
    js.Array(2, 0) ++=: array
    assertArrayEquals(Array(2, 0, 5, 8, 9), array.toArray)
    Seq(-3, -45, 1) ++=: array
    assertArrayEquals(Array(-3, -45, 1, 2, 0, 5, 8, 9), array.toArray)
  }

  @Test def insertAll(): Unit = {
    val array = js.Array(5, 8, 9)
    array.insertAll(2, js.Array(2, 0))
    assertArrayEquals(Array(5, 8, 2, 0, 9), array.toArray)
    array.insertAll(1, Seq(-3, -45, 1))
    assertArrayEquals(Array(5, -3, -45, 1, 8, 2, 0, 9), array.toArray)
  }

  @Test def remove(): Unit = {
    val array = js.Array(5, 8, 2, 0, 9)
    assertEquals(8, array.remove(1))
    assertArrayEquals(Array(5, 2, 0, 9), array.toArray)

    array.remove(0, 3)
    assertArrayEquals(Array(9), array.toArray)
  }

  // Some arbitrary methods to test the builders

  @Test def collect(): Unit = {
    // Ascribe to right type here, so we'll actually produce a WrappedArray
    val seq: js.WrappedArray[Int] = js.Array(3,4,5,6,3,4)
    val res = seq.collect {
      case x if x > 4 => 2*x
    }

    assertEquals(classOf[js.WrappedArray[Int]], res.getClass)
    assertArrayEquals(Array(10,12), res.toArray)
    assertArrayEquals(Array(10,12), res.array.toArray)
  }

  @Test def diff(): Unit = {
    val seq: Seq[Int] = js.Array(1,2,1,3,1,10,9)
    val diff = seq.diff(Seq(1,3,9))
    assertArrayEquals(Array(2,1,1,10), diff.toArray)
  }

  @Test def toList(): Unit = {
    val seq: Seq[Int] = js.Array(1,2,1,3,1,10,9)
    val list = seq.toList
    assertEquals(List(1,2,1,3,1,10,9), list)
  }

  @Test def to[T](): Unit = {
    val seq: Seq[Int] = js.Array(1,2,1,3,1,10,9)
    val list = seq.to[List]
    assertEquals(List(1,2,1,3,1,10,9), list)
  }

}
