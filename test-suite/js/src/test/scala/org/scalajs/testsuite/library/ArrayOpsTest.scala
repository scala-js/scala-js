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

import org.scalajs.testsuite.utils.AssertThrows._

import scala.reflect.ClassTag

class ArrayOpsTest {

  // Methods we actually implement

  @Test def apply(): Unit = {
    val array = js.Array(3,4,5,6,3,4)
    val ops: js.ArrayOps[Int] = array

    assertEquals(3, ops(0))
    assertEquals(6, ops(3))

    array(0) = 4
    assertEquals(4, ops(0))
  }

  @Test def update(): Unit = {
    val array = js.Array(3,4,5,6,3,4)
    val ops: js.ArrayOps[Int] = array

    assertEquals(4, array(1))
    ops(1) = 5
    assertEquals(5, array(1))

    ops(5) = 10
    assertEquals(10, array(5))
  }

  @Test def length(): Unit = {
    val array = js.Array(3,4,5,6,3,4)
    val ops: js.ArrayOps[Int] = array

    assertEquals(6, ops.length)
    array.push(1)
    assertEquals(7, ops.length)
  }

  @Test def seq(): Unit = {
    val array = js.Array(3,4,5,6,3,4)
    val ops: js.ArrayOps[Int] = array
    val seq = ops.seq

    assertEquals(List(3,4,5,6,3,4), seq.toList)
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

  @Test def ++(): Unit = {
    val left = js.Array("hello", "world")
    val right = js.Array("and", "everyone", "else")
    assertArrayEquals(Array[AnyRef]("hello", "world", "and", "everyone", "else"),
        (left ++ right).toArray[AnyRef])

    val ints = js.Array(4, 3)
    val concat = left ++ ints
    assertEquals("hello", concat(0))
    assertEquals("world", concat(1))
    assertEquals(4, concat(2))
    assertEquals(3, concat(3))
  }

  // Some arbitrary methods to test the builders

  @Test def collect(): Unit = {
    def ct[A: ClassTag](x: A): ClassTag[A] = implicitly[ClassTag[A]]
    val array = js.Array(3,4,5,6,3,4)
    val res = array.collect {
      case x if x > 4 => 2*x
    }

    assertTrue(ct(res).runtimeClass == classOf[js.Array[Int]])
    assertArrayEquals(Array(10, 12), res.toArray)
  }

  @Test def diff(): Unit = {
    val array = js.Array(1,2,1,3,1,10,9)
    val diff = array.diff(Seq(1,3,9))
    assertArrayEquals(Array(2,1,1,10), diff.toArray)
  }

  @Test def toList_issue_843(): Unit = {
    val array = js.Array(1,2,1,3,1,10,9)
    val list = array.toList
    assertArrayEquals(array.toArray, list.toArray)
  }

  @Test def to_T_issue_843(): Unit = {
    val array = js.Array(1,2,1,3,1,10,9)
    val list = array.to[List]
    assertArrayEquals(array.toArray, list.toArray)
  }
}
