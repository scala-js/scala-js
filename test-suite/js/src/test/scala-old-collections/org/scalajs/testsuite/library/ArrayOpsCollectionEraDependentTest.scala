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

  @Test def apply(): Unit = {
    val array = js.Array(3, 4, 5, 6, 3, 4)
    val ops: js.ArrayOps[Int] = array

    assertEquals(3, ops(0))
    assertEquals(6, ops(3))

    array(0) = 4
    assertEquals(4, ops(0))
  }

  @Test def update(): Unit = {
    val array = js.Array(3, 4, 5, 6, 3, 4)
    val ops: js.ArrayOps[Int] = array

    assertEquals(4, array(1))
    ops(1) = 5
    assertEquals(5, array(1))

    ops(5) = 10
    assertEquals(10, array(5))
  }

  @Test def length(): Unit = {
    val array = js.Array(3, 4, 5, 6, 3, 4)
    val ops: js.ArrayOps[Int] = array

    assertEquals(6, ops.length)
    array.push(1)
    assertEquals(7, ops.length)
  }

  @Test def seq(): Unit = {
    val array = js.Array(3, 4, 5, 6, 3, 4)
    val ops: js.ArrayOps[Int] = array
    val seq = ops.seq

    assertEquals(List(3, 4, 5, 6, 3, 4), seq.toList)
  }

  @Test def toT_Issue843(): Unit = {
    val array = js.Array(1, 2, 1, 3, 1, 10, 9)
    val list = array.to[List]
    assertArrayEquals(array.toArray, list.toArray)
  }

}
