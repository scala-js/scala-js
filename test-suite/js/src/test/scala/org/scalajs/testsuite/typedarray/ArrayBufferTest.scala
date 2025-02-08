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

package org.scalajs.testsuite.typedarray

import org.junit.Assert._
import org.junit.Test

import scala.scalajs.js.typedarray._

class ArrayBufferTest {

  @Test def lengthConstructor(): Unit = {
    val x = new ArrayBuffer(100)
    assertTrue(x.isInstanceOf[ArrayBuffer])
    assertEquals(100, x.byteLength)
  }

  @Test def sliceWithOneArg(): Unit = {
    val x = new ArrayBuffer(100)
    val y = x.slice(10)
    assertEquals(90, y.byteLength)

  }

  @Test def sliceWithTwoArgs(): Unit = {
    val x = new ArrayBuffer(100)
    val y = x.slice(10, 20)
    assertEquals(10, y.byteLength)
  }
}
