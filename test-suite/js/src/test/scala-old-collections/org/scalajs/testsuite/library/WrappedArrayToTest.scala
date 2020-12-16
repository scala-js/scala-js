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

class WrappedArrayToTest {

  @Test def toT(): Unit = {
    val seq: collection.Seq[Int] = js.Array(1, 2, 1, 3, 1, 10, 9)
    val list = seq.to[List]
    assertEquals(List(1, 2, 1, 3, 1, 10, 9), list)
  }

}
