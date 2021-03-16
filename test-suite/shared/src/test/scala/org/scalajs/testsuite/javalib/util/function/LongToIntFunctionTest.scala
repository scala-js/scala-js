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

package org.scalajs.testsuite.javalib.util.function

import org.junit.Assert._
import org.junit.Test

import java.util.function._

class LongToIntFunctionTest {
  @Test def testApply(): Unit = {
    val f = new LongToIntFunction {
      override def applyAsInt(value: Long): Int = value.toInt / 2
    }
    assertEquals(f.applyAsInt(3), 1)
  }
}
