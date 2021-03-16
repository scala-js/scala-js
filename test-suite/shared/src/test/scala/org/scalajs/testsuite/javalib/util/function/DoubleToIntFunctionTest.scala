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

class DoubleToIntFunctionTest {
  @Test def applyAsInt(): Unit = {
    val f = new DoubleToIntFunction {
      override def applyAsInt(value: Double): Int = value.toInt
    }
    assertEquals(f.applyAsInt(0.5), 0)
    assertEquals(f.applyAsInt(3.3), 3)
  }
}
