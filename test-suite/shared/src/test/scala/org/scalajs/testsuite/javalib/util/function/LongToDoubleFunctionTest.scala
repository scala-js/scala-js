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

class LongToDoubleFunctionTest {
  @Test def testApply(): Unit = {
    val f = new LongToDoubleFunction {
      override def applyAsDouble(value: Long): Double = value.toDouble * 0.5
    }
    assertEquals(f.applyAsDouble(3), 1.5, 0.0)
  }
}
