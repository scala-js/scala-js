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

class IntToDoubleFunctionTest {
  @Test def testApply(): Unit = {
    val f = new IntToDoubleFunction {
      override def applyAsDouble(value: Int): Double = value.toDouble / 10d
    }
    assertEquals(f.applyAsDouble(3), 0.3, 0.0)
    assertEquals(f.applyAsDouble(20), 2, 0.0)
  }
}
