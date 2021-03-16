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

class ToDoubleFunctionTest {
  @Test def applyAsDouble(): Unit = {
    val op = new ToDoubleFunction[String] {
      override def applyAsDouble(value: String): Double = s"$value.5".toDouble
    }
    assertEquals(op.applyAsDouble("1"), 1.5, 0.0)
  }
}
