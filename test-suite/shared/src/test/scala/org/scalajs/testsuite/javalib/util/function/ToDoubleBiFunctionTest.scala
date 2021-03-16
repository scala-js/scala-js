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

class ToDoubleBiFunctionTest {
  @Test def applyAsDouble(): Unit = {
    val op = new ToDoubleBiFunction[String, String] {
      override def applyAsDouble(t: String, u: String): Double = s"$t.$u".toDouble
    }
    assertEquals(op.applyAsDouble("123", "456"), 123.456, 0.0)
  }
}
