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

class ToIntFunctionTest {
  @Test def applyAsInt(): Unit = {
    val op = new ToIntFunction[String] {
      override def applyAsInt(value: String): Int = value.length
    }
    assertEquals(op.applyAsInt("abc"), 3)
  }
}
