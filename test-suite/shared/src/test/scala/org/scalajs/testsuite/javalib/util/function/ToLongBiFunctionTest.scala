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

class ToLongBiFunctionTest {
  @Test def applyAsLong(): Unit = {
    val op = new ToLongBiFunction[String, String] {
      override def applyAsLong(t: String, u: String): Long = t.toLong * u.toLong
    }
    assertEquals(op.applyAsLong("11111111", "2222222"), 24691355308642L)
  }
}
