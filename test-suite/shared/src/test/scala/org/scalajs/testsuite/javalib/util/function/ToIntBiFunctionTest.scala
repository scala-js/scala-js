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

class ToIntBiFunctionTest {
  @Test def applyAsInt(): Unit = {
    val op = new ToIntBiFunction[String, String]{
      override def applyAsInt(t: String, u: String): Int = s"$t$u".toInt
    }
    assertEquals(op.applyAsInt("10", "24"), 1024)
  }
}
