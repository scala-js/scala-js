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

class ToLongFunctionTest {
  @Test def applyAsLong(): Unit = {
    val op = new ToLongFunction[String] {
      override def applyAsLong(value: String): Long = value.toLong
    }
    assertEquals(op.applyAsLong("123456787654321"), 123456787654321L)
  }
}
