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

class IntBinaryOperatorTest {
  @Test def applyAsInt(): Unit = {
    val max = new IntBinaryOperator {
      override def applyAsInt(left: Int, right: Int): Int = left.max(right)
    }
    assertEquals(max.applyAsInt(3, 5), 5)
    assertEquals(max.applyAsInt(0, -2), 0)
  }
}
