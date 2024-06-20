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

class IntUnaryOperatorTest {
  private val f = new IntUnaryOperator {
    override def applyAsInt(operand: Int): Int = operand - 1
  }

  private val g = new IntUnaryOperator {
    override def applyAsInt(operand: Int): Int = operand * 2
  }

  @Test def applyAsInt(): Unit =
    assertEquals(f.applyAsInt(3), 2)

  @Test def andThen(): Unit = {
    val h: IntUnaryOperator = f.andThen(g)
    assertEquals(h.applyAsInt(5), 8)
  }

  @Test def compose(): Unit = {
    val h: IntUnaryOperator = f.compose(g)
    assertEquals(h.applyAsInt(5), 9)
  }

  @Test def identity(): Unit = {
    val f: IntUnaryOperator = IntUnaryOperator.identity()
    assertEquals(1, f.applyAsInt(1))
  }
}
