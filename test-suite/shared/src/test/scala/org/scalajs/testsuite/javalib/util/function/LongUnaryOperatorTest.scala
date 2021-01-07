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

class LongUnaryOperatorTest {
  private val f = new LongUnaryOperator {
    override def applyAsLong(operand: Long): Long = operand * 10
  }
  private val g = new LongUnaryOperator {
    override def applyAsLong(operand: Long): Long = operand - 20
  }

  @Test def applyAsLong(): Unit = {
    assertEquals(f.applyAsLong(3), 30)
  }

  @Test def andThen(): Unit = {
    val h: LongUnaryOperator = f.andThen(g)
    assertEquals(h.applyAsLong(5), 30)
  }

  @Test def compose(): Unit = {
    val h: LongUnaryOperator = f.compose(g)
    assertEquals(h.applyAsLong(5), -150)
  }

  @Test def identity(): Unit = {
    val f: LongUnaryOperator = LongUnaryOperator.identity()
    assertEquals(1L, f.applyAsLong(1))
  }
}
