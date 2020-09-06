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

import java.util.function.BinaryOperator

import org.junit.Assert._
import org.junit.Test

class BinaryOperatorTest {
  @Test def minBy(): Unit = {
    val binOp: BinaryOperator[Int] = BinaryOperator.minBy(Ordering[Int])
    assertEquals(10, binOp.apply(10, 20))
  }

  @Test def maxBy(): Unit = {
    val binOp: BinaryOperator[Int] = BinaryOperator.maxBy(Ordering[Int])
    assertEquals(20, binOp.apply(10, 20))
  }
}
