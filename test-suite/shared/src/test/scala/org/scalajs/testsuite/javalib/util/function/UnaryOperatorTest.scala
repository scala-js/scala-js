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

import java.util.function.UnaryOperator

import org.junit.Assert._
import org.junit.Test

class UnaryOperatorTest {
  import UnaryOperatorTest._

  @Test def identity(): Unit = {
    val unaryOperatorString: UnaryOperator[String] = UnaryOperator.identity()
    assertEquals("scala", unaryOperatorString.apply("scala"))
  }

  @Test def createAndApply(): Unit = {
    val double: UnaryOperator[Int] = makeUnaryOperator(_ * 2)
    assertEquals(20, double.apply(10))
    assertEquals(20, double.apply(10))
  }
}

object UnaryOperatorTest {
  private def makeUnaryOperator[T](f: T => T): UnaryOperator[T] = {
    new UnaryOperator[T] {
      def apply(t: T): T = f(t)
    }
  }
}
