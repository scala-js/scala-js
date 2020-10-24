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

import java.util.function.Function

import org.junit.Assert._
import org.junit.Test

class FunctionTest {
  import FunctionTest._

  @Test def identity(): Unit = {
    assertEquals(10, identityFunc(10))
  }

  @Test def createAndApply(): Unit = {
    assertEquals(2, doubleFunc(1))
  }

  @Test def compose(): Unit = {
    // i.e. before
    assertEquals(21, incFunc.compose(doubleFunc)(10))
  }

  @Test def andThen(): Unit = {
    // i.e. after
    assertEquals(22, incFunc.andThen(doubleFunc)(10))
  }

  @Test def identityComposeAndThen(): Unit = {
    // i.e. (self + 1) * 2
    val combined = identityFunc.andThen(doubleFunc).compose(incFunc)
    assertEquals(42, combined(20))
  }
}

object FunctionTest {
  private val identityFunc: Function[Int, Int] = Function.identity[Int]()
  private val doubleFunc: Function[Int, Int] = makeFunction(x => x * 2)
  private val incFunc: Function[Int, Int] = makeFunction(x => x + 1)

  private[this] def makeFunction[T, R](f: T => R): Function[T, R] = {
    new Function[T, R] {
      def apply(t: T): R = f(t)
    }
  }
}
