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

import java.util.function.{Function, BiFunction}

import org.junit.Assert._
import org.junit.Test

class BiFunctionTest {
  import BiFunctionTest._

  @Test def createAndApply(): Unit =
    assertEquals(3, addBiFunc(1, 2))

  @Test def andThen(): Unit =
    assertEquals(4, addBiFunc.andThen(incFunc)(1, 2))
}

object BiFunctionTest {
  private val addBiFunc: BiFunction[Int, Int, Int] = {
    new BiFunction[Int, Int, Int] {
      def apply(t: Int, u: Int): Int = t + u
    }
  }

  private val incFunc: Function[Int, Int] = {
    new Function[Int, Int] {
      def apply(t: Int): Int = t + 1
    }
  }
}
