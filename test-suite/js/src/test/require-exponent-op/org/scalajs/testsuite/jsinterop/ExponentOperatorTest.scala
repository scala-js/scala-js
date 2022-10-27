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

package org.scalajs.testsuite.jsinterop

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

import org.scalajs.testsuite.utils.Platform._

class ExponentOperatorTest {
  import ExponentOperatorTest._

  @Test def exponentOpOnDouble(): Unit = {
    val lhs = 3.asInstanceOf[ObjWithExponentOperator]
    assertEquals(81, lhs ** 4.0, 0.0)

    val lhsDyn = lhs.asInstanceOf[js.Dynamic]
    val rhsDyn = 4.0.asInstanceOf[js.Dynamic]
    assertEquals(81, lhsDyn ** rhsDyn)
  }

  @Test def exponentOpOnBigInt(): Unit = {
    assumeTrue("requires bigint support", jsBigInts)

    val lhs = js.BigInt(121)
    val rhs = js.BigInt(15)
    assertEquals(js.BigInt("17449402268886407318558803753801"), lhs ** rhs)
  }
}

object ExponentOperatorTest {
  @js.native
  trait ObjWithExponentOperator extends js.Any {
    @JSOperator
    def **(x: Double): Double = js.native
  }
}
