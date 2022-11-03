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

import org.junit.Assert._
import org.junit.Test

class OperatorsTest {
  import OperatorsTest._

  @Test def operatorsWithoutAnnotation(): Unit = {
    val refVal = 5
    val obj = new js.Object().asInstanceOf[js.Dynamic]

    obj.updateDynamic("valueOf")(() => refVal)

    obj.updateDynamic("+")((x: Int) => fail("+ was called as a method"))
    obj.updateDynamic("unary_-")(() => fail("unary_- was called as a method"))

    obj.updateDynamic("-")((x: Int, y: String) => s"$refVal - $x $y")
    obj.updateDynamic("unary_!")((y: String) => s"$refVal ! $y")

    obj.updateDynamic("**")((x: Double) => s"$refVal ** $x")

    val ops = obj.asInstanceOf[OperatorsWithoutAnnotation]

    assertEquals(7, ops + 2)
    assertEquals(-5, -ops)

    assertEquals("5 - 2 foo", ops.-(2, "foo"))
    assertEquals("5 ! bar", ops.unary_!("bar"))

    assertEquals("5 ** 3", ops ** 3.0)
  }
}

object OperatorsTest {
  @js.native
  trait OperatorsWithoutAnnotation extends js.Any {
    // with the correct operator signature -> defaults to operator
    def +(x: Int): Int = js.native
    def unary_- : Int = js.native // scalastyle:ignore

    // with an incorrect operator signature -> defaults to method
    def -(x: Int, y: String): String = js.native
    def unary_!(y: String): String = js.native

    // JavaScript operators that were not part of the initial spec in Scala.js 1.0.0 -> defaults to method
    def **(x: Double): String = js.native
  }
}
