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

package org.scalajs.testsuite.javalib.math

import java.math._

import org.junit.Test
import org.junit.Assert._

class BigDecimalTest {

  @noinline
  private def assertBDExactEquals(expectedUnscaledValue: BigInteger,
      expectedScale: Int, actual: BigDecimal): Unit = {
    assertEquals((expectedUnscaledValue, expectedScale), (actual.unscaledValue(), actual.scale()))
  }

  @Test def valueOfLong(): Unit = {
    assertBDExactEquals(BigInteger.valueOf(3L), 0, BigDecimal.valueOf(3L))
    assertBDExactEquals(BigInteger.valueOf(999999999L), 0, BigDecimal.valueOf(999999999L))
    assertBDExactEquals(BigInteger.valueOf(9999999999L), 0, BigDecimal.valueOf(9999999999L))
    assertBDExactEquals(BigInteger.valueOf(-999999999L), 0, BigDecimal.valueOf(-999999999L))
    assertBDExactEquals(BigInteger.valueOf(-9999999999L), 0, BigDecimal.valueOf(-9999999999L))
  }

  @Test def ctorString(): Unit = {
    assertBDExactEquals(BigInteger.valueOf(3L), 0, new BigDecimal("3"))
    assertBDExactEquals(BigInteger.valueOf(99L), 0, new BigDecimal("99"))
    assertBDExactEquals(BigInteger.valueOf(999999999L), 0, new BigDecimal("999999999"))
    assertBDExactEquals(BigInteger.valueOf(9999999999L), 0, new BigDecimal("9999999999"))
    assertBDExactEquals(BigInteger.valueOf(-99L), 0, new BigDecimal("-99"))
    assertBDExactEquals(BigInteger.valueOf(-999999999L), 0, new BigDecimal("-999999999"))
    assertBDExactEquals(BigInteger.valueOf(-9999999999L), 0, new BigDecimal("-9999999999"))

    assertBDExactEquals(BigInteger.valueOf(99L), 1, new BigDecimal("9.9"))
    assertBDExactEquals(BigInteger.valueOf(9999L), 2, new BigDecimal("99.99"))
    assertBDExactEquals(BigInteger.valueOf(999999L), 3, new BigDecimal("999.999"))
    assertBDExactEquals(BigInteger.valueOf(99999999L), 4, new BigDecimal("9999.9999"))
  }

  @Test def ctorDouble(): Unit = {
    assertBDExactEquals(new BigInteger("0"), 0, new BigDecimal(0.0))
    assertBDExactEquals(new BigInteger("15"), 1, new BigDecimal(1.5))
    assertBDExactEquals(
        new BigInteger("329999999999999982236431605997495353221893310546875"), 50,
        new BigDecimal(3.3))
    assertBDExactEquals(
        new BigInteger("999899999999999948840923025272786617279052734375"), 46,
        new BigDecimal(99.99))
    assertBDExactEquals(
        new BigInteger("9999999900000000707223080098628997802734375"), 39,
        new BigDecimal(9999.9999))
    assertBDExactEquals(
        new BigInteger("9999999999999998509883880615234375"), 26,
        new BigDecimal(99999999.99999999))
    assertBDExactEquals(new BigInteger("1000000000"), 0, new BigDecimal(999999999.999999999))
    assertBDExactEquals(new BigInteger("10000000000"), 0, new BigDecimal(9999999999.9999999999))
    assertBDExactEquals(new BigInteger("10000000000000000000"), 0, new BigDecimal(1e19))

    assertBDExactEquals(new BigInteger("0"), 0, new BigDecimal(-0.0))
    assertBDExactEquals(new BigInteger("-15"), 1, new BigDecimal(-1.5))
    assertBDExactEquals(
        new BigInteger("-329999999999999982236431605997495353221893310546875"), 50,
        new BigDecimal(-3.3))
    assertBDExactEquals(
        new BigInteger("-999899999999999948840923025272786617279052734375"), 46,
        new BigDecimal(-99.99))
    assertBDExactEquals(
        new BigInteger("-9999999900000000707223080098628997802734375"), 39,
        new BigDecimal(-9999.9999))
    assertBDExactEquals(
        new BigInteger("-9999999999999998509883880615234375"), 26,
        new BigDecimal(-99999999.99999999))
    assertBDExactEquals(new BigInteger("-1000000000"), 0, new BigDecimal(-999999999.999999999))
    assertBDExactEquals(new BigInteger("-10000000000"), 0, new BigDecimal(-9999999999.9999999999))
    assertBDExactEquals(new BigInteger("-10000000000000000000"), 0, new BigDecimal(-1e19)) // #5381
  }
}
