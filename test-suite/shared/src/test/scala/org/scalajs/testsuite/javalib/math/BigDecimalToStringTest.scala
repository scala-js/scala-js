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

import java.math.{BigInteger, BigDecimal}

import org.junit.Test
import org.junit.Assert._

class BigDecimalToStringTest {

  @Test def testToStringWithCornerCaseScales(): Unit = {
    val bigIntOne = BigInteger.valueOf(1)

    assertEquals("1", new BigDecimal(bigIntOne, 0).toString())

    assertEquals("0.01", new BigDecimal(bigIntOne, 2).toString())
    assertEquals("0.000001", new BigDecimal(bigIntOne, 6).toString())
    assertEquals("1E-7", new BigDecimal(bigIntOne, 7).toString())
    assertEquals("1E-2147483647", new BigDecimal(bigIntOne, 2147483647).toString())

    assertEquals("1E+1", new BigDecimal(bigIntOne, -1).toString())
    assertEquals("1E+2", new BigDecimal(bigIntOne, -2).toString())
    assertEquals("1E+15", new BigDecimal(bigIntOne, -15).toString())
    assertEquals("1E+2147483647", new BigDecimal(bigIntOne, -2147483647).toString())
    assertEquals("1E+2147483648", new BigDecimal(bigIntOne, -2147483648).toString()) // #4088

    val bigInt123 = BigInteger.valueOf(123)

    assertEquals("123", new BigDecimal(bigInt123, 0).toString())

    assertEquals("1.23", new BigDecimal(bigInt123, 2).toString())
    assertEquals("0.000123", new BigDecimal(bigInt123, 6).toString())
    assertEquals("0.00000123", new BigDecimal(bigInt123, 8).toString())
    assertEquals("1.23E-7", new BigDecimal(bigInt123, 9).toString())
    assertEquals("1.23E-2147483645", new BigDecimal(bigInt123, 2147483647).toString())

    assertEquals("1.23E+3", new BigDecimal(bigInt123, -1).toString())
    assertEquals("1.23E+4", new BigDecimal(bigInt123, -2).toString())
    assertEquals("1.23E+17", new BigDecimal(bigInt123, -15).toString())
    assertEquals("1.23E+2147483649", new BigDecimal(bigInt123, -2147483647).toString()) // #4088
    assertEquals("1.23E+2147483650", new BigDecimal(bigInt123, -2147483648).toString()) // #4088
  }

}
