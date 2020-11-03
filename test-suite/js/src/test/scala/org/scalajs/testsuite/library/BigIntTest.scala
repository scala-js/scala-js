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

package org.scalajs.testsuite.library

import org.junit.Assert._
import org.junit.Assume._
import org.junit.{BeforeClass, Test}
import org.scalajs.testsuite.utils.Platform

import scala.scalajs.js

class BigIntTest {

  @Test def apply(): Unit = {
    val fromString = js.BigInt("9007199254740992")
    assertEquals(fromString.toString(), "9007199254740992")

    val fromInt = js.BigInt(2147483647)
    assertEquals(fromInt.toString(), "2147483647")

    val fromDouble = js.BigInt(4294967295d)
    assertEquals(fromDouble.toString(), "4294967295")
  }

  @Test def asIntN(): Unit = {
    val x = js.BigInt.asIntN(8, js.BigInt("256"))
    assertEquals(x, js.BigInt("0"))
  }

  @Test def asUintN(): Unit = {
    val x = js.BigInt(-123)
    assertEquals(js.BigInt.asUintN(50, x), js.BigInt("1125899906842501"))
  }

  @Test def toLocaleString(): Unit = {
    val bi = js.BigInt("42123456789123456789")
    assertEquals(bi.toString(), "42123456789123456789")

    val result = bi
      .toLocaleString("de-DE",
          new js.BigInt.ToLocaleStringOptions {
        style = "currency"
        currency = "EUR"
      })

    // The exact return value is not specified. Just check the type.
    assertTrue(js.typeOf(result) == "string")
  }

  @Test def valueOf(): Unit = {
    val bi = js.BigInt("42123456789123456789")
    assertEquals(bi, bi.valueOf())
  }

  @Test def operators(): Unit = {
    val previousMaxSafe = js.BigInt("9007199254740991")
    assertEquals(previousMaxSafe, js.BigInt("9007199254740991"))

    val maxPlusOne = previousMaxSafe + js.BigInt(1)
    assertEquals(maxPlusOne, js.BigInt("9007199254740992"))

    val theFuture = previousMaxSafe + js.BigInt(2)
    assertEquals(theFuture, js.BigInt("9007199254740993"))

    val multi = previousMaxSafe * js.BigInt(2)
    assertEquals(multi, js.BigInt("18014398509481982"))

    val subtr = multi - js.BigInt(10)
    assertEquals(subtr, js.BigInt("18014398509481972"))

    val mod = multi % js.BigInt(10)
    assertEquals(mod, js.BigInt("2"))

    // TODO: Scala.js does not recongnize ** as operator
    //    val bigN = js.BigInt(2) ** js.BigInt(54)
    //    assertEquals(bigN, js.BigInt("18014398509481984"))

    val negative = -js.BigInt("18014398509481984")
    assertEquals(negative, js.BigInt("-18014398509481984"))

    val bitAnd = js.BigInt(123) & js.BigInt(31)
    assertEquals(bitAnd, js.BigInt("27"))

    val bitOr = js.BigInt(123) | js.BigInt(31)
    assertEquals(bitOr, js.BigInt("127"))

    val bitXor = js.BigInt(123) ^ js.BigInt(31)
    assertEquals(bitXor, js.BigInt("100"))

    val bitLeftShift = js.BigInt(123) << js.BigInt(31)
    assertEquals(bitLeftShift, js.BigInt("264140488704"))

    val bitRightShift = js.BigInt(12345678) >> js.BigInt(9)
    assertEquals(bitRightShift, js.BigInt("24112"))

    val bitNot = ~js.BigInt("42")
    assertEquals(bitNot, js.BigInt("-43"))
  }

  @Test def compare_with_bigint(): Unit = {
    val n = js.BigInt("42")
    assertTrue(n == js.BigInt("42"))
    assertTrue(n != js.BigInt("43"))
    assertTrue(n > js.BigInt("41"))
    assertTrue(n >= js.BigInt("41"))
    assertTrue(n < js.BigInt("43"))
    assertTrue(n <= js.BigInt("43"))
  }

}
