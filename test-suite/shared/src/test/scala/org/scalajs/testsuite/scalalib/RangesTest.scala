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

package org.scalajs.testsuite.scalalib

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import scala.collection.immutable.NumericRange
import scala.math.BigDecimal

import org.scalajs.testsuite.utils.Platform._

class RangesTest {

  @Test def iterableRangeLinks_Issue650(): Unit = {
    Iterable.range(1, 10)
  }

  @Test def iterableRangeAndSimpleRangeAreEqual(): Unit = {
    // Mostly to exercise more methods of ranges for dce warnings
    assertEquals((0 until 10).toList, Iterable.range(0, 10).toList)
  }

  @Test def numericRangeOverflow_Issue2407(): Unit = {
    val nr = NumericRange(Int.MinValue, Int.MaxValue, 1 << 23)
    assertEquals(Int.MinValue, nr.sum)
  }

  @Test def rangeForeach_Issue2409(): Unit = {
    val r = Int.MinValue to Int.MaxValue by (1 << 23)
    var i = 0
    r.foreach(_ => i += 1)
    assertEquals(512, i)
    assertEquals(512, r.length)
    assertEquals(Int.MinValue, r.sum)
  }

  @Test def rangeToString_Issue2412(): Unit = {
    assertEquals("inexact Range 1 to 10 by 2", (1 to 10 by 2).toString)
    assertEquals("empty Range 1 until 1 by 2", (1 until 1 by 2).toString)
    assertEquals("Range requires step", (BigDecimal(0.0) to BigDecimal(1.0)).toString)
    assertEquals("Range 0 to 1", (0 to 1).toString)
  }

  @Test def numericRangeToString_Issue2412(): Unit = {
    assertEquals("NumericRange 0 to 10 by 2", NumericRange.inclusive(0, 10, 2).toString())
    assertEquals("NumericRange 0 until 10 by 2", NumericRange(0, 10, 2).toString)
  }

  @Test def numericRangeWithArbitraryIntegral(): Unit = {
    // This is broken in Scala JVM up to (including) 2.11.8, 2.12.1 (SI-10086).
    assumeFalse("Assumed not on JVM for 2.12.1",
        executingInJVM && scalaVersion == "2.12.1")

    // Our custom integral type.
    case class A(v: Int)

    implicit object aIsIntegral extends scala.math.Integral[A] {
      def compare(x: A, y: A): Int = x.v.compare(y.v)
      def fromInt(x: Int): A = A(x)
      def minus(x: A, y: A): A = A(x.v - y.v)
      def negate(x: A): A = A(-x.v)
      def plus(x: A, y: A): A = A(x.v + y.v)
      def times(x: A, y: A): A = A(x.v * y.v)
      def quot(x: A, y: A): A = A(x.v / y.v)
      def rem(x: A, y: A): A = A(x.v % y.v)
      def toDouble(x: A): Double = x.v.toDouble
      def toFloat(x: A): Float = x.v.toFloat
      def toInt(x: A): Int = x.v
      def toLong(x: A): Long = x.v.toLong
      def parseString(str: String): Option[A] = Some(A(str.toInt))
    }

    val r = NumericRange(A(1), A(10), A(1))
    assertEquals(A(1), r.min)
    assertEquals(A(9), r.max)

    // Also test with custom ordering.
    assertEquals(A(9), r.min(aIsIntegral.reverse))
    assertEquals(A(1), r.max(aIsIntegral.reverse))
  }
}
