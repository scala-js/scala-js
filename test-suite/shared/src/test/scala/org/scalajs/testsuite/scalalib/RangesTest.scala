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

  @Test def Iterable_range_should_not_emit_dce_warnings_issue_650(): Unit = {
    Iterable.range(1, 10)
  }

  @Test def Iterable_range_and_simple_range_should_be_equal(): Unit = {
    // Mostly to exercise more methods of ranges for dce warnings
    assertEquals((0 until 10).toList, Iterable.range(0, 10).toList)
  }

  @Test def NumericRange_overflow_issue_2407(): Unit = {
    assumeFalse("Assumed not on JVM for 2.10.X",
        executingInJVM && scalaVersion.startsWith("2.10."))
    assumeFalse("Assumed not on JVM for 2.11.{0-7}",
        executingInJVM && (0 to 7).map("2.11." + _).contains(scalaVersion))
    val nr = NumericRange(Int.MinValue, Int.MaxValue, 1 << 23)
    assertEquals(Int.MinValue, nr.sum)
  }

  @Test def Range_foreach_issue_2409(): Unit = {
    assumeFalse("Assumed not on JVM for 2.10.X",
        executingInJVM && scalaVersion.startsWith("2.10."))
    assumeFalse("Assumed not on JVM for 2.11.{0-7}",
        executingInJVM && (0 to 7).map("2.11." + _).contains(scalaVersion))
    val r = Int.MinValue to Int.MaxValue by (1 << 23)
    var i = 0
    r.foreach(_ => i += 1)
    assertEquals(512, i)
    assertEquals(512, r.length)
    assertEquals(Int.MinValue, r.sum)
  }

  @Test def Range_toString_issue_2412(): Unit = {
    if (scalaVersion.startsWith("2.10.") || scalaVersion.startsWith("2.11.")) {
      assertEquals("Range(1, 3, 5, 7, 9)", (1 to 10 by 2).toString)
      assertEquals("Range()", (1 until 1 by 2).toString)
      assertTrue(
          (BigDecimal(0.0) to BigDecimal(1.0)).toString.startsWith("scala.collection.immutable.Range$Partial"))
      assertEquals("Range(0, 1)", (0 to 1).toString)
    } else {
      assertEquals("inexact Range 1 to 10 by 2", (1 to 10 by 2).toString)
      assertEquals("empty Range 1 until 1 by 2", (1 until 1 by 2).toString)
      assertEquals("Range requires step", (BigDecimal(0.0) to BigDecimal(1.0)).toString)
      assertEquals("Range 0 to 1", (0 to 1).toString)
    }
  }

  @Test def NumericRange_toString_issue_2412(): Unit = {
    if (scalaVersion.startsWith("2.10.") || scalaVersion.startsWith("2.11.")) {
      assertEquals("NumericRange(0, 2, 4, 6, 8, 10)",
          NumericRange.inclusive(0, 10, 2).toString())
      assertEquals("NumericRange(0, 2, 4, 6, 8)",
          NumericRange(0, 10, 2).toString)
    } else {
      assertEquals("NumericRange 0 to 10 by 2",
          NumericRange.inclusive(0, 10, 2).toString())
      assertEquals("NumericRange 0 until 10 by 2",
          NumericRange(0, 10, 2).toString)
    }
  }

  @Test def NumericRange_with_arbitrary_integral(): Unit = {
    // This is broken in Scala JVM up to (including) 2.11.8, 2.12.1 (SI-10086).
    assumeFalse("Assumed not on JVM for 2.10.X",
        executingInJVM && scalaVersion.startsWith("2.10."))
    assumeFalse("Assumed not on JVM for 2.11.{0-8}",
        executingInJVM && (0 to 8).map("2.11." + _).contains(scalaVersion))
    assumeFalse("Assumed not on JVM for 2.12.{0-1}",
        executingInJVM && (0 to 1).map("2.12." + _).contains(scalaVersion))

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
