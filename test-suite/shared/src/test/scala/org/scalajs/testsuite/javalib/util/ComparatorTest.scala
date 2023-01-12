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

package org.scalajs.testsuite.javalib.util

import java.{util => ju}
import java.util.{function => juf}

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform

class ComparatorTest {

  @Test def reversed(): Unit = {
    class IntComparator extends ju.Comparator[Int] {
      def compare(a: Int, b: Int): Int = {
        /* Using Int.MinValue makes sure that Comparator.reversed() does not
         * use the naive implementation of negating the original comparator's
         * result.
         */
        if (a == b) 0
        else if (a < b) Int.MinValue
        else Int.MaxValue
      }
    }

    val comparator = new IntComparator
    val reversed = comparator.reversed()

    assertEquals(0, reversed.compare(5, 5))
    assertTrue(reversed.compare(3, 1) < 0)
    assertTrue(reversed.compare(6, 8) > 0)
  }

  @Test def reverseOrder(): Unit = {
    val cmp = ju.Comparator.reverseOrder[String]

    assertEquals(0, cmp.compare("a", "a"))
    assertTrue(cmp.compare("b", "a") < 0)
    assertTrue(cmp.compare("a", "b") > 0)
  }

  @Test def naturalOrder(): Unit = {
    val cmp = ju.Comparator.naturalOrder[String]

    assertEquals(0, cmp.compare("a", "a"))
    assertTrue(cmp.compare("b", "a") > 0)
    assertTrue(cmp.compare("a", "b") < 0)
  }

  @Test def nullsFirst(): Unit = {
    val cmp = ju.Comparator.nullsFirst(ju.Comparator.naturalOrder[String])

    assertEquals(0, cmp.compare("a", "a"))
    assertEquals(0, cmp.compare(null, null))
    assertTrue(cmp.compare(null, "a") < 0)
    assertTrue(cmp.compare("a", null) > 0)
  }

  @Test def nullsFirstNull(): Unit = {
    val cmp = ju.Comparator.nullsFirst(null)

    assertEquals(0, cmp.compare("a", "b"))
    assertEquals(0, cmp.compare(null, null))
    assertTrue(cmp.compare(null, "a") < 0)
    assertTrue(cmp.compare("a", null) > 0)
  }

  @Test def nullsLast(): Unit = {
    val cmp = ju.Comparator.nullsLast(ju.Comparator.naturalOrder[String])
    assertEquals(0, cmp.compare("a", "a"))
    assertEquals(0, cmp.compare(null, null))
    assertTrue(cmp.compare(null, "a") > 0)
    assertTrue(cmp.compare("a", null) < 0)
  }

  @Test def nullsLastNull(): Unit = {
    val cmp = ju.Comparator.nullsLast(null)
    assertEquals(0, cmp.compare("a", "b"))
    assertEquals(0, cmp.compare(null, null))
    assertTrue(cmp.compare(null, "a") > 0)
    assertTrue(cmp.compare("a", null) < 0)
  }

  @Test def comparing(): Unit = {
    val cmp = ju.Comparator.comparing[String, String](
        (_.substring(1)): juf.Function[String, String],
        ju.Comparator.reverseOrder[String])
    assertEquals(0, cmp.compare("ac", "bc"))
    assertTrue(cmp.compare("ba", "ab") > 0)
    assertTrue(cmp.compare("ab", "ba") < 0)

    assertThrowsNPEIfCompliant(ju.Comparator.comparing[String, String](
        null, ju.Comparator.reverseOrder[String]))
    assertThrowsNPEIfCompliant(ju.Comparator.comparing(
        (_.substring(1)): juf.Function[String, String], null))
  }

  @Test def comparingComparable(): Unit = {
    val cmp = ju.Comparator.comparing[String, String](
        (_.substring(1)): juf.Function[String, String])
    assertEquals(0, cmp.compare("ac", "bc"))
    assertTrue(cmp.compare("ba", "ab") < 0)
    assertTrue(cmp.compare("ab", "ba") > 0)

    assertThrowsNPEIfCompliant(
        ju.Comparator.comparing[String, String](null))
  }

  @Test def comparingInt(): Unit = {
    val cmp = ju.Comparator.comparingInt((_: String).length)
    assertEquals(0, cmp.compare("a", "b"))
    assertTrue(cmp.compare("", "a") < 0)
    assertTrue(cmp.compare("ab", "") > 0)

    assertThrowsNPEIfCompliant(ju.Comparator.comparingInt(null))
  }

  @Test def comparingLong(): Unit = {
    val cmp = ju.Comparator.comparingLong((_: String).length.toLong)
    assertEquals(0, cmp.compare("a", "b"))
    assertTrue(cmp.compare("", "a") < 0)
    assertTrue(cmp.compare("ab", "") > 0)

    assertThrowsNPEIfCompliant(ju.Comparator.comparingLong(null))
  }

  @Test def comparingDouble(): Unit = {
    val cmp = ju.Comparator.comparingDouble((_: String).length.toDouble / 2)
    assertEquals(0, cmp.compare("a", "b"))
    assertTrue(cmp.compare("", "a") < 0)
    assertTrue(cmp.compare("ab", "") > 0)

    assertThrowsNPEIfCompliant(ju.Comparator.comparingDouble(null))
  }

  @Test def thenComparingComparator(): Unit = {
    val base = ju.Comparator.comparingInt((x: (Int, Int)) => x._1)

    val cmp = base.thenComparing(
        ju.Comparator.comparingInt((x: (Int, Int)) => x._2))
    assertEquals(0, cmp.compare((1, 2), (1, 2)))
    assertTrue(cmp.compare((1, 1), (1, 2)) < 0)
    assertTrue(cmp.compare((1, 2), (1, 1)) > 0)
    assertTrue(cmp.compare((1, 2), (2, 1)) < 0)
    assertTrue(cmp.compare((2, 1), (1, 2)) > 0)

    assertThrowsNPEIfCompliant(base.thenComparing(null: ju.Comparator[(Int, Int)]))
  }

  @Test def thenComparingExtractorComparator(): Unit = {
    val base = ju.Comparator.comparingInt((x: (Int, String)) => x._1)

    val cmp = base.thenComparing[String](
        ((x: (Int, String)) => x._2): juf.Function[(Int, String), String],
        ju.Comparator.reverseOrder[String])
    assertEquals(0, cmp.compare((1, "a"), (1, "a")))
    assertTrue(cmp.compare((1, "a"), (1, "b")) > 0)
    assertTrue(cmp.compare((1, "b"), (1, "a")) < 0)
    assertTrue(cmp.compare((1, "b"), (2, "a")) < 0)
    assertTrue(cmp.compare((2, "a"), (1, "b")) > 0)

    assertThrowsNPEIfCompliant(base.thenComparing[String](
        null, ju.Comparator.reverseOrder[String]))
    assertThrowsNPEIfCompliant(base.thenComparing[String](
        ((_: (Int, String))._2): juf.Function[(Int, String), String], null))
  }

  @Test def thenComparingExtractor(): Unit = {
    val base = ju.Comparator.comparingInt((x: (Int, String)) => x._1)

    val cmp = base.thenComparing[String](
        ((x: (Int, String)) => x._2): juf.Function[(Int, String), String])
    assertEquals(0, cmp.compare((1, "a"), (1, "a")))
    assertTrue(cmp.compare((1, "a"), (1, "b")) < 0)
    assertTrue(cmp.compare((1, "b"), (1, "a")) > 0)
    assertTrue(cmp.compare((1, "b"), (2, "a")) < 0)
    assertTrue(cmp.compare((2, "a"), (1, "b")) > 0)

    assertThrowsNPEIfCompliant(base.thenComparing[String](
        null: juf.Function[(Int, String), String]))
  }

  @Test def thenComparingInt(): Unit = {
    val base = ju.Comparator.comparingInt((x: (Int, Int)) => x._1)

    val cmp = base.thenComparingInt((x: (Int, Int)) => x._2)
    assertEquals(0, cmp.compare((1, 2), (1, 2)))
    assertTrue(cmp.compare((1, 1), (1, 2)) < 0)
    assertTrue(cmp.compare((1, 2), (1, 1)) > 0)
    assertTrue(cmp.compare((1, 2), (2, 1)) < 0)
    assertTrue(cmp.compare((2, 1), (1, 2)) > 0)

    assertThrowsNPEIfCompliant(base.thenComparingInt(null))
  }

  @Test def thenComparingLong(): Unit = {
    val base = ju.Comparator.comparingInt((x: (Int, Int)) => x._1)

    val cmp = base.thenComparingLong((x: (Int, Int)) => x._2.toLong)
    assertEquals(0, cmp.compare((1, 2), (1, 2)))
    assertTrue(cmp.compare((1, 1), (1, 2)) < 0)
    assertTrue(cmp.compare((1, 2), (1, 1)) > 0)
    assertTrue(cmp.compare((1, 2), (2, 1)) < 0)
    assertTrue(cmp.compare((2, 1), (1, 2)) > 0)

    assertThrowsNPEIfCompliant(base.thenComparingLong(null))
  }

  @Test def thenComparingDouble(): Unit = {
    val base = ju.Comparator.comparingInt((x: (Int, Int)) => x._1)

    val cmp = base.thenComparingDouble((x: (Int, Int)) => x._2.toDouble / 2)
    assertEquals(0, cmp.compare((1, 2), (1, 2)))
    assertTrue(cmp.compare((1, 1), (1, 2)) < 0)
    assertTrue(cmp.compare((1, 2), (1, 1)) > 0)
    assertTrue(cmp.compare((1, 2), (2, 1)) < 0)
    assertTrue(cmp.compare((2, 1), (1, 2)) > 0)

    assertThrowsNPEIfCompliant(base.thenComparingDouble(null))
  }
}
