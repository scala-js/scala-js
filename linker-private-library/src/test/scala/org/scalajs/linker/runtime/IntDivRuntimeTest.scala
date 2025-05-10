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

package org.scalajs.linker.runtime

import org.junit.Assert._
import org.junit.Test

import java.lang.Integer.{toUnsignedLong => toULong}

import java.math.BigInteger
import java.util.SplittableRandom

import org.scalajs.linker.runtime.IntDivRuntime._

class IntDivRuntimeTest {

  /* We fuzz-test by comparing to the "obvious" implementations based on
   * BigIntegers.
   *
   * We use a SplittableRandom generator, because Random cannot generate all
   * Long values.
   *
   * Note: while the implementations come from libdivide, the tests are our own.
   */

  final val Seed = 909209754851418882L
  final val Rounds = 1024 * 1024

  @Test def test_libdivide_mullhi_u32(): Unit = {
    val gen = new SplittableRandom(Seed)

    for (round <- 1 to Rounds) {
      val x = gen.nextInt()
      val y = gen.nextInt()

      val expected = {
        BigInteger.valueOf(toULong(x))
          .multiply(BigInteger.valueOf(toULong(y)))
          .shiftRight(32)
          .intValue()
      }

      assertEquals(s"round $round, x = $x, y = $y", expected, libdivide_mullhi_u32(x, y))
    }
  }

  @Test def test_libdivide_mullhi_s32(): Unit = {
    val gen = new SplittableRandom(Seed)

    for (round <- 1 to Rounds) {
      val x = gen.nextInt()
      val y = gen.nextInt()

      val expected = {
        BigInteger.valueOf(x.toLong)
          .multiply(BigInteger.valueOf(y.toLong))
          .shiftRight(32)
          .intValue()
      }

      assertEquals(s"round $round, x = $x, y = $y", expected, libdivide_mullhi_s32(x, y))
    }
  }

  @Test def test_libdivide_mullhi_u64(): Unit = {
    val gen = new SplittableRandom(Seed)

    val mask = BigInteger.ONE.shiftLeft(64).subtract(BigInteger.ONE)

    def ulongToBigInteger(a: Long): BigInteger =
      BigInteger.valueOf(a).and(mask)

    for (round <- 1 to Rounds) {
      val x = gen.nextLong()
      val y = gen.nextLong()

      val expected = {
        ulongToBigInteger(x)
          .multiply(ulongToBigInteger(y))
          .shiftRight(64)
          .longValue()
      }

      assertEquals(s"round $round, x = $x, y = $y", expected, libdivide_mullhi_u64(x, y))
    }
  }

  @Test def test_libdivide_mullhi_s64(): Unit = {
    val gen = new SplittableRandom(Seed)

    for (round <- 1 to Rounds) {
      val x = gen.nextLong()
      val y = gen.nextLong()

      val expected = {
        BigInteger.valueOf(x)
          .multiply(BigInteger.valueOf(y))
          .shiftRight(64)
          .longValue()
      }

      assertEquals(s"round $round, x = $x, y = $y", expected, libdivide_mullhi_s64(x, y))
    }
  }

}
