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

package org.scalajs.linker.frontend.optimizer

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir.Position

import IntegerDivisions._

/** Tests for the magic data computations in `IntegerDivisions`. */
class IntegerDivisionsMagicTest {
  // The magic computations take a Position for assertion error messages.
  private implicit val noPosition: Position = Position.NoPosition

  @Test def testComputeSignedMagicInt(): Unit = {
    def test(divisor: Int, expectedM: Int, expectedAdd: Int, expectedShift: Int): Unit = {
      assertEquals(
          divisor.toString(),
          MagicData(expectedM, expectedAdd, expectedShift),
          computeSignedMagic(Math.abs(divisor), divisor < 0))
    }

    // From section 10-5

    test(-7, 0x6db6db6d, -1, 2)

    // Test cases from Hacker's Delight, 10-14, Table 10-1, Signed column

    test(-5, 0x99999999, 0, 1)
    test(-3, 0x55555555, -1, 1)
    test(3, 0x55555556, 0, 0)
    test(5, 0x66666667, 0, 1)
    test(6, 0x2aaaaaab, 0, 0)
    test(7, 0x92492493, 1, 2)
    test(9, 0x38e38e39, 0, 1)
    test(10, 0x66666667, 0, 2)
    test(11, 0x2e8ba2e9, 0, 1)
    test(12, 0x2aaaaaab, 0, 1)
    test(25, 0x51eb851f, 0, 3)
    test(125, 0x10624dd3, 0, 3)
    test(625, 0x68db8bad, 0, 8)

    // For completeness, but we never use the results of this function for powers of 2
    for (k <- 1 to 30) {
      test(-(1 << k), 0x7fffffff, -1, k - 1)
      test(1 << k, 0x80000001, 1, k - 1)
    }
  }

  @Test def testComputeUnsignedMagicInt(): Unit = {
    def test(divisor: Int, expectedM: Int, expectedAdd: Int, expectedShift: Int): Unit = {
      assertEquals(
          divisor.toString(),
          MagicData(expectedM, expectedAdd, expectedShift),
          computeUnsignedMagic(divisor))
    }

    // Test cases from Hacker's Delight, 10-14, Table 10-1, Unsigned column

    test(3, 0xaaaaaaab, 0, 1)
    test(5, 0xcccccccd, 0, 2)
    test(6, 0xaaaaaaab, 0, 2)
    test(7, 0x24924925, 1, 3)
    test(9, 0x38e38e39, 0, 1)
    test(10, 0xcccccccd, 0, 3)
    test(11, 0xba2e8ba3, 0, 3)
    test(12, 0xaaaaaaab, 0, 3)
    test(25, 0x51eb851f, 0, 3)
    test(125, 0x10624dd3, 0, 3)
    test(625, 0xd1b71759, 0, 9)

    // For completeness, but we never use the results of this function for powers of 2
    for (k <- 1 to 31)
      test(1 << k, 1 << (32 - k), 0, 0)
  }

  @Test def testComputeSignedMagicLong(): Unit = {
    def test(divisor: Long, expectedM: Long, expectedAdd: Int, expectedShift: Int): Unit = {
      assertEquals(
          divisor.toString(),
          MagicData(expectedM, expectedAdd, expectedShift),
          computeSignedMagic(Math.abs(divisor), divisor < 0L))
    }

    // Test cases from Hacker's Delight, 10-14, Table 10-2, Signed column

    test(-5L, 0x9999999999999999L, 0, 1)
    test(-3L, 0x5555555555555555L, -1, 1)
    test(3L, 0x5555555555555556L, 0, 0)
    test(5L, 0x6666666666666667L, 0, 1)
    test(6L, 0x2aaaaaaaaaaaaaabL, 0, 0)
    test(7L, 0x4924924924924925L, 0, 1)
    test(9L, 0x1c71c71c71c71c72L, 0, 0)
    test(10L, 0x6666666666666667L, 0, 2)
    test(11L, 0x2e8ba2e8ba2e8ba3L, 0, 1)
    test(12L, 0x2aaaaaaaaaaaaaabL, 0, 1)
    test(25L, 0xa3d70a3d70a3d70bL, 1, 4)
    test(125L, 0x20c49ba5e353f7cfL, 0, 4)
    test(625L, 0x346dc5d63886594bL, 0, 7)

    // For completeness, but we never use the results of this function for powers of 2
    for (k <- 1 to 62) {
      test(-(1L << k), 0x7fffffffffffffffL, -1, k - 1)
      test(1L << k, 0x8000000000000001L, 1, k - 1)
    }
  }

  @Test def testComputeUnsignedMagicLong(): Unit = {
    def test(divisor: Long, expectedM: Long, expectedAdd: Int, expectedShift: Int): Unit = {
      assertEquals(
          divisor.toString(),
          MagicData(expectedM, expectedAdd, expectedShift),
          computeUnsignedMagic(divisor))
    }

    // Test cases from Hacker's Delight, 10-14, Table 10-2, Unsigned column

    test(3L, 0xaaaaaaaaaaaaaaabL, 0, 1)
    test(5L, 0xcccccccccccccccdL, 0, 2)
    test(6L, 0xaaaaaaaaaaaaaaabL, 0, 2)
    test(7L, 0x2492492492492493L, 1, 3)
    test(9L, 0xe38e38e38e38e38fL, 0, 3)
    test(10L, 0xcccccccccccccccdL, 0, 3)
    test(11L, 0x2e8ba2e8ba2e8ba3L, 0, 1)
    test(12L, 0xaaaaaaaaaaaaaaabL, 0, 3)
    test(25L, 0x47ae147ae147ae15L, 1, 5)
    test(125L, 0x0624dd2f1a9fbe77L, 1, 7)
    test(625L, 0x346dc5d63886594bL, 0, 7)

    // For completeness, but we never use the results of this function for powers of 2
    for (k <- 1 to 63)
      test(1L << k, 1L << (64 - k), 0, 0)
  }
}
