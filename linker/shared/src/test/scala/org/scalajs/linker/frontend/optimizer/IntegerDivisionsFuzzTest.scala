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
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import IntegerDivisions._

/** Fuzz tests for `IntegerDivisions`.
 *
 *  Test that the optimized trees evaluate to the same result as the original
 *  trees.
 */
class IntegerDivisionsFuzzTest {
  private implicit val noPosition: Position = Position.NoPosition

  /* For tests, we always use the configuration without RuntimeLong. This
   * configuration produces code that is easier to interpret.
   *
   * For the details of the RuntimeLong-specific generated code, we rely on the
   * test suite.
   */
  val integerDivisions = new IntegerDivisions(useRuntimeLong = false)

  @Test @noinline def fuzzTestInt_/(): Unit = fuzzTestIntOp(BinaryOp.Int_/)
  @Test @noinline def fuzzTestInt_%(): Unit = fuzzTestIntOp(BinaryOp.Int_%)
  @Test @noinline def fuzzTestInt_unsigned_/(): Unit = fuzzTestIntOp(BinaryOp.Int_unsigned_/)
  @Test @noinline def fuzzTestInt_unsigned_%(): Unit = fuzzTestIntOp(BinaryOp.Int_unsigned_%)

  @Test @noinline def fuzzTestLong_/(): Unit = fuzzTestLongOp(BinaryOp.Long_/)
  @Test @noinline def fuzzTestLong_%(): Unit = fuzzTestLongOp(BinaryOp.Long_%)
  @Test @noinline def fuzzTestLong_unsigned_/(): Unit = fuzzTestLongOp(BinaryOp.Long_unsigned_/)
  @Test @noinline def fuzzTestLong_unsigned_%(): Unit = fuzzTestLongOp(BinaryOp.Long_unsigned_%)

  private val FuzzTestDivisorsSeed = 4075846924374047794L
  private val FuzzTestNumeratorsSeed = 8312049509154985420L
  private val FuzzTestDivisorsRounds = 10000
  private val FuzzTestNumeratorsRounds = 100
  // There will be FuzzTestDivisorsRound * FuzzTestNumeratorsRound inner iterations!

  private def fuzzTestIntOp(op: BinaryOp.Code): Unit = {
    def testDivisor(divisor: Int): Unit =
      fuzzTestIntDivisor(op, divisor)

    testDivisor(0)
    for (small <- 1 to 13) {
      testDivisor(small)
      testDivisor(-small)
    }
    for (k <- 4 until 32) { // up to 2^3 are tested in small
      testDivisor(1 << k)
      testDivisor(-(1 << k))
    }
    testDivisor(100)
    testDivisor(1000)
    testDivisor(Int.MaxValue - 1)
    testDivisor(Int.MaxValue)
    testDivisor(Int.MinValue)
    testDivisor(Int.MinValue + 1)

    /* Random divisors. We generate a distribution with
     * - a uniform choice between positive and negative,
     * - a uniform distribution of bit lengths of the absolute value, and
     * - within a given bit length, a uniform distribution of bits.
     *
     * If we don't specifically aim for uniform bit lengths, the probability is
     * heavily skewed towards very large divisors, for which the result of the
     * division is very close to 0, and for which small variations in the
     * numerator have not impact. Small divisors are more interesting.
     */
    val rnd = new java.util.SplittableRandom(FuzzTestDivisorsSeed)
    for (_ <- 0 until FuzzTestDivisorsRounds)
      testDivisor(rnd.nextInt() >> rnd.nextInt(32))
  }

  private def fuzzTestLongOp(op: BinaryOp.Code): Unit = {
    def testDivisor(divisor: Long): Unit =
      fuzzTestLongDivisor(op, divisor)

    testDivisor(0L)
    for (small <- 1 to 13) {
      testDivisor(small.toLong)
      testDivisor(-small.toLong)
    }
    for (k <- 4 until 64) { // up to 2^3 are tested in small
      testDivisor(1L << k)
      testDivisor(-(1L << k))
    }
    testDivisor(100L)
    testDivisor(1000L)
    testDivisor(Long.MaxValue - 1L)
    testDivisor(Long.MaxValue)
    testDivisor(Long.MinValue)
    testDivisor(Long.MinValue + 1L)

    // See the comment in fuzzTestIntOp about the distribution of divisors
    val rnd = new java.util.SplittableRandom(FuzzTestDivisorsSeed)
    for (_ <- 0 until FuzzTestDivisorsRounds)
      testDivisor(rnd.nextLong() >> rnd.nextInt(64))
  }

  private def fuzzTestIntDivisor(op: BinaryOp.Code, divisor: Int): Unit = {
    val reference = BinaryOp(op, VarRef(NumeratorArgName)(IntType), IntLiteral(divisor))
    val optimized = integerDivisions.makeOptimizedDivision(op, divisor)

    if (divisor == 0) {
      val UnaryOp(UnaryOp.Throw, _) = optimized // meant as an assertion
    } else {
      def test(numValue: Int): Unit =
        assertEvalEquals(reference, optimized, IntLiteral(numValue))

      test(0)
      for (small <- 1 to 13) {
        test(small)
        test(-small)
      }
      test(divisor - 1)
      test(divisor)
      test(divisor + 1)
      test(-divisor + 1)
      test(-divisor)
      test(-divisor - 1)
      test(Int.MaxValue - 1)
      test(Int.MaxValue)
      test(Int.MinValue)
      test(Int.MinValue + 1)

      /* Random numerators. We use the same distribution as for the divisors,
       * though it is less important here. Still, we want to sometimes get
       * numerators that are within a few orders of magnitude as the divisors;
       * not just very large numerators.
       */
      val rnd = new java.util.SplittableRandom(FuzzTestNumeratorsSeed)
      for (_ <- 0 until FuzzTestNumeratorsRounds)
        test(rnd.nextInt() >> rnd.nextInt(32))
    }
  }

  private def fuzzTestLongDivisor(op: BinaryOp.Code, divisor: Long): Unit = {
    val reference = BinaryOp(op, VarRef(NumeratorArgName)(LongType), LongLiteral(divisor))
    val optimized = integerDivisions.makeOptimizedDivision(op, divisor)

    if (divisor == 0L) {
      val UnaryOp(UnaryOp.Throw, _) = optimized // meant as an assertion
    } else {
      def test(numValue: Long): Unit =
        assertEvalEquals(reference, optimized, LongLiteral(numValue))

      test(0L)
      for (small <- 1 to 13) {
        test(small.toLong)
        test(-small.toLong)
      }
      test(divisor - 1L)
      test(divisor)
      test(divisor + 1L)
      test(-divisor + 1L)
      test(-divisor)
      test(-divisor - 1L)
      test(Long.MaxValue - 1L)
      test(Long.MaxValue)
      test(Long.MinValue)
      test(Long.MinValue + 1L)

      // See the comment in fuzzTestIntDivisor about the distribution of numerators
      val rnd = new java.util.SplittableRandom(FuzzTestNumeratorsSeed)
      for (_ <- 0 until FuzzTestNumeratorsRounds)
        test(rnd.nextLong() >> rnd.nextInt(64))
    }
  }

  private def assertEvalEquals(reference: Tree, optimized: Tree, numValue: Literal): Unit = {
    val referenceResult = ElementaryInterpreter.eval(reference, NumeratorArgName -> numValue)
    val optimizedResult = ElementaryInterpreter.eval(optimized, NumeratorArgName -> numValue)

    if (optimizedResult != referenceResult) {
      fail(
          s"Fuzz test failed\n" +
          s"Expected ${referenceResult.show} but got ${optimizedResult.show}\n" +
          s"Numerator: ${numValue.show}\n" +
          s"Reference code was:\n${reference.show}\n" +
          s"Optimized code was:\n${optimized.show}\n")
    }
  }
}
