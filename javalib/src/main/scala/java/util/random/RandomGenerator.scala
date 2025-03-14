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

package java.util.random

import scala.annotation.tailrec

import java.util.ScalaOps._

trait RandomGenerator {
  // Comments starting with `// >` are cited from the JavaDoc.

  // Not implemented: all the methods using Streams

  // Not implemented, because
  // > The default implementation checks for the @Deprecated annotation.
  // def isDeprecated(): Boolean = ???

  def nextBoolean(): Boolean =
    nextInt() < 0 // is the sign bit 1?

  def nextBytes(bytes: Array[Byte]): Unit = {
    val len = bytes.length // implicit NPE
    var i = 0

    for (_ <- 0 until (len >> 3)) {
      var rnd = nextLong()
      for (_ <- 0 until 8) {
        bytes(i) = rnd.toByte
        rnd >>>= 8
        i += 1
      }
    }

    if (i != len) {
      var rnd = nextLong()
      while (i != len) {
        bytes(i) = rnd.toByte
        rnd >>>= 8
        i += 1
      }
    }
  }

  def nextFloat(): Float = {
    // > Uses the 24 high-order bits from a call to nextInt()
    val bits = nextInt() >>> (32 - 24)
    bits.toFloat * (1.0f / (1 << 24)) // lossless multiplication
  }

  def nextFloat(bound: Float): Float = {
    // false for NaN
    if (bound > 0 && bound != Float.PositiveInfinity)
      ensureBelowBound(nextFloatBoundedInternal(bound), bound)
    else
      throw new IllegalArgumentException(s"Illegal bound: $bound")
  }

  def nextFloat(origin: Float, bound: Float): Float = {
    // `origin < bound` is false if either input is NaN
    if (origin != Float.NegativeInfinity && origin < bound && bound != Float.PositiveInfinity) {
      val difference = bound - origin
      val result = if (difference != Float.PositiveInfinity) {
        // Easy case
        origin + nextFloatBoundedInternal(difference)
      } else {
        // Overflow: scale everything down by 0.5 then scale it back up by 2.0
        val halfOrigin = origin * 0.5f
        val halfBound = bound * 0.5f
        (halfOrigin + nextFloatBoundedInternal(halfBound - halfOrigin)) * 2.0f
      }

      ensureBelowBound(result, bound)
    } else {
      throw new IllegalArgumentException(s"Illegal bounds: [$origin, $bound)")
    }
  }

  @inline
  private def nextFloatBoundedInternal(bound: Float): Float =
    nextFloat() * bound

  @inline
  private def ensureBelowBound(value: Float, bound: Float): Float = {
    /* Based on documentation for Random.doubles to avoid issue #2144 and other
     * possible rounding up issues:
     * https://docs.oracle.com/javase/8/docs/api/java/util/Random.html#doubles-double-double-
     */
    if (value < bound) value
    else Math.nextDown(value)
  }

  def nextDouble(): Double = {
    // > Uses the 53 high-order bits from a call to nextLong()
    val bits = nextLong() >>> (64 - 53)
    bits.toDouble * (1.0 / (1L << 53)) // lossless multiplication
  }

  def nextDouble(bound: Double): Double = {
    // false for NaN
    if (bound > 0 && bound != Double.PositiveInfinity)
      ensureBelowBound(nextDoubleBoundedInternal(bound), bound)
    else
      throw new IllegalArgumentException(s"Illegal bound: $bound")
  }

  def nextDouble(origin: Double, bound: Double): Double = {
    // `origin < bound` is false if either input is NaN
    if (origin != Double.NegativeInfinity && origin < bound && bound != Double.PositiveInfinity) {
      val difference = bound - origin
      val result = if (difference != Double.PositiveInfinity) {
        // Easy case
        origin + nextDoubleBoundedInternal(difference)
      } else {
        // Overflow: scale everything down by 0.5 then scale it back up by 2.0
        val halfOrigin = origin * 0.5
        val halfBound = bound * 0.5
        (halfOrigin + nextDoubleBoundedInternal(halfBound - halfOrigin)) * 2.0
      }

      ensureBelowBound(result, bound)
    } else {
      throw new IllegalArgumentException(s"Illegal bounds: [$origin, $bound)")
    }
  }

  @inline
  private def nextDoubleBoundedInternal(bound: Double): Double =
    nextDouble() * bound

  @inline
  private def ensureBelowBound(value: Double, bound: Double): Double = {
    /* Based on documentation for Random.doubles to avoid issue #2144 and other
     * possible rounding up issues:
     * https://docs.oracle.com/javase/8/docs/api/java/util/Random.html#doubles-double-double-
     */
    if (value < bound) value
    else Math.nextDown(value)
  }

  def nextInt(): Int = {
    // > Uses the 32 high-order bits from a call to nextLong()
    (nextLong() >>> 32).toInt
  }

  /* The algorithms used in nextInt() with bounds were initially part of
   * ThreadLocalRandom. That implementation had been written by Doug Lea with
   * assistance from members of JCP JSR-166 Expert Group and released to the
   * public domain, as explained at
   * http://creativecommons.org/publicdomain/zero/1.0/
   */

  def nextInt(bound: Int): Int = {
    if (bound <= 0)
      throw new IllegalArgumentException(s"Illegal bound: $bound")

    nextIntBoundedInternal(bound)
  }

  def nextInt(origin: Int, bound: Int): Int = {
    if (bound <= origin)
      throw new IllegalArgumentException(s"Illegal bounds: [$origin, $bound)")

    val difference = bound - origin
    if (difference > 0 || difference == Int.MinValue) {
      /* Either the difference did not overflow, or it is the only power of 2
       * that overflows. In both cases, use the straightforward algorithm.
       * It works for `MinValue` because the code path for powers of 2
       * basically interprets the bound as unsigned.
       */
      origin + nextIntBoundedInternal(difference)
    } else {
      /* The interval size here is greater than Int.MaxValue,
       * so the loop will exit with a probability of at least 1/2.
       */
      @tailrec
      def loop(): Int = {
        val rnd = nextInt()
        if (rnd >= origin && rnd < bound)
          rnd
        else
          loop()
      }

      loop()
    }
  }

  private def nextIntBoundedInternal(bound: Int): Int = {
    // bound > 0 || bound == Int.MinValue

    if ((bound & -bound) == bound) { // i.e., bound is a power of 2
      // > If bound is a power of two then limiting is a simple masking operation.
      nextInt() & (bound - 1)
    } else {
      /* > Otherwise, the result is re-calculated by invoking nextInt() until
       * > the result is greater than or equal zero and less than bound.
       */

      /* Taken literally, that spec would lead to huge rejection rates for
       * small bounds.
       * Instead, we start from a random 31-bit (non-negative) int `rnd`, and
       * we compute `rnd % bound`.
       * In order to get a uniform distribution, we must reject and retry if
       * we get an `rnd` that is >= the largest int multiple of `bound`.
       */

      @tailrec
      def loop(): Int = {
        val rnd = nextInt() >>> 1
        val value = rnd % bound // candidate result

        // largest multiple of bound that is <= rnd
        val multiple = rnd - value

        // if multiple + bound overflows
        if (multiple + bound < 0) {
          /* then `multiple` is the largest multiple of bound, and
           * `rnd >= multiple`, so we must retry.
           */
          loop()
        } else {
          value
        }
      }

      loop()
    }
  }

  // The only abstract method of RandomGenerator
  def nextLong(): Long

  /* The algorithms for nextLong() with bounds are copy-pasted from the ones
   * for nextInt(), mutatis mutandis.
   */

  def nextLong(bound: Long): Long = {
    if (bound <= 0)
      throw new IllegalArgumentException(s"Illegal bound: $bound")

    nextLongBoundedInternal(bound)
  }

  def nextLong(origin: Long, bound: Long): Long = {
    if (bound <= origin)
      throw new IllegalArgumentException(s"Illegal bounds: [$origin, $bound)")

    val difference = bound - origin
    if (difference > 0 || difference == Long.MinValue) {
      /* Either the difference did not overflow, or it is the only power of 2
       * that overflows. In both cases, use the straightforward algorithm.
       * It works for `MinValue` because the code path for powers of 2
       * basically interprets the bound as unsigned.
       */
      origin + nextLongBoundedInternal(difference)
    } else {
      /* The interval size here is greater than Long.MaxValue,
       * so the loop will exit with a probability of at least 1/2.
       */
      @tailrec
      def loop(): Long = {
        val rnd = nextLong()
        if (rnd >= origin && rnd < bound)
          rnd
        else
          loop()
      }

      loop()
    }
  }

  private def nextLongBoundedInternal(bound: Long): Long = {
    // bound > 0 || bound == Long.MinValue

    if ((bound & -bound) == bound) { // i.e., bound is a power of 2
      // > If bound is a power of two then limiting is a simple masking operation.
      nextLong() & (bound - 1L)
    } else {
      /* > Otherwise, the result is re-calculated by invoking nextLong() until
       * > the result is greater than or equal zero and less than bound.
       */

      /* Taken literally, that spec would lead to huge rejection rates for
       * small bounds.
       * Instead, we start from a random 63-bit (non-negative) int `rnd`, and
       * we compute `rnd % bound`.
       * In order to get a uniform distribution, we must reject and retry if
       * we get an `rnd` that is >= the largest int multiple of `bound`.
       */

      @tailrec
      def loop(): Long = {
        val rnd = nextLong() >>> 1
        val value = rnd % bound // candidate result

        // largest multiple of bound that is <= rnd
        val multiple = rnd - value

        // if multiple + bound overflows
        if (multiple + bound < 0L) {
          /* then `multiple` is the largest multiple of bound, and
           * `rnd >= multiple`, so we must retry.
           */
          loop()
        } else {
          value
        }
      }

      loop()
    }
  }

  // Not implemented
  // def nextGaussian(): Double = ???
  // def nextGaussian(mean: Double, stddev: Double): Double = ???
  // def nextExponential(): Double = ???
}

object RandomGenerator { // scalastyle:ignore
  // Not implemented
  // def of(name: String): RandomGenerator = ???
  // def getDefault(): RandomGenerator = ???
}
