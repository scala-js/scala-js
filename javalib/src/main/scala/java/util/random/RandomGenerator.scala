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
import java.util.function.{Function, Supplier}

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

  @noinline
  def nextFloat(bound: Float): Float =
    nextFloatDouble(() => nextFloat(), bound)

  @noinline
  def nextFloat(origin: Float, bound: Float): Float =
    nextFloatDouble(() => nextFloat(), origin, bound)

  def nextDouble(): Double = {
    // > Uses the 53 high-order bits from a call to nextLong()
    val bits = nextLong() >>> (64 - 53)
    bits.toDouble * (1.0 / (1L << 53)) // lossless multiplication
  }

  @noinline
  def nextDouble(bound: Double): Double =
    nextFloatDouble(() => nextDouble(), bound)

  @noinline
  def nextDouble(origin: Double, bound: Double): Double =
    nextFloatDouble(() => nextDouble(), origin, bound)

  @inline
  private def nextFloatDouble[I, F](nextF: Supplier[F], bound: F)(
      implicit ops: IntFloatBits[I, F]): F = {
    import ops._

    // false for NaN
    if ((bound > fzero) && (bound !== finf))
      ensureBelowBound(nextF.get() * bound, bound)
    else
      throw new IllegalArgumentException(s"Illegal bound: $bound")
  }

  @inline
  private def nextFloatDouble[I, F](nextF: Supplier[F], origin: F, bound: F)(
      implicit ops: IntFloatBits[I, F]): F = {
    import ops._

    // `origin < bound` is false if either input is NaN
    if ((origin !== fneginf) && (origin < bound) && (bound !== finf)) {
      val generated = nextF.get()
      val difference = bound - origin
      val result = if (difference !== finf) {
        // Easy case
        origin + (generated * difference)
      } else {
        // Overflow: scale everything down by 0.5 then scale it back up by 2.0
        val halfOrigin = origin * fromFloat32(0.5f)
        val halfBound = bound * fromFloat32(0.5f)
        (halfOrigin + (generated * (halfBound - halfOrigin))) * fromFloat32(2.0f)
      }

      ensureBelowBound(result, bound)
    } else {
      throw new IllegalArgumentException(s"Illegal bounds: [$origin, $bound)")
    }
  }

  @inline
  private def ensureBelowBound[I, F](value: F, bound: F)(
      implicit ops: IntFloatBits[I, F]): F = {
    import ops._

    /* Based on documentation for Random.doubles to avoid issue #2144 and other
     * possible rounding up issues:
     * https://docs.oracle.com/javase/8/docs/api/java/util/Random.html#doubles-double-double-
     */
    if (value < bound) value
    else fnextDown(value)
  }

  def nextInt(): Int = {
    // > Uses the 32 high-order bits from a call to nextLong()
    (nextLong() >>> 32).toInt
  }

  @noinline
  def nextInt(bound: Int): Int =
    nextIntLong(nextIntBoundedInternal(_), bound)

  @noinline
  def nextInt(origin: Int, bound: Int): Int =
    nextIntLong(() => nextInt(), nextIntBoundedInternal(_), origin, bound)

  @noinline
  private def nextIntBoundedInternal(bound: Int): Int =
    nextIntLongBoundedInternal(() => nextInt(), () => nextInt(), bound)

  // The only abstract method of RandomGenerator
  def nextLong(): Long

  @noinline
  def nextLong(bound: Long): Long =
    nextIntLong(nextLongBoundedInternal(_), bound)

  @noinline
  def nextLong(origin: Long, bound: Long): Long =
    nextIntLong(() => nextLong(), nextLongBoundedInternal(_), origin, bound)

  @noinline
  private def nextLongBoundedInternal(bound: Long): Long =
    nextIntLongBoundedInternal(() => nextLong(), () => nextLong(), bound)

  /* The algorithms used in nextIntLong() with bounds were initially
   * implemented for Int's as part of ThreadLocalRandom. That implementation
   * had been written by Doug Lea with assistance from members of JCP JSR-166
   * Expert Group and released to the public domain, as explained at
   * http://creativecommons.org/publicdomain/zero/1.0/
   */

  @inline
  def nextIntLong[I, F](nextIBoundedInternal: Function[I, I], bound: I)(
      implicit ops: IntFloatBits[I, F]): I = {
    import ops._

    if (bound <= zero)
      throw new IllegalArgumentException(s"Illegal bound: $bound")

    nextIBoundedInternal(bound)
  }

  @inline
  def nextIntLong[I, F](nextI: Supplier[I], nextIBoundedInternal: Function[I, I],
      origin: I, bound: I)(
      implicit ops: IntFloatBits[I, F]): I = {
    import ops._

    if (bound <= origin)
      throw new IllegalArgumentException(s"Illegal bounds: [$origin, $bound)")

    val difference = bound - origin
    if ((difference > zero) || (difference === minInt)) {
      /* Either the difference did not overflow, or it is the only power of 2
       * that overflows. In both cases, use the straightforward algorithm.
       * It works for `minInt` because the code path for powers of 2
       * basically interprets the bound as unsigned.
       */
      origin + nextIBoundedInternal(difference)
    } else {
      /* The interval size here is greater than maxInt,
       * so the loop will exit with a probability of at least 1/2.
       */
      @inline
      @tailrec
      def loop(): I = {
        val rnd = nextI.get()
        if (rnd >= origin && rnd < bound)
          rnd
        else
          loop()
      }

      loop()
    }
  }

  /* This method requires nextI *twice*, because it needs it at 2 call sites.
   * If we only give it once, the typed closure does not get inlined, and the
   * result needs to be unboxed.
   *
   * TODO This would not be necessary if the optimizer always inlined "small"
   * typed closures (like it always inlines small methods), even if they are
   * called in more than one location.
   */
  @inline
  private def nextIntLongBoundedInternal[I, F](
      nextI1: Supplier[I], nextI2: Supplier[I], bound: I)(
      implicit ops: IntFloatBits[I, F]): I = {
    // bound > zero || bound == minInt

    import ops._

    if ((bound & -bound) === bound) { // i.e., bound is a power of 2
      // > If bound is a power of two then limiting is a simple masking operation.
      nextI1.get() & (bound - one)
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
       *
       * Since both `bound` and `rnd` are non-negative, we can use a signed or
       * unsigned remainder. We use the unsigned remainder because it is faster
       * with RuntimeLong.
       */

      @inline
      @tailrec
      def loop(): I = {
        val rnd = nextI2.get() >>> 1
        val value = remainderUnsigned(rnd, bound) // candidate result

        // largest multiple of bound that is <= rnd
        val multiple = rnd - value

        // if multiple + bound overflows
        if (multiple + bound < zero) {
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
