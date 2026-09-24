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

package java.lang.dectoflt

import java.math.BigInteger
import java.lang.{Long => JLong}

/** DIY floating point number with 64-bit significant bits */
private[dectoflt] class FloatingPoint private (val f: Long, val e: Int) {
  import FloatingPoint._

  override def toString(): String =
    s"${this.f} * 2^${this.e}"

  /** Returns a normalized product */
  def mul(other: FloatingPoint): FloatingPoint = {
    val low = f * other.f
    val high = Math.unsignedMultiplyHigh(f, other.f)

    val nlz = JLong.numberOfLeadingZeros(high)
    val excessBits = 64 - nlz
    val shiftedF =
      if (nlz == 0) high
      else (high << nlz) | (low >>> excessBits)
    val newE = (this.e + other.e + excessBits)

    val remainder = low & (-1L >>> nlz)
    val halfway = 1L << (excessBits - 1)

    val newF = {
      if (JLong.unsigned_>(remainder, halfway)) // roundup
        shiftedF + 1
      else if (JLong.unsigned_<(remainder, halfway))
        shiftedF
      else if ((shiftedF & 1) == 1) // tie, rounddown is odd
        shiftedF + 1 // round to even
      else
        shiftedF
    }
    if (newF == 0) {
      // denormalized by carry, in case shiftedF = 111..111
      // (111...111 + 1) >> 1
      new FloatingPoint(Long.MinValue, newE + 1)
    } else {
      new FloatingPoint(newF, newE)
    }
  }

  def doesOverflowOnRound(fmt: FloatingPointFormat): Boolean = {
    val rounded = roundNormal(fmt)
    val sig = rounded.f
    val exponent = rounded.e

    // Remove the leading implicit bit
    val encodedSig: Long = sig - (1L << (fmt.ExplicitSigBits))

    // Adjust the exponent for exponent bias and mantissa shift
    val encodedExp: Int = exponent + ((1 << (fmt.ExpBits - 1)) - 1) + fmt.ExplicitSigBits

    encodedExp <= 0 || encodedExp >= ((1 << fmt.ExpBits) - 1) ||
      encodedSig <= 0 || encodedSig >= (1 << fmt.ExplicitSigBits)
  }

  /** Round the 64-bit significand to target#SigBits bits with half-to-even. */
  def roundNormal(target: FloatingPointFormat): FloatingPoint = {
    val excess = SigBits - target.SigBits // > 0
    val halfway = 1L << (excess - 1)
    val shiftedF = f >>> excess
    val remainder = f & ((1L << excess) - 1) // > 0
    val normalizedE = e + excess

    val normalizedF = if (remainder > halfway) { // roundup
      shiftedF + 1
    } else if (remainder < halfway) { // rounddown
      shiftedF
    } else if ((shiftedF & 1) == 1) { // tie, rownddown (shiftedF) is odd (1 at LSB)
      shiftedF + 1 // roundup is even
    } else { // tie, rounddown is even
      shiftedF
    }
    if (normalizedF > target.MaxSig) {
      new FloatingPoint(normalizedF >> 1, normalizedE + 1)
    } else {
      new FloatingPoint(normalizedF, normalizedE)
    }
  }
}

private[dectoflt] object FloatingPoint {
  private final val SigBits = 64

  def apply(f: BigInteger): FloatingPoint =
    normalize(f, 0)

  def apply(f: BigInteger, e: Int): FloatingPoint =
    normalize(f, e)

  /** Create a FloatingPoint from normalized f and e. */
  def normalized(f: Long, e: Int): FloatingPoint =
    new FloatingPoint(f, e)

  /** Normalize the given floating point number z = f * 2^e
   *
   *  A floating point number is normalized iff β^(n-1) <= f < β^n (β=2, n=64)
   */
  private def normalize(f: BigInteger, e: Int): FloatingPoint = {
    val shift = f.bitLength() - SigBits
    if (shift == 0) {
      new FloatingPoint(f.longValue(), e)
    } else if (shift < 0) {
      // Significand is smaller. Shift left to normalize.
      new FloatingPoint(f.shiftLeft(-shift).longValue(), e + shift)
    } else {
      // shift > 0, significand is too big, round and shift right using round half to even.
      val shiftedF = f.shiftRight(shift)
      val remainder = f.and(BigInteger.ZERO.setBit(shift).subtract(BigInteger.ONE))
      val halfway = BigInteger.ZERO.setBit(shift - 1)
      val cmpToHalfway = remainder.compareTo(halfway)
      val normalizedF = {
        if (cmpToHalfway > 0) { // roundup
          shiftedF.add(BigInteger.ONE)
        } else if (cmpToHalfway < 0) { // rounddown
          shiftedF
        } else if (shiftedF.testBit(0)) { // tie, rownddown (shiftedF) is odd (1 at LSB)
          shiftedF.add(BigInteger.ONE) // roundup is even
        } else { // tie, rounddown is even
          shiftedF
        }
      }
      val normalizedE = e + shift

      // Check for overflow from rounding
      if (normalizedF.bitLength() > SigBits) {
        new FloatingPoint(normalizedF.shiftRight(1).longValue(), normalizedE + 1)
      } else {
        new FloatingPoint(normalizedF.longValue(), normalizedE)
      }
    }
  }
}
