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
import java.lang.dectoflt.Tables._

import scala.annotation.tailrec

/** An implementation of the conversion from a decimal floating-point number
 *  to a binary floating-point number (Float or Double).
 *
 *  This code implements the Bellerophon algorithm, from
 *  "How to Read Floating Point Numbers Accurately" by William D. Clinger.
 *  It takes an input decimal number, represented by a significand `f` and
 *  an exponent `e` to form the value `f * 10^e`, and finds the best
 *  binary approximation.
 */
private[lang] object Bellerophon {
  private val TableSize = smallPowerOfTens.length

  private final val ExtendedSigBits = 64
  private final val ExtendedMaxSig = BigInteger.ZERO.setBit(ExtendedSigBits)

  def bellerophonFloat(f: BigInteger, e: Int): scala.Float =
    bellerophon(f, e, Binary32).toFloat

  def bellerophonDouble(f: BigInteger, e: Int): scala.Double =
    bellerophon(f, e, Binary64).toDouble

  /** An implementation of Bellerophon algorithm from
   *  "How to Read Floating Point Numbers Accurately" by William D. Clinger.
   *
   *  The Bellerophon algorithm efficiently finds the best binary
   *  approximation for a decimal number `f * 10^e`.
   *  While an iterative approach like AlgorithmM (defined in the paper)
   *  can find the best approximation by repeatedly using slow
   *  BigInteger arithmetic, Bellerophon provides non-iterative approach.
   *
   *  The basic idea is, instead of directly computing the best approximation
   *  for `f * 10^e` it uses higher-precision (p-bit (64-bit) significand)
   *  floating-point numbers to approximate `f` and `10^e` separately.
   *  Then multiplies these approximations and analyzes the
   *  error when multiplying and rounding the result to the target
   *  n-bit (n-bit significand) precision.
   *
   *  In some cases, the calculated approximation isn't the best, and in that case
   *  the algorithm falls back to AlgorithmR.
   *  This algorithm takes a good appoximation as a starting point,
   *  and iteratively finds the best approximation.
   *  Since the calculated approximation by Bellerophon is very close to the best
   *  it converges in just a few steps.
   *
   *  For extreme values that would overflow to infinity or underflow to a
   *  subnormal number or zero, neither the Bellerophon nor AlgorithmR does work.
   *  In these cases, the algorithm falls back to the original, slower AlgorithmM
   *  with the modification mentioned in section 8 of the paper.
   *  > With IEEE arithmetic, for example, a denormalized result
   *  > may be required. Denormalized results can be generate
   *  > by a modified form of AlgorithmM that terminates immediately
   *  > when the minimum exponent is reached.
   */
  private def bellerophon(f: BigInteger, e: Int, fmt: FloatingPointFormat): fmt.Repr = {
    /* Fast path: when f < 2^sigbits (2^53 for Double), and |e| < ceil(log5(2^sigbits)),
     * both f and 10^e can be represented perfectly in target precision without error.
     *
     * Since 10^e = 5^e * 2^e, the 2^e part can be absorbed into the binary exponent.
     * Therefore, the 10^e can be represented without error if 5^e fits within the significand.
     * This condition is expressed as 5^e < 2^sigbits <=> e < log5(2^sigbits).
     * As `e` is an integer, the condition is equivalent to e < ceil(log5(2^sigbits)).
     */
    val sigFitsInTargetFloat = f.compareTo(fmt.MaxSigBigInt) < 0
    if (sigFitsInTargetFloat && e >= 0 && e < fmt.CeilLog5OfMaxSig) {
      fmt.mul(fmt.fromLong(f.longValueExact()), fmt.powerOfTen(e))
    } else if (sigFitsInTargetFloat && e < 0 && -e < fmt.CeilLog5OfMaxSig) {
      fmt.div(fmt.fromLong(f.longValueExact()), fmt.powerOfTen(-e))
    } else {
      /* When f and 10^e cannot be represented exactly at the target precision.
       * Instead of computing it directly, bellerophon calculate the best approximation by:
       * 1. Approximate f and 10^e using 64-bit extended precision floats
       * 2. Multiply the approximations to get z = approx(f) * approx(10^e)
       * 3. Analyze if rounding z to target precision (single of double) gives the correct result
       * 4. If uncertain, fall back to slow path (Algorithm R) with the close approximation.
       *
       * The following error analysis is based on Lemma 2 and Corollary 3 from the paper:
       * when `a` and `b` are 64-bit approximations with errors δ1 and δ2,
       * the error δ of their product (a*b) is bounded by |δ| < 0.5 + 2(δ1 + δ2) ULPs,
       * if 0 < δ1 + δ2 < 4 or δ1 < 1 or δ2 < 1.
       *
       * Below, δ1 represents the error for the 64-bit approximation of the significand 'f',
       * and δ2 represents the error for the approximation of 10^e.
       *
       * Error bounds for δ1 (approximation of f):
       * - If f < 2^64, then δ1 = 0, as f is exactly representable with a 64-bit significand.
       * - If f >= 2^64, then δ1 <= 0.5 ULP, as we use the best approximation.
       *
       * Error bounds for δ2 (approximation of 10^e):
       * - If 0 <= e < table_size, then δ2 = 0, as the value is taken from a pre-computed
       *   table of exact powers of ten.
       * - Otherwise, 10^e is calculated as approx(10^x) * approx(10^y),
       *   where x is 0<=x<table_size, and x+y=e. The error for approx(10^x) is 0,
       *   while the error for approx(10^y) is <= 0.5 ULP (because it might be too big).
       *   Applying the Lemmsa, the resulting error δ2 is bounded by: |δ2| < 0.5 + 2*(0 + 0.5) = 1.5 ULPs.
       *
       * The total error for the final product, approx(f) * approx(10^e), is then bounded by
       * applying the formula |δ| < 0.5 + 2(δ1 + δ2) again for each case:
       *
       * - f < 2^64, 0 <= e < table_size:    δ1=0,    δ2=0.   |δ| < 0.5 ULPs.
       * - f < 2^64, e outside table range:  δ1=0,    δ2<1.5. |δ| < 0.5 + 2*(0 + 1.5)  = 3.5 ULPs.
       * - f >= 2^64, 0 <= e < table_size:   δ1<=0.5, δ2=0.   |δ| < 0.5 + 2*(0.5 + 0)  = 1.5 ULPs.
       * - f >= 2^64, e outside table range: δ1<=0.5, δ2<1.5. |δ| < 0.5 + 2*(0.5 + 1.5) = 4.5 ULPs.
       *
       * When rounding z to target precision, check the low-order bits.
       * If (low_bits ± error_bound) doesn't cross the rounding boundary (halfway),
       * the rounding direction is certain. Otherwise, fallback to slow path.
       */
      val slop = if (f.compareTo(ExtendedMaxSig) < 0) {
        if (e >= 0 && e < TableSize) 0
        else 3
      } else {
        if (e >= 0 && e < TableSize) 1
        else 4
      }
      multiplyAndTest(f, e, slop, fmt)
    }
  }

  private def multiplyAndTest(f: BigInteger, e: Int, slop: Int,
      fmt: FloatingPointFormat): fmt.Repr = {
    if (e < -LargeTableRange * TableSize || e > LargeTableRange * TableSize) {
      // Can't calculate the approximaton of 10^e from pre-computed power of tens, fallback to slow path
      algorithmM(f, e, fmt)
    } else {
      val x = FloatingPoint(f)
      val y = powerOfTen(e)
      val z = x.mul(y)
      if (z.doesOverflowOnRound(fmt)) {
        // bellerophon and algorithmR doesn't converge for overflow and underflow, fallback to slow path
        algorithmM(f, e, fmt)
      } else {
        // Least significant bits to be rounded
        val lowBits = z.f & ((1L << (ExtendedSigBits - fmt.SigBits)) - 1)
        val diff = Math.abs(lowBits - (1L << (ExtendedSigBits - fmt.SigBits - 1)))
        if (diff <= slop) {
          // The value is too close to a rounding boundary. Use a safe backup algorithm.
          algorithmR(f, e, z, fmt)
        } else {
          fmt.toIEEE754(z)
        }
      }
    }
  }

  /** Calculate the approximation of power of ten in 64-bit extended floating point. */
  private def powerOfTen(e: Int): FloatingPoint = {
    if (0 <= e && e < TableSize) {
      smallPowerOfTens(e)
    } else {
      val q = Math.floorDiv(e, TableSize)
      val r = e - q * TableSize
      smallPowerOfTens(r).mul(largePowerOfTens(q + LargeTableRange))
    }
  }

  private def algorithmM(f: BigInteger, e: Int,
      fmt: FloatingPointFormat): fmt.Repr = {
    val MinSig = BigInteger.ZERO.setBit(fmt.SigBits - 1)
    val MaxSig = BigInteger.ZERO.setBit(fmt.SigBits)

    val MaxExp = (1 << fmt.ExpBits - 1) - 1 - fmt.ExplicitSigBits
    val MinExp = -(1 << fmt.ExpBits - 1) + 2 - fmt.ExplicitSigBits // min exp for normal

    // Approximate decimal f*10^e by binary (u/v)*2^k
    // starting from k=0, iterate until u/v is normalized.
    @tailrec def loop(u: BigInteger, v: BigInteger, k: Int): fmt.Repr = {
      val divRem = u.divideAndRemainder(v)
      val q = divRem(0)
      val rem = divRem(1)

      val isBelowNormalizedRange = q.compareTo(MinSig) < 0
      val isAboveNormalizedRange = q.compareTo(MaxSig) >= 0
      val isNormalized = !(isBelowNormalizedRange || isAboveNormalizedRange)

      if (k == MinExp) {
        if (isNormalized) {
          val z = fmt.toIEEE754(FloatingPoint(q, k))
          roundByRemainder(q, rem, v, z)
        } else {
          underflow(q, v, rem)
        }
      } else if (k > MaxExp) {
        fmt.PositiveInfinity
      } else if (isNormalized) {
        val z = fmt.toIEEE754(FloatingPoint(q, k))
        roundByRemainder(q, rem, v, z)
      } else if (isBelowNormalizedRange) {
        loop(u.shiftLeft(1), v, k - 1)
      } else { // x > 2^m
        loop(u, v.shiftLeft(1), k + 1)
      }
    }

    def roundByRemainder(q: BigInteger, rem: BigInteger, v: BigInteger,
        z: fmt.Repr): fmt.Repr = {
      /* u = v*q + r (0 <= r < v)
       * u/v = q + r/v (0 <= r/v < 1)
       * therefore, q <= u/v < q+1
       * if u/v < q + 1/2 then round down
       * if u/v > q + 1/2 then round up
       *
       * u/v < q + 1/2 <=> q + r/v < q + 1/2 <=>
       * r/v < 1/2 <=> r < r/2 <=> 2r < v <=> r < v - r
       */
      val vMinusR = v.subtract(rem)
      val cmp = rem.compareTo(vMinusR)
      if (cmp < 0) z
      else if (cmp > 0) fmt.nextUp(z)
      else if (!q.testBit(0)) z // even
      else fmt.nextUp(z)
    }

    @inline
    def underflow(q: BigInteger, v: BigInteger, rem: BigInteger): fmt.Repr = {
      if (q.compareTo(MinSig) < 0) { // mantissa is small enough
        roundByRemainder(q, rem, v, fmt.reinterpretBits(q))
      } else {
        throw new AssertionError("Shouldn't reach here.")
      }
    }

    if (e < 0) loop(f, BigInteger.TEN.pow(-e), 0)
    else loop(f.multiply(BigInteger.TEN.pow(e)), BigInteger.ONE, 0)
  }

  /** Given an exact floating point number f * 10^e and
   *  a floating point number z0 (initial approximation),
   *  returns the best floating point approximation to f * 10^e.
   */
  private def algorithmR(f: BigInteger, e: Int, z0: FloatingPoint,
      fmt: FloatingPointFormat): fmt.Repr = {
    /* Given exact floating point number f * 10^e = (m+ε)*2^k, where ε is an rouding error.
     * AlgorithmR find positive integers x and y, such that, x/y = f*10^e/m*2^k and |ε| < 1/2.
     * x/y = ((m+ε)*2^k)/(m*2^k)=(m+ε)/m <=> ε=m(x-y)/y.
     * Therefore, |ε|<1/2 <=> |m(x-y)/y|<1/2 <=> |x-y|*2m < y.
     * Now, given D=x-y, and D2=2m*|D|, |ε|<1/2 means D2 < y.
     *
     * If |ε|>=1/2, test D<0 (x<y) or not, when x<y, current approximation (y=(m*2^k))
     * is too big, iterate with the previous floating point number.
     */
    @tailrec
    def loop(z: fmt.Repr): fmt.Repr = {
      val mAndExp = fmt.frexp(z)
      val m = mAndExp.f
      val exp = mAndExp.e

      val mantissa = BigInteger.valueOf(m)
      val (x, y) = {
        if (e >= 0) {
          if (exp >= 0)
            (f.multiply(BigInteger.TEN.pow(e)), mantissa.shiftLeft(exp))
          else
            (f.multiply(BigInteger.TEN.pow(e)).shiftLeft(-exp), mantissa)
        } else {
          if (exp >= 0)
            (f, mantissa.shiftLeft(exp).multiply(BigInteger.TEN.pow(-e)))
          else // e < 0 && exponent < 0
            (f.shiftLeft(-exp), mantissa.multiply(BigInteger.TEN.pow(-e)))
        }
      }
      val d = x.subtract(y) // D = x-y
      val d2 = mantissa.shiftLeft(1).multiply(d.abs()) // D2=2m*|D|
      val cmp = d2.compareTo(y)

      if (cmp < 0) { // D2 < y (|ε|<1/2), found the best approximation.
        /* If m is on the normalization boundary:
         * m = 2^(n-1) (where n is significand bits) and ε=-1/4,
         * there are two closest approximation.
         */
        if ((m == (1 << (fmt.SigBits - 1))) &&
            d2.shiftLeft(1).compareTo(y) > 0 && d.signum() < 0) {
          loop(fmt.nextDown(z))
        } else z
      } else if (cmp == 0) { // on boundary (x equals y), round to even
        if ((m & 1) == 0) {
          if (m == (1 << (fmt.SigBits - 1)) && d.signum() < 0)
            loop(fmt.nextDown(z))
          else z
        } else if (d.signum() < 0) {
          loop(fmt.nextDown(z))
        } else {
          loop(fmt.nextUp(z))
        }
      } else { // D2 > y (|ε|>1/2)
        if (d.signum() < 0) {
          loop(fmt.nextDown(z))
        } else {
          loop(fmt.nextUp(z))
        }
      }
    }
    loop(fmt.toIEEE754(z0))
  }
}
