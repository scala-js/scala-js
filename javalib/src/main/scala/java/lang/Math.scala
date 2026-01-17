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

package java
package lang

import scala.scalajs.js
import js.Dynamic.{global => g}

import scala.scalajs.LinkingInfo
import scala.scalajs.LinkingInfo.ESVersion

object Math {
  final val E = 2.718281828459045
  final val PI = 3.141592653589793

  @inline private def assumingES6: scala.Boolean =
    LinkingInfo.esVersion >= ESVersion.ES2015

  @inline def abs(a: scala.Int): scala.Int = {
    // Hacker's Delight, Section 2-4
    val sign = a >> 31
    (a ^ sign) - sign
  }

  // RuntimeLong intrinsic
  @inline def abs(a: scala.Long): scala.Long = {
    val sign = a >> 63
    (a ^ sign) - sign
  }

  // Wasm intrinsics
  @inline def abs(a: scala.Float): scala.Float = js.Math.abs(a).toFloat
  @inline def abs(a: scala.Double): scala.Double = js.Math.abs(a)

  @inline def max(a: scala.Int, b: scala.Int): scala.Int = if (a > b) a else b
  @inline def max(a: scala.Long, b: scala.Long): scala.Long = if (a > b) a else b

  // Wasm intrinsics
  @inline def max(a: scala.Float, b: scala.Float): scala.Float = js.Math.max(a, b).toFloat
  @inline def max(a: scala.Double, b: scala.Double): scala.Double = js.Math.max(a, b)

  @inline def min(a: scala.Int, b: scala.Int): scala.Int = if (a < b) a else b
  @inline def min(a: scala.Long, b: scala.Long): scala.Long = if (a < b) a else b

  // Wasm intrinsics
  @inline def min(a: scala.Float, b: scala.Float): scala.Float = js.Math.min(a, b).toFloat
  @inline def min(a: scala.Double, b: scala.Double): scala.Double = js.Math.min(a, b)

  // Wasm intrinsics
  @inline def ceil(a: scala.Double): scala.Double = js.Math.ceil(a)
  @inline def floor(a: scala.Double): scala.Double = js.Math.floor(a)

  // Wasm intrinsic
  def rint(a: scala.Double): scala.Double = {
    /* We apply the technique described in Section II of
     *   Claude-Pierre Jeannerod, Jean-Michel Muller, Paul Zimmermann.
     *   On various ways to split a floating-point number.
     *   ARITH 2018 - 25th IEEE Symposium on Computer Arithmetic,
     *   Jun 2018, Amherst (MA), United States.
     *   pp.53-60, 10.1109/ARITH.2018.8464793. hal-01774587v2
     * available at
     *   https://hal.inria.fr/hal-01774587v2/document
     * with β = 2, p = 53, and C = 2^(p-1) = 2^52.
     *
     * That is only valid for values x <= 2^52. Fortunately, all values that
     * are >= 2^52 are already integers, so we can return them as is.
     *
     * We cannot use "the 1.5 trick" with C = 2^(p-1) + 2^(p-2) to handle
     * negative numbers, because that would further reduce the range of valid
     * `x` to maximum 2^51, although we actually need it up to 2^52. Therefore,
     * we have a separate branch for negative numbers. This also allows to
     * gracefully deal with the fact that we need to return -0.0 for values in
     * the range [-0.5,-0.0).
     */
    val C = 4503599627370496.0 // 2^52
    if (a > 0) {
      if (a >= C) a
      else (C + a) - C
    } else if (a < 0) {
      // do not "optimize" as `C - (C - a)`, as it would return +0.0 where it should return -0.0
      if (a <= -C) a
      else -((C - a) - C)
    } else {
      // Handling zeroes here avoids the need to distinguish +0.0 from -0.0
      a // 0.0, -0.0 and NaN
    }
  }

  @inline def round(a: scala.Float): scala.Int = js.Math.round(a).toInt
  @inline def round(a: scala.Double): scala.Long = js.Math.round(a).toLong

  // Wasm intrinsic
  @inline def sqrt(a: scala.Double): scala.Double = js.Math.sqrt(a)

  @inline def pow(a: scala.Double, b: scala.Double): scala.Double = js.Math.pow(a, b)

  @inline def exp(a: scala.Double): scala.Double = js.Math.exp(a)
  @inline def log(a: scala.Double): scala.Double = js.Math.log(a)

  @inline def log10(a: scala.Double): scala.Double = {
    if (assumingES6 || !Utils.isUndefined(g.Math.log10))
      js.Math.log10(a)
    else
      log(a) / 2.302585092994046
  }

  @inline def log1p(a: scala.Double): scala.Double = {
    if (assumingES6 || !Utils.isUndefined(g.Math.log1p))
      js.Math.log1p(a)
    else if (a == 0.0) a
    else log(a + 1)
  }

  @inline def sin(a: scala.Double): scala.Double = js.Math.sin(a)
  @inline def cos(a: scala.Double): scala.Double = js.Math.cos(a)
  @inline def tan(a: scala.Double): scala.Double = js.Math.tan(a)
  @inline def asin(a: scala.Double): scala.Double = js.Math.asin(a)
  @inline def acos(a: scala.Double): scala.Double = js.Math.acos(a)
  @inline def atan(a: scala.Double): scala.Double = js.Math.atan(a)
  @inline def atan2(y: scala.Double, x: scala.Double): scala.Double = js.Math.atan2(y, x)

  @inline def random(): scala.Double = js.Math.random()

  @inline def toDegrees(a: scala.Double): scala.Double = a * 180.0 / PI
  @inline def toRadians(a: scala.Double): scala.Double = a / 180.0 * PI

  @inline def signum(a: scala.Double): scala.Double = {
    if (a > 0) 1.0
    else if (a < 0) -1.0
    else a
  }

  @inline def signum(a: scala.Float): scala.Float = {
    if (a > 0) 1.0f
    else if (a < 0) -1.0f
    else a
  }

  def cbrt(a: scala.Double): scala.Double = {
    if (assumingES6 || !Utils.isUndefined(g.Math.cbrt)) {
      js.Math.cbrt(a)
    } else {
      if (a == 0 || Double.isNaN(a) || Double.isInfinite(a)) {
        a
      } else {
        val sign = if (a < 0.0) -1.0 else 1.0
        val value = sign * a

        // Initial Approximation
        var x = 0.0
        var xi = pow(value, 0.3333333333333333)

        // Halley's Method (http://metamerist.com/cbrt/cbrt.htm)
        while (abs(x - xi) >= 1e-16) {
          x = xi
          val x3 = js.Math.pow(x, 3)
          val x3Plusa = x3 + value
          xi = x * (x3Plusa + value) / (x3Plusa + x3)
        }
        sign * xi
      }
    }
  }

  @noinline
  def nextUp(a: scala.Double): scala.Double =
    nextUpGeneric(a)

  @noinline
  def nextUp(a: scala.Float): scala.Float =
    nextUpGeneric(a)

  @inline
  private def nextUpGeneric[I, F](a: F)(implicit ops: IntFloatBits[I, F]): F = {
    import ops._

    val bits = floatToBits(a)

    /* In most cases, we need to increment the integer value of the bits if it
     * is positive, and decrement it if it is negative, i.e.,
     *   bits + (if (bits >= 0) 1 else -1)
     * We do this in a branchless way with:
     */
    val newBits = bits + ((bits >> (bitSize - 1)) | one)

    /* Among the special cases, the above formula also works for +0.0,
     * MaxValue and NegativeInfinity.
     *
     * It does *not* work for:
     *
     * - the "largest" positive NaN -> -0.0
     * - -0.0                       -> the largest positive NaN
     * - PositiveInfinity           -> the smallest positive NaN
     * - the smallest negative NaN  -> NegativeInfinity
     *
     * We will detect these cases using `bits` and `newBits`.
     *
     * The last 3 give non-finite bit patterns. The first two are the only cases
     * of nextUp overall where the sign bit flips between `bits` and `newBits`.
     * We can turn this flipped bit into a non-finite bit pattern with
     *   magic = (bits ^ newBits) >> ebits
     *
     * So if `newBits | magic` has a finite bit pattern (which we can
     * efficiently test, even with RuntimeLong), we have a correct result.
     *
     * Otherwise, we know the input was -0.0, PositiveInfinity, any NaN value,
     * or (as an unfortunate side effect of the test) MaxValue. For these
     * cases, a clever combination of two floating point additions returns the
     * correct result.
     */

    if (isFiniteBitPattern(newBits | ((bits ^ newBits) >> ebits))) {
      // fast path
      floatFromBits(newBits)
    } else {
      // -fzero -> fminSubnormal ; NaN -> NaN ; finf -> finf ; fmax -> finf
      (a + a) + fminSubnormal
    }
  }

  @noinline
  def nextDown(a: scala.Double): scala.Double =
    nextDownGeneric(a)

  @noinline
  def nextDown(a: scala.Float): scala.Float =
    nextDownGeneric(a)

  @inline
  private def nextDownGeneric[I, F](a: F)(implicit ops: IntFloatBits[I, F]): F = {
    import ops._

    // Symmetric to nextUpGeneric

    val bits = floatToBits(a)
    val newBits = bits - ((bits >> (bitSize - 1)) | one)
    if (isFiniteBitPattern(newBits | ((bits ^ newBits) >> ebits))) {
      // fast path
      floatFromBits(newBits)
    } else {
      // fzero -> -fminSubnormal ; NaN -> NaN ; -finf -> -finf ; -fmax -> -finf
      (a + a) - fminSubnormal
    }
  }

  @noinline
  def nextAfter(a: scala.Double, b: scala.Double): scala.Double =
    nextAfterGeneric(a, b)

  @noinline
  def nextAfter(a: scala.Float, b: scala.Double): scala.Float =
    nextAfterGeneric(a, b)

  @inline
  def nextAfterGeneric[I, F](a: F, b: scala.Double)(implicit ops: IntFloatBits[I, F]): F = {
    import ops._

    val aDouble = toDouble(a)
    if (b > aDouble)
      nextUpGeneric(a) // inlined
    else if (b < aDouble)
      nextDownGeneric(a) // inlined
    else if (aDouble != aDouble)
      fnan
    else
      fromDoubleRound(b)
  }

  @noinline
  def scalb(d: scala.Double, scaleFactor: scala.Int): scala.Double =
    scalbGeneric(d, scaleFactor)

  @noinline
  def scalb(f: scala.Float, scaleFactor: scala.Int): scala.Float =
    scalbGeneric(f, scaleFactor)

  @inline
  def scalbGeneric[I, F](x: F, scaleFactor: scala.Int)(implicit ops: IntFloatBits[I, F]): F = {
    // scalastyle:off return

    import ops._

    // Constants
    val minEForSubnormalResult = -mbits - 1 // 1 additional bit for rounding up to MinPositiveValue
    val subnormalExpAdjustment = -minEForSubnormalResult + 1 // adjust range so that minE becomes 1
    val twoPowMinusAdjustment = fone / intToFloat(one << subnormalExpAdjustment) // exact division

    @inline def isNormalExponent(e: Int): scala.Boolean =
      inRangeIncl(e, 1, emask - 1)

    val bits = floatToBits(x)
    val e = exponentOf(bits)

    // First decode as if it is a normal input (fast path)
    var newE = e + scaleFactor
    val signAndMantissa = newIntBox(bits & (signBit | mmask))

    if (!isNormalExponent(e)) {
      // Special or subnormal input
      if (isSpecialBitPattern(bits)) {
        // All specials are returned as is (also acts as fast path for zeros)
        return x
      }
      // Normalize by shifting the leading 1 just out of the mantissa bits, and adjust newE to compensate
      val clz = ops.clz(bits & mmask)
      newE -= (clz - (bitSize - mbits))
      signAndMantissa() = (bits & signBit) | ((bits << (clz - (bitSize - mbits - 1))) & mmask)
    }

    /* newE may have overflown or underflown if `scaleFactor` is very large or
     * very small. That can only happen when the whole operation would overflow
     * or underflow. Moreover, if it does overflow or underflow, it cannot wrap
     * all the way around to a valid exponent (between minEForSubnormalResult
     * and maxE). Therefore, at this point, newE is in the range of valid
     * exponents if and only if `scalb` does not overflow nor underflow.
     */

    @inline def makeResult(finalNewE: Int, finalSignAndMantissa: I): F =
      floatFromBits((fromUnsignedInt32(finalNewE) << mbits) | finalSignAndMantissa)

    if (isNormalExponent(newE)) {
      // Normal result (fast path)
      makeResult(newE, signAndMantissa())
    } else if (inRangeIncl(newE, minEForSubnormalResult, 0)) {
      // Subnormal result - make a normal adjusted result, then multiply to correct and accurately round
      makeResult(newE + subnormalExpAdjustment, signAndMantissa()) * twoPowMinusAdjustment
    } else {
      /* Overflow (if scaleFactor >= 0) or underflow (if scaleFactor < 0).
       * - copy sign bit from `bits`.
       * - if the sign bit of scaleFactor is 1, we need 0 for the exponent;
       *   if it is 0, we need 0x7ff; use some bit magic to get that without branches.
       * - set mantissa bits to 0.
       */
      makeResult(((~scaleFactor) >> 31) & emask, signBit & bits)
    }

    // scalastyle:on return
  }

  @inline private def inRangeIncl(x: Int, min: Int, max: Int): scala.Boolean =
    Integer.unsigned_<=(x - min, max - min)

  @noinline
  def ulp(a: scala.Double): scala.Double =
    ulpGeneric(a)

  @noinline
  def ulp(a: scala.Float): scala.Float =
    ulpGeneric(a)

  @inline
  private def ulpGeneric[I, F](a: F)(implicit ops: IntFloatBits[I, F]): F = {
    import ops._

    val bits = floatToBits(a)
    val e = exponentOf(bits)
    if (inRangeIncl(e, mbits + 1, emask - 1)) {
      // fast path: normal result
      floatFromBits(fromUnsignedInt32(e - mbits) << mbits)
    } else if (e == 0) {
      // fast path for 0: subnormal input -> min subnormal result
      fminSubnormal
    } else if (e != emask) { // 0 < e < (mbits + 1), given the previous tests, but faster
      // normal input but subnormal result
      floatFromBits(one << (e - 1))
    } else {
      // NaN or Infinity
      floatFromBits(bits & ~signBit)
    }
  }

  def hypot(a: scala.Double, b: scala.Double): scala.Double = {
    if (assumingES6 || !Utils.isUndefined(g.Math.hypot)) {
      js.Math.hypot(a, b)
    } else {
      // http://en.wikipedia.org/wiki/Hypot#Implementation
      if (abs(a) == scala.Double.PositiveInfinity || abs(b) == scala.Double.PositiveInfinity) {
        scala.Double.PositiveInfinity
      } else if (Double.isNaN(a) || Double.isNaN(b)) {
        scala.Double.NaN
      } else if (a == 0 && b == 0) {
        0.0
      } else {
        // To Avoid Overflow and UnderFlow
        // calculate |x| * sqrt(1 - (y/x)^2) instead of sqrt(x^2 + y^2)
        val x = abs(a)
        val y = abs(b)
        val m = max(x, y)
        val t = min(x, y) / m
        m * sqrt(1 + t * t)
      }
    }
  }

  def expm1(a: scala.Double): scala.Double = {
    if (assumingES6 || !Utils.isUndefined(g.Math.expm1)) {
      js.Math.expm1(a)
    } else {
      // https://github.com/ghewgill/picomath/blob/master/javascript/expm1.js
      if (a == 0 || Double.isNaN(a))
        a
      // Power Series http://en.wikipedia.org/wiki/Power_series
      // for small values of a, exp(a) = 1 + a + (a*a)/2
      else if (abs(a) < 1e-5)
        a + 0.5 * a * a
      else
        exp(a) - 1.0
    }
  }

  def sinh(a: scala.Double): scala.Double = {
    if (assumingES6 || !Utils.isUndefined(g.Math.sinh)) {
      js.Math.sinh(a)
    } else {
      if (Double.isNaN(a) || a == 0.0 || abs(a) == scala.Double.PositiveInfinity) a
      else (exp(a) - exp(-a)) / 2.0
    }
  }

  def cosh(a: scala.Double): scala.Double = {
    if (assumingES6 || !Utils.isUndefined(g.Math.cosh)) {
      js.Math.cosh(a)
    } else {
      if (Double.isNaN(a))
        a
      else if (a == 0.0)
        1.0
      else if (abs(a) == scala.Double.PositiveInfinity)
        scala.Double.PositiveInfinity
      else
        (exp(a) + exp(-a)) / 2.0
    }
  }

  def tanh(a: scala.Double): scala.Double = {
    if (assumingES6 || !Utils.isUndefined(g.Math.tanh)) {
      js.Math.tanh(a)
    } else {
      if (Double.isNaN(a) || a == 0.0) {
        a
      } else if (abs(a) == scala.Double.PositiveInfinity) {
        signum(a)
      } else {
        // sinh(a) / cosh(a) =
        // 1 - 2 * (exp(-a)/ (exp(-a) + exp (a)))
        val expma = exp(-a)
        if (expma == scala.Double.PositiveInfinity) { // Infinity / Infinity
          -1.0
        } else {
          val expa = exp(a)
          val ret = expma / (expa + expma)
          1.0 - (2.0 * ret)
        }
      }
    }
  }

  private def intOverflow(): Nothing =
    throw new ArithmeticException("Integer overflow")

  private def longOverflow(): Nothing =
    throw new ArithmeticException("Long overflow")

  @inline
  def addExact(a: scala.Int, b: scala.Int): scala.Int = {
    /* Hacker's Delight, Section 2-13
     *
     * See the paragraph starting with
     *
     * > By choosing the second alternative in the first column, and the first
     * > alternative in the second column [...]
     *
     * We use the second alternative in the first column (addition)
     * with c = 0 (we have no incoming carry).
     */
    val res = a + b
    if (((res ^ a) & (res ^ b)) < 0)
      intOverflow()
    res
  }

  @inline
  def addExact(a: scala.Long, b: scala.Long): scala.Long = {
    /* Hacker's Delight, Section 2-13 (same as the Int overload)
     * With RuntimeLong, the computations of all the lo words in the overflow
     * check are dead-code eliminated.
     */
    val res = a + b
    if (((res ^ a) & (res ^ b)) < 0L)
      longOverflow()
    res
  }

  @inline
  def subtractExact(a: scala.Int, b: scala.Int): scala.Int = {
    /* Hacker's Delight, Section 2-13
     *
     * See the paragraph starting with
     *
     * > By choosing the second alternative in the first column, and the first
     * > alternative in the second column [...]
     *
     * We use the first alternative in the second column (subtraction)
     * with c = 0 (we have no incoming carry).
     */
    val res = a - b
    if (((a ^ b) & (res ^ a)) < 0)
      intOverflow()
    res
  }

  @inline
  def subtractExact(a: scala.Long, b: scala.Long): scala.Long = {
    /* Hacker's Delight, Section 2-13 (same as the Int overload)
     * With RuntimeLong, the computations of all the lo words in the overflow
     * check are dead-code eliminated.
     */
    val res = a - b
    if (((a ^ b) & (res ^ a)) < 0L)
      longOverflow()
    res
  }

  @inline
  def multiplyExact(a: scala.Int, b: scala.Int): scala.Int = {
    // Hacker's Delight, Section 2-13
    val full = multiplyFull(a, b)
    val res = full.toInt
    if ((full >>> 32).toInt != (res >> 31))
      intOverflow()
    res
  }

  @inline
  def multiplyExact(a: scala.Long, b: scala.Int): scala.Long = {
    /* Like multiplyExact(Long, Long), but note that b.toLong cannot be
     * Long.MinValue, so we avoid some checks.
     */
    val bLong = b.toLong
    val res = a * bLong
    if (a != 0 && res / a != bLong)
      longOverflow()
    res
  }

  @inline
  def multiplyExact(a: scala.Long, b: scala.Long): scala.Long = {
    /* Hacker's Delight, Section 2-13
     * We swap the role of a and b to match multiplyExact(Long, Int).
     */
    val res = a * b
    if ((a < 0 && b == scala.Long.MinValue) || (a != 0 && res / a != b))
      longOverflow()
    res
  }

  @inline
  def incrementExact(a: scala.Int): scala.Int = {
    if (a == Int.MaxValue)
      intOverflow()
    a + 1
  }

  @inline
  def incrementExact(a: scala.Long): scala.Long = {
    if (a == scala.Long.MaxValue)
      longOverflow()
    a + 1L
  }

  @inline
  def decrementExact(a: scala.Int): scala.Int = {
    if (a == Int.MinValue)
      intOverflow()
    a - 1
  }

  @inline
  def decrementExact(a: scala.Long): scala.Long = {
    if (a == scala.Long.MinValue)
      longOverflow()
    a - 1L
  }

  @inline
  def negateExact(a: scala.Int): scala.Int = {
    if (a == Int.MinValue)
      intOverflow()
    -a
  }

  @inline
  def negateExact(a: scala.Long): scala.Long = {
    if (a == scala.Long.MinValue)
      longOverflow()
    -a
  }

  @inline
  def toIntExact(a: scala.Long): scala.Int = {
    /* With RuntimeLong, the test only performs a shift and one int comparison
     * ((alo >> 31) != ahi). Comparing to bounds ahead of the extraction
     * would require 4 underlying int comparisons.
     */
    val res = a.toInt
    if (res.toLong != a)
      intOverflow()
    res
  }

  // RuntimeLong intrinsic
  @inline
  def multiplyFull(x: scala.Int, y: scala.Int): scala.Long =
    x.toLong * y.toLong

  @inline
  def multiplyHigh(x: scala.Long, y: scala.Long): scala.Long = {
    /* Hacker's Delight, Section 8-2, Figure 8-2,
     * where we have "inlined" all the variables used only once to help our
     * optimizer perform simplifications.
     */

    val x0 = x & 0xffffffffL
    val x1 = x >> 32
    val y0 = y & 0xffffffffL
    val y1 = y >> 32

    val t = x1 * y0 + ((x0 * y0) >>> 32)
    x1 * y1 + (t >> 32) + (((t & 0xffffffffL) + x0 * y1) >> 32)
  }

  @inline
  def unsignedMultiplyHigh(x: scala.Long, y: scala.Long): scala.Long = {
    /* Hacker's Delight, Section 8-2:
     * > For an unsigned version, simply change all the int declarations to unsigned.
     * In Scala, that means changing all the >> into >>>.
     */

    val x0 = x & 0xffffffffL
    val x1 = x >>> 32
    val y0 = y & 0xffffffffL
    val y1 = y >>> 32

    val t = x1 * y0 + ((x0 * y0) >>> 32)
    x1 * y1 + (t >>> 32) + (((t & 0xffffffffL) + x0 * y1) >>> 32)
  }

  @inline
  def floorDiv(a: scala.Int, b: scala.Int): scala.Int = {
    val quot = a / b
    if ((a ^ b) >= 0 || quot * b == a) quot
    else quot - 1
  }

  @inline
  def floorDiv(a: scala.Long, b: scala.Int): scala.Long =
    floorDiv(a, b.toLong)

  @inline
  def floorDiv(a: scala.Long, b: scala.Long): scala.Long = {
    val quot = a / b
    if ((a ^ b) >= 0L || quot * b == a) quot
    else quot - 1L
  }

  @inline
  def floorMod(a: scala.Int, b: scala.Int): scala.Int = {
    val rem = a % b
    if ((a ^ b) >= 0 || rem == 0) rem
    else rem + b
  }

  @inline
  def floorMod(a: scala.Long, b: scala.Int): scala.Int =
    floorMod(a, b.toLong).toInt

  @inline
  def floorMod(a: scala.Long, b: scala.Long): scala.Long = {
    val rem = a % b
    if ((a ^ b) >= 0L || rem == 0L) rem
    else rem + b
  }

  // TODO

  // def IEEEremainder(f1: scala.Double, f2: scala.Double): Double
  // def copySign(magnitude: scala.Double, sign: scala.Double): scala.Double
  // def copySign(magnitude: scala.Float, sign: scala.Float): scala.Float
  // def getExponent(a: scala.Float): scala.Int
  // def getExponent(a: scala.Double): scala.Int
}
