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

/* IMPORTANT NOTICE about this file
 *
 * The code of RuntimeLong is code-size- and performance critical. The methods
 * of this class are used for every single primitive operation on Longs, and
 * must therefore be as fast as they can.
 *
 * This means that this implementation is oriented for performance over
 * readability and idiomatic code.
 */

/** Implementation for the Long operations on the JavaScript platform.
 *
 *  Several algorithms come from "the JS Long paper":
 *    S. Doeraene and T. Schlatter,
 *    "64-bit Integer Division for the JavaScript Platform,"
 *    33rd IEEE Symposium on Computer Arithmetic (ARITH), Fulda, Germany, 2026.
 */
object RuntimeLong {
  private final val TwoPow32 = 4294967296.0
  private final val TwoPow63 = 9223372036854775808.0

  /** The magical mask that allows to test whether a long is a safe double.
   *  @see isSignedSafeDouble
   */
  private final val SafeDoubleHiMask = 0xffe00000

  @inline
  def pack(lo: Int, hi: Int): Long =
    0L // replaced by a magic Transient(PackLong(lo, hi)) during desugaring

  // Comparisons

  @inline
  def equals(alo: Int, ahi: Int, blo: Int, bhi: Int): Boolean =
    alo == blo && ahi == bhi

  @inline
  def notEquals(alo: Int, ahi: Int, blo: Int, bhi: Int): Boolean =
    !equals(alo, ahi, blo, bhi)

  @inline
  def lt(alo: Int, ahi: Int, blo: Int, bhi: Int): Boolean =
    if (ahi == bhi) inlineUnsignedInt_<(alo, blo)
    else ahi < bhi

  @inline
  def le(alo: Int, ahi: Int, blo: Int, bhi: Int): Boolean =
    if (ahi == bhi) inlineUnsignedInt_<=(alo, blo)
    else ahi < bhi

  @inline
  def gt(alo: Int, ahi: Int, blo: Int, bhi: Int): Boolean =
    if (ahi == bhi) inlineUnsignedInt_>(alo, blo)
    else ahi > bhi

  @inline
  def ge(alo: Int, ahi: Int, blo: Int, bhi: Int): Boolean =
    if (ahi == bhi) inlineUnsignedInt_>=(alo, blo)
    else ahi > bhi

  @inline
  def ltu(alo: Int, ahi: Int, blo: Int, bhi: Int): Boolean =
    if (ahi == bhi) inlineUnsignedInt_<(alo, blo)
    else inlineUnsignedInt_<(ahi, bhi)

  @inline
  def leu(alo: Int, ahi: Int, blo: Int, bhi: Int): Boolean =
    if (ahi == bhi) inlineUnsignedInt_<=(alo, blo)
    else inlineUnsignedInt_<(ahi, bhi)

  @inline
  def gtu(alo: Int, ahi: Int, blo: Int, bhi: Int): Boolean =
    if (ahi == bhi) inlineUnsignedInt_>(alo, blo)
    else inlineUnsignedInt_>(ahi, bhi)

  @inline
  def geu(alo: Int, ahi: Int, blo: Int, bhi: Int): Boolean =
    if (ahi == bhi) inlineUnsignedInt_>=(alo, blo)
    else inlineUnsignedInt_>(ahi, bhi)

  // Bitwise operations

  @inline
  def or(alo: Int, ahi: Int, blo: Int, bhi: Int): Long =
    pack(alo | blo, ahi | bhi)

  @inline
  def and(alo: Int, ahi: Int, blo: Int, bhi: Int): Long =
    pack(alo & blo, ahi & bhi)

  @inline
  def xor(alo: Int, ahi: Int, blo: Int, bhi: Int): Long =
    pack(alo ^ blo, ahi ^ bhi)

  // Shifts

  /** Shift left */
  @inline
  def shl(lo: Int, hi: Int, n: Int): Long = {
    /* This should *reasonably* be:
     *   val n1 = n & 63
     *   if (n1 < 32)
     *     RTLong(lo << n1, if (n1 == 0) hi else (lo >>> 32-n1) | (hi << n1))
     *   else
     *     RTLong(0, lo << n1)
     *
     * Replacing n1 by its definition, we have:
     *   if (n & 63 < 32)
     *     RTLong(lo << (n & 63),
     *         if ((n & 63) == 0) hi else (lo >>> 32-(n & 63)) | (hi << (n & 63)))
     *   else
     *     RTLong(0, lo << (n & 63))
     *
     * Since the values on the rhs of shifts are always in arithmetic mod 32,
     * we can get:
     *   if (n & 63 < 32)
     *     RTLong(lo << n, if ((n & 63) == 0) hi else (lo >>> -n) | (hi << n))
     *   else
     *     RTLong(0, lo << n)
     *
     * The condition `n & 63 < 32` is equivalent to
     *   (n & 63) & 32 == 0
     *   n & (63 & 32) == 0
     *   n & 32 == 0
     *
     * In the then part, we have `n & 32 == 0` hence `n & 63 == n & 31`:
     *   RTLong(lo << n, if ((n & 31) == 0) hi else (lo >>> -n) | (hi << n))
     *
     * Consider the following portion:
     *   if ((n & 31) == 0) hi else (lo >>> -n) | (hi << n)
     * When (n & 31) == 0, `hi == (hi << n)` and therefore we have
     *   (if ((n & 31) == 0) 0 else (lo >>> -n)) | (hi << n)
     *
     * The left part of the |
     *   if ((n & 31) == 0) 0 else (lo >>> -n)
     * has the following branchless version:
     *   lo >>> 1 >>> (31-n)
     * Indeed, when `n & 31 == 0, we have
     *   lo >>> 1 >>> 31 == 0
     * and when `n & 31 != 0`, we know that ((31-n) & 31) < 31, and hence we have
     *   lo >>> 1 >>> (31-n) == lo >>> (1+31-n) == lo >>> (32-n) == lo >>> -n
     *
     * Was it good? We have traded
     *   if ((n & 31) == 0) hi else (lo >>> -n) | (hi << n)
     * for
     *   (lo >>> 1 >>> (31-n)) | (hi << n)
     * When (n & 31) != 0, which is the common case, we have traded a test
     * `if ((n & 31) == 0)` for one additional constant shift `>>> 1`. That's
     * probably worth it performance-wise. The code is also shorter.
     *
     * Summarizing, so far we have
     *   if (n & 32 == 0)
     *     RTLong(lo << n, (lo >>> 1 >>> (31-n)) | (hi << n))
     *   else
     *     RTLong(0, lo << n)
     *
     * If we distribute the condition in the lo and hi arguments of the
     * constructors, we get a version with only one pack output, which avoids
     * reification as records by the optimizer, yielding shorter code.
     * It is potentially slightly less efficient, except when `n` is constant,
     * which is often the case anyway.
     *
     * Finally we have:
     */
    pack(
        if ((n & 32) == 0) lo << n else 0,
        if ((n & 32) == 0) (lo >>> 1 >>> (31 - n)) | (hi << n) else lo << n)
  }

  /** Logical shift right */
  @inline
  def shr(lo: Int, hi: Int, n: Int): Long = {
    // This derives in a similar way as in <<
    pack(
        if ((n & 32) == 0) (lo >>> n) | (hi << 1 << (31 - n)) else hi >>> n,
        if ((n & 32) == 0) hi >>> n else 0)
  }

  /** Arithmetic shift right */
  @inline
  def sar(lo: Int, hi: Int, n: Int): Long = {
    // This derives in a similar way as in <<
    pack(
        if ((n & 32) == 0) (lo >>> n) | (hi << 1 << (31 - n)) else hi >> n,
        if ((n & 32) == 0) hi >> n else hi >> 31)
  }

  // Arithmetic operations

  @inline
  def add(alo: Int, ahi: Int, blo: Int, bhi: Int): Long = {
    // See the JS Long paper, Section IV, Algorithm 1.
    val lo = alo + blo
    pack(lo,
        ahi + bhi + (if (inlineUnsignedInt_<(lo, alo)) 1 else 0)) // branchless if
  }

  @inline
  def sub(alo: Int, ahi: Int, blo: Int, bhi: Int): Long = {
    // See the JS Long paper, Section IV, Algorithm 2.
    val lo = alo - blo
    pack(lo,
        ahi - bhi - (if (inlineUnsignedInt_>(lo, alo)) 1 else 0)) // branchless if
  }

  @inline
  def mul(alo: Int, ahi: Int, blo: Int, bhi: Int): Long = {
    // See the JS Long paper, Section IV, Algorithm 3.

    /* Note that the optimizer normalizes constants in * to be on the
     * left-hand-side (when it cannot do constant-folding to begin with).
     * Therefore, `b` is never constant in practice.
     */

    val a0 = alo & 0xffff
    val a1 = alo >>> 16
    val b0 = blo & 0xffff
    val b1 = blo >>> 16

    val a0b0 = a0 * b0
    val a1b0 = a1 * b0 // collapses to 0 when a is constant and 0 <= a <= 0xffff
    val a0b1 = a0 * b1 // (*)

    /* (*) Since b is never constant in practice, the only case where a0b1
     * would be constant 0 is if b's lo part is constant but not its hi part.
     * That's not a likely scenario, though (not seen at all in our test suite).
     */

    /* lo = a.lo * b.lo, but we compute the above 3 subproducts for hi
     * anyway, we reuse them to compute lo too, trading a * for 2 +'s and 1 <<.
     */
    val lo = a0b0 + ((a1b0 + a0b1) << 16)

    // hi = a.lo*b.hi + a.hi*b.lo + carry_from_lo_*
    val c1part = (a0b0 >>> 16) + a0b1
    val hi = {
      alo * bhi + ahi * blo + a1 * b1 + (c1part >>> 16) +
      (((c1part & 0xffff) + a1b0) >>> 16) // collapses to 0 when a1b0 = 0
    }

    pack(lo, hi)
  }

  /** Computes `longBitsToDouble(a)`.
   *
   *  `fpBitsDataView` must be a scratch `js.typedarray.DataView` whose
   *  underlying buffer is at least 8 bytes long.
   */
  @inline
  def bitsToDouble(lo: Int, hi: Int,
      fpBitsDataView: scala.scalajs.js.typedarray.DataView): Double = {

    fpBitsDataView.setInt32(0, lo, littleEndian = true)
    fpBitsDataView.setInt32(4, hi, littleEndian = true)
    fpBitsDataView.getFloat64(0, littleEndian = true)
  }

  def toString(lo: Int, hi: Int): String = {
    // See the JS Long paper, Section VIII, Algorithm 11.
    if (isInt32(lo, hi)) {
      lo.toString()
    } else if (isSignedSafeDouble(hi)) {
      asSafeDouble(lo, hi).toString()
    } else {
      val aAbs = abs(lo, hi)
      // Calls back into toInt() and shr()
      val s = toUnsignedStringLarge(aAbs.toInt, (aAbs >>> 32).toInt)
      if (hi < 0) "-" + s else s
    }
  }

  @inline
  private def toUnsignedStringLarge(lo: Int, hi: Int): String = {
    /* This is called only if (lo, hi) is >= 2^53.
     * See the JS Long paper, Section VIII, Algorithm 12, with constants
     * specialized for Δ = 10.
     */

    // constants
    val d = 1000000000 // 10^9
    val mHat = (1.0 / d.toDouble) + 2.6469779601696886e-23 // the constant is 2^(-75)

    // initial approximation of the quotient and remainder
    val aHat = unsignedToDoubleApprox(lo, hi)
    var qHat = scala.scalajs.js.Math.floor(aHat * mHat)
    var rHat = lo - d * unsignedSafeDoubleLo(qHat)

    // correct the approximations
    if (rHat < 0) {
      qHat -= 1.0
      rHat += d
    }

    // build the result string
    val remStr = rHat.toString()
    qHat.toString() + substring("000000000", remStr.length()) + remStr
  }

  @inline
  def toInt(lo: Int, hi: Int): Int =
    lo

  @inline
  def toDouble(lo: Int, hi: Int): Double =
    signedToDoubleApprox(lo, hi)

  @inline
  def toFloat(lo: Int, hi: Int): Float = {
    /* This implementation is based on the property that, *if* the conversion
     * `x.toDouble` is lossless, then the result of `x.toFloat` is equivalent
     * to `x.toDouble.toFloat`.
     *
     * However, as illustrated in #4466, it is not equivalent when the Long
     * value is very close though not equal to a Float midpoint. Indeed, in
     * that case the conversion to Double can be rounded to the Float midpoint,
     * destroying the information of whether the Long value was above or below
     * the midpoint. When converting the Double into Float, this causes
     * rounding to "even" where a round up or down would have been necessary.
     *
     * To avoid that, we first "compress" the input Long `x` into another Long
     * `y` such that:
     * - `y.toDouble` is lossless, and
     * - `y.toFloat == x.toFloat`.
     *
     * The algorithm works as follows:
     *
     * If the input is a signed safe Double, then the conversion to double is
     * lossless, so we don't have to do anything special (`y == x` in terms of
     * the above explanation).
     *
     * Otherwise, let us first assume that `x >= 0`. In that case, we know that
     * the input's highest 1 bit is in the 11 highest-order bits. That means
     * that rounding to float, which only has 24 bits in the significand, can
     * only take into account the `11 + 23 + 1 = 35` highest-order bits (the
     * `+ 1` is for the rounding bit). The remaining bits can only affect the
     * result by two states: either they are all 0's, or there is at least one
     * 1. We use that property to "compress" the 16 low-order bits into a
     * single 0 or 1 bit representing those two states. The compressed Long
     * value `y = (compressedLo, hi)` has at most `32 + 17 = 49` significant
     * bits. Therefore its conversion to Double is lossless.
     *
     * Now that we always have a lossless compression to Double, we can perform
     * it, followed by a conversion from Double to Float, which will apply the
     * appropriate rounding.
     *
     * (A similar strategy is used in `parseFloat` for the hexadecimal format,
     * where we only have the non-negative case.)
     *
     * For the case `x < 0`, logically we should negate it, perform the above
     * transformation and convert to Double, then negate the result. It turns
     * out we do not need a separate code path. Indeed, if x is a safe double,
     * then -x also converts losslessly (-x may not be safe double by our
     * definition, because it could be exactly 2^53, but the conversion is
     * still exact). Otherwise, we should apply a compression if
     * `(-x & 0xffffL) != 0L`. Because of how two's complement negation work,
     * that is equivalent to `(x & 0xffffL) != 0L`, and therefore also
     * equivalent to `(lo & 0xffff) != 0`. When we do need a compression, we
     * can do it on the signed representation just as well as the unsigned
     * representation, because it only affects `lo`, and `lo` is interpreted as
     * unsigned regardless, when converting to a double.
     */

    val compressedLo =
      if (isSignedSafeDouble(hi) || (lo & 0xffff) == 0) lo
      else (lo & ~0xffff) | 0x8000
    signedToDoubleApprox(compressedLo, hi).toFloat
  }

  @inline
  def clz(lo: Int, hi: Int): Int = {
    /* Warning to the next adventurer to come here: the best branchless
     * algorithm I found was worse (performance-wise) than the naive
     * implementation here.
     * The algorithm was `val hiz = clz(hi); hiz + ((hiz << 26 >> 31) & clz(lo))`.
     */
    if (hi != 0) Integer.numberOfLeadingZeros(hi)
    else 32 + Integer.numberOfLeadingZeros(lo)
  }

  @inline
  def fromInt(value: Int): Long =
    pack(value, value >> 31)

  @inline
  def fromUnsignedInt(value: Int): Long =
    pack(value, 0)

  def fromDouble(value: Double): Long = {
    /* When value is NaN, the conditions of the 3 `if`s are false, and we end
     * up returning (NaN | 0, (NaN / TwoPow32) | 0), which is correctly (0, 0).
     */

    if (value < -TwoPow63) {
      Long.MinValue
    } else if (value >= TwoPow63) {
      Long.MaxValue
    } else {
      val rawLo = rawToInt(value)
      val rawHi = rawToInt(value / TwoPow32)

      /* Magic!
       *
       * When value < 0, this should *reasonably* be:
       *   val absValue = -value
       *   val absLo = rawToInt(absValue)
       *   val absHi = rawToInt(absValue / TwoPow32)
       *   val lo = -absLo
       *   hiReturn = if (absLo != 0) ~absHi else -absHi
       *   return lo
       * where absLo and absHi follow the JS Long paper, Section V.B.
       *
       * Using the fact that rawToInt(-x) == -rawToInt(x), we can rewrite
       * absLo and absHi without absValue as:
       *   val absLo = -rawToInt(value)
       *             = -rawLo
       *   val absHi = -rawToInt(value / TwoPow32)
       *             = -rawHi
       *
       * Now, we can replace absLo in the definition of lo and get:
       *   val lo = -(-rawLo)
       *          = rawLo
       *
       * The `hiReturn` definition can be rewritten as
       *   hiReturn = if (lo != 0) -absHi - 1 else -absHi
       *            = if (rawLo != 0) -(-rawHi) - 1 else -(-rawHi)
       *            = if (rawLo != 0) rawHi - 1 else rawHi
       *
       * Now that we do not need absValue, absLo nor absHi anymore, we end
       * end up with:
       *   hiReturn = if (rawLo != 0) rawHi - 1 else rawHi
       *   return rawLo
       *
       * When value >= 0, the definitions are simply
       *   hiReturn = rawToInt(value / TwoPow32) = rawHi
       *   lo = rawToInt(value) = rawLo
       *
       * Combining the negative and positive cases, we get:
       */
      pack(rawLo, if (value < 0 && rawLo != 0) rawHi - 1 else rawHi)
    }
  }

  /** Computes `doubleToRawLongBits(value)`.
   *
   *  `fpBitsDataView` must be a scratch `js.typedarray.DataView` whose
   *  underlying buffer is at least 8 bytes long.
   */
  @inline
  def fromDoubleBits(value: Double,
      fpBitsDataView: scala.scalajs.js.typedarray.DataView): Long = {

    fpBitsDataView.setFloat64(0, value, littleEndian = true)
    pack(
      fpBitsDataView.getInt32(0, littleEndian = true),
      fpBitsDataView.getInt32(4, littleEndian = true)
    )
  }

  @inline
  def compare(alo: Int, ahi: Int, blo: Int, bhi: Int): Int = {
    if (ahi == bhi) {
      if (alo == blo) 0
      else if (inlineUnsignedInt_<(alo, blo)) -1
      else 1
    } else {
      if (ahi < bhi) -1
      else 1
    }
  }

  /** Intrinsic for Math.multiplyFull.
   *
   *  Compared to the regular expansion of `x.toLong * y.toLong`, this
   *  intrinsic avoids 2 int multiplications.
   */
  @inline
  def multiplyFull(a: Int, b: Int): Long = {
    /* We use Hacker's Delight, Section 8-2, Figure 8-2, to compute the hi
     * word of the result. We reuse intermediate products to compute the lo
     * word, like we do in `RuntimeLong.mul`.
     *
     * We swap the role of a1b0 and a0b1 compared to Hacker's Delight, to
     * optimize for the case where a1b0 collapses to 0, like we do in
     * `RuntimeLong.mul`. The optimizer normalizes constants in multiplyFull to
     * be on the left-hand-side (when it cannot do constant-folding to begin
     * with). Therefore, `b` is never constant in practice.
     */

    val a0 = a & 0xffff
    val a1 = a >> 16
    val b0 = b & 0xffff
    val b1 = b >> 16

    val a0b0 = a0 * b0
    val a1b0 = a1 * b0 // collapses to 0 when a is constant and 0 <= a <= 0xffff
    val a0b1 = a0 * b1

    /* lo = a * b, but we compute the above 3 subproducts for hi anyway,
     * so we reuse them to compute lo too, trading a * for 2 +'s and 1 <<.
     */
    val lo = a0b0 + ((a1b0 + a0b1) << 16)

    val t = a0b1 + (a0b0 >>> 16)
    val hi = {
      a1 * b1 + (t >> 16) +
      (((t & 0xffff) + a1b0) >> 16) // collapses to 0 when a1b0 = 0
    }

    pack(lo, hi)
  }

  def divide(alo: Int, ahi: Int, blo: Int, bhi: Int): Long = {
    val aAbs = abs(alo, ahi)
    val bAbs = abs(blo, bhi)

    // Calls back into toInt() and shr(), free after optimizations
    val aAbsLo = aAbs.toInt
    val aAbsHi = (aAbs >>> 32).toInt
    val bAbsLo = bAbs.toInt
    val bAbsHi = (bAbs >>> 32).toInt

    val absR = unsignedDivModHelper(aAbsLo, aAbsHi, bAbsLo, bAbsHi,
        askQuotient = true, forSigned = true)
    if ((ahi ^ bhi) >= 0)
      absR // a and b have the same sign bit
    else
      -absR // calls back into sub()
  }

  @inline // inline the static forwarder ...
  def divideUnsigned(alo: Int, ahi: Int, blo: Int, bhi: Int): Long =
    divideUnsignedImpl(alo, ahi, blo, bhi)

  @noinline // ... but don't inline all of unsignedDivModHelper at call site
  def divideUnsignedImpl(alo: Int, ahi: Int, blo: Int, bhi: Int): Long =
    unsignedDivModHelper(alo, ahi, blo, bhi, askQuotient = true, forSigned = false)

  def remainder(alo: Int, ahi: Int, blo: Int, bhi: Int): Long = {
    val aAbs = abs(alo, ahi)
    val bAbs = abs(blo, bhi)

    // Calls back into toInt() and shr(), free after optimizations
    val aAbsLo = aAbs.toInt
    val aAbsHi = (aAbs >>> 32).toInt
    val bAbsLo = bAbs.toInt
    val bAbsHi = (bAbs >>> 32).toInt

    val absR = unsignedDivModHelper(aAbsLo, aAbsHi, bAbsLo, bAbsHi,
        askQuotient = false, forSigned = true)
    if (ahi < 0)
      -absR // calls back into sub()
    else
      absR
  }

  @inline // inline the static forwarder ...
  def remainderUnsigned(alo: Int, ahi: Int, blo: Int, bhi: Int): Long =
    remainderUnsignedImpl(alo, ahi, blo, bhi)

  @noinline // ... but don't inline all of unsignedDivModHelper at call site
  def remainderUnsignedImpl(alo: Int, ahi: Int, blo: Int, bhi: Int): Long =
    unsignedDivModHelper(alo, ahi, blo, bhi, askQuotient = false, forSigned = false)

  /** Main unsigned division routine.
   *
   *  If `askQuotient` is true, computes the quotient, otherwise computes the
   *  remainder.
   *
   *  See the JS Long paper, Section VI.
   *
   *  We inline this method 4 times, specialized for each combination of
   *  (askQuotient, forSigned).
   */
  @inline
  private def unsignedDivModHelper(alo: Int, ahi: Int, blo: Int, bhi: Int,
      askQuotient: Boolean, forSigned: Boolean): Long = {

    /* Conveniently, when b = 0, we enter the first case, where the first thing
     * we do is an int division by blo, which will throw an ArithmeticException.
     * So we don't need a separate check for that case here.
     */

    if (bothZero(bhi, blo & 0xffe00000)) {
      // b < 2^21, Algorithm 4

      if (askQuotient) {
        val quotHi = Integer.divideUnsigned(ahi, blo)
        val k = ahi - blo * quotHi
        val quotLo = rawToInt(asSafeDouble(alo, k) / blo.toDouble)
        pack(quotLo, quotHi)
      } else {
        val k = Integer.remainderUnsigned(ahi, blo)
        val quotLo = rawToInt(asSafeDouble(alo, k) / blo.toDouble)
        val remLo = alo - blo * quotLo
        pack(remLo, 0)
      }
    } else if (forSigned || bhi >= 0) {
      // 2^21 <= b <= 2^63, Algorithm 5 (the case 2^63 is only for signed division)

      val a = pack(alo, ahi)
      val b = pack(blo, bhi)
      val aHat = unsignedToDoubleApprox(alo, ahi)
      val bHat = unsignedToDoubleApprox(blo, bhi)
      val qHat = fromUnsignedSafeDouble(aHat / bHat + 0.00390625) // 2^(-8)
      val rHat = a - b * qHat

      if (rHat < 0L) {
        if (askQuotient) qHat - 1L
        else rHat + b
      } else {
        if (askQuotient) qHat
        else rHat
      }
    } else {
      divModHuge(pack(alo, ahi), pack(blo, bhi), askQuotient)
    }
  }

  @inline
  def divModByConstantSmallSigned(a: Long, absB: Int, bIsNeg: Boolean,
      mHat: Double, askQuotient: Boolean): Long = {
    val absR = divModByConstantSmall(abs(a), absB, mHat, askQuotient)
    maybeNegateDivModResult(a, bIsNeg, askQuotient, absR)
  }

  @inline
  def divModByConstantMediumSigned(a: Long, absB: Int, bIsNeg: Boolean,
      mHat: Double, askQuotient: Boolean): Long = {
    val absR = divModByConstantMedium(abs(a), absB, mHat, askQuotient)
    maybeNegateDivModResult(a, bIsNeg, askQuotient, absR)
  }

  @inline
  def divModByConstantLargeSigned(a: Long, absB: Long, bIsNeg: Boolean,
      mHat: Double, askQuotient: Boolean): Long = {
    val absR = divModByConstantLarge(abs(a), absB, mHat, askQuotient)
    maybeNegateDivModResult(a, bIsNeg, askQuotient, absR)
  }

  @inline
  def maybeNegateDivModResult(a: Long, bIsNeg: Boolean,
      askQuotient: Boolean, absR: Long): Long = {
    if (bIsNeg && askQuotient) { // constant-folded
      if (a < 0L) absR else -absR
    } else {
      if (a < 0L) -absR else absR
    }
  }

  /** Divide by a small constant, `0 < b < 2^18`.
   *
   *  See the JS Long paper, Algorithm 8.
   */
  @inline
  def divModByConstantSmall(a: Long, b: Int, mHat: Double, askQuotient: Boolean): Long = {
    val alo = a.toInt
    val ahi = (a >>> 32).toInt

    if (askQuotient) {
      val quotHi = Integer.divideUnsigned(ahi, b)
      val k = ahi - b * quotHi
      val quotLo = rawToInt(asSafeDouble(alo, k) * mHat)
      pack(quotLo, quotHi)
    } else {
      val k = Integer.remainderUnsigned(ahi, b)
      val quotLo = rawToInt(asSafeDouble(alo, k) * mHat)
      val remLo = alo - b * quotLo
      pack(remLo, 0)
    }
  }

  /** Divide by a medium constant, `2^18 <= b < 2^31`.
   *
   *  See the JS Long paper, Algorithm 9, with the 32-bit remainder improvement.
   */
  @inline
  def divModByConstantMedium(a: Long, b: Int, mHat: Double, askQuotient: Boolean): Long = {
    val aHat = unsignedToDoubleApprox(a)
    val qHat = fromUnsignedSafeDouble(aHat * mHat)
    val rHat = a.toInt - b * qHat.toInt

    if (rHat < 0) {
      if (askQuotient) qHat - 1L
      else pack(rHat + b, 0)
    } else {
      if (askQuotient) qHat
      else pack(rHat, 0)
    }
  }

  /** Divide by a large constant, `2^31 <= b < 2^63`.
   *
   *  See the JS Long paper, Algorithm 9.
   */
  @inline
  def divModByConstantLarge(a: Long, b: Long, mHat: Double, askQuotient: Boolean): Long = {
    val aHat = unsignedToDoubleApprox(a)
    val qHat = fromUnsignedSafeDouble(aHat * mHat)
    val rHat = a - b * qHat

    if (rHat < 0L) {
      if (askQuotient) qHat - 1L
      else rHat + b
    } else {
      if (askQuotient) qHat
      else rHat
    }
  }

  /** Divide by a huge constant, `2^63 <= b < 2^64`. */
  @inline
  def divModByConstantHuge(a: Long, b: Long, askQuotient: Boolean): Long =
    divModHuge(a, b, askQuotient)

  @inline
  def divModHuge(a: Long, b: Long, askQuotient: Boolean): Long = {
    if (ltu(a, b)) {
      if (askQuotient) 0L
      else a
    } else {
      if (askQuotient) 1L
      else a - b
    }
  }

  @inline
  private def substring(s: String, start: Int): String = {
    import scala.scalajs.js.JSStringOps.enableJSStringOps
    s.jsSubstring(start)
  }

  /** Tests whether `a == 0 && b == 0` with a single comparison. */
  @inline def bothZero(a: Int, b: Int): Boolean =
    (a | b) == 0

  /** Tests whether the long (lo, hi)'s mathematical value fits in a signed Int. */
  @inline def isInt32(lo: Int, hi: Int): Boolean =
    hi == (lo >> 31)

  /** Tests whether a signed long (lo, hi) is a safe Double.
   *
   *  This test is in fact slightly stricter than necessary, as it tests
   *  whether `-2^53 <= x < 2^53`, although x == 2^53 would be a perfectly safe
   *  Double. We do it this way because it corresponds to testing whether the
   *  value can be represented as a signed 54-bit integer. That is true if and
   *  only if the (64 - 54) = 10 most significant bits are all equal to bit 53,
   *  or equivalently, whether the 11 most significant bits all equal.
   *
   *  Since there is virtually no gain to treating 2^53 itself as a safe
   *  Double, compared to all numbers smaller than it, we don't bother, and
   *  stay on the fast side.
   */
  @inline def isSignedSafeDouble(hi: Int): Boolean =
    ((hi ^ (hi >> 10)) & SafeDoubleHiMask) == 0

  /** Converts a safe double (signed or unsigned) into its exact Double representation. */
  @inline def asSafeDouble(lo: Int, hi: Int): Double =
    signedToDoubleApprox(lo, hi)

  /** Converts an unsigned safe double into its Long representation.
   *
   *  See the JS Long paper, Section V.B.
   */
  @inline def fromUnsignedSafeDouble(x: Double): Long =
    pack(unsignedSafeDoubleLo(x), unsignedSafeDoubleHi(x))

  /** Computes the lo part of a long from an unsigned safe double. */
  @inline def unsignedSafeDoubleLo(x: Double): Int =
    rawToInt(x)

  /** Computes the hi part of a long from an unsigned safe double. */
  @inline def unsignedSafeDoubleHi(x: Double): Int =
    rawToInt(x / TwoPow32)

  /** Approximates an unsigned (lo, hi) with a Double.
   *
   *  See the JS Long paper, Section V.A.
   */
  @inline def unsignedToDoubleApprox(lo: Int, hi: Int): Double =
    uintToDouble(hi) * TwoPow32 + uintToDouble(lo)

  /** Approximates an unsigned long with a Double.
   *
   *  See the JS Long paper, Section V.A.
   */
  @inline def unsignedToDoubleApprox(x: Long): Double =
    unsignedToDoubleApprox(x.toInt, (x >>> 32).toInt)

  /** Approximates a signed (lo, hi) with a Double.
   *
   *  See the JS Long paper, Section V.A.
   *
   *  If `hi` is known to be non-negative, this method is equivalent to
   *  `unsignedToDoubleApprox`, but it can fold away part of the computation if
   *  `hi` is in fact constant.
   */
  @inline def signedToDoubleApprox(lo: Int, hi: Int): Double =
    hi.toDouble * TwoPow32 + uintToDouble(lo)

  /** Interprets an `Int` as an unsigned integer and returns its value as a
   *  `Double`.
   *
   *  In user space, this would be `Integer.toUnsignedLong(x).toDouble`.
   *  However, we cannot use that, since it would circle back here into an
   *  infinite recursion.
   */
  @inline def uintToDouble(x: Int): Double = {
    import scala.scalajs.js.DynamicImplicits.number2dynamic
    (x.toDouble >>> 0).asInstanceOf[Double]
  }

  /** Performs the JavaScript operation `(x | 0)`. */
  @inline def rawToInt(x: Double): Int = {
    import scala.scalajs.js.DynamicImplicits.number2dynamic
    (x | 0).asInstanceOf[Int]
  }

  @inline
  def inlineUnsignedInt_<(a: Int, b: Int): Boolean =
    (a ^ 0x80000000) < (b ^ 0x80000000)

  @inline
  def inlineUnsignedInt_<=(a: Int, b: Int): Boolean =
    (a ^ 0x80000000) <= (b ^ 0x80000000)

  @inline
  def inlineUnsignedInt_>(a: Int, b: Int): Boolean =
    (a ^ 0x80000000) > (b ^ 0x80000000)

  @inline
  def inlineUnsignedInt_>=(a: Int, b: Int): Boolean =
    (a ^ 0x80000000) >= (b ^ 0x80000000)

  // used in the division algorithm; calls back into the expanded ltu function
  @inline
  def ltu(a: Long, b: Long): Boolean =
    (a ^ 0x8000000000000000L) < (b ^ 0x8000000000000000L)

  // used in the division algorithm; calls back into the expanded geu function
  @inline
  def geu(a: Long, b: Long): Boolean =
    (a ^ 0x8000000000000000L) >= (b ^ 0x8000000000000000L)

  @inline
  def abs(x: Long): Long =
    abs(x.toInt, (x >>> 32).toInt)

  @inline
  def abs(lo: Int, hi: Int): Long = {
    /* The algorithm here is inspired by Hacker's Delight formula for `abs`.
     * However, a naive application of that formula does not give good code for
     * our RuntimeLong implementation.
     *
     * Spec:
     * - if a = (lo, hi) >= 0L, return a
     * - otherwise, return `0L - a`
     *
     * Proof:
     *
     * If a >= 0, we have sign = 0, and we can verify that r = x = a.
     *
     * Otherwise, sign = -1, therefore x = (~alo, ~ahi) = ~a.
     *   rlo = xlo - sign = xlo + 1
     *   rhi = xhi + 0 + (rlo < xlo)
     * Observe that (rlo, rhi) is the expansion of add(x, 1L).
     * Therefore,
     *   r = x + 1L = ~a + 1L = 0L - a
     * as desired.
     */
    val sign = hi >> 31
    val xlo = lo ^ sign
    // val xhi = hi ^ sign ; but we integrate that directly in rhi for code size
    val rlo = xlo - sign
    val rhi = (hi ^ sign) + (if (inlineUnsignedInt_<(rlo, xlo)) 1 else 0)
    pack(rlo, rhi)
  }

}
