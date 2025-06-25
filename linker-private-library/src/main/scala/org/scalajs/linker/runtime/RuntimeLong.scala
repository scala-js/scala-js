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

/** Implementation for the Long operations on the JavaScript platform. */
object RuntimeLong {
  private final val TwoPow32 = 4294967296.0
  private final val TwoPow63 = 9223372036854775808.0

  /** The magical mask that allows to test whether an unsigned long is a safe
   *  double.
   *  @see isUnsignedSafeDouble
   */
  private final val SafeDoubleHiMask = 0xffe00000

  @inline
  def pack(lo: Int, hi: Int): Long =
    0L // replaced by a magic Transient(PackLong(lo, hi)) when loading the IR of this class

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
        if ((n & 32) == 0) (lo >>> 1 >>> (31-n)) | (hi << n) else lo << n)
  }

  /** Logical shift right */
  @inline
  def shr(lo: Int, hi: Int, n: Int): Long = {
    // This derives in a similar way as in <<
    pack(
        if ((n & 32) == 0) (lo >>> n) | (hi << 1 << (31-n)) else hi >>> n,
        if ((n & 32) == 0) hi >>> n else 0)
  }

  /** Arithmetic shift right */
  @inline
  def sar(lo: Int, hi: Int, n: Int): Long = {
    // This derives in a similar way as in <<
    pack(
        if ((n & 32) == 0) (lo >>> n) | (hi << 1 << (31-n)) else hi >> n,
        if ((n & 32) == 0) hi >> n else hi >> 31)
  }

  // Arithmetic operations

  @inline
  def add(alo: Int, ahi: Int, blo: Int, bhi: Int): Long = {
    // Hacker's Delight, Section 2-16
    val lo = alo + blo
    pack(lo,
        ahi + bhi + (((alo & blo) | ((alo | blo) & ~lo)) >>> 31))
  }

  @inline
  def sub(alo: Int, ahi: Int, blo: Int, bhi: Int): Long = {
    /* Hacker's Delight, Section 2-16
     *
     * We deviate a bit from the original algorithm. Hacker's Delight uses
     * `- (... >>> 31)`. Instead, we use `+ (... >> 31)`. These are equivalent,
     * since `(x >> 31) == -(x >>> 31)` for all x. The variant with `+` folds
     * better when `a.hi` and `b.hi` are both known to be 0. This happens in
     * practice when `a` and `b` are 0-extended from `Int` values.
     */
    val lo = alo - blo
    pack(lo,
        ahi - bhi + (((~alo & blo) | (~(alo ^ blo) & lo)) >> 31))
  }

  @inline
  def mul(alo: Int, ahi: Int, blo: Int, bhi: Int): Long = {
    /* The following algorithm is based on the decomposition in 32-bit and then
     * 16-bit subproducts of the unsigned interpretation of operands.
     *
     * Since everything is interpreted as unsigned, all values are Natural
     * numbers and are >= 0, by construction.
     *
     * We are looking to compute
     * a *[64] b = (a * b) % 2^64
     *
     * We use the notation * and + for mathematical, non-overflowing
     * operations, and *[64] and +[64] for overflowing operations. Eventually,
     * we need to implement everything in terms of *[32] and +[32]. The symbol
     * ^ is used for exponentiation (not bitwise xor).
     *
     * The decomposition in 32-bit components yields:
     *
     * a *[64] b
     *   = ( (2^32*ahi + alo) * (2^32*bhi + blo) ) % 2^64
     *   = ( 2^64*ahi*bhi + 2^32*(ahi*blo + bhi*alo) + alo*blo ) % 2^64
     *
     * With Natural numbers, congruence theory tells us that we can "distribute"
     * `% n` on + and *, as long as we also keep the outer `% n`. To be more
     * precise:
     *   (a + b) % n = (a%n + b) % n = (a + b%n) % n = (a%n + b%n) % n
     *   (a * b) % n = (a%n * b) % n = (a * b%n) % n = (a%n * b%n) % n
     *
     * From the latter, we derive a corollary that we'll implicitly use several
     * times later:
     *   (n * x) % n = 0  for any n > 0 and any x
     *
     * We can use these equivalences to get rid of parts of our computation:
     *
     * ( 2^64*ahi*bhi + 2^32*(ahi*blo + bhi*alo) + alo*blo ) % 2^64
     *   = ( (2^64*ahi*bhi % 2^64) + 2^32*(ahi*blo + bhi*alo) + alo*blo ) % 2^64
     *        -------------------
     *             = 0
     *   = ( 2^32*(ahi*blo + bhi*alo) + alo*blo ) % 2^64
     *
     * Observe that we can rewrite any quantity x as
     * x = n*(x/n) + x%n
     * where n is > 0 and / denotes the floor division.
     *
     * We can rewrite the product ahi*blo as
     *
     * ahi*blo
     *   = 2^32*(ahi*blo / 2^32) + (ahi*blo % 2^32)
     *   = 2^32*(ahi*blo / 2^32) + (ahi *[32] blo)
     *
     * Similarly,
     *
     * bhi*alo = 2^32*(alo*bhi / 2^32) + (alo *[32] bhi)
     *
     * Taking back our complete computation:
     *
     * a *[64] b
     *   = ( 2^32*(ahi*blo + bhi*alo) + alo*blo ) % 2^64
     *   = ( 2^64*(ahi*blo / 2^32) + 2^32*(ahi *[32] blo)
     *        + 2^64*(alo*bhi / 2^32) + 2^32*(alo *[32] bhi)
     *        + alo*blo ) % 2^64
     *
     * where distributing % 2^64 allows to get rid of the most awful parts:
     *
     *   = ( 2^32*(ahi *[32] blo) + 2^32*(alo *[32] bhi) + alo*blo) % 2^64
     *
     * Now we focus on the `alo*blo` part. We decompose it in 16-bit components.
     *
     * alo * blo
     *   = 2^32*a1*b1 + 2^16*a1*b0 + 2^16*a0*b1 + a0*b0
     *
     * Because a1, a0, b1 and b0 are all <= 2^16-1, their pair-wise products
     * are all <= (2^16-1)^2 = 2^32 - 2*2^16 + 1 = 0xfffe0001 < 2^32. This
     * means that, for example,
     *   a1*b0 = (a1*b0) % 2^32 = a1 *[32] b0
     * with the same applying to other subproducts.
     *
     * Let
     *   a1b1 = a1 *[32] b1
     *   a1b0 = a1 *[32] b0
     *   a0b1 = a0 *[32] b1
     *   a0b0 = a0 *[32] b0
     *
     * Each of those is <= 0xfffe0001.
     *
     * We now have:
     *
     * alo * blo
     *   = 2^32*a1b1 + 2^16*a1b0 + 2^16*a0b1 + a0b0
     *
     * We can decompose it using / and % as follows:
     * alo * blo
     *   = 2^32*((alo * blo) / 2^32) + ((alo * blo) % 2^32)
     *
     * Let
     *   aloblo = (alo * blo) % 2^32
     *   carry_from_lo_* = (alo * blo) / 2^32
     *
     * Then
     * alo * blo = 2^32 * carry_from_lo_* + aloblo
     *
     * aloblo = (alo * blo) % 2^32
     *   = (2^32*a1b1 + 2^16*a1b0 + 2^16*a0b1 + a0b0) % 2^32
     *   = (2^16*a1b0 + 2^16*a0b1 + a0b0) % 2^32
     *   = (((2^16*a1b0 % 2^32 + 2^16*a0b1 % 2^32) % 2^32) + a0b0 % 2^32) % 2^32
     *   = (2^16*a1b0 % 2^32) +[32] (2^16*a0b1 % 2^32) +[32] (a0b0 % 2^32)
     *   = (a1b0 <<[32] 16) +[32] (a0b1 <<[32] 16) +[32] a0b0
     *
     * carry_from_lo_* is more difficult.
     *
     * carry_from_lo_* = (alo * blo) / 2^32
     *   = (2^32*a1b1 + 2^16*a1b0 + 2^16*a0b1 + a0b0) / 2^32
     *   = a1b1 + (2^16*a1b0 + 2^16*a0b1 + a0b0) / 2^32
     *   = a1b1 + (2^16*a1b0 + 2^16*a0b1 + (2^16*(a0b0 / 2^16) + (a0b0 % 2^16))) / 2^32
     *   = a1b1 + (2^16*(a1b0 + a0b1 + (a0b0 / 2^16)) + (a0 % 2^16)) / 2^32
     *             ----------------------------------
     *                  multiple of 2^16
     *   = a1b1 + ( (2^16*(a1b0 + a0b1 + (a0b0 / 2^16))) / 2^16 + (a0 % 2^16) / 2^16 ) / 2^16
     *                                                             ---------
     *                                                               < 2^16
     *   = a1b1 + (a1b0 + a0b1 + (a0b0 / 2^16)) / 2^16
     *   = a1b1 + (a1b0 + (a0b1 + (a0b0 >>>[32] 16))) / 2^16
     *                     ----    ---------------
     *               <= 0xfffe0001    <= 0xffff
     *                     ------------------------
     *                       <= 0xffff0000, hence the + does not overflow
     *   = a1b1 + (a1b0 + (a0b1 +[32] (a0b0 >>>[32] 16))) / 2^16
     *
     * Let
     *   c1part = a0b1 +[32] (a0b0 >>>[32] 16)
     *
     * carry_from_lo_*
     *   = a1b1 + (a1b0 + c1part) / 2^16
     *   = a1b1 + (a1b0 + (2^16*(c1part / 2^16) + (c1part % 2^16))) / 2^16
     *   = a1b1 + (2^16*(c1part / 2^16) + (a1b0 + (c1part % 2^16))) / 2^16
     *   = a1b1 + (2^16*(c1part / 2^16) + (a1b0 + (c1part &[32] 0xffff))) / 2^16
     *                                     ----    -------------------
     *                               <= 0xfffe0001     <= 0xffff
     *                                     ----------------------------
     *                              <= 0xffff0000, hence the + does not overflow
     *   = a1b1 + (2^16*(c1part / 2^16) + (a1b0 +[32] (c1part &[32] 0xffff))) / 2^16
     *             --------------------
     *               multiple of 2^16
     *   = a1b1 + ( 2^16*(c1part / 2^16) / 2^16 + (a1b0 +[32] (c1part &[32] 0xffff)) / 2^16 )
     *   = a1b1 + (c1part / 2^16) + (a1b0 +[32] (c1part &[32] 0xffff)) / 2^16
     *             ------            --------------------------------
     *             < 2^32                      < 2^32
     *   = a1b1 + (c1part >>>[32] 16) + ((a1b0 +[32] (c1part &[32] 0xffff)) >>>[32] 16)
     *
     * Recap so far:
     *
     * a *[64] b
     *   = ( 2^32*(ahi *[32] blo) + 2^32*(alo *[32] bhi) + alo*blo ) % 2^64
     * alo*blo
     *   = 2^32*carry_from_lo_* + aloblo
     * aloblo
     *   = (a1b0 <<[32] 16) +[32] (a0b1 <<[32] 16) +[32] a0b0
     * carry_from_lo_*
     *   = a1b1 + (c1part >>>[32] 16) + ((a1b0 +[32] (c1part &[32] 0xffff)) >>>[32] 16)
     *
     * Substituting,
     *
     * a *[64] b
     *   = ( 2^32*(ahi *[32] blo) + 2^32*(alo *[32] bhi) + 2^32*carry_from_lo_* + aloblo ) % 2^64
     *   = ( 2^32*((ahi *[32] blo) + (alo *[32] bhi) + carry_from_lo_*) + aloblo ) % 2^64
     *   = ( 2^32*((ahi *[32] blo) + (alo *[32] bhi) + carry_from_lo_*) % 2^64 + aloblo ) % 2^64
     *       Using (n * x) % (n * m) = (n * (x % m)) with n = m = 2^32 (see proof below)
     *   = ( 2^32*(((ahi *[32] blo) + (alo *[32] bhi) + carry_from_lo_*) % 2^32) + aloblo ) % 2^64
     *   = ( 2^32*((ahi *[32] blo) +[32] (alo *[32] bhi) +[32] (carry_from_lo_* % 2^32)) + aloblo ) % 2^64
     *
     * Lemma: (n * x) % (n * m) = n * (x % m)
     * (n * x) % (n * m)
     *   = (n * x) - ((n * x) / (n * m))*(n * m)   using a % b = a - (a / b)*b
     *   = (n * x) - (x / m)*(n * m)
     *   = n * (x - (x / m)*m)
     *   = n * (x % m)              using again a % b = a - (a / b)*b
     *
     * Since aloblo < 2^32 and the inner sum is also < 2^32:
     *
     * lo = aloblo
     *   = (a1b0 <<[32] 16) +[32] (a0b1 <<[32] 16) +[32] a0b0
     *   = ((a1b0 +[32] a0a1) <<[32] 16) +[32] a0b0
     *
     * hi = (ahi *[32] blo) +[32] (alo *[32] bhi) +[32] (carry_from_lo_* % 2^32)
     *   = (ahi *[32] blo) +[32] (alo *[32] bhi) +[32]
     *        (a1b1 + (c1part >>>[32] 16) + ((a1b0 +[32] (c1part &[32] 0xffff)) >>>[32] 16)) % 2^32
     *   = (ahi *[32] blo) +[32] (alo *[32] bhi) +[32]
     *        a1b1 +[32] (c1part >>>[32] 16) +[32] ((a1b0 +[32] (c1part &[32] 0xffff)) >>>[32] 16)
     */

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
      alo*bhi + ahi*blo + a1 * b1 + (c1part >>> 16) +
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

  @inline
  def toString(lo: Int, hi: Int): String =
    toStringImpl(lo, hi)

  private def toStringImpl(lo: Int, hi: Int): String = {
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
     *
     * The idea is to divide (lo, hi) once by 10^9 and keep the remainder.
     *
     * The remainder must then be < 10^9, and is therefore an int32.
     *
     * The quotient must be <= ULong.MaxValue / 10^9, which is < 2^53, and
     * is therefore a valid double. It must also be non-zero, since
     * (lo, hi) >= 2^53 > 10^9.
     *
     * We should do that single division as a Long division. However, that is
     * slow. We can cheat with a Double division instead.
     *
     * We convert the unsigned value num = (lo, hi) to a Double value
     * approxNum. This is an approximation. It can lose as many as
     * 64 - 53 = 11 low-order bits. Hence |approxNum - num| <= 2^12.
     *
     * We then compute an approximated quotient
     *   approxQuot = floor(approxNum / 10^9)
     * instead of the theoretical value
     *   quot = floor(num / 10^9)
     *
     * Since 10^9 > 2^29 > 2^12, we have |approxNum - num| < 10^9.
     * Therefore, |approxQuot - quot| <= 1.
     *
     * We also have 0 <= approxQuot < 2^53, which means that approxQuot is an
     * "unsigned safe double" and that `approxQuot.toLong` is lossless.
     *
     * At this point, we compute the approximated remainder
     *   approxRem = num - 10^9 * approxQuot.toLong
     * as if with Long arithmetics.
     *
     * Since the theoretical remainder rem = num - 10^9 * quot is such that
     * 0 <= rem < 10^9, and since |approxQuot - quot| <= 1, we have that
     *   -10^9 <= approxRem < 2 * 10^9
     *
     * Interestingly, that range entirely fits within a signed int32.
     * That means approxRem = approxRem.toInt, and therefore
     *
     *   approxRem
     *     = (num - 10^9 * approxQuot.toLong).toInt
     *     = num.toInt - 10^9 * approxQuot.toLong.toInt (thanks to modular arithmetics)
     *     = lo - 10^9 * unsignedSafeDoubleLo(approxQuot)
     *
     * That allows to compute approxRem with Int arithmetics without loss of
     * precision.
     *
     * We can use approxRem to detect and correct the error on approxQuot.
     * If approxRem < 0, correct approxQuot by -1 and approxRem by +10^9.
     * If approxRem >= 10^9, correct them by +1 and -10^9, respectively.
     *
     * After the correction, we know that approxQuot and approxRem are equal
     * to their theoretical counterparts quot and rem. We have successfully
     * computed the correct quotient and remainder without using any Long
     * division.
     *
     * We can finally convert both to strings using the native string
     * conversions, and concatenate the results to produce our final result.
     */

    // constants
    val divisor = 1000000000 // 10^9
    val divisorInv = 1.0 / divisor.toDouble

    // initial approximation of the quotient and remainder
    val approxNum = unsignedToDoubleApprox(lo, hi)
    var approxQuot = scala.scalajs.js.Math.floor(approxNum * divisorInv)
    var approxRem = lo - divisor * unsignedSafeDoubleLo(approxQuot)

    // correct the approximations
    if (approxRem < 0) {
      approxQuot -= 1.0
      approxRem += divisor
    } else if (approxRem >= divisor) {
      approxQuot += 1.0
      approxRem -= divisor
    }

    // build the result string
    val remStr = approxRem.toString()
    approxQuot.toString() + substring("000000000", remStr.length()) + remStr
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

  @inline
  def fromDouble(value: Double): Long =
    fromDoubleImpl(value)

  private def fromDoubleImpl(value: Double): Long = {
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

  @inline
  def divide(alo: Int, ahi: Int, blo: Int, bhi: Int): Long =
    divideImpl(alo, ahi, blo, bhi)

  def divideImpl(alo: Int, ahi: Int, blo: Int, bhi: Int): Long = {
    if (isZero(blo, bhi))
      throw new ArithmeticException("/ by zero")

    if (isInt32(alo, ahi)) {
      if (isInt32(blo, bhi)) {
        if (alo == Int.MinValue && blo == -1)
          0x80000000L
        else
          fromInt(alo / blo)
      } else {
        // Either a == Int.MinValue && b == (Int.MaxValue + 1), or (abs(b) > abs(a))
        if (alo == Int.MinValue && (blo == 0x80000000 && bhi == 0))
          -1L
        else
          0L // because abs(b) > abs(a)
      }
    } else {
      val aAbs = abs(alo, ahi)
      val bAbs = abs(blo, bhi)
      // Calls back into toInt() and shr()
      val absR = unsigned_/(aAbs.toInt, (aAbs >>> 32).toInt, bAbs.toInt, (bAbs >>> 32).toInt)
      if ((ahi ^ bhi) >= 0)
        absR // a and b have the same sign bit
      else
        -absR // calls back into sub()
    }
  }

  @inline
  def divideUnsigned(alo: Int, ahi: Int, blo: Int, bhi: Int): Long =
    divideUnsignedImpl(alo, ahi, blo, bhi)

  def divideUnsignedImpl(alo: Int, ahi: Int, blo: Int, bhi: Int): Long = {
    if (isZero(blo, bhi))
      throw new ArithmeticException("/ by zero")

    if (isUInt32(ahi)) {
      if (isUInt32(bhi))
        pack(Integer.divideUnsigned(alo, blo), 0)
      else
        0L // a < b
    } else {
      unsigned_/(alo, ahi, blo, bhi)
    }
  }

  private def unsigned_/(alo: Int, ahi: Int, blo: Int, bhi: Int): Long = {
    // This method is not called if isInt32(alo, ahi) nor if isZero(blo, bhi)
    if (isUnsignedSafeDouble(ahi)) {
      if (isUnsignedSafeDouble(bhi))
        fromUnsignedSafeDouble(asSafeDouble(alo, ahi) / asSafeDouble(blo, bhi))
      else
        0L // 0L, because b > a
    } else {
      if (bhi == 0 && isPowerOfTwo_IKnowItsNot0(blo)) {
        val pow = log2OfPowerOfTwo(blo)
        pack((alo >>> pow) | (ahi << 1 << (31-pow)), ahi >>> pow)
      } else if (blo == 0 && isPowerOfTwo_IKnowItsNot0(bhi)) {
        val pow = log2OfPowerOfTwo(bhi)
        pack(ahi >>> pow, 0)
      } else {
        unsignedDivModHelper(alo, ahi, blo, bhi, askQuotient = true)
      }
    }
  }

  @inline
  def remainder(alo: Int, ahi: Int, blo: Int, bhi: Int): Long =
    remainderImpl(alo, ahi, blo, bhi)

  def remainderImpl(alo: Int, ahi: Int, blo: Int, bhi: Int): Long = {
    if (isZero(blo, bhi))
      throw new ArithmeticException("/ by zero")

    if (isInt32(alo, ahi)) {
      if (isInt32(blo, bhi)) {
        fromInt(alo % blo)
      } else {
        // Either a == Int.MinValue && b == (Int.MaxValue + 1), or (abs(b) > abs(a))
        if (alo == Int.MinValue && (blo == 0x80000000 && bhi == 0))
          0L
        else
          pack(alo, ahi) // a, because abs(b) > abs(a)
      }
    } else {
      val aAbs = abs(alo, ahi)
      val bAbs = abs(blo, bhi)
      // Calls back into toInt() and shr()
      val absR = unsigned_%(aAbs.toInt, (aAbs >>> 32).toInt, bAbs.toInt, (bAbs >>> 32).toInt)
      if (ahi < 0)
        -absR // calls back into sub()
      else
        absR
    }
  }

  @inline
  def remainderUnsigned(alo: Int, ahi: Int, blo: Int, bhi: Int): Long =
    remainderUnsignedImpl(alo, ahi, blo, bhi)

  def remainderUnsignedImpl(alo: Int, ahi: Int, blo: Int, bhi: Int): Long = {
    if (isZero(blo, bhi))
      throw new ArithmeticException("/ by zero")

    if (isUInt32(ahi)) {
      if (isUInt32(bhi))
        pack(Integer.remainderUnsigned(alo, blo), 0)
      else
        pack(alo, ahi) // a < b
    } else {
      unsigned_%(alo, ahi, blo, bhi)
    }
  }

  private def unsigned_%(alo: Int, ahi: Int, blo: Int, bhi: Int): Long = {
    // This method is not called if isInt32(alo, ahi) nor if isZero(blo, bhi)
    if (isUnsignedSafeDouble(ahi)) {
      if (isUnsignedSafeDouble(bhi))
        fromUnsignedSafeDouble(asSafeDouble(alo, ahi) % asSafeDouble(blo, bhi))
      else
        pack(alo, ahi) // a, because b > a
    } else {
      if (bhi == 0 && isPowerOfTwo_IKnowItsNot0(blo))
        pack(alo & (blo - 1), 0)
      else if (blo == 0 && isPowerOfTwo_IKnowItsNot0(bhi))
        pack(alo, ahi & (bhi - 1))
      else
        unsignedDivModHelper(alo, ahi, blo, bhi, askQuotient = false)
    }
  }

  /** Helper for `unsigned_/` and `unsigned_%`.
   *
   *  If `askQuotient` is true, computes the quotient, otherwise computes the
   *  remainder. Stores the hi word of the result in `hiReturn`, and returns
   *  the lo word.
   */
  private def unsignedDivModHelper(alo: Int, ahi: Int, blo: Int, bhi: Int,
      askQuotient: Boolean): Long = {

    var shift =
      inlineNumberOfLeadingZeros(blo, bhi) - inlineNumberOfLeadingZeros(alo, ahi)

    // var bShift = shl(blo, bhi, shift)
    var bShiftLo = if ((shift & 32) == 0) blo << shift else 0
    var bShiftHi = if ((shift & 32) == 0) (blo >>> 1 >>> (31-shift)) | (bhi << shift) else blo << shift

    var remLo = alo
    var remHi = ahi
    var quotLo = 0
    var quotHi = 0

    /* Invariants:
     *   bShift == b << shift == b * 2^shift
     *   quot >= 0
     *   0 <= rem < 2 * bShift
     *   quot * b + rem == a
     *
     * The loop condition should be
     *   while (shift >= 0 && !isUnsignedSafeDouble(remHi))
     * but we manually inline isUnsignedSafeDouble because remHi is a var. If
     * we let the optimizer inline it, it will first store remHi in a temporary
     * val, which will explose the while condition as a while(true) + if +
     * break, and we don't want that.
     */
    while (shift >= 0 && (remHi & SafeDoubleHiMask) != 0) {
      if (inlineUnsigned_>=(remLo, remHi, bShiftLo, bShiftHi)) {
        // val newRem = rem - bShift
        val newRemLo = remLo - bShiftLo
        val newRemHi = remHi - bShiftHi + (((~remLo & bShiftLo) | (~(remLo ^ bShiftLo) & newRemLo)) >> 31)

        remLo = newRemLo
        remHi = newRemHi
        if (shift < 32)
          quotLo |= (1 << shift)
        else
          quotHi |= (1 << shift) // == (1 << (shift - 32))
      }
      shift -= 1

      // val newBShift = bShift >>> 1
      val newBShiftLo = (bShiftLo >>> 1) | (bShiftHi << 31)
      val newBShiftHi = bShiftHi >>> 1

      bShiftLo = newBShiftLo
      bShiftHi = newBShiftHi
    }

    // Now rem < 2^53, we can finish with a double division
    if (inlineUnsigned_>=(remLo, remHi, blo, bhi)) {
      val remDouble = asSafeDouble(remLo, remHi)
      val bDouble = asSafeDouble(blo, bhi)

      if (askQuotient) {
        val rem_div_bDouble = fromUnsignedSafeDouble(remDouble / bDouble)
        pack(quotLo, quotHi) + rem_div_bDouble // calls back into add()
      } else {
        fromUnsignedSafeDouble(remDouble % bDouble)
      }
    } else {
      if (askQuotient)
        pack(quotLo, quotHi)
      else
        pack(remLo, remHi)
    }
  }

  @inline
  private def substring(s: String, start: Int): String = {
    import scala.scalajs.js.JSStringOps.enableJSStringOps
    s.jsSubstring(start)
  }

  /** Tests whether the long (lo, hi) is 0. */
  @inline def isZero(lo: Int, hi: Int): Boolean =
    (lo | hi) == 0

  /** Tests whether the long (lo, hi)'s mathematical value fits in a signed Int. */
  @inline def isInt32(lo: Int, hi: Int): Boolean =
    hi == (lo >> 31)

  /** Tests whether the long (_, hi)'s mathematical value fits in an unsigned Int. */
  @inline def isUInt32(hi: Int): Boolean =
    hi == 0

  /** Tests whether an unsigned long (lo, hi) is a safe Double.
   *  This test is in fact slightly stricter than necessary, as it tests
   *  whether `x < 2^53`, although x == 2^53 would be a perfectly safe
   *  Double. The reason we do this is that testing `x <= 2^53` is much
   *  slower, as `x == 2^53` basically has to be treated specially.
   *  Since there is virtually no gain to treating 2^53 itself as a safe
   *  Double, compared to all numbers smaller than it, we don't bother, and
   *  stay on the fast side.
   */
  @inline def isUnsignedSafeDouble(hi: Int): Boolean =
    (hi & SafeDoubleHiMask) == 0

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

  /** Converts an unsigned safe double into its Long representation. */
  @inline def fromUnsignedSafeDouble(x: Double): Long =
    pack(unsignedSafeDoubleLo(x), unsignedSafeDoubleHi(x))

  /** Computes the lo part of a long from an unsigned safe double. */
  @inline def unsignedSafeDoubleLo(x: Double): Int =
    rawToInt(x)

  /** Computes the hi part of a long from an unsigned safe double. */
  @inline def unsignedSafeDoubleHi(x: Double): Int =
    rawToInt(x / TwoPow32)

  /** Approximates an unsigned (lo, hi) with a Double. */
  @inline def unsignedToDoubleApprox(lo: Int, hi: Int): Double =
    uintToDouble(hi) * TwoPow32 + uintToDouble(lo)

  /** Approximates a signed (lo, hi) with a Double.
   *
   *  If `hi` is known to be non-negative, this method is equivalent to
   *  `unsignedToDoubleApprox`, but it can fold away part of the computation if
   *  `hi` is in fact constant.
   */
  @inline def signedToDoubleApprox(lo: Int, hi: Int): Double = {
    /* We note a_u the mathematical value of a when interpreted as an unsigned
     * quantity, and a_s when interpreted as a signed quantity.
     *
     * For x = (lo, hi), the result must be the correctly rounded value of x_s.
     *
     * If x_s >= 0, then hi_s >= 0. The obvious mathematical value of x_s is
     *   x_s = hi_s * 2^32 + lo_u
     *
     * If x_s < 0, then hi_s < 0. The fundamental definition of two's
     * completement means that
     *   x_s = -2^64 + hi_u * 2^32 + lo_u
     * Likewise,
     *   hi_s = -2^32 + hi_u
     *
     * Now take the computation for the x_s >= 0 case, but substituting values
     * for the negative case:
     *   hi_s * 2^32 + lo_u
     *     = (-2^32 + hi_u) * 2^32 + lo_u
     *     = (-2^64 + hi_u * 2^32) + lo_u
     * which is the correct mathematical result for x_s in the negative case.
     *
     * Therefore, we can always compute
     *   x_s = hi_s * 2^32 + lo_u
     * When computed with `Double` values, only the last `+` can be inexact,
     * hence the result is correctly round.
     */
    hi.toDouble * TwoPow32 + uintToDouble(lo)
  }

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

  /** Tests whether the given non-zero unsigned Int is an exact power of 2. */
  @inline def isPowerOfTwo_IKnowItsNot0(i: Int): Boolean =
    (i & (i - 1)) == 0

  /** Returns the log2 of the given unsigned Int assuming it is an exact power of 2. */
  @inline def log2OfPowerOfTwo(i: Int): Int =
    31 - Integer.numberOfLeadingZeros(i)

  /** Returns the number of leading zeros in the given long (lo, hi). */
  @inline def inlineNumberOfLeadingZeros(lo: Int, hi: Int): Int =
    if (hi != 0) Integer.numberOfLeadingZeros(hi)
    else Integer.numberOfLeadingZeros(lo) + 32

  /** Tests whether the unsigned long (alo, ahi) is >= (blo, bhi). */
  @inline
  def inlineUnsigned_>=(alo: Int, ahi: Int, blo: Int, bhi: Int): Boolean =
    if (ahi == bhi) inlineUnsignedInt_>=(alo, blo)
    else inlineUnsignedInt_>=(ahi, bhi)

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

  @inline
  def abs(lo: Int, hi: Int): Long = {
    /* The algorithm here is inspired by Hacker's Delight formula for `abs`.
     * However, a naive application of that formula does not give good code for
     * our RuntimeLong implementation.
     *
     * Let a be the input value RTLong(lo, hi). Overall, we want to compute:
     *   val longSign = a >> 63
     *   (a ^ longSign) + (if (longSign) 1L else 0L)
     * The last addition is performed as `- longSign` in the original formula.
     * For our purpose, it is more convenient to take a step back and express
     * it as the if expression.
     *
     * First, observe that `longSign.lo` and `longSign.hi` are both equal to
     * `hi >> 31`. We therefore define
     *   val sign = hi >> 31
     * and rewrite our second expression by expanding the long xor:
     *   RTLong(lo ^ sign, hi ^ sign) + (if (sign) 1L else 0L)
     *
     * Making the lo/hi words of the result of the xor explicit, we get:
     *
     *   val xlo = lo ^ sign
     *   val xhi = hi ^ sign
     *   RTLong(xlo, xhi) + (if (sign) 1L else 0L)
     *
     * At this point, it is convenient to examine what happens when we expand
     * the addition, assuming that the 1L branch is taken. We expand
     * `RTLong(xlo, xhi) + RTLong(1, 0)` and get:
     *
     *   val rlo = xlo + 1
     *   val rhi = xhi + 0 + (((xlo & 1) | ((xlo | 1) & ~rlo)) >>> 31)
     *   val rhi = xhi + (((xlo & 1) | ((xlo | 1) & ~rlo)) >>> 31)
     *
     * Since only the most significant bit of the complicated logic expression
     * is relevant, we can rewrite the 1's as 0's so that `& 0` and `| 0` fold
     * away:
     *
     *   val rhi = xhi + (((xlo & 0) | ((xlo | 0) & ~rlo)) >>> 31)
     *           = xhi + (((      0) | ((xlo    ) & ~rlo)) >>> 31)
     *           = xhi + ((       0  | ( xlo      & ~rlo)) >>> 31)
     *           = xhi + ((            ( xlo      & ~rlo)) >>> 31)
     *           = xhi + ((xlo & ~rlo) >>> 31)
     *
     * If the 0L branch was taken, then we should instead have
     *
     *   val rlo = xlo + 0 = xlo
     *   val rhi = xhi + 0 = xhi
     *
     * Let us put back together everything we have so far:
     *
     *   val sign = hi >> 31
     *   val xlo = lo ^ sign
     *   val xhi = hi ^ sign
     *   val rlo = xlo + (if (sign) 1 else 0)
     *   val rhi = xhi + (if (sign) ((xlo & ~rlo) >>> 31) else 0)
     *
     * We can remove the branch for rlo as
     *
     *   val rlo = xlo - sign
     *
     * Now let's just imagine we always took the then branch for rhi. We would
     * overall get:
     *
     *   val sign = hi >> 31
     *   val xlo = lo ^ sign
     *   val xhi = hi ^ sign
     *   val rlo = xlo - sign
     *   val rhi = xhi + ((xlo & ~rlo) >>> 31)
     *
     * That's correct when `sign = -1`. But what happens when `sign = 0`? Let
     * us work it out:
     *
     *   val sign = 0
     *   val xlo = lo ^ sign = lo
     *   val xhi = hi ^ sign = hi
     *   val rlo = xlo - sign = xlo = lo
     *   val rhi = xhi + ((xlo & ~rlo) >>> 31)
     *           = xhi + (( lo & ~lo) >>> 31)
     *           = xhi + ((    0    ) >>> 31)
     *           = xhi + 0
     *
     * Bingo! It works as well! So we can take the code of the "let's just
     * imagine" step. We inline the rhs of xhi at the only place where it is
     * used, and we get the final algorithm.
     */
    val sign = hi >> 31
    val xlo = lo ^ sign
    val rlo = xlo - sign
    val rhi = (hi ^ sign) + ((xlo & ~rlo) >>> 31)
    pack(rlo, rhi)
  }

}
