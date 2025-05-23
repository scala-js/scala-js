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

import scala.annotation.tailrec

/* IMPORTANT NOTICE about this file
 *
 * The code of RuntimeLong is code-size- and performance critical. The methods
 * of this class are used for every single primitive operation on Longs, and
 * must therefore be as fast as they can.
 *
 * This means that this implementation is oriented for performance over
 * readability and idiomatic code.
 *
 * DRY is applied as much as possible but is bounded by the performance and
 * code size requirements. We use a lot of inline_xyz helpers meant to be used
 * when we already have the parameters on stack, but they are sometimes
 * duplicated in entry points to avoid the explicit extraction of heap fields
 * into temporary variables when they are used only once.
 *
 * Otherwise, we typically extract the lo and hi fields from the heap into
 * local variables once, whether explicitly in vals or implicitly when passed
 * as arguments to inlineable methods. This reduces heap/record accesses, and
 * allows both our optimizer and the JIT to know that we indeed always have the
 * same value (the JIT does not even know that fields are immutable, but even
 * our optimizer does not make use of that information).
 */

/** Emulates a Long on the JavaScript platform. */
@inline
final class RuntimeLong(val lo: Int, val hi: Int) {
  a =>

  import RuntimeLong._

  // Universal equality

  @inline
  override def equals(that: Any): Boolean = that match {
    case b: RuntimeLong => inline_equals(b)
    case _              => false
  }

  @inline override def hashCode(): Int = lo ^ hi

  // String operations

  @inline override def toString(): String =
    RuntimeLong.toString(lo, hi)

  // Conversions

  @inline def toByte: Byte = lo.toByte
  @inline def toShort: Short = lo.toShort
  @inline def toChar: Char = lo.toChar
  @inline def toInt: Int = lo
  @inline def toLong: Long = this.asInstanceOf[Long]
  @inline def toFloat: Float = RuntimeLong.toFloat(lo, hi)
  @inline def toDouble: Double = RuntimeLong.toDouble(lo, hi)

  // java.lang.Number

  @inline def byteValue(): Byte = toByte
  @inline def shortValue(): Short = toShort
  @inline def intValue(): Int = toInt
  @inline def longValue(): Long = toLong
  @inline def floatValue(): Float = toFloat
  @inline def doubleValue(): Double = toDouble

  // java.lang.Comparable, including bridges

  @inline
  def compareTo(that: Object): Int =
    compareTo(that.asInstanceOf[RuntimeLong])

  @inline
  def compareTo(that: java.lang.Long): Int =
    compareTo(that.asInstanceOf[RuntimeLong])

  // Comparisons

  @inline
  def compareTo(b: RuntimeLong): Int =
    RuntimeLong.compare(a.lo, a.hi, b.lo, b.hi)

  @inline
  private def inline_equals(b: RuntimeLong): Boolean =
    a.lo == b.lo && a.hi == b.hi

  @inline
  def equals(b: RuntimeLong): Boolean =
    inline_equals(b)

  @inline
  def notEquals(b: RuntimeLong): Boolean =
    !inline_equals(b)

  @inline
  def <(b: RuntimeLong): Boolean = {
    /* We should use `inlineUnsignedInt_<(a.lo, b.lo)`, but that first extracts
     * a.lo and b.lo into local variables, which cause the if/else not to be
     * a valid JavaScript expression anymore. This causes useless explosion of
     * JavaScript code at call site, when inlined. So we manually inline
     * `inlineUnsignedInt_<(a.lo, b.lo)` to avoid that problem.
     */
    val ahi = a.hi
    val bhi = b.hi
    if (ahi == bhi) (a.lo ^ 0x80000000) < (b.lo ^ 0x80000000)
    else ahi < bhi
  }

  @inline
  def <=(b: RuntimeLong): Boolean = {
    /* Manually inline `inlineUnsignedInt_<=(a.lo, b.lo)`.
     * See the comment in `<` for the rationale.
     */
    val ahi = a.hi
    val bhi = b.hi
    if (ahi == bhi) (a.lo ^ 0x80000000) <= (b.lo ^ 0x80000000)
    else ahi < bhi
  }

  @inline
  def >(b: RuntimeLong): Boolean = {
    /* Manually inline `inlineUnsignedInt_>a.lo, b.lo)`.
     * See the comment in `<` for the rationale.
     */
    val ahi = a.hi
    val bhi = b.hi
    if (ahi == bhi) (a.lo ^ 0x80000000) > (b.lo ^ 0x80000000)
    else ahi > bhi
  }

  @inline
  def >=(b: RuntimeLong): Boolean = {
    /* Manually inline `inlineUnsignedInt_>=(a.lo, b.lo)`.
     * See the comment in `<` for the rationale.
     */
    val ahi = a.hi
    val bhi = b.hi
    if (ahi == bhi) (a.lo ^ 0x80000000) >= (b.lo ^ 0x80000000)
    else ahi > bhi
  }

  // Bitwise operations

  @inline
  def unary_~ : RuntimeLong = // scalastyle:ignore
    new RuntimeLong(~lo, ~hi)

  @inline
  def |(b: RuntimeLong): RuntimeLong =
    new RuntimeLong(a.lo | b.lo, a.hi | b.hi)

  @inline
  def &(b: RuntimeLong): RuntimeLong =
    new RuntimeLong(a.lo & b.lo, a.hi & b.hi)

  @inline
  def ^(b: RuntimeLong): RuntimeLong =
    new RuntimeLong(a.lo ^ b.lo, a.hi ^ b.hi)

  // Shifts

  /** Shift left */
  @inline
  def <<(n: Int): RuntimeLong = {
    /* This should *reasonably* be:
     *   val n1 = n & 63
     *   if (n1 < 32)
     *     new RuntimeLong(lo << n1, if (n1 == 0) hi else (lo >>> 32-n1) | (hi << n1))
     *   else
     *     new RuntimeLong(0, lo << n1)
     *
     * Replacing n1 by its definition, we have:
     *   if (n & 63 < 32)
     *     new RuntimeLong(lo << (n & 63),
     *         if ((n & 63) == 0) hi else (lo >>> 32-(n & 63)) | (hi << (n & 63)))
     *   else
     *     new RuntimeLong(0, lo << (n & 63))
     *
     * Since the values on the rhs of shifts are always in arithmetic mod 32,
     * we can get:
     *   if (n & 63 < 32)
     *     new RuntimeLong(lo << n, if ((n & 63) == 0) hi else (lo >>> -n) | (hi << n))
     *   else
     *     new RuntimeLong(0, lo << n)
     *
     * The condition `n & 63 < 32` is equivalent to
     *   (n & 63) & 32 == 0
     *   n & (63 & 32) == 0
     *   n & 32 == 0
     *
     * In the then part, we have `n & 32 == 0` hence `n & 63 == n & 31`:
     *   new RuntimeLong(lo << n, if ((n & 31) == 0) hi else (lo >>> -n) | (hi << n))
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
     *     new RuntimeLong(lo << n, (lo >>> 1 >>> (31-n)) | (hi << n))
     *   else
     *     new RuntimeLong(0, lo << n)
     *
     * If we distribute the condition in the lo and hi arguments of the
     * constructors, we get a version with only one RuntimeLong output, which
     * avoids reification as records by the optimizer, yielding shorter code.
     * It is potentially slightly less efficient, except when `n` is constant,
     * which is often the case anyway.
     *
     * Finally we have:
     */
    val lo = this.lo
    new RuntimeLong(
        if ((n & 32) == 0) lo << n else 0,
        if ((n & 32) == 0) (lo >>> 1 >>> (31-n)) | (hi << n) else lo << n)
  }

  /** Logical shift right */
  @inline
  def >>>(n: Int): RuntimeLong = {
    // This derives in a similar way as in <<
    val hi = this.hi
    new RuntimeLong(
        if ((n & 32) == 0) (lo >>> n) | (hi << 1 << (31-n)) else hi >>> n,
        if ((n & 32) == 0) hi >>> n else 0)
  }

  /** Arithmetic shift right */
  @inline
  def >>(n: Int): RuntimeLong = {
    // This derives in a similar way as in <<
    val hi = this.hi
    new RuntimeLong(
        if ((n & 32) == 0) (lo >>> n) | (hi << 1 << (31-n)) else hi >> n,
        if ((n & 32) == 0) hi >> n else hi >> 31)
  }

  // Arithmetic operations

  @inline
  def unary_- : RuntimeLong = { // scalastyle:ignore
    val lo = this.lo
    val hi = this.hi
    new RuntimeLong(inline_lo_unary_-(lo), inline_hi_unary_-(lo, hi))
  }

  @inline
  def +(b: RuntimeLong): RuntimeLong = {
    val alo = a.lo
    val ahi = a.hi
    val bhi = b.hi
    val lo = alo + b.lo
    new RuntimeLong(lo,
        if (inlineUnsignedInt_<(lo, alo)) ahi + bhi + 1 else ahi + bhi)
  }

  @inline
  def -(b: RuntimeLong): RuntimeLong = {
    val alo = a.lo
    val ahi = a.hi
    val bhi = b.hi
    val lo = alo - b.lo
    new RuntimeLong(lo,
        if (inlineUnsignedInt_>(lo, alo)) ahi - bhi - 1 else ahi - bhi)
  }

  @inline
  def *(b: RuntimeLong): RuntimeLong = {
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

    val alo = a.lo
    val blo = b.lo

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
      alo*b.hi + a.hi*blo + a1 * b1 + (c1part >>> 16) +
      (((c1part & 0xffff) + a1b0) >>> 16) // collapses to 0 when a1b0 = 0
    }

    new RuntimeLong(lo, hi)
  }

  @inline
  def /(b: RuntimeLong): RuntimeLong =
    RuntimeLong.divide(a, b)

  /** `java.lang.Long.divideUnsigned(a, b)` */
  @inline
  def divideUnsigned(b: RuntimeLong): RuntimeLong =
    RuntimeLong.divideUnsigned(a, b)

  @inline
  def %(b: RuntimeLong): RuntimeLong =
    RuntimeLong.remainder(a, b)

  /** `java.lang.Long.remainderUnsigned(a, b)` */
  @inline
  def remainderUnsigned(b: RuntimeLong): RuntimeLong =
    RuntimeLong.remainderUnsigned(a, b)

  /** Computes `longBitsToDouble(this)`.
   *
   *  `fpBitsDataView` must be a scratch `js.typedarray.DataView` whose
   *  underlying buffer is at least 8 bytes long.
   */
  @inline
  def bitsToDouble(fpBitsDataView: scala.scalajs.js.typedarray.DataView): Double = {
    fpBitsDataView.setInt32(0, lo, littleEndian = true)
    fpBitsDataView.setInt32(4, hi, littleEndian = true)
    fpBitsDataView.getFloat64(0, littleEndian = true)
  }

}

object RuntimeLong {
  private final val TwoPow32 = 4294967296.0
  private final val TwoPow63 = 9223372036854775808.0

  /** The magical mask that allows to test whether an unsigned long is a safe
   *  double.
   *  @see isUnsignedSafeDouble
   */
  private final val UnsignedSafeDoubleHiMask = 0xffe00000

  private final val AskQuotient = 0
  private final val AskRemainder = 1
  private final val AskToString = 2

  /** The hi part of a (lo, hi) return value. */
  private[this] var hiReturn: Int = _

  private def toString(lo: Int, hi: Int): String = {
    if (isInt32(lo, hi)) {
      lo.toString()
    } else if (hi < 0) {
      "-" + toUnsignedString(inline_lo_unary_-(lo), inline_hi_unary_-(lo, hi))
    } else {
      toUnsignedString(lo, hi)
    }
  }

  private def toUnsignedString(lo: Int, hi: Int): String = {
    // This is called only if (lo, hi) is not an Int32

    if (isUnsignedSafeDouble(hi)) {
      // (lo, hi) is small enough to be a Double, use that directly
      asUnsignedSafeDouble(lo, hi).toString
    } else {
      /* At this point, (lo, hi) >= 2^53.
       * We divide (lo, hi) once by 10^9 and keep the remainder.
       *
       * The remainder must then be < 10^9, and is therefore an int32.
       *
       * The quotient must be <= ULong.MaxValue / 10^9, which is < 2^53, and
       * is therefore a valid double. It must also be non-zero, since
       * (lo, hi) >= 2^53 > 10^9.
       *
       * To avoid allocating a tuple with the quotient and remainder, we push
       * the final conversion to string inside unsignedDivModHelper. According
       * to micro-benchmarks, this optimization makes toString 25% faster in
       * this branch.
       */
      unsignedDivModHelper(lo, hi, 1000000000, 0,
          AskToString).asInstanceOf[String]
    }
  }

  private def toDouble(lo: Int, hi: Int): Double = {
    if (hi < 0) {
      // We do asUint() on the hi part specifically for MinValue
      -(asUint(inline_hi_unary_-(lo, hi)) * TwoPow32 +
          asUint(inline_lo_unary_-(lo)))
    } else {
      hi * TwoPow32 + asUint(lo)
    }
  }

  private def toFloat(lo: Int, hi: Int): Float = {
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
     * First, we take the absolute value of the input. We will negate the
     * result at the end if the input was negative.
     *
     * Second, if the abs input is an unsigned safe Double, then the conversion
     * to double is lossless, so we don't have to do anything special
     * (`y == x` in terms of the above explanation).
     *
     * Otherwise, we know that the input's highest 1 bit is in the 11
     * highest-order bits. That means that rounding to float, which only has 24
     * bits in the significand, can only take into account the
     * `11 + 23 + 1 = 35` highest-order bits (the `+ 1` is for the rounding
     * bit). The remaining bits can only affect the result by two states:
     * either they are all 0's, or there is at least one 1. We use that
     * property to "compress" the 16 low-order bits into a single 0 or 1 bit
     * representing those two states. The compressed Long value
     * `y = (compressedAbsLo, abs.hi)` has at most `32 + 17 = 49` significant
     * bits. Therefore its conversion to Double is lossless.
     *
     * Now that we always have a lossless compression to Double, we can perform
     * it, followed by a conversion from Double to Float, which will apply the
     * appropriate rounding.
     *
     * (A similar strategy is used in `parseFloat` for the hexadecimal format.)
     */

    val abs = inline_abs(lo, hi)
    val compressedAbsLo =
      if (isUnsignedSafeDouble(abs.hi) || (abs.lo & 0xffff) == 0) abs.lo
      else (abs.lo & ~0xffff) | 0x8000

    // We do asUint() on the hi part specifically for MinValue
    val absRes = (asUint(abs.hi) * TwoPow32 + asUint(compressedAbsLo))

    (if (hi < 0) -absRes else absRes).toFloat
  }

  @inline
  def fromInt(value: Int): RuntimeLong =
    new RuntimeLong(value, value >> 31)

  @inline
  def fromDouble(value: Double): RuntimeLong = {
    val lo = fromDoubleImpl(value)
    new RuntimeLong(lo, hiReturn)
  }

  private def fromDoubleImpl(value: Double): Int = {
    /* When value is NaN, the conditions of the 3 `if`s are false, and we end
     * up returning (NaN | 0, (NaN / TwoPow32) | 0), which is correctly (0, 0).
     */

    if (value < -TwoPow63) {
      hiReturn = 0x80000000
      0
    } else if (value >= TwoPow63) {
      hiReturn = 0x7fffffff
      0xffffffff
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
      hiReturn = if (value < 0 && rawLo != 0) rawHi - 1 else rawHi
      rawLo
    }
  }

  /** Computes `doubleToLongBits(value)`.
   *
   *  `fpBitsDataView` must be a scratch `js.typedarray.DataView` whose
   *  underlying buffer is at least 8 bytes long.
   */
  @inline
  def fromDoubleBits(value: Double,
      fpBitsDataView: scala.scalajs.js.typedarray.DataView): RuntimeLong = {

    fpBitsDataView.setFloat64(0, value, littleEndian = true)
    new RuntimeLong(
      fpBitsDataView.getInt32(0, littleEndian = true),
      fpBitsDataView.getInt32(4, littleEndian = true)
    )
  }

  private def compare(alo: Int, ahi: Int, blo: Int, bhi: Int): Int = {
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
  def multiplyFull(a: Int, b: Int): RuntimeLong = {
    /* We use Hacker's Delight, Section 8-2, Figure 8-2, to compute the hi
     * word of the result. We reuse intermediate products to compute the lo
     * word, like we do in `RuntimeLong.*`.
     *
     * We swap the role of a1b0 and a0b1 compared to Hacker's Delight, to
     * optimize for the case where a1b0 collapses to 0, like we do in
     * `RuntimeLong.*`. The optimizer normalizes constants in multiplyFull to
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

    new RuntimeLong(lo, hi)
  }

  @inline
  def divide(a: RuntimeLong, b: RuntimeLong): RuntimeLong = {
    val lo = divideImpl(a.lo, a.hi, b.lo, b.hi)
    new RuntimeLong(lo, hiReturn)
  }

  def divideImpl(alo: Int, ahi: Int, blo: Int, bhi: Int): Int = {
    if (isZero(blo, bhi))
      throw new ArithmeticException("/ by zero")

    if (isInt32(alo, ahi)) {
      if (isInt32(blo, bhi)) {
        if (alo == Int.MinValue && blo == -1) {
          hiReturn = 0
          Int.MinValue
        } else {
          val lo = alo / blo
          hiReturn = lo >> 31
          lo
        }
      } else {
        // Either a == Int.MinValue && b == (Int.MaxValue + 1), or (abs(b) > abs(a))
        if (alo == Int.MinValue && (blo == 0x80000000 && bhi == 0)) {
          hiReturn = -1
          -1
        } else {
          // 0L, because abs(b) > abs(a)
          hiReturn = 0
          0
        }
      }
    } else {
      val aAbs = inline_abs(alo, ahi)
      val bAbs = inline_abs(blo, bhi)
      val absRLo = unsigned_/(aAbs.lo, aAbs.hi, bAbs.lo, bAbs.hi)
      if ((ahi ^ bhi) >= 0) absRLo // a and b have the same sign bit
      else inline_hiReturn_unary_-(absRLo, hiReturn)
    }
  }

  @inline
  def divideUnsigned(a: RuntimeLong, b: RuntimeLong): RuntimeLong = {
    val lo = divideUnsignedImpl(a.lo, a.hi, b.lo, b.hi)
    new RuntimeLong(lo, hiReturn)
  }

  def divideUnsignedImpl(alo: Int, ahi: Int, blo: Int, bhi: Int): Int = {
    if (isZero(blo, bhi))
      throw new ArithmeticException("/ by zero")

    if (isUInt32(ahi)) {
      if (isUInt32(bhi)) {
        hiReturn = 0
        Integer.divideUnsigned(alo, blo)
      } else {
        // a < b
        hiReturn = 0
        0
      }
    } else {
      unsigned_/(alo, ahi, blo, bhi)
    }
  }

  private def unsigned_/(alo: Int, ahi: Int, blo: Int, bhi: Int): Int = {
    // This method is not called if isInt32(alo, ahi) nor if isZero(blo, bhi)
    if (isUnsignedSafeDouble(ahi)) {
      if (isUnsignedSafeDouble(bhi)) {
        val aDouble = asUnsignedSafeDouble(alo, ahi)
        val bDouble = asUnsignedSafeDouble(blo, bhi)
        val rDouble = aDouble / bDouble
        hiReturn = unsignedSafeDoubleHi(rDouble)
        unsignedSafeDoubleLo(rDouble)
      } else {
        // 0L, because b > a
        hiReturn = 0
        0
      }
    } else {
      if (bhi == 0 && isPowerOfTwo_IKnowItsNot0(blo)) {
        val pow = log2OfPowerOfTwo(blo)
        hiReturn = ahi >>> pow
        (alo >>> pow) | (ahi << 1 << (31-pow))
      } else if (blo == 0 && isPowerOfTwo_IKnowItsNot0(bhi)) {
        val pow = log2OfPowerOfTwo(bhi)
        hiReturn = 0
        ahi >>> pow
      } else {
        unsignedDivModHelper(alo, ahi, blo, bhi, AskQuotient).asInstanceOf[Int]
      }
    }
  }

  @inline
  def remainder(a: RuntimeLong, b: RuntimeLong): RuntimeLong = {
    val lo = remainderImpl(a.lo, a.hi, b.lo, b.hi)
    new RuntimeLong(lo, hiReturn)
  }

  def remainderImpl(alo: Int, ahi: Int, blo: Int, bhi: Int): Int = {
    if (isZero(blo, bhi))
      throw new ArithmeticException("/ by zero")

    if (isInt32(alo, ahi)) {
      if (isInt32(blo, bhi)) {
        if (blo != -1) {
          val lo = alo % blo
          hiReturn = lo >> 31
          lo
        } else {
          // Work around https://github.com/ariya/phantomjs/issues/12198
          hiReturn = 0
          0
        }
      } else {
        // Either a == Int.MinValue && b == (Int.MaxValue + 1), or (abs(b) > abs(a))
        if (alo == Int.MinValue && (blo == 0x80000000 && bhi == 0)) {
          hiReturn = 0
          0
        } else {
          // a, because abs(b) > abs(a)
          hiReturn = ahi
          alo
        }
      }
    } else {
      val aAbs = inline_abs(alo, ahi)
      val bAbs = inline_abs(blo, bhi)
      val absRLo = unsigned_%(aAbs.lo, aAbs.hi, bAbs.lo, bAbs.hi)
      if (ahi < 0) inline_hiReturn_unary_-(absRLo, hiReturn)
      else absRLo
    }
  }

  @inline
  def remainderUnsigned(a: RuntimeLong, b: RuntimeLong): RuntimeLong = {
    val lo = remainderUnsignedImpl(a.lo, a.hi, b.lo, b.hi)
    new RuntimeLong(lo, hiReturn)
  }

  def remainderUnsignedImpl(alo: Int, ahi: Int, blo: Int, bhi: Int): Int = {
    if (isZero(blo, bhi))
      throw new ArithmeticException("/ by zero")

    if (isUInt32(ahi)) {
      if (isUInt32(bhi)) {
        hiReturn = 0
        Integer.remainderUnsigned(alo, blo)
      } else {
        // a < b
        hiReturn = ahi
        alo
      }
    } else {
      unsigned_%(alo, ahi, blo, bhi)
    }
  }

  private def unsigned_%(alo: Int, ahi: Int, blo: Int, bhi: Int): Int = {
    // This method is not called if isInt32(alo, ahi) nor if isZero(blo, bhi)
    if (isUnsignedSafeDouble(ahi)) {
      if (isUnsignedSafeDouble(bhi)) {
        val aDouble = asUnsignedSafeDouble(alo, ahi)
        val bDouble = asUnsignedSafeDouble(blo, bhi)
        val rDouble = aDouble % bDouble
        hiReturn = unsignedSafeDoubleHi(rDouble)
        unsignedSafeDoubleLo(rDouble)
      } else {
        // a, because b > a
        hiReturn = ahi
        alo
      }
    } else {
      if (bhi == 0 && isPowerOfTwo_IKnowItsNot0(blo)) {
        hiReturn = 0
        alo & (blo - 1)
      } else if (blo == 0 && isPowerOfTwo_IKnowItsNot0(bhi)) {
        hiReturn = ahi & (bhi - 1)
        alo
      } else {
        unsignedDivModHelper(alo, ahi, blo, bhi, AskRemainder).asInstanceOf[Int]
      }
    }
  }

  /** Helper for `unsigned_/`, `unsigned_%` and `toUnsignedString()`.
   *
   *  The value of `ask` may be one of:
   *
   *  - `AskQuotient`: returns the quotient (with the hi part in `hiReturn`)
   *  - `AskRemainder`: returns the remainder (with the hi part in `hiReturn`)
   *  - `AskToString`: returns the conversion of `(alo, ahi)` to string.
   *    In this case, `blo` must be 10^9 and `bhi` must be 0.
   */
  private def unsignedDivModHelper(alo: Int, ahi: Int, blo: Int, bhi: Int,
      ask: Int): Any = {

    var shift =
      inlineNumberOfLeadingZeros(blo, bhi) - inlineNumberOfLeadingZeros(alo, ahi)
    val initialBShift = new RuntimeLong(blo, bhi) << shift
    var bShiftLo = initialBShift.lo
    var bShiftHi = initialBShift.hi
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
    while (shift >= 0 && (remHi & UnsignedSafeDoubleHiMask) != 0) {
      if (inlineUnsigned_>=(remLo, remHi, bShiftLo, bShiftHi)) {
        val newRem =
          new RuntimeLong(remLo, remHi) - new RuntimeLong(bShiftLo, bShiftHi)
        remLo = newRem.lo
        remHi = newRem.hi
        if (shift < 32)
          quotLo |= (1 << shift)
        else
          quotHi |= (1 << shift) // == (1 << (shift - 32))
      }
      shift -= 1
      val newBShift = new RuntimeLong(bShiftLo, bShiftHi) >>> 1
      bShiftLo = newBShift.lo
      bShiftHi = newBShift.hi
    }

    // Now rem < 2^53, we can finish with a double division
    if (inlineUnsigned_>=(remLo, remHi, blo, bhi)) {
      val remDouble = asUnsignedSafeDouble(remLo, remHi)
      val bDouble = asUnsignedSafeDouble(blo, bhi)

      if (ask != AskRemainder) {
        val rem_div_bDouble = fromUnsignedSafeDouble(remDouble / bDouble)
        val newQuot = new RuntimeLong(quotLo, quotHi) + rem_div_bDouble
        quotLo = newQuot.lo
        quotHi = newQuot.hi
      }

      if (ask != AskQuotient) {
        val rem_mod_bDouble = remDouble % bDouble
        remLo = unsignedSafeDoubleLo(rem_mod_bDouble)
        remHi = unsignedSafeDoubleHi(rem_mod_bDouble)
      }
    }

    if (ask == AskQuotient) {
      hiReturn = quotHi
      quotLo
    } else if (ask == AskRemainder) {
      hiReturn = remHi
      remLo
    } else {
      // AskToString (recall that b = 10^9 in this case)
      val quot = asUnsignedSafeDouble(quotLo, quotHi) // != 0
      val remStr = remLo.toString // remHi is always 0
      quot.toString + substring("000000000", remStr.length) + remStr
    }
  }

  @inline
  private def inline_hiReturn_unary_-(lo: Int, hi: Int): Int = {
    hiReturn = inline_hi_unary_-(lo, hi)
    inline_lo_unary_-(lo)
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
    (hi & UnsignedSafeDoubleHiMask) == 0

  /** Converts an unsigned safe double into its Double representation. */
  @inline def asUnsignedSafeDouble(lo: Int, hi: Int): Double =
    hi * TwoPow32 + asUint(lo)

  /** Converts an unsigned safe double into its RuntimeLong representation. */
  @inline def fromUnsignedSafeDouble(x: Double): RuntimeLong =
    new RuntimeLong(unsignedSafeDoubleLo(x), unsignedSafeDoubleHi(x))

  /** Computes the lo part of a long from an unsigned safe double. */
  @inline def unsignedSafeDoubleLo(x: Double): Int =
    rawToInt(x)

  /** Computes the hi part of a long from an unsigned safe double. */
  @inline def unsignedSafeDoubleHi(x: Double): Int =
    rawToInt(x / TwoPow32)

  /** Interprets an `Int` as an unsigned integer and returns its value as a
   *  `Double`.
   */
  @inline def asUint(x: Int): Double = {
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
  def inlineUnsignedInt_>(a: Int, b: Int): Boolean =
    (a ^ 0x80000000) > (b ^ 0x80000000)

  @inline
  def inlineUnsignedInt_>=(a: Int, b: Int): Boolean =
    (a ^ 0x80000000) >= (b ^ 0x80000000)

  @inline
  def inline_lo_unary_-(lo: Int): Int =
    -lo

  @inline
  def inline_hi_unary_-(lo: Int, hi: Int): Int =
    if (lo != 0) ~hi else -hi

  @inline
  def inline_abs(lo: Int, hi: Int): RuntimeLong = {
    if (hi < 0)
      new RuntimeLong(inline_lo_unary_-(lo), inline_hi_unary_-(lo, hi))
    else
      new RuntimeLong(lo, hi)
  }

}
