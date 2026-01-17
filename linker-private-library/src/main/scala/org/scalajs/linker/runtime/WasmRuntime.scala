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

object WasmRuntime {

  /** `fmod` for `Double`s, the implementation of `Double_%`.
   *
   *  Floating point remainders are specified by
   *  https://262.ecma-international.org/#sec-numeric-types-number-remainder
   *  which says that it is equivalent to the C library function `fmod`.
   *
   *  This method is a translation to Scala of the Rust generic implementation,
   *  which can be found at
   *  https://github.com/rust-lang/libm/blob/c9672e5a1a75bfa82981b6240b7bc3ed3524b8b3/src/math/generic/fmod.rs
   *  under the MIT license
   *
   *  (The naive function `x - trunc(x / y) * y` that we can find on the
   *  Web does not work.)
   */
  @inline // inline into the static forwarder, which will be the entry point
  def fmodd(x: Double, y: Double): Double = {
    // Configuration that depends on the floating-point type and its corresponding integer type

    type FType = Double
    type IType = Long

    val mbits = 52
    val ebits = 11
    val fZero = 0.0
    val iZero = 0L
    val iOne = 1L

    @inline def toBits(v: FType): IType = java.lang.Double.doubleToRawLongBits(v)
    @inline def fromBits(v: IType): FType = java.lang.Double.longBitsToDouble(v)
    @inline def leadingZeros(v: IType): Int = java.lang.Long.numberOfLeadingZeros(v)
    @inline def extendFromInt(v: Int): IType = v.toLong
    @inline def wrapToInt(v: IType): Int = v.toInt

    /* Usually we use an @inline def to specialize code like this. Here,
     * copy-pasting is (unfortunately) better. There are lot of primitive
     * numeric operations (arithmetics, shifts, comparisons) that are correctly
     * resolved because they have the same name, which works with the
     * copy-paste. They don't have a common abstraction, though, so the @inline
     * def would not work.
     */

    // --- BEGIN COPY-PASTE wrt. fmodf ----------------------------------------

    // scalastyle:off return

    // --- BEGIN MIT license

    // derived constants
    val mmask: IType = (iOne << mbits) - iOne
    val emask: Int = (1 << ebits) - 1
    val SignBit: IType = iOne << (mbits + ebits)

    /* let mut ix = x.to_bits();
     * let mut iy = y.to_bits();
     */
    var ix: IType = toBits(x)
    var iy: IType = toBits(y)

    /* let sx = ix & F::SIGN_MASK;
     * The Rust implementation does this after initializing ex and ey. We must
     * do it now, *before* masking off the sign bits.
     */
    val sx: IType = ix & SignBit

    /* The Rust implementation exploits the unsinged semantics of `ix` and
     * `iy`. In Scala, this is a bit awkward to do. Instead, we mask off the
     * sign bit of both now, so we have less work to do later.
     */
    ix &= ~SignBit
    iy &= ~SignBit

    /* let mut ex = x.ex().signed();
     * let mut ey = y.ex().signed();
     * -> wrapToInt(ix >>> mbits) & emask
     *    but `& emask` is redundant since the sign bit is already 0 for us
     */
    var ex: Int = wrapToInt(ix >>> mbits)
    var ey: Int = wrapToInt(iy >>> mbits)

    /* if iy << 1 == zero || y.is_nan() || ex == F::EXP_SAT as i32 {
     *     return (x * y) / (x * y);
     * }
     *
     * `iy << 1 == zero` is the same as `iy == zero` for us, since we masked
     * off the sign bit.
     */
    if (iy == iZero || y != y || ex == emask)
      return (x * y) / (x * y)

    /* if ix << 1 <= iy << 1 {
     *     if ix << 1 == iy << 1 {
     *         return F::ZERO * x;
     *     }
     *     return x;
     * }
     * Since we masked off the sign bit already, all the `<< 1` above are not
     * necessary. That also means that our signed comparison does the right
     * thing.
     */
    if (ix <= iy) { // semantically, this tests whether abs(x) <= abs(y) at this point
      if (ix == iy)
        return fZero * x
      return x
    }

    /* if ex == 0 {
     *     let i = ix << (F::EXP_BITS + 1);
     *     ex -= i.leading_zeros() as i32;
     *     ix <<= -ex + 1;
     * } else {
     *     ix &= F::Int::MAX >> (F::EXP_BITS + 1); // this is mmask
     *     ix |= one << F::SIG_BITS;
     * }
     * and same for y
     *
     * Note that the two `F::EXP_BITS + 1` do *not* have the `+ 1` in the
     * original Rust implementation. The `+ 1`s I added account for the sign
     * bit, in addition to the exponent bits.
     *
     * I believe the first one is just wrong, and fails to correctly handle
     * subnormal values. Removing that one causes the tests for subnormals to
     * fail.
     *
     * The second one makes no actual difference, since the bit that is
     * affected is anyway or'ed back to 1 on the last line. However to me it
     * makes more sense to have it, as it then really corresponds to `mmask`,
     * and we use it to mask mantissa bits.
     */
    if (ex == 0) {
      // subnormal
      ex -= leadingZeros(ix << (ebits + 1))
      ix <<= -ex + 1
    } else {
      // normal form
      ix = (ix & mmask) | (iOne << mbits)
    }
    if (ey == 0) {
      // subnormal
      ey -= leadingZeros(iy << (ebits + 1))
      iy <<= -ey + 1
    } else {
      // normal form
      iy = (iy & mmask) | (iOne << mbits)
    }

    // x mod y

    /* while ex > ey {
     *     let i = ix.wrapping_sub(iy);
     *     if i >> (F::BITS - 1) == zero { // i.e., i >= 0 with signed semantics
     *         if i == zero {
     *             return F::ZERO * x;
     *         }
     *         ix = i;
     *     }
     *
     *     ix <<= 1;
     *     ex -= 1;
     * }
     *
     * let i = ix.wrapping_sub(iy);
     * if i >> (F::BITS - 1) == zero {
     *     if i == zero {
     *         return F::ZERO * x;
     *     }
     *     ix = i;
     * }
     *
     * but we factor out the duplicate block inside the loop, taking
     * advantage of a funny-looking "while loop with condition in the middle".
     */
    while ({
      val i = ix - iy
      if (i >= iZero) {
        if (i == iZero)
          return fZero * x
        ix = i
      }

      ex > ey // the actual condition of the while loop
    }) {
      ix <<= 1
      ex -= 1
    }

    // re-normalize the result in ix

    /* let shift = ix.leading_zeros().saturating_sub(F::EXP_BITS);
     * ix <<= shift;
     * ex -= shift as i32;
     */
    val shift = leadingZeros(ix) - ebits
    ix <<= shift
    ex -= shift

    // scale result

    /* if ex > 0 {
     *     ix -= one << F::SIG_BITS;
     *     ix |= F::Int::cast_from(ex) << F::SIG_BITS;
     * } else {
     *     ix >>= -ex + 1;
     * }
     */
    if (ex > 0) {
      // normal form
      ix = (ix - (iOne << mbits)) | (extendFromInt(ex) << mbits)
    } else {
      // subnormal
      ix >>>= -ex + 1
    }

    // integrate the sign bit and return

    /* ix |= sx;
     * F::from_bits(ix)
     */
    fromBits(ix | sx)

    // --- END MIT license

    // scalastyle:on return

    // --- END COPY-PASTE wrt. fmodf ------------------------------------------
  }

  /** `fmod` for `Float`s, the implementation of `Float_%`.
   *
   *  See `fmodd` for details.
   */
  @inline // inline into the static forwarder, which will be the entry point
  def fmodf(x: Float, y: Float): Float = {
    // Configuration that depends on the floating-point type and its corresponding integer type

    type FType = Float
    type IType = Int

    val mbits = 23
    val ebits = 8
    val fZero = 0.0f
    val iZero = 0
    val iOne = 1

    @inline def toBits(v: FType): IType = java.lang.Float.floatToRawIntBits(v)
    @inline def fromBits(v: IType): FType = java.lang.Float.intBitsToFloat(v)
    @inline def leadingZeros(v: IType): Int = java.lang.Integer.numberOfLeadingZeros(v)
    @inline def extendFromInt(v: Int): IType = v
    @inline def wrapToInt(v: IType): Int = v

    // --- BEGIN COPY-PASTE wrt. fmodd ----------------------------------------

    // scalastyle:off return

    // --- BEGIN MIT license

    // derived constants
    val mmask: IType = (iOne << mbits) - iOne
    val emask: Int = (1 << ebits) - 1
    val SignBit: IType = iOne << (mbits + ebits)

    /* let mut ix = x.to_bits();
     * let mut iy = y.to_bits();
     */
    var ix: IType = toBits(x)
    var iy: IType = toBits(y)

    /* let sx = ix & F::SIGN_MASK;
     * The Rust implementation does this after initializing ex and ey. We must
     * do it now, *before* masking off the sign bits.
     */
    val sx: IType = ix & SignBit

    /* The Rust implementation exploits the unsinged semantics of `ix` and
     * `iy`. In Scala, this is a bit awkward to do. Instead, we mask off the
     * sign bit of both now, so we have less work to do later.
     */
    ix &= ~SignBit
    iy &= ~SignBit

    /* let mut ex = x.ex().signed();
     * let mut ey = y.ex().signed();
     * -> wrapToInt(ix >>> mbits) & emask
     *    but `& emask` is redundant since the sign bit is already 0 for us
     */
    var ex: Int = wrapToInt(ix >>> mbits)
    var ey: Int = wrapToInt(iy >>> mbits)

    /* if iy << 1 == zero || y.is_nan() || ex == F::EXP_SAT as i32 {
     *     return (x * y) / (x * y);
     * }
     *
     * `iy << 1 == zero` is the same as `iy == zero` for us, since we masked
     * off the sign bit.
     */
    if (iy == iZero || y != y || ex == emask)
      return (x * y) / (x * y)

    /* if ix << 1 <= iy << 1 {
     *     if ix << 1 == iy << 1 {
     *         return F::ZERO * x;
     *     }
     *     return x;
     * }
     * Since we masked off the sign bit already, all the `<< 1` above are not
     * necessary. That also means that our signed comparison does the right
     * thing.
     */
    if (ix <= iy) { // semantically, this tests whether abs(x) <= abs(y) at this point
      if (ix == iy)
        return fZero * x
      return x
    }

    /* if ex == 0 {
     *     let i = ix << (F::EXP_BITS + 1);
     *     ex -= i.leading_zeros() as i32;
     *     ix <<= -ex + 1;
     * } else {
     *     ix &= F::Int::MAX >> (F::EXP_BITS + 1); // this is mmask
     *     ix |= one << F::SIG_BITS;
     * }
     * and same for y
     *
     * Note that the two `F::EXP_BITS + 1` do *not* have the `+ 1` in the
     * original Rust implementation. The `+ 1`s I added account for the sign
     * bit, in addition to the exponent bits.
     *
     * I believe the first one is just wrong, and fails to correctly handle
     * subnormal values. Removing that one causes the tests for subnormals to
     * fail.
     *
     * The second one makes no actual difference, since the bit that is
     * affected is anyway or'ed back to 1 on the last line. However to me it
     * makes more sense to have it, as it then really corresponds to `mmask`,
     * and we use it to mask mantissa bits.
     */
    if (ex == 0) {
      // subnormal
      ex -= leadingZeros(ix << (ebits + 1))
      ix <<= -ex + 1
    } else {
      // normal form
      ix = (ix & mmask) | (iOne << mbits)
    }
    if (ey == 0) {
      // subnormal
      ey -= leadingZeros(iy << (ebits + 1))
      iy <<= -ey + 1
    } else {
      // normal form
      iy = (iy & mmask) | (iOne << mbits)
    }

    // x mod y

    /* while ex > ey {
     *     let i = ix.wrapping_sub(iy);
     *     if i >> (F::BITS - 1) == zero { // i.e., i >= 0 with signed semantics
     *         if i == zero {
     *             return F::ZERO * x;
     *         }
     *         ix = i;
     *     }
     *
     *     ix <<= 1;
     *     ex -= 1;
     * }
     *
     * let i = ix.wrapping_sub(iy);
     * if i >> (F::BITS - 1) == zero {
     *     if i == zero {
     *         return F::ZERO * x;
     *     }
     *     ix = i;
     * }
     *
     * but we factor out the duplicate block inside the loop, taking
     * advantage of a funny-looking "while loop with condition in the middle".
     */
    while ({
      val i = ix - iy
      if (i >= iZero) {
        if (i == iZero)
          return fZero * x
        ix = i
      }

      ex > ey // the actual condition of the while loop
    }) {
      ix <<= 1
      ex -= 1
    }

    // re-normalize the result in ix

    /* let shift = ix.leading_zeros().saturating_sub(F::EXP_BITS);
     * ix <<= shift;
     * ex -= shift as i32;
     */
    val shift = leadingZeros(ix) - ebits
    ix <<= shift
    ex -= shift

    // scale result

    /* if ex > 0 {
     *     ix -= one << F::SIG_BITS;
     *     ix |= F::Int::cast_from(ex) << F::SIG_BITS;
     * } else {
     *     ix >>= -ex + 1;
     * }
     */
    if (ex > 0) {
      // normal form
      ix = (ix - (iOne << mbits)) | (extendFromInt(ex) << mbits)
    } else {
      // subnormal
      ix >>>= -ex + 1
    }

    // integrate the sign bit and return

    /* ix |= sx;
     * F::from_bits(ix)
     */
    fromBits(ix | sx)

    // --- END MIT license

    // scalastyle:on return

    // --- END COPY-PASTE wrt. fmodd ------------------------------------------
  }
}
