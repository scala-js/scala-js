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

/* NOTICE -- The code in this file was adapted from code in libdivide,
 * licensed under the zlib license below.
 *
 * Copyright (C) 2010 - 2019 ridiculous_fish, <libdivide@ridiculousfish.com>
 * Copyright (C) 2016 - 2019 Kim Walisch, <kim.walisch@gmail.com>
 *
 * This software is provided 'as-is', without any express or implied
 * warranty.  In no event will the authors be held liable for any damages
 * arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not
 *    claim that you wrote the original software. If you use this software
 *    in a product, an acknowledgment in the product documentation would be
 *    appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 */

package org.scalajs.linker.runtime

import scala.scalajs.LinkingInfo

import java.lang.Integer.{toUnsignedLong => toULong}

/** Wide multiplication routines; run-time support for integer divisions.
 *
 *  The optimizer generates calls to methods of this object for its rewriting
 *  of integer division by constants.
 */
object IntDivRuntime {

  @inline
  def libdivide_mullhi_u32(x: Int, y: Int): Int = {
    /* Perform the multiplication on Longs, as does libdivide.
     *
     * Even when using RuntimeLong's, this is as good as it gets. The `hi`
     * word of each operand will constant-fold to 0, and hence the relevant
     * terms of the multiplication will be dropped, leaving only the best code
     * we can think of anyway.
     */
    ((toULong(x) * toULong(y)) >>> 32).toInt
  }

  @inline
  def libdivide_mullhi_s32(x: Int, y: Int): Int = {
    if (LinkingInfo.isWebAssembly) {
      // Longs are efficient -- use the implementation of libdivide
      ((x.toLong * y.toLong) >> 32).toInt
    } else {
      // Adapted from libdivide_mullhi_s64 with smaller operands

      // full 64 bits are x0 * y0 + (x0 * y1 << 16) + (x1 * y0 << 16) + (x1 * y1 << 16)
      val mask = 0xffff
      val x0 = x & mask
      val y0 = y & mask
      val x1 = x >> 16
      val y1 = y >> 16
      val x0y0_hi = (x0 * y0) >>> 16 // mullhi_u16(x0, y0)
      val t = x1 * y0 + x0y0_hi
      val w1 = x0 * y1 + (t & mask)

      x1 * y1 + (t >> 16) + (w1 >> 16)
    }
  }

  @inline
  def libdivide_mullhi_u64(x: Long, y: Long): Long = {
    /* We don't have 128-bit arithmetic on any of our targets, so we always
     * use the software fallback.
     *
     * We keep Longs everywhere, instead of using libdivide's uint32_t's,
     * because there that avoids a lot of calls to toULong that are implicit
     * in the original code.
     */

    // full 128 bits are x0 * y0 + (x0 * y1 << 32) + (x1 * y0 << 32) + (x1 * y1 << 64)
    val mask = 0xffffffffL                                          // uint32_t mask = 0xFFFFFFFF;
    val x0 = x & mask                                               // uint32_t x0 = (uint32_t)(x & mask);
    val x1 = x >>> 32                                               // uint32_t x1 = (uint32_t)(x >> 32);
    val y0 = y & mask                                               // uint32_t y0 = (uint32_t)(y & mask);
    val y1 = y >>> 32                                               // uint32_t y1 = (uint32_t)(y >> 32);
    val x0y0_hi = toULong(libdivide_mullhi_u32(x0.toInt, y0.toInt)) // uint32_t x0y0_hi = libdivide_mullhi_u32(x0, y0);
    val x0y1 = x0 * y1                                              // uint64_t x0y1 = x0 * (uint64_t)y1;
    val x1y0 = x1 * y0                                              // uint64_t x1y0 = x1 * (uint64_t)y0;
    val x1y1 = x1 * y1                                              // uint64_t x1y1 = x1 * (uint64_t)y1;
    val temp = x1y0 + x0y0_hi                                       // uint64_t temp = x1y0 + x0y0_hi;
    val temp_lo = temp & mask                                       // uint64_t temp_lo = temp & mask;
    val temp_hi = temp >>> 32                                       // uint64_t temp_hi = temp >> 32;

    x1y1 + temp_hi + ((temp_lo + x0y1) >>> 32)                      // return x1y1 + temp_hi + ((temp_lo + x0y1) >> 32);
  }

  @inline
  def libdivide_mullhi_s64(x: Long, y: Long): Long = {
    /* We don't have 128-bit arithmetic on any of our targets, so we always
     * use the software fallback.
     *
     * We keep Longs everywhere, instead of using libdivide's uint32_t's,
     * to better keep track of whether values are supposed to zero-extend or
     * sign-extend when widened to 64 bits.
     */

    // full 128 bits are x0 * y0 + (x0 * y1 << 32) + (x1 * y0 << 32) + (x1 * y1 << 64)
    val mask = 0xffffffffL                                          // uint32_t mask = 0xFFFFFFFF;
    val x0 = x & mask                                               // uint32_t x0 = (uint32_t)(x & mask);
    val y0 = y & mask                                               // uint32_t y0 = (uint32_t)(y & mask);
    val x1 = x >> 32                                                // int32_t x1 = (int32_t)(x >> 32);
    val y1 = y >> 32                                                // int32_t y1 = (int32_t)(y >> 32);
    val x0y0_hi = toULong(libdivide_mullhi_u32(x0.toInt, y0.toInt)) // uint32_t x0y0_hi = libdivide_mullhi_u32(x0, y0);
    val t = x1 * y0 + x0y0_hi                                       // int64_t t = x1 * (int64_t)y0 + x0y0_hi;
    val w1 = x0 * y1 + (t & mask)                                   // int64_t w1 = x0 * (int64_t)y1 + (t & mask);

    x1 * y1 + (t >> 32) + (w1 >> 32)                                // return x1 * (int64_t)y1 + (t >> 32) + (w1 >> 32);
  }

  @inline
  def libdivide_u32_do(numer: Int, divisor: Int, magic: Int,
      shift: Int, addMarker: Boolean, negativeDivisor: Boolean, isQuotient: Boolean): Int = {

    // if (!magic)
    if (magic == 0L) {
      if (isQuotient) {
        // return numer >> more;
        numer >>> shift
      } else {
        // Scala.js-only
        numer & ((1 << shift) - 1)
      }
    } else {
      // uint32_t q = libdivide_mullhi_u32(numer, magic);
      val q = libdivide_mullhi_u32(numer, magic)

      // if (more & LIBDIVIDE_ADD_MARKER)
      val quotient = if (addMarker) {
        // uint32_t t = ((numer - q) >> 1) + q;
        val t = ((numer - q) >>> 1) + q
        // return t >> (more & LIBDIVIDE_32_SHIFT_MASK);
        t >>> shift
      } else {
        // All upper bits are 0, don't need to mask them off.
        // return q >> more;
        q >>> shift
      }

      // Scala.js-only
      if (isQuotient)
        quotient
      else
        numer - (divisor * quotient)
    }
  }

  @inline
  def libdivide_s32_do(numer: Int, divisor: Int, magic: Int,
      shift: Int, addMarker: Boolean, negativeDivisor: Boolean, isQuotient: Boolean): Int = {

    // if (!magic)
    if (magic == 0) {
      // uint32_t mask = ((uint32_t)1 << shift) - 1;
      val mask = (1 << shift) - 1

      if (isQuotient) {
        // uint32_t uq = numer + ((numer >> 31) & mask);
        val uq = numer + ((numer >> 31) & mask)
        // int32_t q = (int32_t)uq;
        // q >>= shift;
        val q = uq >> shift
        // uint32_t sign = (int8_t)more >> 7;
        // q = (q ^ sign) - sign;
        // return q;
        if (negativeDivisor) -q else q
      } else {
        /* Scala.js only
         *
         * By definition:
         *   val floorMod = numer & mask
         *   if (numer >= 0 || floorMod == 0) floorMod
         *   else if -(1 << shift) + floorMod
         * which can be written as
         *   (if (numer >= 0 || floorMod == 0) 0 else -(1 << shift)) + floorMod
         * Since floorMod >= 0, `floorMod == 0` is equivalent to
         * `floorMod <= 0`, which is also `-floorMod >= 0`, hence
         * the condition of the if can reformulated as
         *   numer >= 0 || -floorMod >= 0
         * which is
         *   (numer & -floorMod) >= 0
         * and the if/else can be obtained in a branchless way as
         *   -(1 << shift) & ((numer & -floorMod) >> 31)
         */
        val floorMod = numer & mask
        (-(1 << shift) & ((numer & -floorMod) >> 31)) + floorMod
      }
    } else {
      // uint32_t uq = (uint32_t)libdivide_mullhi_s32(numer, magic);
      val uq = libdivide_mullhi_s32(numer, magic)
      // if (more & LIBDIVIDE_ADD_MARKER)
      val q1 = if (addMarker) {
        // // must be arithmetic shift and then sign extend
        // int32_t sign = (int8_t)more >> 7;
        // // q += (more < 0 ? -numer : numer) // Scala.js: this is what we actually do
        // // cast required to avoid UB
        // uq += ((uint32_t)numer ^ sign) - sign;
        if (negativeDivisor)
          uq - numer
        else
          uq + numer
      } else {
        uq
      }

      // int32_t q = (int32_t)uq;
      // q >>= shift;
      val q2 = q1 >> shift

      // q += (q < 0);
      // return q;
      val quotient = q2 + (q2 >>> 31) // Scala.js: add 1 if sign bit is on, 0 otherwise, in a branchless way

      // Scala.js-only
      if (isQuotient)
        quotient
      else
        numer - (divisor * quotient)
    }
  }

  @inline
  def libdivide_u64_do(numer: Long, divisor: Long, magic: Long,
      shift: Int, addMarker: Boolean, negativeDivisor: Boolean, isQuotient: Boolean): Long = {
    // if (!magic)
    if (magic == 0L) {
      if (isQuotient) {
        // return numer >> more;
        numer >>> shift
      } else {
        // Scala.js-only
        numer & ((1L << shift) - 1L)
      }
    } else {
      // uint64_t q = libdivide_mullhi_u64(numer, magic);
      val q = libdivide_mullhi_u64(numer, magic)
      // if (more & LIBDIVIDE_ADD_MARKER)
      val quotient = if (addMarker) {
        // uint64_t t = ((numer - q) >> 1) + q;
        val t = ((numer - q) >>> 1) + q
        // return t >> (more & LIBDIVIDE_64_SHIFT_MASK);
        t >>> shift
      } else {
        // All upper bits are 0, don't need to mask them off.
        // return q >> more;
        q >>> shift
      }

      // Scala.js-only
      if (isQuotient)
        quotient
      else
        numer - (divisor * quotient)
    }
  }

  @inline
  def libdivide_s64_do(numer: Long, divisor: Long, magic: Long,
      shift: Int, addMarker: Boolean, negativeDivisor: Boolean, isQuotient: Boolean): Long = {

    // if (!magic)
    if (magic == 0) {
      // uint64_t mask = ((uint64_t)1 << shift) - 1;
      val mask = (1L << shift) - 1L

      if (isQuotient) {
        // uint64_t uq = numer + ((numer >> 63) & mask);
        val uq = numer + ((numer >> 63) & mask)

        // int64_t q = (int64_t)uq;
        // q >>= shift;
        val q = uq >> shift

        // // must be arithmetic shift and then sign-extend
        // int64_t sign = (int8_t)more >> 7;
        // q = (q ^ sign) - sign;
        // return q;

        // Scala.js: sign is actually `if (negativeDivisor) -1 else 0`
        // and the magic with `(q ^ sign) - sign` is therefore `if (negativeDivisor) -q else q`
        if (negativeDivisor) -q else q
      } else {
        /* Scala.js only
         * See reasoning in libdivide_s32_do
         */
        val floorMod = numer & mask
        (-(1L << shift) & ((numer & -floorMod) >> 63)) + floorMod
      }
    } else {
      // uint64_t uq = (uint64_t)libdivide_mullhi_s64(numer, magic);
      val uq = libdivide_mullhi_s64(numer, magic)

      //if (more & LIBDIVIDE_ADD_MARKER) {
      //  // must be arithmetic shift and then sign extend
      //  int64_t sign = (int8_t)more >> 7;
      //  // q += (more < 0 ? -numer : numer) // Scala.js: this is what we actually do
      //  // cast required to avoid UB
      //  uq += ((uint64_t)numer ^ sign) - sign;
      //}
      //int64_t q = (int64_t)uq;
      val q1 = if (addMarker) {
        if (negativeDivisor)
          uq - numer
        else
          uq + numer
      } else {
        uq
      }

      // q >>= shift;
      val q2 = q1 >> shift

      // q += (q < 0);
      // return q;
      val quotient = q2 + (q2 >>> 63) // Scala.js: add 1 if sign bit is on, 0 otherwise, in a branchless way

      // Scala.js-only
      if (isQuotient)
        quotient
      else
        numer - (divisor * quotient)
    }
  }
}
