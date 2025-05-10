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

package org.scalajs.linker.frontend.optimizer

import java.lang.{Long => JLong}
import java.math.BigInteger

import org.scalajs.ir.Names._
import org.scalajs.ir.Types._

private[optimizer] object IntegerDivisions {

  val IntDivRuntimeClass = ClassName("org.scalajs.linker.runtime.IntDivRuntime")

  private val ConfigParamRefs = List(IntRef, BooleanRef, BooleanRef, BooleanRef)
  private val IntDivParamRefs = IntRef :: IntRef :: IntRef :: ConfigParamRefs
  private val LongDivParamRefs = LongRef :: LongRef :: LongRef :: ConfigParamRefs

  val libdivide_u32_do_MethodName =
    MethodName("libdivide_u32_do", IntDivParamRefs, IntRef)

  val libdivide_s32_do_MethodName =
    MethodName("libdivide_s32_do", IntDivParamRefs, IntRef)

  val libdivide_u64_do_MethodName =
    MethodName("libdivide_u64_do", LongDivParamRefs, LongRef)

  val libdivide_s64_do_MethodName =
    MethodName("libdivide_s64_do", LongDivParamRefs, LongRef)

  val AllIntDivRuntimeMethodNames: List[MethodName] = List(
    libdivide_u32_do_MethodName,
    libdivide_s32_do_MethodName,
    libdivide_u64_do_MethodName,
    libdivide_s64_do_MethodName
  )

  // This type is a structured equivalent of the libdivide_x_t structs
  final case class Strategy[T](
      magic: T, shift: Int, addMarker: Boolean, negativeDivisor: Boolean)

  /** Like Integral[T], but with some unsigned operations that we need. */
  sealed trait UnsignedIntegral[T] extends Integral[T] {
    /** Divides a wide (2x bit width of T) uint `{numhi, numlo}` by a uint `{den}`.
     *
     *  The result must fit in T's bit width. Returns the pair `(quotient, remainder)`.
     */
    def unsignedDivideWide(numhi: T, numlo: T, den: T): (T, T)

    /** For example, for Int this is `31 - Integer.numberOfLeadingZeros(x)`. */
    def unsignedFloorLog2(x: T): Int

    def compareUnsigned(x: T, y: T): Int

    def isUnsignedPowerOf2(x: T): Boolean

    def one_<<(y: Int): T
  }

  implicit object IntIsUnsignedIntegral
      extends UnsignedIntegral[Int]
      with Numeric.IntIsIntegral with Ordering.IntOrdering {

    private final val MaxUIntAsLong = 0xffffffffL

    // Port of libdivide_64_div_32_to_32
    def unsignedDivideWide(numhi: Int, numlo: Int, den: Int): (Int, Int) = {
      val num = (numhi.toLong << 32) | Integer.toUnsignedLong(numlo)
      val denLong = Integer.toUnsignedLong(den)

      val quotient = JLong.divideUnsigned(num, denLong)
      val remainder = JLong.remainderUnsigned(num, denLong)

      assert(JLong.compareUnsigned(quotient, MaxUIntAsLong) <= 0)
      assert(JLong.compareUnsigned(remainder, denLong) < 0)

      (quotient.toInt, remainder.toInt)
    }

    def unsignedFloorLog2(x: Int): Int =
      31 - Integer.numberOfLeadingZeros(x)

    def compareUnsigned(x: Int, y: Int): Int =
      Integer.compareUnsigned(x, y)

    def isUnsignedPowerOf2(x: Int): Boolean =
      (x & (x - 1)) == 0 && x != 0

    def one_<<(y: Int): Int =
      1 << y
  }

  implicit object LongIsUnsignedIntegral
      extends UnsignedIntegral[Long]
      with Numeric.LongIsIntegral with Ordering.LongOrdering {

    private val MaxULongAsBigInteger =
      BigInteger.ONE.shiftLeft(64).subtract(BigInteger.ONE)

    private def ulongToBigInteger(a: Long): BigInteger =
      BigInteger.valueOf(a).and(MaxULongAsBigInteger)

    def unsignedDivideWide(numhi: Long, numlo: Long, den: Long): (Long, Long) = {
      /* We cheat here. Instead of porting libdivide's implementation, we use
       * full-blown BigInteger's to perform the operation.
       */

      val num = ulongToBigInteger(numhi).shiftLeft(64).or(ulongToBigInteger(numlo))
      val denBig = ulongToBigInteger(den)
      val results = num.divideAndRemainder(denBig)

      assert(results(0).compareTo(MaxULongAsBigInteger) <= 0)
      assert(results(1).compareTo(denBig) < 0)

      (results(0).longValue(), results(1).longValue())
    }

    def unsignedFloorLog2(x: Long): Int =
      63 - JLong.numberOfLeadingZeros(x)

    def compareUnsigned(x: Long, y: Long): Int =
      JLong.compareUnsigned(x, y)

    def isUnsignedPowerOf2(x: Long): Boolean =
      (x & (x - 1L)) == 0L && x != 0L

    def one_<<(y: Int): Long =
      1L << y
  }

  /** Port and factorization of the libdivide_x_gen for x in {u32,s32,u64,s64}.
   *
   *  We can afford the factorization because this code is not
   *  performance-sensitive. In libdivide, these 4 functions are meant to be
   *  used at run time.
   *
   *  We only use the branch-full variants of libdivide. The branches will be
   *  performed by optimizer, so they are always free.
   */
  def libdivide_gen[T](d: T, isSigned: Boolean)(implicit int: UnsignedIntegral[T]): Strategy[T] = {
    import int.mkNumericOps

    assert(d != int.zero)

    val negativeDivisor = isSigned && int.lt(d, int.zero)
    val absD = if (negativeDivisor) -d else d
    val floor_log_2_d = int.unsignedFloorLog2(absD)

    if (int.isUnsignedPowerOf2(absD)) {
      Strategy(
        magic = int.zero,
        shift = floor_log_2_d,
        addMarker = false,
        negativeDivisor
      )
    } else {
      assert(floor_log_2_d >= 1) // because absD is neither 0 (illegal) nor 1 (power of 2)
      val tentativeShift =
        if (isSigned) floor_log_2_d - 1
        else floor_log_2_d

      var (proposed_m, rem) = int.unsignedDivideWide(int.one_<<(tentativeShift), int.zero, absD)
      val e = absD - rem

      val (shift, addMarker) = if (int.compareUnsigned(e, int.one_<<(floor_log_2_d)) < 0) {
        // This power works
        (tentativeShift, false)
      } else {
        // We do a+a instead of 2*a because we don't have a 2 of type T
        proposed_m += proposed_m
        val twice_rem = rem + rem

        if (int.compareUnsigned(twice_rem, absD) >= 0 || int.compareUnsigned(twice_rem, rem) < 0)
          proposed_m += int.one

        (floor_log_2_d, true)
      }

      proposed_m += int.one

      Strategy(
        magic = if (negativeDivisor) -proposed_m else proposed_m,
        shift,
        addMarker,
        negativeDivisor
      )
    }
  }

}
