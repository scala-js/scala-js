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

package org.scalajs.linker.frontend.optimizer

import scala.annotation.{switch, tailrec}

import java.lang.{Long => JLong}

import org.scalajs.ir.Position
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._
import org.scalajs.ir.WellKnownNames._

import org.scalajs.linker.backend.emitter.LongImpl

/** Rewrites for integer divisions by constants using adds, muls and shifts.
 *
 *  This class uses strategies from Hacker's Delight, Chapter 10.
 */
private[optimizer] final class IntegerDivisions(useRuntimeLong: Boolean) {
  // #5272 Do not simplify this import, even if it appears to compile on your machine
  import IntegerDivisions.{IntIsUnsignedIntegral => _, LongIsUnsignedIntegral => _, _}

  /** Should we apply the optimized rewrite for division by a constant?
   *
   *  The particular conditions were decided based on the results of
   *  microbenchmarks. We apply the rewrite if the result was measured to be
   *  reliably at least as good as the division.
   *
   *  Currently, we apply the rewrite if at least one of the following
   *  conditions holds:
   *
   *  - the divisor is zero (rewrite to a trivial `throw`);
   *  - the divisor is a power of 2 (including negative values for the signed
   *    operations, and including 1's);
   *  - it is a `long` operation and we are targeting JavaScript; or
   *  - it is an `int` operation and we are targeting WebAssembly.
   *
   *  The remaining possibilities were measured to be detrimental. The
   *  measurements actually make sense. I had even predicted them before
   *  running the benchmarks.
   *
   *  For non-powers of two, rewriting `int` operations on JavaScript is
   *  detrimental, as we don't have efficient access to the hi result of a
   *  32-bit multiplication. We don't have it either for 64 bits, but `long`
   *  division is so bad on JS that it is a win anyway.
   *
   *  On WebAssembly, we have efficient access to the hi result of a 32-bit
   *  multiplication, but not for 64 bits.
   */
  def shouldRewriteDivision[T](op: BinaryOp.Code, divisor: T, isWebAssembly: Boolean)(
      implicit int: UnsignedIntegral[T]): Boolean = {

    val isSigned = (op: @switch) match {
      case BinaryOp.Int_/ | BinaryOp.Int_% | BinaryOp.Long_/ | BinaryOp.Long_% =>
        true
      case _ =>
        false
    }

    val negativeDivisor = isSigned && int.lt(divisor, int.zero)
    val absDivisor = if (negativeDivisor) int.negate(divisor) else divisor

    val isLong = int.bitSize == 64

    (
      int.isUnsignedPowerOf2OrZero(absDivisor) ||
      isLong != isWebAssembly
    )
  }

  /** Generates optimized code for division by a given constant divisor.
   *
   *  The generated code must be integrated in a context where the numerator is
   *  bound to the local name `IntegerDivisions.NumeratorArgName` (as if it had
   *  been inlined from a method taking `NumeratorArgName` as formal parameter).
   *
   *  This method produces correct code for all divisors (including zero).
   *  However, for best performance, it should only be used if
   *  `shouldRewriteDivision` returns `true`.
   *
   *  The produced code is intended to go through the optimizer again, in
   *  particular folding. No effort is made here to avoid shifts by 0, for
   *  example.
   */
  def makeOptimizedDivision[T](op: BinaryOp.Code, divisor: T)(
      implicit int: UnsignedIntegral[T], pos: Position): Tree = {

    val (isSigned, isQuotient) = (op: @switch) match {
      case BinaryOp.Int_/ | BinaryOp.Long_/                   => (true, true)
      case BinaryOp.Int_% | BinaryOp.Long_%                   => (true, false)
      case BinaryOp.Int_unsigned_/ | BinaryOp.Long_unsigned_/ => (false, true)
      case BinaryOp.Int_unsigned_% | BinaryOp.Long_unsigned_% => (false, false)
    }

    val negativeDivisor = isSigned && int.lt(divisor, int.zero)
    val absDivisor = if (negativeDivisor) int.negate(divisor) else divisor

    if (int.isUnsignedPowerOf2OrZero(absDivisor)) {
      if (absDivisor == int.zero) {
        genZero()
      } else if (absDivisor == int.one) {
        // The algorithms below assume absDivisor >= 2, so special-case 1
        genOne(isQuotient, negativeDivisor)
      } else {
        if (isSigned)
          genPowerOfTwoSigned(isQuotient, negativeDivisor, absDivisor)
        else
          genPowerOfTwoUnsigned(isQuotient, absDivisor)
      }
    } else {
      val quotient =
        if (isSigned) genNonPowerOfTwoSignedQuotient(negativeDivisor, absDivisor)
        else genNonPowerOfTwoUnsignedQuotient(absDivisor)

      if (isQuotient) {
        quotient
      } else {
        // n - divisor * quotient
        BinaryOp(int.Op_-, argRef, BinaryOp(int.Op_*, int.literal(divisor), quotient))
      }
    }
  }

  private def argRef[T](implicit int: UnsignedIntegral[T], pos: Position): VarRef =
    VarRef(NumeratorArgName)(int.tpe)

  private def genZero()(implicit pos: Position): Tree = {
    UnaryOp(
      UnaryOp.Throw,
      New(
        ArithmeticExceptionClass,
        MethodIdent(MethodName.constructor(List(ClassRef(BoxedStringClass)))),
        List(StringLiteral("/ by zero"))
      )
    )
  }

  private def genOne[T](
      isQuotient: Boolean, negativeDivisor: Boolean)(
      implicit int: UnsignedIntegral[T], pos: Position): Tree = {

    // Straightforward

    if (isQuotient) {
      if (negativeDivisor)
        BinaryOp(int.Op_-, int.literalZero, argRef)
      else
        argRef
    } else {
      int.literalZero
    }
  }

  private def genPowerOfTwoSigned[T](
      isQuotient: Boolean, negativeDivisor: Boolean, absDivisor: T)(
      implicit int: UnsignedIntegral[T], pos: Position): Tree = {

    // Sections 10-1 (quotient) and 10-2 (remainder)

    val k = int.log2Exact(absDivisor) // i.e., absDivisor == 2^k

    var t = BinaryOp(int.Op_>>, argRef, IntLiteral(k - 1))
    t = BinaryOp(int.Op_>>>, t, IntLiteral(int.bitSize - k))

    if (isQuotient) {
      t = BinaryOp(int.Op_+, argRef, t)
      val q = BinaryOp(int.Op_>>, t, IntLiteral(k))

      if (negativeDivisor)
        BinaryOp(int.Op_-, int.literalZero, q)
      else
        q
    } else {
      val temp1Def = tempVarDef(tName, t)
      Block(
        temp1Def,
        // ((arg + temp1) & (2^k - 1)) - temp1
        BinaryOp(
          int.Op_-,
          BinaryOp(
            int.Op_&,
            BinaryOp(int.Op_+, argRef, temp1Def.ref),
            int.literal(int.minus(absDivisor, int.one))
          ),
          temp1Def.ref
        )
      )
    }
  }

  private def genPowerOfTwoUnsigned[T](
      isQuotient: Boolean, absDivisor: T)(
      implicit int: UnsignedIntegral[T], pos: Position): Tree = {

    // Straightforward

    if (isQuotient) {
      val k = int.log2Exact(absDivisor) // i.e., absDivisor == 2^k
      BinaryOp(int.Op_>>>, argRef, IntLiteral(k))
    } else {
      BinaryOp(int.Op_&, argRef, int.literal(int.minus(absDivisor, int.one)))
    }
  }

  private def genNonPowerOfTwoSignedQuotient[T](
      negativeDivisor: Boolean, absDivisor: T)(
      implicit int: UnsignedIntegral[T], pos: Position): Tree = {

    // Hacker's Delight, Sections 10-3 to 10-6

    val data = computeSignedMagic(absDivisor, negativeDivisor)

    var q = int.genMulSignedHi(data.M, argRef, useRuntimeLong)
    if (data.add > 0)
      q = BinaryOp(int.Op_+, q, argRef)
    else if (data.add < 0)
      q = BinaryOp(int.Op_-, q, argRef) // Section 10-5 for negative divisors
    q = BinaryOp(int.Op_>>, q, IntLiteral(data.shift))

    if (!negativeDivisor) {
      // Add 1 to q if n is negative
      BinaryOp(int.Op_+, q, BinaryOp(int.Op_>>>, argRef, IntLiteral(int.bitSize - 1)))
    } else {
      // Section 10-5 for negative divisors: add 1 to t if t is negative (i.e., n is positive)
      val temp1Def = tempVarDef(tName, q)
      val t = temp1Def.ref
      Block(
        temp1Def,
        BinaryOp(int.Op_+, t, BinaryOp(int.Op_>>>, t, IntLiteral(int.bitSize - 1)))
      )
    }
  }

  private def genNonPowerOfTwoUnsignedQuotient[T](
      absDivisor: T)(
      implicit int: UnsignedIntegral[T], pos: Position): Tree = {

    // Hacker's Delight, Sections 10-8 to 10-10

    val data = computeUnsignedMagic(absDivisor)

    val computeMulHi = int.genMulUnsignedHi(data.M, argRef)
    if (data.add > 0) {
      // Granlund-Montgomery trick to avoid the 33/65-bit quantity
      val hiDef = tempVarDef(hiName, computeMulHi) // stored in `q` in the original algorithm
      val hi = hiDef.ref
      var t = BinaryOp(int.Op_-, argRef, hi)
      t = BinaryOp(int.Op_>>>, t, IntLiteral(1))
      t = BinaryOp(int.Op_+, t, hi)
      t = BinaryOp(int.Op_>>>, t, IntLiteral(data.shift - 1))
      Block(hiDef, t)
    } else {
      BinaryOp(int.Op_>>>, computeMulHi, IntLiteral(data.shift))
    }
  }
}

private[optimizer] object IntegerDivisions {
  /** Local argument name for the numerator, used by the generated code.
   *
   *  The optimizer should bind this name to the numerator in the scope of the
   *  code generated by `makeOptimizedDivision` (as if it had been inlined from
   *  a method taking `NumeratorArgName` as formal parameter).
   */
  val NumeratorArgName = LocalName("num")

  private[optimizer] val tName = LocalName("t") // accessible to tests
  private[optimizer] val hiName = LocalName("hi") // accessible to tests
  private val y0Name = LocalName("y0")
  private val y1Name = LocalName("y1")
  private val mulhiTempName = LocalName("mulht")

  private def tempVarDef(name: LocalName, rhs: Tree)(implicit pos: Position): VarDef =
    VarDef(LocalIdent(name), NoOriginalName, rhs.tpe, mutable = false, rhs)

  /** Magic data, from which we derive the code to generate.
   *
   *  `add` is +1 if we need an addition, -1 for a subtraction, and 0 for none.
   */
  private[optimizer] final case class MagicData[T](M: T, add: Int, shift: Int)

  // private[optimizer] for unit tests
  private[optimizer] def computeSignedMagic[T](absDivisor: T, negativeDivisor: Boolean)(
      implicit int: UnsignedIntegral[T], pos: Position): MagicData[T] = {

    /* Inspired by Hacker's Delight, Section 10-15, with formulas
     * from Sections 10-4 and 10-5.
     *
     * For d > 0 (Section 10-4):
     *
     * Compute
     *   nc = 2^(W-1) - rem(2^(W-1), d) - 1
     * the largest (positive) value of n such that rem(nc, d) = d - 1.
     *
     * Then find the smallest p >= W such that (equation 6)
     *   2^p > nc * (d - rem(2^p, d))
     *
     * Once found, compute the magic number as (equation 5)
     *   m = (2^p + d - rem(2^p, d)) / d
     *
     * For d < 0 (Section 10-5):
     *
     * Compute
     *   nc = -2^(W-1) + rem(2^(W-1) + 1, d)
     * the most negative value of n such that nc = kd + 1 for some integer k.
     *
     * Then find the smallest p >= W such that (equation 18)
     *   2^p > nc * (d + rem(2^p, d))
     *       = (-nc) * (-d - rem(2^p, d))
     *
     * Once found, compute the magic number as (equation 17)
     *   m = (2^p - d - rem(2^p, d)) / d
     *
     * Combining both, and expressing in terms of ad = |d| and negativeDivisor:
     *
     *   anc =
     *     if (negativeDivisor)
     *       2^(W-1) - rem(2^(W-1) + 1, ad) // = -nc
     *     else
     *       2^(W-1) - rem(2^(W-1), ad) - 1 // = nc
     *
     * The condition is
     *
     *   2^p > anc * (ad - rem(2^p, ad))
     *
     * The absolute value of the magic number is
     *
     *   am = (2^p + ad - rem(2^p, ad)) / ad
     *
     * m = if (negativeDivisor) -am else am
     */

    val W = int.bitSize

    val one = BigInt(1)
    val ad = int.toUnsignedBigInt(absDivisor)
    val twoPowWMin1 = one << (W - 1)

    val anc =
      if (negativeDivisor) twoPowWMin1 - ((twoPowWMin1 + one) % ad)
      else twoPowWMin1 - (twoPowWMin1 % ad) - one

    // Invariant: twoPowP == 2^p
    @inline @tailrec
    def loop(p: Int, twoPowP: BigInt): MagicData[T] = {
      assert(
          p < 2 * W,
          s"Could not find a suitable (m, p) pair to divide by " +
          s"${if (negativeDivisor) -ad else ad}; something went wrong near $pos")

      val temp = ad - (twoPowP % ad)
      if (twoPowP > anc * temp) {
        // We found a valid result
        val am = (twoPowP + temp) / ad
        val m = int.fromBigInt(if (negativeDivisor) -am else am)

        /* Compute the 'add' flag to signal sign discrepancies between the
         * mathematical value of m (which should always be negativeDivisor)
         * and its wrapped value.
         */
        val add =
          if (!negativeDivisor && int.lt(m, int.zero)) +1
          else if (negativeDivisor && int.gt(m, int.zero)) -1
          else 0

        MagicData(m, add, shift = p - W)
      } else {
        // Keep looking
        loop(p + 1, twoPowP << 1)
      }
    }

    loop(p = W, twoPowP = twoPowWMin1 << 1)
  }

  // private[optimizer] for unit tests
  private[optimizer] def computeUnsignedMagic[T](absDivisor: T)(
      implicit int: UnsignedIntegral[T], pos: Position): MagicData[T] = {

    /* Inspired by Hacker's Delight, Section 10-15, with formulas
     * from Section 10-9.
     *
     * Compute
     *   nc = 2^W - rem(2^W, d) - 1
     * the largest value of n such that rem(nc, d) = d - 1.
     *
     * Then find the smallest p >= W such that (equation 27)
     *   2^p > nc * (d - 1 - rem(2^p - 1, d))
     *
     * Once found, compute the magic number as (equation 26)
     *   m = (2^p + d - 1 - rem(2^p - 1, d)) / d
     *
     * If m wraps around when converted to T, set the 'add' flag of the result,
     * which will use the Granlund-Montgomery trick to avoid the 33/65-bit
     * quantity.
     */

    val W = int.bitSize

    val one = BigInt(1)
    val d = int.toUnsignedBigInt(absDivisor)
    val dMin1 = d - one
    val twoPowW = one << W

    val nc = twoPowW - (twoPowW % d) - one

    // Invariant: twoPowP == 2^p
    @inline @tailrec
    def loop(p: Int, twoPowP: BigInt): MagicData[T] = {
      assert(
          p <= 2 * W,
          s"Could not find a suitable (m, p) pair to divide by " +
          s"$d; something went wrong near $pos")

      val temp = dMin1 - (twoPowP - one) % d
      if (twoPowP > nc * temp) {
        // We found a valid result
        val m = (twoPowP + temp) / d
        val add = if (m.bitLength > W) 1 else 0 // will m wrap when converted to T?
        MagicData(int.fromBigInt(m), add, shift = p - W)
      } else {
        // Keep looking
        loop(p + 1, twoPowP << 1)
      }
    }

    loop(p = W, twoPowP = twoPowW)
  }

  /** Like Integral[T], but with some unsigned operations that we need. */
  sealed trait UnsignedIntegral[T] extends Integral[T] {
    /** Number of bits used to represent a value of type `T`. */
    val bitSize: Int

    def isUnsignedPowerOf2OrZero(x: T): Boolean

    /** Computes log2(x), assuming x is a power of 2. */
    def log2Exact(x: T): Int

    def toUnsignedBigInt(x: T): BigInt

    def fromBigInt(x: BigInt): T

    // IR-related operations

    val tpe: Type

    def literal(x: T)(implicit pos: Position): Literal

    final def literalZero(implicit pos: Position): Literal = literal(zero)

    // scalastyle:off disallow.space.before.token
    val Op_+ : BinaryOp.Code
    val Op_- : BinaryOp.Code
    val Op_* : BinaryOp.Code
    val Op_& : BinaryOp.Code
    val Op_>>> : BinaryOp.Code
    val Op_>> : BinaryOp.Code
    // scalastyle:on disallow.space.before.token

    def genMulSignedHi(x: T, y: VarRef, useRuntimeLong: Boolean)(implicit pos: Position): Tree
    def genMulUnsignedHi(x: T, y: VarRef)(implicit pos: Position): Tree
  }

  implicit object IntIsUnsignedIntegral
      extends UnsignedIntegral[Int]
      with Numeric.IntIsIntegral with Ordering.IntOrdering {

    val bitSize = 32

    def isUnsignedPowerOf2OrZero(x: Int): Boolean =
      (x & (x - 1)) == 0

    def log2Exact(x: Int): Int =
      31 - Integer.numberOfLeadingZeros(x)

    def toUnsignedBigInt(x: Int): BigInt =
      BigInt(Integer.toUnsignedLong(x))

    def fromBigInt(x: BigInt): Int =
      x.toInt

    // IR-related operations

    val tpe: Type = IntType

    def literal(x: Int)(implicit pos: Position): Literal = IntLiteral(x)

    // scalastyle:off disallow.space.before.token
    val Op_+ : BinaryOp.Code = BinaryOp.Int_+
    val Op_- : BinaryOp.Code = BinaryOp.Int_-
    val Op_* : BinaryOp.Code = BinaryOp.Int_*
    val Op_& : BinaryOp.Code = BinaryOp.Int_&
    val Op_>>> : BinaryOp.Code = BinaryOp.Int_>>>
    val Op_>> : BinaryOp.Code = BinaryOp.Int_>>
    // scalastyle:on disallow.space.before.token

    def genMulSignedHi(x: Int, y: VarRef, useRuntimeLong: Boolean)(
        implicit pos: Position): Tree = {
      /* (Math.multiplyFull(x, y) >>> 32).toInt
       *
       * Unfortunately, we cannot directly call that method, because it won't
       * be available when we link a javalib < 1.20.
       *
       * Its user-land IR is easy enough to hard-code. However, we lose the
       * handling as a RuntimeLong intrinsic done by the optimizer.
       * That is why we special-case useRuntimeLong at this level, to directly
       * emit a call to the intrinsic implementation.
       *
       * On the flip side, when useRuntimeLong = false, it is easy for the
       * ElementaryInterpreter to handle this code (only basic operations and
       * literals required).
       */

      val multiplyFullResult = if (useRuntimeLong) {
        // RuntimeLong.multiplyFull(x, y)
        ApplyStatic(ApplyFlags.empty, LongImpl.RuntimeLongClass,
            MethodIdent(LongImpl.multiplyFull), List(IntLiteral(x), y))(
            ClassType(LongImpl.RuntimeLongClass, nullable = true))
      } else {
        // x.toLong * y.toLong
        BinaryOp(
          BinaryOp.Long_*,
          LongLiteral(x.toLong),
          UnaryOp(UnaryOp.IntToLong, y)
        )
      }

      // (multiplyFullResult >>> 32).toInt
      UnaryOp(
        UnaryOp.LongToInt,
        BinaryOp(
          BinaryOp.Long_>>>,
          multiplyFullResult,
          IntLiteral(32)
        )
      )
    }

    def genMulUnsignedHi(x: Int, y: VarRef)(implicit pos: Position): Tree = {
      // ((Integer.toUnsignedLong(x) * Integer.toUnsignedLong(y)) >>> 32).toInt
      UnaryOp(
        UnaryOp.LongToInt,
        BinaryOp(
          BinaryOp.Long_>>>,
          BinaryOp(
            BinaryOp.Long_*,
            LongLiteral(Integer.toUnsignedLong(x)),
            UnaryOp(UnaryOp.UnsignedIntToLong, y)
          ),
          IntLiteral(32)
        )
      )
    }
  }

  implicit object LongIsUnsignedIntegral
      extends UnsignedIntegral[Long]
      with Numeric.LongIsIntegral with Ordering.LongOrdering {

    val bitSize = 64

    def isUnsignedPowerOf2OrZero(x: Long): Boolean =
      (x & (x - 1L)) == 0L

    def log2Exact(x: Long): Int =
      63 - JLong.numberOfLeadingZeros(x)

    def toUnsignedBigInt(x: Long): BigInt = {
      val allButSignBit = BigInt(x & ~Long.MinValue)
      if (x < 0L)
        allButSignBit.setBit(63)
      else
        allButSignBit
    }

    def fromBigInt(x: BigInt): Long =
      x.toLong

    // IR-related operations

    val tpe: Type = LongType

    def literal(x: Long)(implicit pos: Position): Literal = LongLiteral(x)

    // scalastyle:off disallow.space.before.token
    val Op_+ : BinaryOp.Code = BinaryOp.Long_+
    val Op_- : BinaryOp.Code = BinaryOp.Long_-
    val Op_* : BinaryOp.Code = BinaryOp.Long_*
    val Op_& : BinaryOp.Code = BinaryOp.Long_&
    val Op_>>> : BinaryOp.Code = BinaryOp.Long_>>>
    val Op_>> : BinaryOp.Code = BinaryOp.Long_>>
    // scalastyle:on disallow.space.before.token

    def genMulSignedHi(x: Long, y: VarRef, useRuntimeLong: Boolean)(
        implicit pos: Position): Tree = {
      /* Math.multiplyHigh(x, y)
       * Unfortunately, we cannot directly call that method, because it won't
       * be available when we link a javalib < 1.20. So we hard-code its IR.
       * On the flip side, that allows the ElementaryInterpreter to easily
       * handle this code (only basic operations and literals required).
       */
      genMulHiCommon(x, y, BinaryOp.Long_>>)
    }

    def genMulUnsignedHi(x: Long, y: VarRef)(implicit pos: Position): Tree = {
      /* Math.unsignedMultiplyHigh(x, y)
       * Same remark as above.
       */
      genMulHiCommon(x, y, BinaryOp.Long_>>>)
    }

    private def genMulHiCommon(x: Long, y: VarRef, shiftOp: BinaryOp.Code)(
        implicit pos: Position): Tree = {
      /* In this code, >>> is unconditionally `Long_>>>`, but >> is `shiftOp`.
       *
       * val x0 = x & 0xffffffffL
       * val x1 = x >> 32
       * val y0 = y & 0xffffffffL
       * val y1 = y >> 32
       * val t = x1 * y0 + ((x0 * y0) >>> 32)
       * x1 * y1 + (t >> 32) + (((t & 0xffffffffL) + x0 * y1) >> 32)
       */

      val x0 = LongLiteral(x & 0xffffffffL)
      val x1 = LongLiteral(if (shiftOp == BinaryOp.Long_>>) x >> 32 else x >>> 32)
      val lit32 = IntLiteral(32)
      val litMask = LongLiteral(0xffffffffL)

      val y0Def = tempVarDef(y0Name, BinaryOp(BinaryOp.Long_&, litMask, y))
      val y0 = y0Def.ref
      val y1Def = tempVarDef(y1Name, BinaryOp(shiftOp, y, lit32))
      val y1 = y1Def.ref

      val tDef = tempVarDef(
        mulhiTempName,
        BinaryOp(
          BinaryOp.Long_+,
          BinaryOp(BinaryOp.Long_*, x1, y0),
          BinaryOp(
            BinaryOp.Long_>>>,
            BinaryOp(BinaryOp.Long_*, x0, y0),
            lit32
          )
        )
      )
      val t = tDef.ref

      val result = BinaryOp(
        BinaryOp.Long_+,
        BinaryOp(
          BinaryOp.Long_+,
          // x1 * y1
          BinaryOp(BinaryOp.Long_*, x1, y1),
          // t >> 32
          BinaryOp(shiftOp, t, lit32)
        ),
        // ((t & 0xffffffffL) + x0 * y1) >> 32
        BinaryOp(
          shiftOp,
          BinaryOp(
            BinaryOp.Long_+,
            BinaryOp(BinaryOp.Long_&, litMask, t),
            BinaryOp(BinaryOp.Long_*, x0, y1)
          ),
          lit32
        )
      )

      Block(
        y0Def,
        y1Def,
        tDef,
        result
      )
    }
  }
}
