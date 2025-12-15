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

package java.lang

import scala.language.implicitConversions

import java.util.internal.GenericArrayOps._

/** Typeclass for integer, float and bit manipulations of a given bit size.
 *
 *  This typeclass is designed to generically write algorithms that manipulate
 *  integers, floats, and their bits. We have a number of algorithms that need
 *  to be duplicated for `Int`/`Long` or `Float`/`Double`. This typeclass
 *  allows to write them only once.
 *
 *  Method that use this typeclass should always be inlined into a context
 *  where the actual types are statically known. Otherwise, usage will result
 *  in poor performance. A typical shape for such a method is
 *
 *  {{{
 *  @noinline def algo(x: Float): Int = algoGeneric(x)
 *  @noinline def algo(x: Double): Long = algoGeneric(x)
 *
 *  @inline
 *  private def algoGeneric[I, F](x: I)(implicit ops: IntFloatBits[I, F]): I = {
 *    import ops._
 *
 *    // ...
 *  }
 *  }}}
 *
 *  Inside the body, one can usually write the code in a natural way, as if the
 *  types `I` and `F` were concrete.
 *
 *  There is one exception: avoid declaring `var`s of type `I` or `F`. They
 *  will be boxed. Instead of `I`, declare a `val` obtained with
 *  `ops.newIntBox`. This gives you a concrete instance of an inlineable class
 *  that has the correct, monomorphic type of variable inside. Paradoxically,
 *  this allows the optimizer to completely remove the box, unlike a `var` of a
 *  generic type. If you need a `var` of type `F`, add a corresponding
 *  `def newFloatBox` to this class; so far we have had no use case for it.
 */
private[java] sealed abstract class IntFloatBits[I, F] {
  import IntFloatBits._

  // Type members usable in this class and its subclasses, for copy-pastable signatures
  type IntType = I
  type FloatType = F

  // Essential constants

  def intArrayOps: ArrayOps[I] with ArrayCreateOps[I]

  /** Bit size, 32 or 64. */
  def bitSize: Int

  /** Number of mantissa bits in the floating point format. */
  def mbits: Int

  /** Number of exponent bits in the floating point format. */
  def ebits: Int

  // Common constants
  // We *could* derive them, but that would be a lot of busywork for the optimizer.

  def zero: IntType
  def one: IntType
  def minusOne: IntType
  def minInt: IntType
  def maxInt: IntType

  def fzero: FloatType
  def fone: FloatType
  def fminNormal: FloatType
  def fminSubnormal: FloatType
  def fmax: FloatType
  def fnan: FloatType
  def finf: FloatType
  def fneginf: FloatType

  // Essential operations

  def newIntBox(x: I): Box[IntType]

  def fromInt32(x: Int): IntType
  def fromUnsignedInt32(x: Int): IntType
  def toInt32Wrap(x: IntType): Int

  def fromFloat32(x: scala.Float): FloatType
  def fromDoubleRound(x: scala.Double): FloatType
  def toDouble(x: FloatType): scala.Double

  def intToFloat(x: IntType): FloatType

  def floatFromBits(bits: IntType): FloatType
  def floatToBits(x: FloatType): IntType

  def add(x: IntType, y: IntType): IntType
  def sub(x: IntType, y: IntType): IntType
  def mul(x: IntType, y: IntType): IntType
  def div(x: IntType, y: IntType): IntType
  def rem(x: IntType, y: IntType): IntType

  def and(x: IntType, y: IntType): IntType
  def or(x: IntType, y: IntType): IntType
  def xor(x: IntType, y: IntType): IntType
  def shl(x: IntType, n: Int): IntType
  def shr(x: IntType, n: Int): IntType
  def sar(x: IntType, n: Int): IntType

  def clz(x: IntType): Int

  def ieq(x: IntType, y: IntType): scala.Boolean
  def ine(x: IntType, y: IntType): scala.Boolean
  def ilt(x: IntType, y: IntType): scala.Boolean
  def ile(x: IntType, y: IntType): scala.Boolean
  def igt(x: IntType, y: IntType): scala.Boolean
  def ige(x: IntType, y: IntType): scala.Boolean

  def unsigned_<(x: IntType, y: IntType): scala.Boolean
  def unsigned_<=(x: IntType, y: IntType): scala.Boolean
  def unsigned_>(x: IntType, y: IntType): scala.Boolean
  def unsigned_>=(x: IntType, y: IntType): scala.Boolean

  def fadd(x: FloatType, y: FloatType): FloatType
  def fsub(x: FloatType, y: FloatType): FloatType
  def fmul(x: FloatType, y: FloatType): FloatType
  def fdiv(x: FloatType, y: FloatType): FloatType

  def fabs(x: FloatType): FloatType
  def fnextUp(x: FloatType): FloatType
  def fnextDown(x: FloatType): FloatType

  def feq(x: FloatType, y: FloatType): scala.Boolean
  def fne(x: FloatType, y: FloatType): scala.Boolean
  def flt(x: FloatType, y: FloatType): scala.Boolean
  def fle(x: FloatType, y: FloatType): scala.Boolean
  def fgt(x: FloatType, y: FloatType): scala.Boolean
  def fge(x: FloatType, y: FloatType): scala.Boolean

  def isSpecialBitPattern(bits: IntType): scala.Boolean

  // Derived constants

  @inline final def signBit: IntType = minInt

  @inline final def logBitSize: Int = 31 - Integer.numberOfLeadingZeros(bitSize)

  /** Mask for the mantissa bits. */
  @inline final def mmask: IntType = sub(shl(one, mbits), one)

  /** Mask for the exponent bits, when shifted to low-order bits. */
  @inline final def emask: Int = (1 << ebits) - 1

  /** Bias of the floating point exponent for normal values (positive). */
  @inline final def bias: Int = (1 << (ebits - 1)) - 1

  // Derived operations

  @inline final def isNaN(x: FloatType): scala.Boolean = fne(x, x)

  @inline final def mantissaBitsOf(bits: IntType): IntType = and(bits, mmask)
  @inline final def exponentOf(bits: IntType): Int = toInt32Wrap(shr(bits, mbits)) & emask

  @inline final implicit def intOps(x: IntType): IntOps[I, F] = new IntOps(x)(this)
  @inline final implicit def floatOps(x: FloatType): FloatOps[I, F] = new FloatOps(x)(this)
}

private[java] object IntFloatBits {
  sealed abstract class Box[T] {
    def apply(): T
    def update(v: T): Unit
  }

  @inline
  final class Int32Box(private var value: scala.Int) extends Box[scala.Int] {
    @inline def apply(): scala.Int = value
    @inline def update(v: scala.Int): Unit = value = v
  }

  @inline
  final class Int64Box(private var value: scala.Long) extends Box[scala.Long] {
    @inline def apply(): scala.Long = value
    @inline def update(v: scala.Long): Unit = value = v
  }

  @inline
  final class IntOps[I, F](private val x: I)(implicit ops: IntFloatBits[I, F]) {
    @inline def unary_- : I = ops.sub(ops.zero, x) // scalastyle:ignore
    @inline def unary_~ : I = ops.xor(ops.minusOne, x) // scalastyle:ignore

    @inline def +(y: I): I = ops.add(x, y)
    @inline def -(y: I): I = ops.sub(x, y)
    @inline def *(y: I): I = ops.mul(x, y)
    @inline def /(y: I): I = ops.div(x, y)
    @inline def %(y: I): I = ops.rem(x, y)

    @inline def &(y: I): I = ops.and(x, y)
    @inline def |(y: I): I = ops.or(x, y)
    @inline def ^(y: I): I = ops.xor(x, y)
    @inline def <<(y: Int): I = ops.shl(x, y)
    @inline def >>>(y: Int): I = ops.shr(x, y)
    @inline def >>(y: Int): I = ops.sar(x, y)

    @inline def ===(y: I): scala.Boolean = ops.ieq(x, y)
    @inline def !==(y: I): scala.Boolean = ops.ine(x, y)
    @inline def <(y: I): scala.Boolean = ops.ilt(x, y)
    @inline def <=(y: I): scala.Boolean = ops.ile(x, y)
    @inline def >(y: I): scala.Boolean = ops.igt(x, y)
    @inline def >=(y: I): scala.Boolean = ops.ige(x, y)
  }

  @inline
  final class FloatOps[I, F](private val x: F)(implicit ops: IntFloatBits[I, F]) {
    @inline def unary_- : F = ops.fsub(ops.fzero, x) // scalastyle:ignore

    @inline def +(y: F): F = ops.fadd(x, y)
    @inline def -(y: F): F = ops.fsub(x, y)
    @inline def *(y: F): F = ops.fmul(x, y)
    @inline def /(y: F): F = ops.fdiv(x, y)

    @inline def ===(y: F): scala.Boolean = ops.feq(x, y)
    @inline def !==(y: F): scala.Boolean = ops.fne(x, y)
    @inline def <(y: F): scala.Boolean = ops.flt(x, y)
    @inline def <=(y: F): scala.Boolean = ops.fle(x, y)
    @inline def >(y: F): scala.Boolean = ops.fgt(x, y)
    @inline def >=(y: F): scala.Boolean = ops.fge(x, y)
  }

  implicit object Bits32 extends IntFloatBits[Int, scala.Float] {
    @inline def intArrayOps: IntArrayOps.type = IntArrayOps

    @inline def bitSize: Int = 32
    @inline def mbits: Int = 23
    @inline def ebits: Int = 8

    @inline def zero: IntType = 0
    @inline def one: IntType = 1
    @inline def minusOne: IntType = -1
    @inline def minInt: IntType = Int.MinValue
    @inline def maxInt: IntType = Int.MaxValue

    @inline def fzero: FloatType = 0.0f
    @inline def fone: FloatType = 1.0f
    @inline def fminNormal: FloatType = Float.MIN_NORMAL
    @inline def fminSubnormal: FloatType = scala.Float.MinPositiveValue
    @inline def fmax: FloatType = scala.Float.MaxValue
    @inline def fnan: FloatType = scala.Float.NaN
    @inline def finf: FloatType = scala.Float.PositiveInfinity
    @inline def fneginf: FloatType = scala.Float.NegativeInfinity

    @inline def newIntBox(x: IntType): Box[IntType] = new Int32Box(x)

    @inline def fromInt32(x: Int): IntType = x
    @inline def fromUnsignedInt32(x: Int): IntType = x
    @inline def toInt32Wrap(x: IntType): Int = x

    @inline def fromFloat32(x: scala.Float): FloatType = x
    @inline def fromDoubleRound(x: scala.Double): FloatType = x.toFloat
    @inline def toDouble(x: FloatType): scala.Double = x.toDouble

    @inline def intToFloat(x: IntType): FloatType = x.toFloat

    @inline def floatFromBits(bits: IntType): FloatType = Float.intBitsToFloat(bits)
    @inline def floatToBits(x: FloatType): IntType = Float.floatToRawIntBits(x)

    @inline def add(x: IntType, y: IntType): IntType = x + y
    @inline def sub(x: IntType, y: IntType): IntType = x - y
    @inline def mul(x: IntType, y: IntType): IntType = x * y
    @inline def div(x: IntType, y: IntType): IntType = x / y
    @inline def rem(x: IntType, y: IntType): IntType = x % y

    @inline def and(x: IntType, y: IntType): IntType = x & y
    @inline def or(x: IntType, y: IntType): IntType = x | y
    @inline def xor(x: IntType, y: IntType): IntType = x ^ y
    @inline def shl(x: IntType, n: Int): IntType = x << n
    @inline def shr(x: IntType, n: Int): IntType = x >>> n
    @inline def sar(x: IntType, n: Int): IntType = x >> n

    @inline def ieq(x: IntType, y: IntType): scala.Boolean = x == y
    @inline def ine(x: IntType, y: IntType): scala.Boolean = x != y
    @inline def ilt(x: IntType, y: IntType): scala.Boolean = x < y
    @inline def ile(x: IntType, y: IntType): scala.Boolean = x <= y
    @inline def igt(x: IntType, y: IntType): scala.Boolean = x > y
    @inline def ige(x: IntType, y: IntType): scala.Boolean = x >= y

    @inline def unsigned_<(x: IntType, y: IntType): scala.Boolean = Integer.unsigned_<(x, y)
    @inline def unsigned_<=(x: IntType, y: IntType): scala.Boolean = Integer.unsigned_<=(x, y)
    @inline def unsigned_>(x: IntType, y: IntType): scala.Boolean = Integer.unsigned_>(x, y)
    @inline def unsigned_>=(x: IntType, y: IntType): scala.Boolean = Integer.unsigned_>=(x, y)

    @inline def clz(x: IntType): Int = Integer.numberOfLeadingZeros(x)

    @inline def fadd(x: FloatType, y: FloatType): FloatType = x + y
    @inline def fsub(x: FloatType, y: FloatType): FloatType = x - y
    @inline def fmul(x: FloatType, y: FloatType): FloatType = x * y
    @inline def fdiv(x: FloatType, y: FloatType): FloatType = x / y

    @inline def fabs(x: FloatType): FloatType = Math.abs(x)
    @inline def fnextUp(x: FloatType): FloatType = Math.nextUp(x)
    @inline def fnextDown(x: FloatType): FloatType = Math.nextDown(x)

    @inline def feq(x: FloatType, y: FloatType): scala.Boolean = x == y
    @inline def fne(x: FloatType, y: FloatType): scala.Boolean = x != y
    @inline def flt(x: FloatType, y: FloatType): scala.Boolean = x < y
    @inline def fle(x: FloatType, y: FloatType): scala.Boolean = x <= y
    @inline def fgt(x: FloatType, y: FloatType): scala.Boolean = x > y
    @inline def fge(x: FloatType, y: FloatType): scala.Boolean = x >= y

    @inline def isSpecialBitPattern(bits: IntType): scala.Boolean = Float.isSpecialBitPattern(bits)
  }

  implicit object Bits64 extends IntFloatBits[scala.Long, scala.Double] {
    @inline def intArrayOps: LongArrayOps.type = LongArrayOps

    @inline def bitSize: Int = 64
    @inline def mbits: Int = 52
    @inline def ebits: Int = 11

    @inline def zero: IntType = 0L
    @inline def one: IntType = 1L
    @inline def minusOne: IntType = -1L
    @inline def minInt: IntType = scala.Long.MinValue
    @inline def maxInt: IntType = scala.Long.MaxValue

    @inline def fzero: FloatType = 0.0
    @inline def fone: FloatType = 1.0
    @inline def fminNormal: FloatType = Double.MIN_NORMAL
    @inline def fminSubnormal: FloatType = scala.Double.MinPositiveValue
    @inline def fmax: FloatType = scala.Double.MaxValue
    @inline def fnan: FloatType = scala.Double.NaN
    @inline def finf: FloatType = scala.Double.PositiveInfinity
    @inline def fneginf: FloatType = scala.Double.NegativeInfinity

    @inline def newIntBox(x: IntType): Box[IntType] = new Int64Box(x)

    @inline def fromInt32(x: Int): IntType = x.toLong
    @inline def fromUnsignedInt32(x: Int): IntType = Integer.toUnsignedLong(x)
    @inline def toInt32Wrap(x: IntType): Int = x.toInt

    @inline def fromFloat32(x: scala.Float): FloatType = x.toDouble
    @inline def fromDoubleRound(x: scala.Double): FloatType = x
    @inline def toDouble(x: FloatType): scala.Double = x

    @inline def intToFloat(x: IntType): FloatType = x.toDouble

    @inline def floatFromBits(bits: IntType): FloatType = Double.longBitsToDouble(bits)
    @inline def floatToBits(x: FloatType): IntType = Double.doubleToRawLongBits(x)

    @inline def add(x: IntType, y: IntType): IntType = x + y
    @inline def sub(x: IntType, y: IntType): IntType = x - y
    @inline def mul(x: IntType, y: IntType): IntType = x * y
    @inline def div(x: IntType, y: IntType): IntType = x / y
    @inline def rem(x: IntType, y: IntType): IntType = x % y

    @inline def and(x: IntType, y: IntType): IntType = x & y
    @inline def or(x: IntType, y: IntType): IntType = x | y
    @inline def xor(x: IntType, y: IntType): IntType = x ^ y
    @inline def shl(x: IntType, n: Int): IntType = x << n
    @inline def shr(x: IntType, n: Int): IntType = x >>> n
    @inline def sar(x: IntType, n: Int): IntType = x >> n

    @inline def ieq(x: IntType, y: IntType): scala.Boolean = x == y
    @inline def ine(x: IntType, y: IntType): scala.Boolean = x != y
    @inline def ilt(x: IntType, y: IntType): scala.Boolean = x < y
    @inline def ile(x: IntType, y: IntType): scala.Boolean = x <= y
    @inline def igt(x: IntType, y: IntType): scala.Boolean = x > y
    @inline def ige(x: IntType, y: IntType): scala.Boolean = x >= y

    @inline def unsigned_<(x: IntType, y: IntType): scala.Boolean = Long.unsigned_<(x, y)
    @inline def unsigned_<=(x: IntType, y: IntType): scala.Boolean = Long.unsigned_<=(x, y)
    @inline def unsigned_>(x: IntType, y: IntType): scala.Boolean = Long.unsigned_>(x, y)
    @inline def unsigned_>=(x: IntType, y: IntType): scala.Boolean = Long.unsigned_>=(x, y)

    @inline def clz(x: IntType): Int = Long.numberOfLeadingZeros(x)

    @inline def fadd(x: FloatType, y: FloatType): FloatType = x + y
    @inline def fsub(x: FloatType, y: FloatType): FloatType = x - y
    @inline def fmul(x: FloatType, y: FloatType): FloatType = x * y
    @inline def fdiv(x: FloatType, y: FloatType): FloatType = x / y

    @inline def fabs(x: FloatType): FloatType = Math.abs(x)
    @inline def fnextUp(x: FloatType): FloatType = Math.nextUp(x)
    @inline def fnextDown(x: FloatType): FloatType = Math.nextDown(x)

    @inline def feq(x: FloatType, y: FloatType): scala.Boolean = x == y
    @inline def fne(x: FloatType, y: FloatType): scala.Boolean = x != y
    @inline def flt(x: FloatType, y: FloatType): scala.Boolean = x < y
    @inline def fle(x: FloatType, y: FloatType): scala.Boolean = x <= y
    @inline def fgt(x: FloatType, y: FloatType): scala.Boolean = x > y
    @inline def fge(x: FloatType, y: FloatType): scala.Boolean = x >= y

    @inline def isSpecialBitPattern(bits: IntType): scala.Boolean = Double.isSpecialBitPattern(bits)
  }
}
