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

import java.lang.Math
import java.math.BigInteger

private[dectoflt] sealed trait FloatingPointFormat {

  /** Representation for this floating format.
   *
   *  In an ideal world, this would be an abstract type member, refined to
   *  `Float` in `Binary32` and `Double` in `Binary64`. However, that causes
   *  boxing. Instead, we represent both with a concrete-but-opaque value
   *  class, which always contains a primitive `Double`. The value class
   *  ensures we do not accidently apply double operations instead of float
   *  operations when using `Binary32`.
   */
  type Repr = FloatingPointFormat.ReprImpl

  val ExpBits: Int
  val SigBits: Int
  val CeilLog5OfMaxSig: Int
  val MaxSig: Long
  val PositiveInfinity: Repr

  final val ExplicitSigBits: Int = SigBits - 1
  final val MaxSigBigInt: BigInteger = BigInteger.valueOf(MaxSig)

  def powerOfTen(i: Int): Repr
  def nextDown(v: Repr): Repr
  def nextUp(v: Repr): Repr
  def mul(a: Repr, b: Repr): Repr
  def div(a: Repr, b: Repr): Repr

  /** Decompose the given floating-point value into a normalized mantissa and a power of 2. */
  def frexp(v: Repr): FloatingPoint
  def fromLong(v: Long): Repr
  def reinterpretBits(v: BigInteger): Repr
  def toIEEE754(v: FloatingPoint): Repr
}

private[dectoflt] object FloatingPointFormat {
  final class ReprImpl private (private val x: Double) extends AnyVal {
    @inline def toFloat: Float = x.toFloat
    @inline def toDouble: Double = x
  }

  object ReprImpl {
    @inline def apply(x: Float): ReprImpl = new ReprImpl(x.toDouble)
    @inline def apply(x: Double): ReprImpl = new ReprImpl(x)
  }
}

private[dectoflt] object Binary32 extends FloatingPointFormat {
  import FloatingPointFormat.ReprImpl

  final val ExpBits = 8
  final val SigBits = 24

  final val MaxSig = (1L << SigBits) - 1
  final val CeilLog5OfMaxSig = 11
  final val PositiveInfinity = ReprImpl(Float.PositiveInfinity)

  private final val PowerOfTens = Array(
      1.0f,
      10.0f,
      100.0f,
      1000.0f,
      10000.0f,
      100000.0f,
      1000000.0f,
      10000000.0f,
      100000000.0f,
      1000000000.0f,
      10000000000.0f
  )

  def powerOfTen(i: Int): Repr = ReprImpl(PowerOfTens(i))
  def nextDown(v: Repr): Repr = ReprImpl(Math.nextDown(v.toFloat))
  def nextUp(v: Repr): Repr = ReprImpl(Math.nextUp(v.toFloat))
  def mul(a: Repr, b: Repr): Repr = ReprImpl(a.toFloat * b.toFloat)
  def div(a: Repr, b: Repr): Repr = ReprImpl(a.toFloat / b.toFloat)

  def frexp(v: Repr): FloatingPoint = {
    val bits = java.lang.Float.floatToRawIntBits(v.toFloat)
    val m = (bits & 0x7fffff + (1 << ExplicitSigBits)).toLong
    val exp = ((bits >>> ExplicitSigBits) & 0xff) - ((1 << (ExpBits - 1)) - 1) - ExplicitSigBits
    FloatingPoint.normalized(m, exp)
  }

  def reinterpretBits(v: BigInteger): Repr =
    ReprImpl(java.lang.Float.intBitsToFloat(v.intValue()))

  def fromLong(v: Long): Repr = ReprImpl(v.toFloat)

  def toIEEE754(v: FloatingPoint): Repr = {
    val rounded = v.roundNormal(this)
    val sig = rounded.f
    val exponent = rounded.e

    // Remove the leading implicit bit
    // It is safe cast sig.toInt, because Float has 23 sig bits
    val encodedSig: Int = sig.toInt - (1 << (ExplicitSigBits))
    // Adjust the exponent for exponent bias and mantissa shift
    val encodedExp: Int = exponent + ((1 << (ExpBits - 1)) - 1) + ExplicitSigBits
    // combine bits
    val bits = (encodedExp << ExplicitSigBits) | encodedSig
    ReprImpl(java.lang.Float.intBitsToFloat(bits))
  }
}

private[dectoflt] object Binary64 extends FloatingPointFormat {
  import FloatingPointFormat.ReprImpl

  final val ExpBits = 11
  // final val ExplicitSigBits = 52
  final val SigBits = 53
  final val MaxSig: Long = (1L << SigBits) - 1
  final val CeilLog5OfMaxSig = 23

  private final val PowerOfTens = Array(
      1.0,
      10.0,
      100.0,
      1000.0,
      10000.0,
      100000.0,
      1000000.0,
      10000000.0,
      100000000.0,
      1000000000.0,
      10000000000.0,
      100000000000.0,
      1000000000000.0,
      10000000000000.0,
      100000000000000.0,
      1000000000000000.0,
      10000000000000000.0,
      100000000000000000.0,
      1000000000000000000.0,
      10000000000000000000.0,
      100000000000000000000.0,
      1000000000000000000000.0,
      10000000000000000000000.0
  )

  final val PositiveInfinity = ReprImpl(Double.PositiveInfinity)

  def powerOfTen(i: Int): Repr = ReprImpl(PowerOfTens(i))
  def nextDown(v: Repr): Repr = ReprImpl(Math.nextDown(v.toDouble))
  def nextUp(v: Repr): Repr = ReprImpl(Math.nextUp(v.toDouble))
  def mul(a: Repr, b: Repr): Repr = ReprImpl(a.toDouble * b.toDouble)
  def div(a: Repr, b: Repr): Repr = ReprImpl(a.toDouble / b.toDouble)

  def frexp(v: Repr): FloatingPoint = {
    val bits = java.lang.Double.doubleToLongBits(v.toDouble)
    val m = bits & 0xfffffffffffffL + (1 << ExplicitSigBits)
    val exp =
      ((bits >>> ExplicitSigBits) & 0x7ff).toInt - ((1 << (ExpBits - 1)) - 1) - ExplicitSigBits
    FloatingPoint.normalized(m, exp)
  }

  def reinterpretBits(v: BigInteger): Repr =
    ReprImpl(java.lang.Double.longBitsToDouble(v.longValue()))

  def fromLong(v: Long): Repr = ReprImpl(v.toDouble)

  def toIEEE754(v: FloatingPoint): Repr = {
    val rounded = v.roundNormal(this)
    val sig = rounded.f
    val exponent = rounded.e

    // Remove the leading implicit bit
    val encodedSig: Long = sig - (1L << (ExplicitSigBits))
    // Adjust the exponent for exponent bias and mantissa shift
    val encodedExp: Long = exponent + ((1 << (ExpBits - 1)) - 1) + ExplicitSigBits
    // combine bits
    val bits = encodedExp << ExplicitSigBits | encodedSig
    ReprImpl(java.lang.Double.longBitsToDouble(bits))
  }
}
