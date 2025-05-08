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

import java.lang.constant.{Constable, ConstantDesc}

import scala.scalajs.js
import scala.scalajs.LinkingInfo._

/* This is a hijacked class. Its instances are primitive numbers.
 * Constructors are not emitted.
 */
final class Float private ()
    extends Number with Comparable[Float] with Constable with ConstantDesc {

  def this(value: scala.Float) = this()
  def this(s: String) = this()

  @inline def floatValue(): scala.Float =
    this.asInstanceOf[scala.Float]

  @inline override def byteValue(): scala.Byte = floatValue().toByte
  @inline override def shortValue(): scala.Short = floatValue().toShort
  @inline def intValue(): scala.Int = floatValue().toInt
  @inline def longValue(): scala.Long = floatValue().toLong
  @inline def doubleValue(): scala.Double = floatValue().toDouble

  @inline override def equals(that: Any): scala.Boolean =
    this eq that.asInstanceOf[AnyRef]

  @inline override def hashCode(): Int =
    Float.hashCode(floatValue())

  @inline override def compareTo(that: Float): Int =
    Float.compare(floatValue(), that.floatValue())

  @inline override def toString(): String =
    Float.toString(floatValue())

  @inline def isNaN(): scala.Boolean =
    Float.isNaN(floatValue())

  @inline def isInfinite(): scala.Boolean =
    Float.isInfinite(floatValue())

}

object Float {
  /* TYPE should be a `final val`, but that crashes the JVM back-end, so we
   * use a 'def' instead, which is binary compatible.
   */
  def TYPE: Class[_] = scala.Predef.classOf[scala.Float]

  final val POSITIVE_INFINITY = 1.0f / 0.0f
  final val NEGATIVE_INFINITY = 1.0f / -0.0f
  final val NaN = 0.0f / 0.0f
  final val MAX_VALUE = scala.Float.MaxValue
  final val MIN_NORMAL = 1.17549435e-38f
  final val MIN_VALUE = scala.Float.MinPositiveValue
  final val MAX_EXPONENT = 127
  final val MIN_EXPONENT = -126
  final val SIZE = 32
  final val BYTES = 4

  @inline def `new`(value: scala.Float): Float = valueOf(value)

  @inline def `new`(value: scala.Double): Float = valueOf(value.toFloat)

  @inline def `new`(s: String): Float = valueOf(s)

  @inline def valueOf(f: scala.Float): Float = f.asInstanceOf[Float]

  @inline def valueOf(s: String): Float = valueOf(parseFloat(s))

  private[this] lazy val parseFloatRegExp = new js.RegExp(
      "^" +
      "[\\x00-\\x20]*" +                 // optional whitespace
      "([+-]?)" +                        // 1: optional sign
      "(?:" +
        "(NaN)|" +                       // 2: NaN
        "(Infinity)|" +                  // 3: Infinity
        "(?:" +
          "(" +                          // 4: decimal notation
            "(?:(\\d+)(?:\\.(\\d*))?|" + // 5-6: w/ digit before .
              "\\.(\\d+))" +             // 7: w/o digit before .
            "(?:[eE]([+-]?\\d+))?" +     // 8: optional exponent
          ")|" +
          "(" +                          // 9: hexadecimal notation
            "0[xX]" +                    // hex marker
            "(?:([0-9A-Fa-f]+)(?:\\.([0-9A-Fa-f]*))?|" + // 10-11: w/ digit before .
              "\\.([0-9A-Fa-f]+))" +                     // 12: w/o digit before .
            "[pP]([+-]?\\d+)" +          // 13: binary exponent
          ")" +
        ")" +
        "[fFdD]?" +                      // optional float / double specifier (ignored)
      ")" +
      "[\\x00-\\x20]*" +                 // optional whitespace
      "$"
  )

  def parseFloat(s: String): scala.Float = {
    import Utils._

    val groups = parseFloatRegExp.exec(s)
    if (groups == null)
      throw new NumberFormatException(s"""For input string: "$s"""")

    val absResult = if (undefOrIsDefined(groups(2))) {
      scala.Float.NaN
    } else if (undefOrIsDefined(groups(3))) {
      scala.Float.PositiveInfinity
    } else if (undefOrIsDefined(groups(4))) {
      // Decimal notation
      val fullNumberStr = undefOrForceGet(groups(4))
      val integralPartStr = undefOrGetOrElse(groups(5))(() => "")
      val fractionalPartStr = undefOrGetOrElse(groups(6))(() => "") + undefOrGetOrElse(groups(7))(() => "")
      val exponentStr = undefOrGetOrElse(groups(8))(() => "0")
      parseFloatDecimal(fullNumberStr, integralPartStr, fractionalPartStr, exponentStr)
    } else {
      // Hexadecimal notation
      val integralPartStr = undefOrGetOrElse(groups(10))(() => "")
      val fractionalPartStr = undefOrGetOrElse(groups(11))(() => "") + undefOrGetOrElse(groups(12))(() => "")
      val binaryExpStr = undefOrForceGet(groups(13))
      parseFloatHexadecimal(integralPartStr, fractionalPartStr, binaryExpStr)
    }

    val signStr = undefOrForceGet(groups(1))
    if (signStr == "-")
      -absResult
    else
      absResult
  }

  private def parseFloatDecimal(fullNumberStr: String,
      integralPartStr: String, fractionalPartStr: String,
      exponentStr: String): scala.Float = {

    val z0 = js.Dynamic.global.parseFloat(fullNumberStr).asInstanceOf[scala.Double]
    val z = z0.toFloat
    val zDouble = z.toDouble

    if (zDouble == z0) {
      /* This branch is always taken when z0 is 0.0 or Infinity, which the
       * `else` branch assumes does not happen.
       */
      z
    } else {
      /* #4035 `z` might be 1 ULP above or below the best approximation if `z0`
       * is exactly halfway between two adjacent Float values.
       * We need to detect that case, and fall back to the slow algorithm.
       */
      if (zDouble == scala.Double.PositiveInfinity) {
        // Magical constant = Float.MaxValue.toDouble + (Math.ulp(Float.MaxValue).toDouble / 2.0)
        val mid = 3.4028235677973366e38
        if (z0 == mid)
          parseFloatDecimalCorrection(integralPartStr, fractionalPartStr, exponentStr, MAX_VALUE, z, mid)
        else
          z
      } else if (zDouble < z0) {
        val zUp = Math.nextUp(z)
        val mid = (zDouble + zUp.toDouble) / 2.0
        if (z0 == mid)
          parseFloatDecimalCorrection(integralPartStr, fractionalPartStr, exponentStr, z, zUp, mid)
        else
          z
      } else {
        val zDown = Math.nextDown(z)
        val mid = (zDouble + zDown.toDouble) / 2.0
        if (z0 == mid)
          parseFloatDecimalCorrection(integralPartStr, fractionalPartStr, exponentStr, zDown, z, mid)
        else
          z
      }
    }
  }

  /** Slow algorithm to correct the initial approximation.
   *
   *  `zDown` and `zUp` must be adjacent Float values that surround the exact
   *  result, `zDown` being the smallest one. `zUp` can be `Infinity`.
   *
   *  `mid` must be the mid-point between `zDown` and `zUp`. It is a `Double`
   *  so that it can exactly hold that value. If the exact value is below
   *  `mid`, this function returns `zDown`; if it is above `mid`, it returns
   *  `zUp`. If it is exactly equal to `mid`, `parseFloatCorrection` breaks
   *  the tie to even.
   *
   *  When `zUp` is `Infinity`, `mid` must be the value
   *  `3.4028235677973366e38`, which is equal to
   *  `Float.MaxValue.toDouble + (Math.ulp(Float.MaxValue).toDouble / 2.0)`.
   *
   *  ---
   *
   *  As proven in the paper "How to Read Float Point Numbers Accurately" by
   *  William D. Clinger, there is no solution that does not require big
   *  integer arithmetic at some point. We take inspiration from the
   *  `AlgorithmR` from that paper, which takes an initial value "close" to the
   *  best approximation and improves it by 1 ULP. Since we already have a
   *  close approximation (one that is at most 1 ULP away from the best one),
   *  we can use that. However, we can dramatically simplify the algorithm
   *  because we can leverage Double arithmetics to parse only a Float. In
   *  particular, we can accurately compute and represent the two adjacent
   *  Floats that enclose the best approximation, as well as the midpoint
   *  between those, which is a Double. We receive those from
   *  `parseFloatDecimal`, which already had to compute them in order to decide
   *  whether a correction was needed. The only real thing we keep from the
   *  paper is the step 3: how to accurately compare that midpoint with the
   *  exact value represented by the string, using big integer arithmetics.
   *  This allows us to decide whether we need to round up, down, or break a
   *  tie to even.
   *
   *  `AlgorithmR` in the paper is generic wrt. the bases of the input and
   *  output. In our case, the input base Δ is 10 and the output base β is 2.
   */
  private def parseFloatDecimalCorrection(integralPartStr: String,
      fractionalPartStr: String, exponentStr: String,
      zDown: scala.Float, zUp: scala.Float, mid: scala.Double): scala.Float = {

    /* Get the best available implementation of big integers for the given platform.
     *
     * If JS bigint's are supported, use them. Otherwise fall back on
     * `java.math.BigInteger`.
     *
     * We need a `linkTimeIf` here because the JS bigint implementation uses
     * the `**` operator, which does not link when `esVersion < ESVersion.ES2016`.
     */
    val bigIntImpl = linkTimeIf[BigIntImpl](esVersion >= ESVersion.ES2020) {
      BigIntImpl.JSBigInt
    } {
      BigIntImpl.JBigInteger
    }

    // 1. Accurately parse the string with the representation f × 10ᵉ

    val f: bigIntImpl.Repr = bigIntImpl.fromString(integralPartStr + fractionalPartStr)
    val e: Int = Integer.parseInt(exponentStr) - fractionalPartStr.length()

    /* Note: we know that `e` is "reasonable" (in the range [-324, +308]). If
     * it were way too big or way too small, the original JS `parseFloat` in
     * `parseFloatDecimal` would have returned `Infinity` or `0.0`,
     * respectively. In that case, we would have selected the first branch, and
     * never called `parseFloatDecimalCorrection`.
     *
     * Since `e` is reasonable and `fractionPartStr.length()` is a non-negative
     * Int, the above computation cannot underflow, and the only way it could
     * overflow is if the length of the string were `>= (Int.MaxValue - 308)`,
     * which is not worth caring for.
     */

    // 2. Accurately decompose `mid` with the representation m × 2ᵏ

    val mbits = 52 // number of bits of the mantissa (without the implicit '1')
    val kbits = 11 // number of bits of the exponent
    val bias = (1 << (kbits - 1)) - 1 // the bias of the exponent

    val midBits = Double.doubleToLongBits(mid)
    val biasedK = (midBits >> mbits).toInt

    /* Because `mid` is a double value halfway between two floats, it cannot
     * be a double subnormal (even if the two floats that surround it are
     * subnormal floats).
     */
    if (biasedK == 0)
      throw new AssertionError(s"parseFloatCorrection was given a subnormal mid: $mid")

    val mExplicitBits = midBits & ((1L << mbits) - 1)
    val mImplicit1Bit = 1L << mbits // the implicit '1' bit of a normalized floating-point number
    val m = bigIntImpl.fromUnsignedLong53(mExplicitBits | mImplicit1Bit)
    val k = biasedK - bias - mbits

    // 3. Accurately compare f × 10ᵉ to m × 2ᵏ

    import bigIntImpl.{multiplyBy2Pow, multiplyBy10Pow}

    val cmp = if (e >= 0) {
      if (k >= 0)
        bigIntImpl.compare(multiplyBy10Pow(f, e), multiplyBy2Pow(m, k))
      else
        bigIntImpl.compare(multiplyBy2Pow(multiplyBy10Pow(f, e), -k), m) // this branch may be dead code in practice
    } else {
      if (k >= 0)
        bigIntImpl.compare(f, multiplyBy2Pow(multiplyBy10Pow(m, -e), k))
      else
        bigIntImpl.compare(multiplyBy2Pow(f, -k), multiplyBy10Pow(m, -e))
    }

    // 4. Choose zDown or zUp depending on the result of the comparison

    if (cmp < 0)
      zDown
    else if (cmp > 0)
      zUp
    else if ((floatToIntBits(zDown) & 1) == 0) // zDown is even
      zDown
    else
      zUp
  }

  /** An implementation of big integer arithmetics that we need in the above method. */
  private sealed abstract class BigIntImpl {
    type Repr

    def fromString(str: String): Repr

    /** Creates a big integer from a `Long` that needs at most 53 bits (unsigned). */
    def fromUnsignedLong53(x: scala.Long): Repr

    def multiplyBy2Pow(v: Repr, e: Int): Repr
    def multiplyBy10Pow(v: Repr, e: Int): Repr

    def compare(x: Repr, y: Repr): Int
  }

  private object BigIntImpl {
    object JSBigInt extends BigIntImpl {
      type Repr = js.BigInt

      @inline def fromString(str: String): Repr = js.BigInt(str)

      // The 53-bit restriction guarantees that the conversion to `Double` is lossless.
      @inline def fromUnsignedLong53(x: scala.Long): Repr = js.BigInt(x.toDouble)

      @inline def multiplyBy2Pow(v: Repr, e: Int): Repr = v << js.BigInt(e)
      @inline def multiplyBy10Pow(v: Repr, e: Int): Repr = v * (js.BigInt(10) ** js.BigInt(e))

      @inline def compare(x: Repr, y: Repr): Int = {
        if (x < y) -1
        else if (x > y) 1
        else 0
      }
    }

    object JBigInteger extends BigIntImpl {
      import java.math.BigInteger

      type Repr = BigInteger

      @inline def fromString(str: String): Repr = new BigInteger(str)
      @inline def fromUnsignedLong53(x: scala.Long): Repr = BigInteger.valueOf(x)

      @inline def multiplyBy2Pow(v: Repr, e: Int): Repr = v.shiftLeft(e)
      @inline def multiplyBy10Pow(v: Repr, e: Int): Repr = v.multiply(BigInteger.TEN.pow(e))

      @inline def compare(x: Repr, y: Repr): Int = x.compareTo(y)
    }
  }

  private def parseFloatHexadecimal(integralPartStr: String,
      fractionalPartStr: String, binaryExpStr: String): scala.Float = {
    val doubleValue = Double.parseHexDoubleImpl(integralPartStr,
        fractionalPartStr, binaryExpStr, maxPrecisionChars = 7)
    doubleValue.toFloat
  }

  @inline def toString(f: scala.Float): String =
    "" + f

  def toHexString(f: scala.Float): String = {
    val ebits = 8 // exponent size
    val mbits = 23 // mantissa size
    val bias = (1 << (ebits - 1)) - 1

    val bits = floatToIntBits(f)
    val s = bits < 0
    val m = bits & ((1 << mbits) - 1)
    val e = (bits >>> mbits).toInt & ((1 << ebits) - 1) // biased

    val posResult = if (e > 0) {
      if (e == (1 << ebits) - 1) {
        // Special
        if (m != 0) "NaN"
        else "Infinity"
      } else {
        // Normalized
        "0x1." + mantissaToHexString(m) + "p" + (e - bias)
      }
    } else {
      if (m != 0) {
        // Subnormal
        "0x0." + mantissaToHexString(m) + "p-126"
      } else {
        // Zero
        "0x0.0p0"
      }
    }

    if (bits < 0) "-" + posResult else posResult
  }

  @inline
  private def mantissaToHexString(m: Int): String = {
    @inline def padHex6(i: Int): String = {
      val s = Integer.toHexString(i)
      "000000".substring(s.length) + s // 6 zeros
    }

    // The << 1 turns `m` from a 23-bit int into a 24-bit int (multiple of 4)
    val padded = padHex6(m << 1)
    var len = padded.length
    while (len > 1 && padded.charAt(len - 1) == '0')
      len -= 1
    padded.substring(0, len)
  }

  @inline def compare(a: scala.Float, b: scala.Float): scala.Int =
    Double.compare(a, b)

  @inline def isNaN(v: scala.Float): scala.Boolean =
    v != v

  @inline def isInfinite(v: scala.Float): scala.Boolean =
    v == POSITIVE_INFINITY || v == NEGATIVE_INFINITY

  @inline def isFinite(f: scala.Float): scala.Boolean =
    !isNaN(f) && !isInfinite(f)

  @inline def hashCode(value: scala.Float): Int =
    Double.hashCode(value.toDouble)

  // Wasm intrinsic
  @inline def intBitsToFloat(bits: scala.Int): scala.Float =
    FloatingPointBits.intBitsToFloat(bits)

  // Wasm intrinsic
  @inline def floatToIntBits(value: scala.Float): scala.Int =
    FloatingPointBits.floatToIntBits(value)

  @inline def sum(a: scala.Float, b: scala.Float): scala.Float =
    a + b

  @inline def max(a: scala.Float, b: scala.Float): scala.Float =
    Math.max(a, b)

  @inline def min(a: scala.Float, b: scala.Float): scala.Float =
    Math.min(a, b)
}
