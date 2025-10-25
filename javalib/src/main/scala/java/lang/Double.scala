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
import scala.scalajs.LinkingInfo

import Utils._

/* This is a hijacked class. Its instances are primitive numbers.
 * Constructors are not emitted.
 */
final class Double private ()
    extends Number with Comparable[Double] with Constable with ConstantDesc {

  def this(value: scala.Double) = this()
  def this(s: String) = this()

  @inline def doubleValue(): scala.Double =
    this.asInstanceOf[scala.Double]

  @inline override def byteValue(): scala.Byte = doubleValue().toByte
  @inline override def shortValue(): scala.Short = doubleValue().toShort
  @inline def intValue(): scala.Int = doubleValue().toInt
  @inline def longValue(): scala.Long = doubleValue().toLong
  @inline def floatValue(): scala.Float = doubleValue().toFloat

  @inline override def equals(that: Any): scala.Boolean =
    this eq that.asInstanceOf[AnyRef]

  @inline override def hashCode(): Int =
    Double.hashCode(doubleValue())

  @inline override def compareTo(that: Double): Int =
    Double.compare(doubleValue(), that.doubleValue())

  @inline override def toString(): String =
    Double.toString(doubleValue())

  @inline def isNaN(): scala.Boolean =
    Double.isNaN(doubleValue())

  @inline def isInfinite(): scala.Boolean =
    Double.isInfinite(doubleValue())

}

object Double {
  /* TYPE should be a `final val`, but that crashes the JVM back-end, so we
   * use a 'def' instead, which is binary compatible.
   */
  def TYPE: Class[_] = scala.Predef.classOf[scala.Double]

  final val POSITIVE_INFINITY = 1.0 / 0.0
  final val NEGATIVE_INFINITY = 1.0 / -0.0
  final val NaN = 0.0 / 0.0
  final val MAX_VALUE = scala.Double.MaxValue
  final val MIN_NORMAL = 2.2250738585072014e-308
  final val MIN_VALUE = scala.Double.MinPositiveValue
  final val MAX_EXPONENT = 1023
  final val MIN_EXPONENT = -1022
  final val SIZE = 64
  final val BYTES = 8

  private final val PosInfinityBits = 0x7ff0000000000000L
  private final val CanonicalNaNBits = 0x7ff8000000000000L

  @inline def `new`(value: scala.Double): Double = valueOf(value)

  @inline def `new`(s: String): Double = valueOf(s)

  @inline def valueOf(d: scala.Double): Double = d.asInstanceOf[Double]

  @inline def valueOf(s: String): Double = valueOf(parseDouble(s))

  private[this] lazy val doubleStrPat = new js.RegExp(
      "^"                   +
      "[\\x00-\\x20]*("     + // optional whitespace
      "[+-]?"               + // optional sign
      "(?:NaN|Infinity|"    + // special cases
       "(?:\\d+\\.?\\d*|"   + // literal w/  leading digit
        "\\.\\d+)"          + // literal w/o leading digit
       "(?:[eE][+-]?\\d+)?" + // optional exponent
      ")[fFdD]?"            + // optional float / double specifier (ignored)
      ")[\\x00-\\x20]*"     + // optional whitespace
      "$")

  private[this] lazy val doubleStrHexPat = new js.RegExp(
      "^"                   +
      "[\\x00-\\x20]*"      + // optional whitespace
      "([+-]?)"             + // optional sign
      "0[xX]"               + // hex marker
      "([0-9A-Fa-f]*)"      + // integral part
      "\\.?([0-9A-Fa-f]*)"  + // fractional part
      "[pP]([+-]?\\d+)"     + // binary exponent
      "[fFdD]?"             + // optional float / double specifier (ignored)
      "[\\x00-\\x20]*"      + // optional whitespace
      "$")

  def parseDouble(s: String): scala.Double = {
    val groups = doubleStrPat.exec(s)
    if (groups != null)
      js.Dynamic.global.parseFloat(undefOrForceGet[String](groups(1))).asInstanceOf[scala.Double]
    else
      parseDoubleSlowPath(s)
  }

  // Slow path of `parseDouble` for hexadecimal notation and failure
  private def parseDoubleSlowPath(s: String): scala.Double = {
    def fail(): Nothing =
      throw new NumberFormatException(s"""For input string: "$s"""")

    val groups = doubleStrHexPat.exec(s)
    if (groups == null)
      fail()

    val signStr = undefOrForceGet(groups(1))
    val integralPartStr = undefOrForceGet(groups(2))
    val fractionalPartStr = undefOrForceGet(groups(3))
    val binaryExpStr = undefOrForceGet(groups(4))

    if (integralPartStr == "" && fractionalPartStr == "")
      fail()

    val absResult = parseHexDoubleImpl(integralPartStr, fractionalPartStr,
        binaryExpStr, maxPrecisionChars = 15)

    if (signStr == "-")
      -absResult
    else
      absResult
  }

  /** Parses a non-negative Double expressed in hexadecimal notation.
   *
   *  This returns the result of parsing
   *  {{{
   *  "0x" + integralPartStr + "." + fractionalPartStr + "p" + binaryExpStr
   *  }}}
   *  but truncating the total number of characters in `integralPartStr` and
   *  `fractionalPartStr` participating in the resulting precision to
   *  `maxPrecisionChars`.
   *
   *  `maxPrecisionChars` must be 15 to parse Double values, and 7 to parse
   *  Float values.
   */
  private[lang] def parseHexDoubleImpl(integralPartStr: String,
      fractionalPartStr: String, binaryExpStr: String,
      maxPrecisionChars: Int): scala.Double = {
    // scalastyle:off return

    /* We concatenate the integral part and fractional part together, then
     * we parse the result as an integer. This means that we need to remember
     * a correction to be applied to the final result, as a diff to the
     * binary exponent
     */
    val mantissaStr0 = integralPartStr + fractionalPartStr
    val correction1 = -(fractionalPartStr.length * 4) // 1 hex == 4 bits

    /* Remove leading 0's in `mantissaStr`, because our algorithm assumes
     * that there is none. Leave a single 0 if all characters are 0's, so that
     * we can still parse the result as a valid integer mantissa.
     */
    val limit = mantissaStr0.length() - 1
    var i = 0
    while (i != limit && mantissaStr0.charAt(i) == '0')
      i += 1
    val mantissaStr = mantissaStr0.substring(i)

    /* If there are more than `maxPrecisionChars` characters left, we compress
     * the tail as a single character. This has two purposes:
     *
     * - First, if we don't, there can be corner cases where the `mantissaStr`
     *   would parse as `Infinity` because it is too large on its own, but
     *   where the binary exponent can "fix it" by being sufficiently under or
     *   above 0. (see #4431)
     * - Second, when parsing Floats, this ensures that values very close above
     *   or below a Float midpoint are parsed as a Double that is actually
     *   above or below the midpoint. If we don't, the parsed value can be
     *   rounded to exactly the midpoint, which will cause incorrect rounding
     *   when later converting it to a Float value. (see #4035)
     *
     * Only `maxPrecisionChars` characters can directly participate in the
     * precision of the final result. The last one may already loose precision,
     * but will determine whether to round up or down. If its low-order bits
     * that are lost are exactly a '1' followed by '0's, then even a character
     * very far away in the tail can make the difference between rounding up
     * or down (see #4431). However the only possible difference is between
     * "all-zeros" or "at least one non-zero" after the `maxPrecisionChars`th
     * character. We can therefore compress the entire tail as single "0" or
     * "1".
     *
     * Of course, we remember that we need to apply a correction to the
     * exponent of the final result.
     *
     * (A similar strategy is used in the primitive Long-to-Float conversion.)
     */
    val mantissaStrLen = mantissaStr.length()
    val needsCorrection2 = mantissaStrLen > maxPrecisionChars
    val truncatedMantissaStr = if (needsCorrection2) {
      var hasNonZeroChar = false
      var j = maxPrecisionChars
      while (!hasNonZeroChar && j != mantissaStrLen) {
        if (mantissaStr.charAt(j) != '0')
          hasNonZeroChar = true
        j += 1
      }
      val compressedTail = if (hasNonZeroChar) "1" else "0"
      mantissaStr.substring(0, maxPrecisionChars) + compressedTail
    } else {
      mantissaStr
    }
    val correction2 =
      if (needsCorrection2) (mantissaStr.length - (maxPrecisionChars + 1)) * 4 // one hex == 4 bits
      else 0

    val fullCorrection = correction1 + correction2

    /* Note that we do not care too much about overflows and underflows when
     * manipulating binary exponents and corrections, because the corrections
     * are directly related to the length of the input string, so they cannot
     * be *that* big (or we have bigger problems), and the final result needs
     * to fit in the [-1024, 1023] range, which can only happen if the
     * `binaryExp` (see below) did not stray too far from that range itself.
     */

    @inline def nativeParseInt(s: String, radix: Int): scala.Double =
      js.Dynamic.global.parseInt(s, radix).asInstanceOf[scala.Double]

    val mantissa = nativeParseInt(truncatedMantissaStr, 16)
    // Assert: mantissa != 0.0 && mantissa != scala.Double.PositiveInfinity

    val binaryExpDouble = nativeParseInt(binaryExpStr, 10)
    val binaryExp = binaryExpDouble.toInt // caps to [MinValue, MaxValue]

    Math.scalb(mantissa, binaryExp + fullCorrection)

    // scalastyle:on return
  }

  @inline def toString(d: scala.Double): String =
    "" + d

  @noinline
  def toHexString(d: scala.Double): String =
    FloatDouble.toHexString(d, mantissaToHexString(_))

  @inline
  private def mantissaToHexString(m: scala.Long): String = {
    @inline def padHex5(i: Int): String = {
      val s = Integer.toHexString(i)
      "00000".substring(s.length) + s // 5 zeros
    }

    @inline def padHex8(i: Int): String = {
      val s = Integer.toHexString(i)
      "00000000".substring(s.length) + s // 8 zeros
    }

    padHex5((m >>> 32).toInt) + padHex8(m.toInt)
  }

  def compare(a: scala.Double, b: scala.Double): scala.Int = {
    // NaN must equal itself, and be greater than anything else
    if (isNaN(a)) {
      if (isNaN(b)) 0
      else 1
    } else if (isNaN(b)) {
      -1
    } else {
      if (a == b) {
        // -0.0 must be smaller than 0.0
        if (a == 0.0) {
          val ainf = 1.0/a
          if (ainf == 1.0/b) 0
          else if (ainf < 0) -1
          else 1
        } else {
          0
        }
      } else {
        if (a < b) -1
        else 1
      }
    }
  }

  @inline def isNaN(v: scala.Double): scala.Boolean =
    v != v

  @inline def isInfinite(v: scala.Double): scala.Boolean =
    v == POSITIVE_INFINITY || v == NEGATIVE_INFINITY

  @inline def isFinite(d: scala.Double): scala.Boolean =
    !isNaN(d) && !isInfinite(d)

  /** Hash code of a number (excluding Longs).
   *
   *  Because of the common encoding for integer and floating point values,
   *  the hashCode of Floats and Doubles must align with that of Ints for the
   *  common values.
   *
   *  For other values, we use the hashCode specified by the JavaDoc for
   *  *Doubles*, even for values which are valid Float values. Because of the
   *  previous point, we cannot align completely with the Java specification,
   *  so there is no point trying to be a bit more aligned here. Always using
   *  the Double version requires fewer branches.
   *
   *  We use different code paths in JS and Wasm for performance reasons.
   *  The two implementations compute the same results.
   */
  @inline def hashCode(value: scala.Double): Int = {
    if (LinkingInfo.isWebAssembly)
      hashCodeForWasm(value)
    else
      hashCodeForJS(value)
  }

  @inline
  private def hashCodeForWasm(value: scala.Double): Int = {
    val bits = doubleToRawLongBits(value)
    val valueInt = value.toInt
    if (doubleToRawLongBits(valueInt.toDouble) == bits)
      valueInt
    else if (isNaNBitPattern(bits))
      Long.hashCode(CanonicalNaNBits)
    else
      Long.hashCode(bits)
  }

  @inline
  private def hashCodeForJS(value: scala.Double): Int = {
    val valueInt = (value.asInstanceOf[js.Dynamic] | 0.asInstanceOf[js.Dynamic]).asInstanceOf[Int]
    if (valueInt.toDouble == value && 1.0/value != scala.Double.NegativeInfinity)
      valueInt
    else if (value != value)
      Long.hashCode(CanonicalNaNBits)
    else
      Long.hashCode(doubleToRawLongBits(value))
  }

  @inline def longBitsToDouble(bits: scala.Long): scala.Double =
    throw new Error("stub") // body replaced by the compiler back-end

  @inline def doubleToRawLongBits(value: scala.Double): scala.Long =
    throw new Error("stub") // body replaced by the compiler back-end

  @inline def doubleToLongBits(value: scala.Double): scala.Long = {
    if (LinkingInfo.isWebAssembly) {
      val rawBits = doubleToRawLongBits(value)
      if (isNaNBitPattern(rawBits))
        CanonicalNaNBits
      else
        rawBits
    } else {
      /* On JS, the Long comparison inside isNaNBitPattern is expensive.
       * We compare to NaN at the double level instead.
       */
      if (value != value)
        CanonicalNaNBits
      else
        doubleToRawLongBits(value)
    }
  }

  @inline private def isNaNBitPattern(bits: scala.Long): scala.Boolean = {
    // Both operands are non-negative; it does not matter whether the comparison is signed or not
    (bits & ~scala.Long.MinValue) > PosInfinityBits
  }

  /** Do `bits` correspond to a pattern for a "special" value.
   *
   *  Specials are zeros, infinities and NaNs.
   */
  @inline private[lang] def isSpecialBitPattern(bits: scala.Long): scala.Boolean = {
    val bitsNoSign = bits & ~scala.Long.MinValue
    if (LinkingInfo.isWebAssembly)
      Long.unsigned_>=(bitsNoSign - 1L, PosInfinityBits - 1L) // fast long operations
    else
      bitsNoSign == 0L || bitsNoSign >= PosInfinityBits // for RuntimeLong, this is faster than the above
  }

  @inline def sum(a: scala.Double, b: scala.Double): scala.Double =
    a + b

  @inline def max(a: scala.Double, b: scala.Double): scala.Double =
    Math.max(a, b)

  @inline def min(a: scala.Double, b: scala.Double): scala.Double =
    Math.min(a, b)
}
