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

import scala.scalajs.js

/* This is a hijacked class. Its instances are primitive numbers.
 * Constructors are not emitted.
 */
final class Double private () extends Number with Comparable[Double] {

  def this(value: scala.Double) = this()
  def this(s: String) = this()

  @inline def doubleValue(): scala.Double =
    this.asInstanceOf[scala.Double]

  @inline override def byteValue(): scala.Byte = doubleValue.toByte
  @inline override def shortValue(): scala.Short = doubleValue.toShort
  @inline def intValue(): scala.Int = doubleValue.toInt
  @inline def longValue(): scala.Long = doubleValue.toLong
  @inline def floatValue(): scala.Float = doubleValue.toFloat

  override def equals(that: Any): scala.Boolean = that match {
    case that: Double =>
      val a = doubleValue
      val b = that.doubleValue
      (a == b) || (Double.isNaN(a) && Double.isNaN(b))
    case _ =>
      false
  }

  @inline override def hashCode(): Int =
    Double.hashCode(doubleValue)

  @inline override def compareTo(that: Double): Int =
    Double.compare(doubleValue, that.doubleValue)

  @inline override def toString(): String =
    Double.toString(doubleValue)

  @inline def isNaN(): scala.Boolean =
    Double.isNaN(doubleValue)

  @inline def isInfinite(): scala.Boolean =
    Double.isInfinite(doubleValue)

}

object Double {
  final val TYPE = scala.Predef.classOf[scala.Double]
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

  @inline def valueOf(doubleValue: scala.Double): Double =
    new Double(doubleValue)

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
    def fail(): Nothing =
      throw new NumberFormatException(s"""For input string: "$s"""")

    // (Very) slow path for hexadecimal notation
    def parseHexDoubleImpl(match2: js.RegExp.ExecResult): scala.Double = {
      // scalastyle:off return

      val signStr = match2(1).asInstanceOf[String]
      val integralPartStr = match2(2).asInstanceOf[String]
      val fractionalPartStr = match2(3).asInstanceOf[String]
      val binaryExpStr = match2(4).asInstanceOf[String]

      if (integralPartStr == "" && fractionalPartStr == "")
        fail()

      /* We concatenate the integral part and fractional part together, then
       * we parse the result as an integer. This means that we need to remember
       * a correction to be applied to the final result, as a diff to the
       * binary exponent
       */
      val mantissaStr0 = integralPartStr + fractionalPartStr
      val correction1 = -(fractionalPartStr.length * 4) // 1 hex == 4 bits

      /* Remove leading 0's in `mantissaStr`, because our algorithm assumes
       * that there is none.
       */
      var i = 0
      while (i != mantissaStr0.length && mantissaStr0.charAt(i) == '0')
        i += 1
      val mantissaStr = mantissaStr0.substring(i)

      /* If the mantissa is empty, it means there were only 0's, and we
       * short-cut to directly returning 0.0 or -0.0. This is important because
       * the final step of the algorithm (multiplying by `correctingPow`)
       * assumes that `mantissa` is non-zero in the case of overflow.
       */
      if (mantissaStr == "") {
        if (signStr == "-")
          return -0.0
        else
          return 0.0
      }

      /* If there are more than 15 characters left, we cut them out. They will
       * never influence the result because of the limited precision of
       * doubles. Note that the 15th character itself gets lost too, but can
       * influence the *rounding* applied to the 14th character.
       *
       * We need to cut them out for corner cases where the full `mantissaStr`
       * would parse as Infinity because it is too large, but where the binary
       * exponent can "fix it" by being sufficiently under 0.
       *
       * Of course, we remember that we need to apply a correction to the
       * exponent of the final result.
       */
      val needsCorrection2 = mantissaStr.length > 15
      val truncatedMantissaStr =
        if (needsCorrection2) mantissaStr.substring(0, 15)
        else mantissaStr
      val correction2 =
        if (needsCorrection2) (mantissaStr.length - 15) * 4 // one hex == 4 bits
        else 0

      val fullCorrection = correction1 + correction2

      /* Note that we do not care too much about overflows and underflows when
       * manipulating binary exponents and corrections, because the corrections
       * are directly related to the length of the input string, so they cannot
       * be *that* big (or we have bigger problems), and the final result needs
       * to fit in the [-1074, 1023] range, which can only happen if the
       * `binaryExp` (see below) did not stray too far from that range itself.
       */

      val mantissa =
        js.Dynamic.global.parseInt(truncatedMantissaStr, 16).asInstanceOf[scala.Double]
      // Assert: mantissa != 0.0 && mantissa != scala.Double.PositiveInfinity

      val binaryExpDouble =
        js.Dynamic.global.parseInt(binaryExpStr, 10).asInstanceOf[scala.Double]
      val binaryExp = binaryExpDouble.toInt // caps to [MinValue, MaxValue]

      val binExpAndCorrection = binaryExp + fullCorrection

      /* If `baseExponent` is the IEEE exponent of `mantissa`, then
       * `binExpAndCorrection + baseExponent` must be in the valid range of
       * IEEE exponents, which is [-1074, 1023]. Therefore, if
       * `binExpAndCorrection` is out of twice that range, we will end up with
       * an overflow or an underflow anyway.
       *
       * If it is inside twice that range, then we need to multiply `mantissa`
       * by `Math.pow(2, binExpAndCorrection)`. However that `pow` could
       * overflow or underflow itself, so we cut it in 3 parts. If that does
       * not suffice for it not to overflow or underflow, it's because it
       * wasn't in the safe range to begin with.
       */
      val binExpAndCorrection_div_3 = binExpAndCorrection / 3
      val correctingPow = Math.pow(2, binExpAndCorrection_div_3)
      val correctingPow3 =
        Math.pow(2, binExpAndCorrection - 2*binExpAndCorrection_div_3)

      val r = ((mantissa * correctingPow) * correctingPow) * correctingPow3

      if (signStr == "-") -r
      else r

      // scalastyle:on return
    }

    val match1 = doubleStrPat.exec(s)
    if (match1 != null) {
      js.Dynamic.global.parseFloat(match1(1)).asInstanceOf[scala.Double]
    } else {
      val match2 = doubleStrHexPat.exec(s)
      if (match2 != null)
        parseHexDoubleImpl(match2)
      else
        fail()
    }
  }

  @inline def toString(d: scala.Double): String =
    "" + d

  def toHexString(d: scala.Double): String = {
    val ebits = 11 // exponent size
    val mbits = 52 // mantissa size
    val bias = (1 << (ebits - 1)) - 1

    val bits = doubleToLongBits(d)
    val s = bits < 0
    val m = bits & ((1L << mbits) - 1L)
    val e = (bits >>> mbits).toInt & ((1 << ebits) - 1) // biased

    val posResult = if (e > 0) {
      if (e == (1 << ebits) - 1) {
        // Special
        if (m != 0L) "NaN"
        else "Infinity"
      } else {
        // Normalized
        "0x1." + mantissaToHexString(m) + "p" + (e - bias)
      }
    } else {
      if (m != 0L) {
        // Subnormal
        "0x0." + mantissaToHexString(m) + "p-1022"
      } else {
        // Zero
        "0x0.0p0"
      }
    }

    if (bits < 0) "-" + posResult else posResult
  }

  @inline
  private def mantissaToHexString(m: scala.Long): String =
    mantissaToHexStringLoHi(m.toInt, (m >>> 32).toInt)

  private def mantissaToHexStringLoHi(lo: Int, hi: Int): String = {
    @inline def padHex5(i: Int): String = {
      val s = Integer.toHexString(i)
      "00000".substring(s.length) + s // 5 zeros
    }

    @inline def padHex8(i: Int): String = {
      val s = Integer.toHexString(i)
      "00000000".substring(s.length) + s // 8 zeros
    }

    val padded = padHex5(hi) + padHex8(lo)

    var len = padded.length
    while (len > 1 && padded.charAt(len - 1) == '0')
      len -= 1
    padded.substring(0, len)
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

  @inline def hashCode(value: scala.Double): Int =
    scala.scalajs.runtime.Bits.numberHashCode(value)

  @inline def longBitsToDouble(bits: scala.Long): scala.Double =
    scala.scalajs.runtime.Bits.longBitsToDouble(bits)

  @inline def doubleToLongBits(value: scala.Double): scala.Long =
    scala.scalajs.runtime.Bits.doubleToLongBits(value)

  @inline def sum(a: scala.Double, b: scala.Double): scala.Double =
    a + b

  @inline def max(a: scala.Double, b: scala.Double): scala.Double =
    Math.max(a, b)

  @inline def min(a: scala.Double, b: scala.Double): scala.Double =
    Math.min(a, b)
}
