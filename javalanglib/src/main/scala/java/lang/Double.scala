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
  final val TYPE = classOf[scala.Double]
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

  private[this] lazy val doubleStrPat = new js.RegExp("^" +
      "[\\x00-\\x20]*"   + // optional whitespace
      "[+-]?"            + // optional sign
      "(NaN|Infinity|"   + // special cases
       "(\\d+\\.?\\d*|"  + // literal w/  leading digit
        "\\.\\d+)"       + // literal w/o leading digit
       "([eE][+-]?\\d+)?"+ // optional exponent
      ")[fFdD]?"         + // optional float / double specifier (ignored)
      "[\\x00-\\x20]*"   + // optional whitespace
      "$")

  def parseDouble(s: String): scala.Double = {
    if (doubleStrPat.test(s))
      js.Dynamic.global.parseFloat(s).asInstanceOf[scala.Double]
    else
      throw new NumberFormatException(s"""For input string: "$s"""")
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
