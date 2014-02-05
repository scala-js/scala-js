package java.lang

import scala.scalajs.js

final class Long(private val value: scala.Long) extends Number {
  import scala.scalajs.runtime.Long.{fromRuntimeLong, toRuntimeLong}

  override def byteValue() = toRuntimeLong(value).toByte
  override def shortValue() = toRuntimeLong(value).toShort
  def intValue() = toRuntimeLong(value).toInt
  def longValue() = value
  def floatValue() = toRuntimeLong(value).toFloat
  def doubleValue() = toRuntimeLong(value).toDouble

  override def equals(that: Any) =
    that.isInstanceOf[Long] && (value == that.asInstanceOf[Long].value)

  override def toString = toRuntimeLong(value).toString()

  /*
   * Methods on scala.Long
   * The following methods are only here to properly support reflective calls
   * on boxed primitive values. YOU WILL NOT BE ABLE TO USE THESE METHODS, since
   * we use the true javalib to lookup symbols, this file contains only
   * implementations.
   */
  def toByte: scala.Byte     = value.toByte
  def toShort: scala.Short   = value.toShort
  def toChar: scala.Char     = value.toChar
  def toInt: scala.Int       = value.toInt
  def toLong: scala.Long     = value
  def toFloat: scala.Float   = value.toFloat
  def toDouble: scala.Double = value.toDouble

  /**
 * Returns the bitwise negation of this value.
 * @example {{{
 * ~5 == -6
 * // in binary: ~00000101 ==
 * //             11111010
 * }}}
 */
  def unary_~ : scala.Long = ~value
  /**
 * Returns this value, unmodified.
 */
  def unary_+ : scala.Long = value
  /**
 * Returns the negation of this value.
 */
  def unary_- : scala.Long = -value

  def +(x: String): String = value + x

  /**
  * Returns this value bit-shifted left by the specified number of bits,
  *         filling in the new right bits with zeroes.
  * @example {{{ 6 << 3 == 48 // in binary: 0110 << 3 == 0110000 }}}
  */
  def <<(x: scala.Int): scala.Long = value << x
  /**
  * Returns this value bit-shifted left by the specified number of bits,
  *         filling in the new right bits with zeroes.
  * @example {{{ 6 << 3 == 48 // in binary: 0110 << 3 == 0110000 }}}
  */
  def <<(x: scala.Long): scala.Long = value << x
  /**
  * Returns this value bit-shifted right by the specified number of bits,
  *         filling the new left bits with zeroes.
  * @example {{{ 21 >>> 3 == 2 // in binary: 010101 >>> 3 == 010 }}}
  * @example {{{
  * -21 >>> 3 == 536870909
  * // in binary: 11111111 11111111 11111111 11101011 >>> 3 ==
  * //            00011111 11111111 11111111 11111101
  * }}}
  */
  def >>>(x: scala.Int): scala.Long = value >>> x
  /**
  * Returns this value bit-shifted right by the specified number of bits,
  *         filling the new left bits with zeroes.
  * @example {{{ 21 >>> 3 == 2 // in binary: 010101 >>> 3 == 010 }}}
  * @example {{{
  * -21 >>> 3 == 536870909
  * // in binary: 11111111 11111111 11111111 11101011 >>> 3 ==
  * //            00011111 11111111 11111111 11111101
  * }}}
  */
  def >>>(x: scala.Long): scala.Long = value >>> x
  /**
  * Returns this value bit-shifted left by the specified number of bits,
  *         filling in the right bits with the same value as the left-most bit of this.
  *         The effect of this is to retain the sign of the value.
  * @example {{{
  * -21 >> 3 == -3
  * // in binary: 11111111 11111111 11111111 11101011 >> 3 ==
  * //            11111111 11111111 11111111 11111101
  * }}}
  */
  def >>(x: scala.Int): scala.Long = value >> x
  /**
  * Returns this value bit-shifted left by the specified number of bits,
  *         filling in the right bits with the same value as the left-most bit of this.
  *         The effect of this is to retain the sign of the value.
  * @example {{{
  * -21 >> 3 == -3
  * // in binary: 11111111 11111111 11111111 11101011 >> 3 ==
  * //            11111111 11111111 11111111 11111101
  * }}}
  */
  def >>(x: scala.Long): scala.Long = value >> x

  /**
  * Returns `true` if this value is less than x, `false` otherwise.
  */
  def <(x: scala.Byte): Boolean = value < x
  /**
  * Returns `true` if this value is less than x, `false` otherwise.
  */
  def <(x: scala.Short): Boolean = value < x
  /**
  * Returns `true` if this value is less than x, `false` otherwise.
  */
  def <(x: scala.Char): Boolean = value < x
  /**
  * Returns `true` if this value is less than x, `false` otherwise.
  */
  def <(x: scala.Int): Boolean = value < x
  /**
  * Returns `true` if this value is less than x, `false` otherwise.
  */
  def <(x: scala.Long): Boolean = value < x
  /**
  * Returns `true` if this value is less than x, `false` otherwise.
  */
  def <(x: scala.Float): Boolean = value < x
  /**
  * Returns `true` if this value is less than x, `false` otherwise.
  */
  def <(x: scala.Double): Boolean = value < x

  /**
  * Returns `true` if this value is less than or equal to x, `false` otherwise.
  */
  def <=(x: scala.Byte): Boolean = value <= x
  /**
  * Returns `true` if this value is less than or equal to x, `false` otherwise.
  */
  def <=(x: scala.Short): Boolean = value <= x
  /**
  * Returns `true` if this value is less than or equal to x, `false` otherwise.
  */
  def <=(x: scala.Char): Boolean = value <= x
  /**
  * Returns `true` if this value is less than or equal to x, `false` otherwise.
  */
  def <=(x: scala.Int): Boolean = value <= x
  /**
  * Returns `true` if this value is less than or equal to x, `false` otherwise.
  */
  def <=(x: scala.Long): Boolean = value <= x
  /**
  * Returns `true` if this value is less than or equal to x, `false` otherwise.
  */
  def <=(x: scala.Float): Boolean = value <= x
  /**
  * Returns `true` if this value is less than or equal to x, `false` otherwise.
  */
  def <=(x: scala.Double): Boolean = value <= x

  /**
  * Returns `true` if this value is greater than x, `false` otherwise.
  */
  def >(x: scala.Byte): Boolean = value > x
  /**
  * Returns `true` if this value is greater than x, `false` otherwise.
  */
  def >(x: scala.Short): Boolean = value > x
  /**
  * Returns `true` if this value is greater than x, `false` otherwise.
  */
  def >(x: scala.Char): Boolean = value > x
  /**
  * Returns `true` if this value is greater than x, `false` otherwise.
  */
  def >(x: scala.Int): Boolean = value > x
  /**
  * Returns `true` if this value is greater than x, `false` otherwise.
  */
  def >(x: scala.Long): Boolean = value > x
  /**
  * Returns `true` if this value is greater than x, `false` otherwise.
  */
  def >(x: scala.Float): Boolean = value > x
  /**
  * Returns `true` if this value is greater than x, `false` otherwise.
  */
  def >(x: scala.Double): Boolean = value > x

  /**
  * Returns `true` if this value is greater than or equal to x, `false` otherwise.
  */
  def >=(x: scala.Byte): Boolean = value >= x
  /**
  * Returns `true` if this value is greater than or equal to x, `false` otherwise.
  */
  def >=(x: scala.Short): Boolean = value >= x
  /**
  * Returns `true` if this value is greater than or equal to x, `false` otherwise.
  */
  def >=(x: scala.Char): Boolean = value >= x
  /**
  * Returns `true` if this value is greater than or equal to x, `false` otherwise.
  */
  def >=(x: scala.Int): Boolean = value >= x
  /**
  * Returns `true` if this value is greater than or equal to x, `false` otherwise.
  */
  def >=(x: scala.Long): Boolean = value >= x
  /**
  * Returns `true` if this value is greater than or equal to x, `false` otherwise.
  */
  def >=(x: scala.Float): Boolean = value >= x
  /**
  * Returns `true` if this value is greater than or equal to x, `false` otherwise.
  */
  def >=(x: scala.Double): Boolean = value >= x

  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary:   11110000
  * //            | 10101010
  * //              --------
  * //              11111010
  * }}}
  */
  def |(x: scala.Byte): scala.Long = value | x
  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary:   11110000
  * //            | 10101010
  * //              --------
  * //              11111010
  * }}}
  */
  def |(x: scala.Short): scala.Long = value | x
  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary:   11110000
  * //            | 10101010
  * //              --------
  * //              11111010
  * }}}
  */
  def |(x: scala.Char): scala.Long = value | x
  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary:   11110000
  * //            | 10101010
  * //              --------
  * //              11111010
  * }}}
  */
  def |(x: scala.Int): scala.Long = value | x
  /**
  * Returns the bitwise OR of this value and `x`.
  * @example {{{
  * (0xf0 | 0xaa) == 0xfa
  * // in binary:   11110000
  * //            | 10101010
  * //              --------
  * //              11111010
  * }}}
  */
  def |(x: scala.Long): scala.Long = value | x

  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary:   11110000
  * //            & 10101010
  * //              --------
  * //              10100000
  * }}}
  */
  def &(x: scala.Byte): scala.Long = value & x
  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary:   11110000
  * //            & 10101010
  * //              --------
  * //              10100000
  * }}}
  */
  def &(x: scala.Short): scala.Long = value & x
  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary:   11110000
  * //            & 10101010
  * //              --------
  * //              10100000
  * }}}
  */
  def &(x: scala.Char): scala.Long = value & x
  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary:   11110000
  * //            & 10101010
  * //              --------
  * //              10100000
  * }}}
  */
  def &(x: scala.Int): scala.Long = value & x
  /**
  * Returns the bitwise AND of this value and `x`.
  * @example {{{
  * (0xf0 & 0xaa) == 0xa0
  * // in binary:   11110000
  * //            & 10101010
  * //              --------
  * //              10100000
  * }}}
  */
  def &(x: scala.Long): scala.Long = value & x

  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary:   11110000
  * //            ^ 10101010
  * //              --------
  * //              01011010
  * }}}
  */
  def ^(x: scala.Byte): scala.Long = value ^ x
  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary:   11110000
  * //            ^ 10101010
  * //              --------
  * //              01011010
  * }}}
  */
  def ^(x: scala.Short): scala.Long = value ^ x
  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary:   11110000
  * //            ^ 10101010
  * //              --------
  * //              01011010
  * }}}
  */
  def ^(x: scala.Char): scala.Long = value ^ x
  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary:   11110000
  * //            ^ 10101010
  * //              --------
  * //              01011010
  * }}}
  */
  def ^(x: scala.Int): scala.Long = value ^ x
  /**
  * Returns the bitwise XOR of this value and `x`.
  * @example {{{
  * (0xf0 ^ 0xaa) == 0x5a
  * // in binary:   11110000
  * //            ^ 10101010
  * //              --------
  * //              01011010
  * }}}
  */
  def ^(x: scala.Long): scala.Long = value ^ x

  /**
  * Returns the sum of this value and `x`.
  */
  def +(x: scala.Byte): scala.Long = value + x
  /**
  * Returns the sum of this value and `x`.
  */
  def +(x: scala.Short): scala.Long = value + x
  /**
  * Returns the sum of this value and `x`.
  */
  def +(x: scala.Char): scala.Long = value + x
  /**
  * Returns the sum of this value and `x`.
  */
  def +(x: scala.Int): scala.Long = value + x
  /**
  * Returns the sum of this value and `x`.
  */
  def +(x: scala.Long): scala.Long = value + x
  /**
  * Returns the sum of this value and `x`.
  */
  def +(x: scala.Float): scala.Float = value + x
  /**
  * Returns the sum of this value and `x`.
  */
  def +(x: scala.Double): scala.Double = value + x

  /**
  * Returns the difference of this value and `x`.
  */
  def -(x: scala.Byte): scala.Long = value - x
  /**
  * Returns the difference of this value and `x`.
  */
  def -(x: scala.Short): scala.Long = value - x
  /**
  * Returns the difference of this value and `x`.
  */
  def -(x: scala.Char): scala.Long = value - x
  /**
  * Returns the difference of this value and `x`.
  */
  def -(x: scala.Int): scala.Long = value - x
  /**
  * Returns the difference of this value and `x`.
  */
  def -(x: scala.Long): scala.Long = value - x
  /**
  * Returns the difference of this value and `x`.
  */
  def -(x: scala.Float): scala.Float = value - x
  /**
  * Returns the difference of this value and `x`.
  */
  def -(x: scala.Double): scala.Double = value - x

  /**
  * Returns the product of this value and `x`.
  */
  def *(x: scala.Byte): scala.Long = value - x
  /**
  * Returns the product of this value and `x`.
  */
  def *(x: scala.Short): scala.Long = value - x
  /**
  * Returns the product of this value and `x`.
  */
  def *(x: scala.Char): scala.Long = value - x
  /**
  * Returns the product of this value and `x`.
  */
  def *(x: scala.Int): scala.Long = value - x
  /**
  * Returns the product of this value and `x`.
  */
  def *(x: scala.Long): scala.Long = value - x
  /**
  * Returns the product of this value and `x`.
  */
  def *(x: scala.Float): scala.Float = value - x
  /**
  * Returns the product of this value and `x`.
  */
  def *(x: scala.Double): scala.Double = value - x

  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: scala.Byte): scala.Long = value / x
  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: scala.Short): scala.Long = value / x
  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: scala.Char): scala.Long = value / x
  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: scala.Int): scala.Long = value / x
  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: scala.Long): scala.Long = value / x
  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: scala.Float): scala.Float = value / x
  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: scala.Double): scala.Double = value / x

  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: scala.Byte): scala.Long = value % x
  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: scala.Short): scala.Long = value % x
  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: scala.Char): scala.Long = value % x
  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: scala.Int): scala.Long = value % x
  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: scala.Long): scala.Long = value % x
  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: scala.Float): scala.Float = value % x
  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: scala.Double): scala.Double = value % x

}

object Long {
  import scala.scalajs.runtime.{ Long => RTLong }
  import RTLong.{fromRuntimeLong, toRuntimeLong}

  val TYPE = classOf[scala.Long]
  val MIN_VALUE: scala.Long = -9223372036854775808L
  val MAX_VALUE: scala.Long = 9223372036854775807L
  val SIZE: scala.Int = 64

  def valueOf(longValue: scala.Long) = new Long(longValue)
  def parseLong(s: String): scala.Long = fromRuntimeLong(RTLong.fromString(s))
  def toString(l: scala.Long) = toRuntimeLong(l).toString

  def bitCount(i: scala.Long): scala.Int = toRuntimeLong(i).bitCount

  def reverseBytes(i: scala.Long): scala.Long = sys.error("unimplemented")
  def rotateLeft(i: scala.Long, distance: scala.Int): scala.Long = sys.error("unimplemented")
  def rotateRight(i: scala.Long, distance: scala.Int): scala.Long = sys.error("unimplemented")

  def signum(i: scala.Long): scala.Long =
    if (i == 0) 0 else if (i < 0) -1 else 1

  def numberOfLeadingZeros(l: scala.Long) =
    toRuntimeLong(l).numberOfLeadingZeros

  def toBinaryString(l: scala.Long): String =
    dropLZ(toRuntimeLong(l).toBinaryString)
  def toHexString(l: scala.Long): String =
    dropLZ(toRuntimeLong(l).toHexString)
  def toOctalString(l: scala.Long): String =
    dropLZ(toRuntimeLong(l).toOctalString)

  /** Drop leading zeros
   *
   * This method was:
   *
   *     s.dropWhile(_ == '0').padTo(1, '0')
   *
   * but generated too much JS code
   */
  private def dropLZ(s: js.String) = {
    var i = 0
    while ("0" == s.charAt(i)) { i += 1 }
    s.substring(Math.min(i,s.length - 1))
  }
}
