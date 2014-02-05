package java.lang

import scala.scalajs.js

final class Byte(private val value: scala.Byte) extends Number {

  override def byteValue() = value
  override def shortValue() = value.toShort
  def intValue() = value.toInt
  def longValue() = value.toLong
  def floatValue() = value.toFloat
  def doubleValue() = value.toDouble

  override def hashCode(): Int = value.##

  override def equals(that: Any) =
    that.isInstanceOf[Byte] && (value == that.asInstanceOf[Byte].value)

  override def toString = (value:js.Number).toString()

  /*
   * Methods on scala.Byte
   * The following methods are only here to properly support reflective calls
   * on boxed primitive values. YOU WILL NOT BE ABLE TO USE THESE METHODS, since
   * we use the true javalib to lookup symbols, this file contains only
   * implementations.
   */
  def toByte: scala.Byte     = value
  def toShort: scala.Short   = value.toShort
  def toChar: scala.Char     = value.toChar
  def toInt: scala.Int       = value.toInt
  def toLong: scala.Long     = value.toLong
  def toFloat: scala.Float   = value.toFloat
  def toDouble: scala.Double = value.toDouble

  /**
 * Returns the bitwise negation of this value.
 * @example {{{
 * ~5 == -6
 * // in binary: ~00000101 ==
 * //             11111010
 * }}}
 *
 * ScalaJS note: Yes, this actually returns an Int.
 */
  def unary_~ : scala.Int = ~value
  /**
 * Returns this value, unmodified.
 *
 * ScalaJS note: Yes, this actually returns an Int.
 */
  def unary_+ : scala.Int = value
  /**
 * Returns the negation of this value.
 *
 * ScalaJS note: Yes, this actually returns an Int.
 */
  def unary_- : scala.Int = -value

  def +(x: String): String = value + x

  /**
  * Returns this value bit-shifted left by the specified number of bits,
  *         filling in the new right bits with zeroes.
  * @example {{{ 6 << 3 == 48 // in binary: 0110 << 3 == 0110000 }}}
  */
  def <<(x: scala.Int): scala.Int = value << x
  /**
  * Returns this value bit-shifted left by the specified number of bits,
  *         filling in the new right bits with zeroes.
  * @example {{{ 6 << 3 == 48 // in binary: 0110 << 3 == 0110000 }}}
  */
  def <<(x: scala.Long): scala.Int = value << x
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
  def >>>(x: scala.Int): scala.Int = value >>> x
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
  def >>>(x: scala.Long): scala.Int = value >>> x
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
  def >>(x: scala.Int): scala.Int = value >> x
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
  def >>(x: scala.Long): scala.Int = value >> x

  /**
  * Returns `true` if this value is less than x, `false` otherwise.
  */
  def <(x: scala.Byte): scala.Boolean = value < x
  /**
  * Returns `true` if this value is less than x, `false` otherwise.
  */
  def <(x: scala.Short): scala.Boolean = value < x
  /**
  * Returns `true` if this value is less than x, `false` otherwise.
  */
  def <(x: scala.Char): scala.Boolean = value < x
  /**
  * Returns `true` if this value is less than x, `false` otherwise.
  */
  def <(x: scala.Int): scala.Boolean = value < x
  /**
  * Returns `true` if this value is less than x, `false` otherwise.
  */
  def <(x: scala.Long): scala.Boolean = value < x
  /**
  * Returns `true` if this value is less than x, `false` otherwise.
  */
  def <(x: scala.Float): scala.Boolean = value < x
  /**
  * Returns `true` if this value is less than x, `false` otherwise.
  */
  def <(x: scala.Double): scala.Boolean = value < x

  /**
  * Returns `true` if this value is less than or equal to x, `false` otherwise.
  */
  def <=(x: scala.Byte): scala.Boolean = value <= x
  /**
  * Returns `true` if this value is less than or equal to x, `false` otherwise.
  */
  def <=(x: scala.Short): scala.Boolean = value <= x
  /**
  * Returns `true` if this value is less than or equal to x, `false` otherwise.
  */
  def <=(x: scala.Char): scala.Boolean = value <= x
  /**
  * Returns `true` if this value is less than or equal to x, `false` otherwise.
  */
  def <=(x: scala.Int): scala.Boolean = value <= x
  /**
  * Returns `true` if this value is less than or equal to x, `false` otherwise.
  */
  def <=(x: scala.Long): scala.Boolean = value <= x
  /**
  * Returns `true` if this value is less than or equal to x, `false` otherwise.
  */
  def <=(x: scala.Float): scala.Boolean = value <= x
  /**
  * Returns `true` if this value is less than or equal to x, `false` otherwise.
  */
  def <=(x: scala.Double): scala.Boolean = value <= x

  /**
  * Returns `true` if this value is greater than x, `false` otherwise.
  */
  def >(x: scala.Byte): scala.Boolean = value > x
  /**
  * Returns `true` if this value is greater than x, `false` otherwise.
  */
  def >(x: scala.Short): scala.Boolean = value > x
  /**
  * Returns `true` if this value is greater than x, `false` otherwise.
  */
  def >(x: scala.Char): scala.Boolean = value > x
  /**
  * Returns `true` if this value is greater than x, `false` otherwise.
  */
  def >(x: scala.Int): scala.Boolean = value > x
  /**
  * Returns `true` if this value is greater than x, `false` otherwise.
  */
  def >(x: scala.Long): scala.Boolean = value > x
  /**
  * Returns `true` if this value is greater than x, `false` otherwise.
  */
  def >(x: scala.Float): scala.Boolean = value > x
  /**
  * Returns `true` if this value is greater than x, `false` otherwise.
  */
  def >(x: scala.Double): scala.Boolean = value > x

  /**
  * Returns `true` if this value is greater than or equal to x, `false` otherwise.
  */
  def >=(x: scala.Byte): scala.Boolean = value >= x
  /**
  * Returns `true` if this value is greater than or equal to x, `false` otherwise.
  */
  def >=(x: scala.Short): scala.Boolean = value >= x
  /**
  * Returns `true` if this value is greater than or equal to x, `false` otherwise.
  */
  def >=(x: scala.Char): scala.Boolean = value >= x
  /**
  * Returns `true` if this value is greater than or equal to x, `false` otherwise.
  */
  def >=(x: scala.Int): scala.Boolean = value >= x
  /**
  * Returns `true` if this value is greater than or equal to x, `false` otherwise.
  */
  def >=(x: scala.Long): scala.Boolean = value >= x
  /**
  * Returns `true` if this value is greater than or equal to x, `false` otherwise.
  */
  def >=(x: scala.Float): scala.Boolean = value >= x
  /**
  * Returns `true` if this value is greater than or equal to x, `false` otherwise.
  */
  def >=(x: scala.Double): scala.Boolean = value >= x

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
  def |(x: scala.Byte): scala.Int = value | x
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
  def |(x: scala.Short): scala.Int = value | x
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
  def |(x: scala.Char): scala.Int = value | x
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
  def |(x: scala.Int): scala.Int = value | x
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
  def &(x: scala.Byte): scala.Int = value & x
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
  def &(x: scala.Short): scala.Int = value & x
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
  def &(x: scala.Char): scala.Int = value & x
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
  def &(x: scala.Int): scala.Int = value & x
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
  def ^(x: scala.Byte): scala.Int = value ^ x
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
  def ^(x: scala.Short): scala.Int = value ^ x
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
  def ^(x: scala.Char): scala.Int = value ^ x
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
  def ^(x: scala.Int): scala.Int = value ^ x
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
  def +(x: scala.Byte): scala.Int = value + x
  /**
  * Returns the sum of this value and `x`.
  */
  def +(x: scala.Short): scala.Int = value + x
  /**
  * Returns the sum of this value and `x`.
  */
  def +(x: scala.Char): scala.Int = value + x
  /**
  * Returns the sum of this value and `x`.
  */
  def +(x: scala.Int): scala.Int = value + x
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
  def -(x: scala.Byte): scala.Int = value - x
  /**
  * Returns the difference of this value and `x`.
  */
  def -(x: scala.Short): scala.Int = value - x
  /**
  * Returns the difference of this value and `x`.
  */
  def -(x: scala.Char): scala.Int = value - x
  /**
  * Returns the difference of this value and `x`.
  */
  def -(x: scala.Int): scala.Int = value - x
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
  def *(x: scala.Byte): scala.Int = value * x
  /**
  * Returns the product of this value and `x`.
  */
  def *(x: scala.Short): scala.Int = value * x
  /**
  * Returns the product of this value and `x`.
  */
  def *(x: scala.Char): scala.Int = value * x
  /**
  * Returns the product of this value and `x`.
  */
  def *(x: scala.Int): scala.Int = value * x
  /**
  * Returns the product of this value and `x`.
  */
  def *(x: scala.Long): scala.Long = value * x
  /**
  * Returns the product of this value and `x`.
  */
  def *(x: scala.Float): scala.Float = value * x
  /**
  * Returns the product of this value and `x`.
  */
  def *(x: scala.Double): scala.Double = value * x

  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: scala.Byte): scala.Int = value / x
  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: scala.Short): scala.Int = value / x
  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: scala.Char): scala.Int = value / x
  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: scala.Int): scala.Int = value / x
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
  def %(x: scala.Byte): scala.Int = value % x
  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: scala.Short): scala.Int = value % x
  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: scala.Char): scala.Int = value % x
  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: scala.Int): scala.Int = value % x
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

object Byte {
  val TYPE = classOf[scala.Byte]
  val MIN_VALUE: scala.Byte = -128
  val MAX_VALUE: scala.Byte = 127
  val SIZE: scala.Int = 8

  def valueOf(byteValue: scala.Byte) = new Byte(byteValue)
  def parseByte(s: String): scala.Byte = Integer.parseInt(s).toByte
  def toString(b: scala.Byte) = Integer.valueOf(b.toInt).toString
}
