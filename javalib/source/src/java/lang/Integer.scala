package java.lang

import scala.scalajs.js

final class Integer(private val value: scala.Int) extends Number {
  protected[lang] val isInt = true

  def intValue() = value
  def longValue() = value.toLong
  def floatValue() = value.toFloat
  def doubleValue() = value.toDouble

  override def hashCode(): Int = value.##

  override def equals(that: Any) =
    that.isInstanceOf[Integer] && (value == that.asInstanceOf[Integer].value)

  override def toString = (value:js.Number).toString()

  /*
   * Methods on scala.Int
   * The following methods are only here to properly support reflective calls
   * on boxed primitive values. YOU WILL NOT BE ABLE TO USE THESE METHODS, since
   * we use the true javalib to lookup symbols, this file contains only
   * implementations.
   */
  def toByte: scala.Byte     = value.toByte
  def toShort: scala.Short   = value.toShort
  def toChar: scala.Char     = value.toChar
  def toInt: scala.Int       = value
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
 */
  def unary_~ : scala.Int = ~value
  /**
 * Returns this value, unmodified.
 */
  def unary_+ : scala.Int = value
  /**
 * Returns the negation of this value.
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

object Integer {
  val TYPE = classOf[scala.Int]
  val MIN_VALUE: scala.Int = -2147483648
  val MAX_VALUE: scala.Int = 2147483647
  val SIZE: Int = 32

  def valueOf(intValue: scala.Int) = new Integer(intValue)

  def parseInt(s: String): scala.Int =
    // explicitly specify radix to avoid interpretation as octal (by JS)
    parseInt(s, 10)

  def parseInt(s: String, radix: scala.Int): scala.Int = {
    val res = js.parseInt(s, radix)
    if (js.isNaN(res))
      throw new NumberFormatException(s"""For input string: "$s"""")
    else
      res.toInt
  }

  def toString(i: scala.Int) = valueOf(i).toString

  def bitCount(i: scala.Int): scala.Int = {
    // See http://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetParallel
    // The implicit casts to 32-bit ints due to binary ops make this work in JS too
    val t1 = i - ((i >> 1) & 0x55555555)
    val t2 = (t1 & 0x33333333) + ((t1 >> 2) & 0x33333333)
    ((t2 + (t2 >> 4) & 0xF0F0F0F) * 0x1010101) >> 24
  }

  def reverseBytes(i: scala.Int): scala.Int = {
    val byte3 = i >>> 24
    val byte2 = (i >>> 8) & 0xFF00
    val byte1 = (i << 8) & 0xFF0000
    val byte0 = (i << 24)
    byte0 | byte1 | byte2 | byte3
  }

  def rotateLeft(i: scala.Int, distance: scala.Int): scala.Int = {
    (i << distance) | (i >>> (32-distance))
  }

  def rotateRight(i: scala.Int, distance: scala.Int): scala.Int = {
    (i >>> distance) | (i << (32-distance))
  }

  def signum(i: scala.Int): scala.Int =
    if (i == 0) 0 else if (i < 0) -1 else 1

  def numberOfLeadingZeros(i: scala.Int): scala.Int = {
    // See http://aggregate.org/MAGIC/#Leading%20Zero%20Count
    var x = i
    x |= (x >>> 1)
    x |= (x >>> 2)
    x |= (x >>> 4)
    x |= (x >>> 8)
    x |= (x >>> 16)
    32 - bitCount(x)
  }

  def numberOfTrailingZeros(i: scala.Int): scala.Int =
    // See http://aggregate.org/MAGIC/#Trailing%20Zero%20Count
    bitCount((i & -i) - 1)

  def toBinaryString(i: scala.Int): String = (i:js.Number).toString(2)
  def toHexString(i: scala.Int): String = (i:js.Number).toString(16)
  def toOctalString(i: scala.Int): String = (i:js.Number).toString(8)
}
