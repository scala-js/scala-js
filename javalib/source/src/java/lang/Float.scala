package java.lang

import scala.scalajs.js

final class Float(private val value: scala.Float) extends Number {

  override def byteValue() = value.toByte
  override def shortValue() = value.toShort
  def intValue() = value.toInt
  def longValue() = value.toLong
  def floatValue() = value
  def doubleValue() = value.toDouble

  override def equals(that0: Any) = that0.isInstanceOf[Float] && {
    val that = that0.asInstanceOf[Float]

    isNaN && that.isNaN ||
    value == that.value && (
      // check that they have the same sign if they are 0
      value != 0 || 1 / value == 1 / that.value
    )
  }

  override def toString = {
    if (value == 0 && 1 / value < 0) {
      "-0.0"
    } else {
      val s = (value: js.Number).toString()
      if (s.indexOf(".") < 0 && !js.isNaN(value))
        s + ".0"
      else s
    }
  }

  def isNaN: scala.Boolean = Float.isNaN(value)

  /*
   * Methods on scala.Float
   * The following methods are only here to properly support reflective calls
   * on boxed primitive values. YOU WILL NOT BE ABLE TO USE THESE METHODS, since
   * we use the true javalib to lookup symbols, this file contains only
   * implementations.
   */
  def toByte: scala.Byte     = value.toByte
  def toShort: scala.Short   = value.toShort
  def toChar: scala.Char     = value.toChar
  def toInt: scala.Int       = value.toInt
  def toLong: scala.Long     = value.toLong
  def toFloat: scala.Float   = value
  def toDouble: scala.Double = value.toDouble

  /**
 * Returns this value, unmodified.
 */
  def unary_+ : scala.Float = value
  /**
 * Returns the negation of this value.
 */
  def unary_- : scala.Float = -value

  def +(x: String): String = value + x

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
  * Returns the sum of this value and `x`.
  */
  def +(x: scala.Byte): scala.Float = value + x
  /**
  * Returns the sum of this value and `x`.
  */
  def +(x: scala.Short): scala.Float = value + x
  /**
  * Returns the sum of this value and `x`.
  */
  def +(x: scala.Char): scala.Float = value + x
  /**
  * Returns the sum of this value and `x`.
  */
  def +(x: scala.Int): scala.Float = value + x
  /**
  * Returns the sum of this value and `x`.
  */
  def +(x: scala.Long): scala.Float = value + x
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
  def -(x: scala.Byte): scala.Float = value - x
  /**
  * Returns the difference of this value and `x`.
  */
  def -(x: scala.Short): scala.Float = value - x
  /**
  * Returns the difference of this value and `x`.
  */
  def -(x: scala.Char): scala.Float = value - x
  /**
  * Returns the difference of this value and `x`.
  */
  def -(x: scala.Int): scala.Float = value - x
  /**
  * Returns the difference of this value and `x`.
  */
  def -(x: scala.Long): scala.Float = value - x
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
  def *(x: scala.Byte): scala.Float = value * x
  /**
  * Returns the product of this value and `x`.
  */
  def *(x: scala.Short): scala.Float = value * x
  /**
  * Returns the product of this value and `x`.
  */
  def *(x: scala.Char): scala.Float = value * x
  /**
  * Returns the product of this value and `x`.
  */
  def *(x: scala.Int): scala.Float = value * x
  /**
  * Returns the product of this value and `x`.
  */
  def *(x: scala.Long): scala.Float = value * x
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
  def /(x: scala.Byte): scala.Float = value / x
  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: scala.Short): scala.Float = value / x
  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: scala.Char): scala.Float = value / x
  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: scala.Int): scala.Float = value / x
  /**
  * Returns the quotient of this value and `x`.
  */
  def /(x: scala.Long): scala.Float = value / x
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
  def %(x: scala.Byte): scala.Float = value % x
  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: scala.Short): scala.Float = value % x
  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: scala.Char): scala.Float = value % x
  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: scala.Int): scala.Float = value % x
  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: scala.Long): scala.Float = value % x
  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: scala.Float): scala.Float = value % x
  /**
  * Returns the remainder of the division of this value by `x`.
  */
  def %(x: scala.Double): scala.Double = value % x

}

object Float {
  val TYPE = classOf[scala.Float]
  val POSITIVE_INFINITY = js.Number.POSITIVE_INFINITY.toFloat
  val NEGATIVE_INFINITY = js.Number.NEGATIVE_INFINITY.toFloat
  val NaN = js.Number.NaN.toFloat
  val MAX_VALUE = js.Number.MAX_VALUE.toFloat // 0x1.fffffeP+127f
  val MIN_NORMAL = 0.0f // 0x1.0p-126f
  val MIN_VALUE = js.Number.MIN_VALUE.toFloat // 0x0.000002P-126f
  val MAX_EXPONENT = 127
  val MIN_EXPONENT = -126
  val SIZE = 32

  def valueOf(floatValue: scala.Float) = new Float(floatValue)

  def parseFloat(s: String): scala.Float = {
    val res = js.parseFloat(s)
    if (s != "NaN" && js.isNaN(res))
      throw new NumberFormatException(s"""For input string: "$s"""")
    else
      res.toFloat
  }

  def toString(f: scala.Float) = valueOf(f).toString

  def compare(a: scala.Float, b: scala.Float): scala.Int = {
    if (a == b) 0
    else if (a < b) -1
    else 1
  }

  def isNaN(v: scala.Float): scala.Boolean = js.isNaN(v)
  def isInfinite(v: scala.Float): scala.Boolean =
    !js.isFinite(v) && !js.isNaN(v)

  def intBitsToFloat(bits: scala.Int): scala.Float = sys.error("unimplemented")
  def floatToIntBits(value: scala.Float): scala.Int = sys.error("unimplemented")
}
