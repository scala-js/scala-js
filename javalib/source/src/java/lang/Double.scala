package java.lang

import scala.scalajs.js

final class Double(private val value: scala.Double) extends Number {

  override def byteValue() = value.toByte
  override def shortValue() = value.toShort
  def intValue() = value.toInt
  def longValue() = value.toLong
  def floatValue() = value.toFloat
  def doubleValue() = value

  override def equals(that0: Any) = that0.isInstanceOf[Double] && {
    val that = that0.asInstanceOf[Double]

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

  def isNaN: scala.Boolean = Double.isNaN(value)

  /*
   * Methods on scala.Double
   * The following methods are only here to properly support reflective calls
   * on boxed primitive values. YOU WILL NOT BE ABLE TO USE THESE METHODS, since
   * we use the true javalib to lookup symbols, this file contains only
   * implementations.
   */
  protected def toByte: scala.Byte     = value.toByte
  protected def toShort: scala.Short   = value.toShort
  protected def toChar: scala.Char     = value.toChar
  protected def toInt: scala.Int       = value.toInt
  protected def toLong: scala.Long     = value.toLong
  protected def toFloat: scala.Float   = value.toFloat
  protected def toDouble: scala.Double = value

  protected def unary_+ : scala.Double = value
  protected def unary_- : scala.Double = -value

  protected def +(x: String): String = value + x

  protected def ==(x: scala.Byte): scala.Boolean = value == x
  protected def ==(x: scala.Short): scala.Boolean = value == x
  protected def ==(x: scala.Char): scala.Boolean = value == x
  protected def ==(x: scala.Int): scala.Boolean = value == x
  protected def ==(x: scala.Long): scala.Boolean = value == x
  protected def ==(x: scala.Float): scala.Boolean = value == x
  protected def ==(x: scala.Double): scala.Boolean = value == x

  protected def !=(x: scala.Byte): scala.Boolean = value != x
  protected def !=(x: scala.Short): scala.Boolean = value != x
  protected def !=(x: scala.Char): scala.Boolean = value != x
  protected def !=(x: scala.Int): scala.Boolean = value != x
  protected def !=(x: scala.Long): scala.Boolean = value != x
  protected def !=(x: scala.Float): scala.Boolean = value != x
  protected def !=(x: scala.Double): scala.Boolean = value != x

  protected def <(x: scala.Byte): scala.Boolean = value < x
  protected def <(x: scala.Short): scala.Boolean = value < x
  protected def <(x: scala.Char): scala.Boolean = value < x
  protected def <(x: scala.Int): scala.Boolean = value < x
  protected def <(x: scala.Long): scala.Boolean = value < x
  protected def <(x: scala.Float): scala.Boolean = value < x
  protected def <(x: scala.Double): scala.Boolean = value < x

  protected def <=(x: scala.Byte): scala.Boolean = value <= x
  protected def <=(x: scala.Short): scala.Boolean = value <= x
  protected def <=(x: scala.Char): scala.Boolean = value <= x
  protected def <=(x: scala.Int): scala.Boolean = value <= x
  protected def <=(x: scala.Long): scala.Boolean = value <= x
  protected def <=(x: scala.Float): scala.Boolean = value <= x
  protected def <=(x: scala.Double): scala.Boolean = value <= x

  protected def >(x: scala.Byte): scala.Boolean = value > x
  protected def >(x: scala.Short): scala.Boolean = value > x
  protected def >(x: scala.Char): scala.Boolean = value > x
  protected def >(x: scala.Int): scala.Boolean = value > x
  protected def >(x: scala.Long): scala.Boolean = value > x
  protected def >(x: scala.Float): scala.Boolean = value > x
  protected def >(x: scala.Double): scala.Boolean = value > x

  protected def >=(x: scala.Byte): scala.Boolean = value >= x
  protected def >=(x: scala.Short): scala.Boolean = value >= x
  protected def >=(x: scala.Char): scala.Boolean = value >= x
  protected def >=(x: scala.Int): scala.Boolean = value >= x
  protected def >=(x: scala.Long): scala.Boolean = value >= x
  protected def >=(x: scala.Float): scala.Boolean = value >= x
  protected def >=(x: scala.Double): scala.Boolean = value >= x

  protected def +(x: scala.Byte): scala.Double = value + x
  protected def +(x: scala.Short): scala.Double = value + x
  protected def +(x: scala.Char): scala.Double = value + x
  protected def +(x: scala.Int): scala.Double = value + x
  protected def +(x: scala.Long): scala.Double = value + x
  protected def +(x: scala.Float): scala.Double = value + x
  protected def +(x: scala.Double): scala.Double = value + x

  protected def -(x: scala.Byte): scala.Double = value - x
  protected def -(x: scala.Short): scala.Double = value - x
  protected def -(x: scala.Char): scala.Double = value - x
  protected def -(x: scala.Int): scala.Double = value - x
  protected def -(x: scala.Long): scala.Double = value - x
  protected def -(x: scala.Float): scala.Double = value - x
  protected def -(x: scala.Double): scala.Double = value - x

  protected def *(x: scala.Byte): scala.Double = value * x
  protected def *(x: scala.Short): scala.Double = value * x
  protected def *(x: scala.Char): scala.Double = value * x
  protected def *(x: scala.Int): scala.Double = value * x
  protected def *(x: scala.Long): scala.Double = value * x
  protected def *(x: scala.Float): scala.Double = value * x
  protected def *(x: scala.Double): scala.Double = value * x

  protected def /(x: scala.Byte): scala.Double = value / x
  protected def /(x: scala.Short): scala.Double = value / x
  protected def /(x: scala.Char): scala.Double = value / x
  protected def /(x: scala.Int): scala.Double = value / x
  protected def /(x: scala.Long): scala.Double = value / x
  protected def /(x: scala.Float): scala.Double = value / x
  protected def /(x: scala.Double): scala.Double = value / x

  protected def %(x: scala.Byte): scala.Double = value % x
  protected def %(x: scala.Short): scala.Double = value % x
  protected def %(x: scala.Char): scala.Double = value % x
  protected def %(x: scala.Int): scala.Double = value % x
  protected def %(x: scala.Long): scala.Double = value % x
  protected def %(x: scala.Float): scala.Double = value % x
  protected def %(x: scala.Double): scala.Double = value % x

}

object Double {
  val TYPE = classOf[scala.Double]
  val POSITIVE_INFINITY = js.Number.POSITIVE_INFINITY.toDouble
  val NEGATIVE_INFINITY = js.Number.NEGATIVE_INFINITY.toDouble
  val NaN = js.Number.NaN.toDouble
  val MAX_VALUE = js.Number.MAX_VALUE // 0x1.fffffffffffffP+1023
  val MIN_NORMAL = 0.0d // 0x1.0p-1022
  val MIN_VALUE = js.Number.MIN_VALUE // 0x0.0000000000001P-1022
  val MAX_EXPONENT = 1023
  val MIN_EXPONENT = -1022
  val SIZE = 64

  def valueOf(doubleValue: scala.Double) = new Double(doubleValue)
  def parseDouble(s: String): scala.Double = Float.parseFloat(s).toDouble
  def toString(d: scala.Double) = Float.valueOf(d.toFloat).toString

  def compare(a: scala.Double, b: scala.Double): scala.Int = {
    if (a == b) 0
    else if (a < b) -1
    else 1
  }

  def isNaN(v: scala.Double): scala.Boolean = js.isNaN(v)
  def isInfinite(v: scala.Double): scala.Boolean =
    !js.isFinite(v) && !js.isNaN(v)

  def longBitsToDouble(bits: scala.Long): scala.Double = sys.error("unimplemented")
  def doubleToLongBits(value: scala.Double): scala.Long = sys.error("unimplemented")
}
