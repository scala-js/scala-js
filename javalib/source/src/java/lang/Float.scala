package java.lang

import scala.scalajs.js

final class Float(private val value: scala.Float)
    extends Number with Comparable[Float] {

  def this(s: String) = this(Float.parseFloat(s))

  override def byteValue() = value.toByte
  override def shortValue() = value.toShort
  def intValue() = value.toInt
  def longValue() = value.toLong
  def floatValue() = value
  def doubleValue() = value.toDouble

  override def equals(that: Any): scala.Boolean = that match {
    case that: Float =>
      // Always be consistent with the way java.lang.Double performs equality
      new Double(doubleValue()).equals(new Double(that.doubleValue()))
    case _ =>
      false
  }

  override def compareTo(that: Float): Int =
    if (equals(that)) 0 else if (value < that.value) -1 else 1

  override def toString(): String = (value: js.Number).toString()

  def isNaN(): scala.Boolean = Float.isNaN(value)

  /*
   * Methods on scala.Float
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
  protected def toFloat: scala.Float   = value
  protected def toDouble: scala.Double = value.toDouble

  protected def unary_+ : scala.Float = value
  protected def unary_- : scala.Float = -value

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

  protected def +(x: scala.Byte): scala.Float = value + x
  protected def +(x: scala.Short): scala.Float = value + x
  protected def +(x: scala.Char): scala.Float = value + x
  protected def +(x: scala.Int): scala.Float = value + x
  protected def +(x: scala.Long): scala.Float = value + x
  protected def +(x: scala.Float): scala.Float = value + x
  protected def +(x: scala.Double): scala.Double = value + x

  protected def -(x: scala.Byte): scala.Float = value - x
  protected def -(x: scala.Short): scala.Float = value - x
  protected def -(x: scala.Char): scala.Float = value - x
  protected def -(x: scala.Int): scala.Float = value - x
  protected def -(x: scala.Long): scala.Float = value - x
  protected def -(x: scala.Float): scala.Float = value - x
  protected def -(x: scala.Double): scala.Double = value - x

  protected def *(x: scala.Byte): scala.Float = value * x
  protected def *(x: scala.Short): scala.Float = value * x
  protected def *(x: scala.Char): scala.Float = value * x
  protected def *(x: scala.Int): scala.Float = value * x
  protected def *(x: scala.Long): scala.Float = value * x
  protected def *(x: scala.Float): scala.Float = value * x
  protected def *(x: scala.Double): scala.Double = value * x

  protected def /(x: scala.Byte): scala.Float = value / x
  protected def /(x: scala.Short): scala.Float = value / x
  protected def /(x: scala.Char): scala.Float = value / x
  protected def /(x: scala.Int): scala.Float = value / x
  protected def /(x: scala.Long): scala.Float = value / x
  protected def /(x: scala.Float): scala.Float = value / x
  protected def /(x: scala.Double): scala.Double = value / x

  protected def %(x: scala.Byte): scala.Float = value % x
  protected def %(x: scala.Short): scala.Float = value % x
  protected def %(x: scala.Char): scala.Float = value % x
  protected def %(x: scala.Int): scala.Float = value % x
  protected def %(x: scala.Long): scala.Float = value % x
  protected def %(x: scala.Float): scala.Float = value % x
  protected def %(x: scala.Double): scala.Double = value % x

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

  def valueOf(floatValue: scala.Float): Float = new Float(floatValue)
  def valueOf(s: String): Float = valueOf(parseFloat(s))

  def parseFloat(s: String): scala.Float = {
    val res = js.parseFloat(s)
    if (s != "NaN" && js.isNaN(res))
      throw new NumberFormatException(s"""For input string: "$s"""")
    else
      res.toFloat
  }

  def toString(f: scala.Float): String = valueOf(f).toString

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
