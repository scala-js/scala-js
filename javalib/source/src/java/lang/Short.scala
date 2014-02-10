package java.lang

import scala.scalajs.js

final class Short(private val value: scala.Short) extends Number {
  protected[lang] val isInt = true

  override def byteValue() = value.toByte
  override def shortValue() = value
  def intValue() = value.toInt
  def longValue() = value.toLong
  def floatValue() = value.toFloat
  def doubleValue() = value.toDouble

  override def hashCode(): Int = value.##

  override def equals(that: Any) =
    that.isInstanceOf[Short] && (value == that.asInstanceOf[Short].value)

  override def toString = (value:js.Number).toString()

  /*
   * Methods on scala.Short
   * The following methods are only here to properly support reflective calls
   * on boxed primitive values. YOU WILL NOT BE ABLE TO USE THESE METHODS, since
   * we use the true javalib to lookup symbols, this file contains only
   * implementations.
   */
  protected def toByte: scala.Byte     = value.toByte
  protected def toShort: scala.Short   = value
  protected def toChar: scala.Char     = value.toChar
  protected def toInt: scala.Int       = value.toInt
  protected def toLong: scala.Long     = value.toLong
  protected def toFloat: scala.Float   = value.toFloat
  protected def toDouble: scala.Double = value.toDouble

  protected def unary_~ : scala.Int = ~value
  protected def unary_+ : scala.Int = value
  protected def unary_- : scala.Int = -value

  protected def +(x: String): String = value + x

  protected def <<(x: scala.Int): scala.Int = value << x
  protected def <<(x: scala.Long): scala.Int = value << x
  protected def >>>(x: scala.Int): scala.Int = value >>> x
  protected def >>>(x: scala.Long): scala.Int = value >>> x
  protected def >>(x: scala.Int): scala.Int = value >> x
  protected def >>(x: scala.Long): scala.Int = value >> x

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

  protected def |(x: scala.Byte): scala.Int = value | x
  protected def |(x: scala.Short): scala.Int = value | x
  protected def |(x: scala.Char): scala.Int = value | x
  protected def |(x: scala.Int): scala.Int = value | x
  protected def |(x: scala.Long): scala.Long = value | x

  protected def &(x: scala.Byte): scala.Int = value & x
  protected def &(x: scala.Short): scala.Int = value & x
  protected def &(x: scala.Char): scala.Int = value & x
  protected def &(x: scala.Int): scala.Int = value & x
  protected def &(x: scala.Long): scala.Long = value & x

  protected def ^(x: scala.Byte): scala.Int = value ^ x
  protected def ^(x: scala.Short): scala.Int = value ^ x
  protected def ^(x: scala.Char): scala.Int = value ^ x
  protected def ^(x: scala.Int): scala.Int = value ^ x
  protected def ^(x: scala.Long): scala.Long = value ^ x

  protected def +(x: scala.Byte): scala.Int = value + x
  protected def +(x: scala.Short): scala.Int = value + x
  protected def +(x: scala.Char): scala.Int = value + x
  protected def +(x: scala.Int): scala.Int = value + x
  protected def +(x: scala.Long): scala.Long = value + x
  protected def +(x: scala.Float): scala.Float = value + x
  protected def +(x: scala.Double): scala.Double = value + x

  protected def -(x: scala.Byte): scala.Int = value - x
  protected def -(x: scala.Short): scala.Int = value - x
  protected def -(x: scala.Char): scala.Int = value - x
  protected def -(x: scala.Int): scala.Int = value - x
  protected def -(x: scala.Long): scala.Long = value - x
  protected def -(x: scala.Float): scala.Float = value - x
  protected def -(x: scala.Double): scala.Double = value - x

  protected def *(x: scala.Byte): scala.Int = value * x
  protected def *(x: scala.Short): scala.Int = value * x
  protected def *(x: scala.Char): scala.Int = value * x
  protected def *(x: scala.Int): scala.Int = value * x
  protected def *(x: scala.Long): scala.Long = value * x
  protected def *(x: scala.Float): scala.Float = value * x
  protected def *(x: scala.Double): scala.Double = value * x

  protected def /(x: scala.Byte): scala.Int = value / x
  protected def /(x: scala.Short): scala.Int = value / x
  protected def /(x: scala.Char): scala.Int = value / x
  protected def /(x: scala.Int): scala.Int = value / x
  protected def /(x: scala.Long): scala.Long = value / x
  protected def /(x: scala.Float): scala.Float = value / x
  protected def /(x: scala.Double): scala.Double = value / x

  protected def %(x: scala.Byte): scala.Int = value % x
  protected def %(x: scala.Short): scala.Int = value % x
  protected def %(x: scala.Char): scala.Int = value % x
  protected def %(x: scala.Int): scala.Int = value % x
  protected def %(x: scala.Long): scala.Long = value % x
  protected def %(x: scala.Float): scala.Float = value % x
  protected def %(x: scala.Double): scala.Double = value % x

}

object Short {
  val TYPE = classOf[scala.Short]
  val MIN_VALUE: scala.Short = -32768
  val MAX_VALUE: scala.Short = 32767
  val SIZE: Int = 16

  def valueOf(shortValue: scala.Short) = new Short(shortValue)
  def parseShort(s: String): scala.Short = Integer.parseInt(s).toShort
  def toString(s: scala.Short) = Integer.valueOf(s.toInt).toString

  def reverseBytes(i: scala.Short): scala.Short =
    (((i >>> 8) & 0xff) + ((i & 0xff) << 8)).toShort
}
