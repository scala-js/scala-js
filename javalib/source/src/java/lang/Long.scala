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
  protected def toByte: scala.Byte     = value.toByte
  protected def toShort: scala.Short   = value.toShort
  protected def toChar: scala.Char     = value.toChar
  protected def toInt: scala.Int       = value.toInt
  protected def toLong: scala.Long     = value
  protected def toFloat: scala.Float   = value.toFloat
  protected def toDouble: scala.Double = value.toDouble

  protected def unary_~ : scala.Long = ~value
  protected def unary_+ : scala.Long = value
  protected def unary_- : scala.Long = -value

  protected def +(x: String): String = value + x

  protected def <<(x: scala.Int): scala.Long = value << x
  protected def <<(x: scala.Long): scala.Long = value << x
  protected def >>>(x: scala.Int): scala.Long = value >>> x
  protected def >>>(x: scala.Long): scala.Long = value >>> x
  protected def >>(x: scala.Int): scala.Long = value >> x
  protected def >>(x: scala.Long): scala.Long = value >> x

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

  protected def |(x: scala.Byte): scala.Long = value | x
  protected def |(x: scala.Short): scala.Long = value | x
  protected def |(x: scala.Char): scala.Long = value | x
  protected def |(x: scala.Int): scala.Long = value | x
  protected def |(x: scala.Long): scala.Long = value | x

  protected def &(x: scala.Byte): scala.Long = value & x
  protected def &(x: scala.Short): scala.Long = value & x
  protected def &(x: scala.Char): scala.Long = value & x
  protected def &(x: scala.Int): scala.Long = value & x
  protected def &(x: scala.Long): scala.Long = value & x

  protected def ^(x: scala.Byte): scala.Long = value ^ x
  protected def ^(x: scala.Short): scala.Long = value ^ x
  protected def ^(x: scala.Char): scala.Long = value ^ x
  protected def ^(x: scala.Int): scala.Long = value ^ x
  protected def ^(x: scala.Long): scala.Long = value ^ x

  protected def +(x: scala.Byte): scala.Long = value + x
  protected def +(x: scala.Short): scala.Long = value + x
  protected def +(x: scala.Char): scala.Long = value + x
  protected def +(x: scala.Int): scala.Long = value + x
  protected def +(x: scala.Long): scala.Long = value + x
  protected def +(x: scala.Float): scala.Float = value + x
  protected def +(x: scala.Double): scala.Double = value + x

  protected def -(x: scala.Byte): scala.Long = value - x
  protected def -(x: scala.Short): scala.Long = value - x
  protected def -(x: scala.Char): scala.Long = value - x
  protected def -(x: scala.Int): scala.Long = value - x
  protected def -(x: scala.Long): scala.Long = value - x
  protected def -(x: scala.Float): scala.Float = value - x
  protected def -(x: scala.Double): scala.Double = value - x

  protected def *(x: scala.Byte): scala.Long = value - x
  protected def *(x: scala.Short): scala.Long = value - x
  protected def *(x: scala.Char): scala.Long = value - x
  protected def *(x: scala.Int): scala.Long = value - x
  protected def *(x: scala.Long): scala.Long = value - x
  protected def *(x: scala.Float): scala.Float = value - x
  protected def *(x: scala.Double): scala.Double = value - x

  protected def /(x: scala.Byte): scala.Long = value / x
  protected def /(x: scala.Short): scala.Long = value / x
  protected def /(x: scala.Char): scala.Long = value / x
  protected def /(x: scala.Int): scala.Long = value / x
  protected def /(x: scala.Long): scala.Long = value / x
  protected def /(x: scala.Float): scala.Float = value / x
  protected def /(x: scala.Double): scala.Double = value / x

  protected def %(x: scala.Byte): scala.Long = value % x
  protected def %(x: scala.Short): scala.Long = value % x
  protected def %(x: scala.Char): scala.Long = value % x
  protected def %(x: scala.Int): scala.Long = value % x
  protected def %(x: scala.Long): scala.Long = value % x
  protected def %(x: scala.Float): scala.Float = value % x
  protected def %(x: scala.Double): scala.Double = value % x

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
