package scala.scalajs.runtime

import java.lang.{Double => JDouble, Integer => JInteger}

/** Explicit box for number values when doing a reflective call.
 *  This class and its methods are only here to properly support reflective
 *  calls on numbers.
 */
class NumberReflectiveCall(value: Double) {

  // Methods of java.lang.Double and java.lang.Integer

  def byteValue(): Byte = value.toByte
  def shortValue(): Short = value.toShort
  def intValue(): Int = value.toInt
  def longValue(): scala.Long = value.toLong
  def floatValue(): Float = value.toFloat
  def doubleValue(): Double = value

  def compareTo(that: JDouble): Int =
    new JDouble(value).compareTo(that)
  def compareTo(that: JInteger): Int =
    new JDouble(value).compareTo(new JDouble(that.doubleValue()))
  def compareTo(that: AnyRef): Int =
    new JDouble(value).compareTo(that.asInstanceOf[JDouble])

  def isNaN(): scala.Boolean = new JDouble(value).isNaN()
  def isInfinite(): scala.Boolean = new JDouble(value).isInfinite()

  // Methods of scala.Double

  def toByte: scala.Byte     = value.toByte
  def toShort: scala.Short   = value.toShort
  def toChar: scala.Char     = value.toChar
  def toInt: scala.Int       = value.toInt
  def toLong: scala.Long     = value.toLong
  def toFloat: scala.Float   = value.toFloat
  def toDouble: scala.Double = value

  // scalastyle:off disallow.space.before.token
  def unary_+ : scala.Double = value
  def unary_- : scala.Double = -value
  // scalastyle:on disallow.space.before.token

  def +(x: String): String = value + x

  def ==(x: scala.Byte): scala.Boolean = value == x
  def ==(x: scala.Short): scala.Boolean = value == x
  def ==(x: scala.Char): scala.Boolean = value == x
  def ==(x: scala.Int): scala.Boolean = value == x
  def ==(x: scala.Long): scala.Boolean = value == x
  def ==(x: scala.Float): scala.Boolean = value == x
  def ==(x: scala.Double): scala.Boolean = value == x

  def !=(x: scala.Byte): scala.Boolean = value != x
  def !=(x: scala.Short): scala.Boolean = value != x
  def !=(x: scala.Char): scala.Boolean = value != x
  def !=(x: scala.Int): scala.Boolean = value != x
  def !=(x: scala.Long): scala.Boolean = value != x
  def !=(x: scala.Float): scala.Boolean = value != x
  def !=(x: scala.Double): scala.Boolean = value != x

  def <(x: scala.Byte): scala.Boolean = value < x
  def <(x: scala.Short): scala.Boolean = value < x
  def <(x: scala.Char): scala.Boolean = value < x
  def <(x: scala.Int): scala.Boolean = value < x
  def <(x: scala.Long): scala.Boolean = value < x
  def <(x: scala.Float): scala.Boolean = value < x
  def <(x: scala.Double): scala.Boolean = value < x

  def <=(x: scala.Byte): scala.Boolean = value <= x
  def <=(x: scala.Short): scala.Boolean = value <= x
  def <=(x: scala.Char): scala.Boolean = value <= x
  def <=(x: scala.Int): scala.Boolean = value <= x
  def <=(x: scala.Long): scala.Boolean = value <= x
  def <=(x: scala.Float): scala.Boolean = value <= x
  def <=(x: scala.Double): scala.Boolean = value <= x

  def >(x: scala.Byte): scala.Boolean = value > x
  def >(x: scala.Short): scala.Boolean = value > x
  def >(x: scala.Char): scala.Boolean = value > x
  def >(x: scala.Int): scala.Boolean = value > x
  def >(x: scala.Long): scala.Boolean = value > x
  def >(x: scala.Float): scala.Boolean = value > x
  def >(x: scala.Double): scala.Boolean = value > x

  def >=(x: scala.Byte): scala.Boolean = value >= x
  def >=(x: scala.Short): scala.Boolean = value >= x
  def >=(x: scala.Char): scala.Boolean = value >= x
  def >=(x: scala.Int): scala.Boolean = value >= x
  def >=(x: scala.Long): scala.Boolean = value >= x
  def >=(x: scala.Float): scala.Boolean = value >= x
  def >=(x: scala.Double): scala.Boolean = value >= x

  def +(x: scala.Byte): scala.Double = value + x
  def +(x: scala.Short): scala.Double = value + x
  def +(x: scala.Char): scala.Double = value + x
  def +(x: scala.Int): scala.Double = value + x
  def +(x: scala.Long): scala.Double = value + x
  def +(x: scala.Float): scala.Double = value + x
  def +(x: scala.Double): scala.Double = value + x

  def -(x: scala.Byte): scala.Double = value - x
  def -(x: scala.Short): scala.Double = value - x
  def -(x: scala.Char): scala.Double = value - x
  def -(x: scala.Int): scala.Double = value - x
  def -(x: scala.Long): scala.Double = value - x
  def -(x: scala.Float): scala.Double = value - x
  def -(x: scala.Double): scala.Double = value - x

  def *(x: scala.Byte): scala.Double = value * x
  def *(x: scala.Short): scala.Double = value * x
  def *(x: scala.Char): scala.Double = value * x
  def *(x: scala.Int): scala.Double = value * x
  def *(x: scala.Long): scala.Double = value * x
  def *(x: scala.Float): scala.Double = value * x
  def *(x: scala.Double): scala.Double = value * x

  def /(x: scala.Byte): scala.Double = value / x
  def /(x: scala.Short): scala.Double = value / x
  def /(x: scala.Char): scala.Double = value / x
  def /(x: scala.Int): scala.Double = value / x
  def /(x: scala.Long): scala.Double = value / x
  def /(x: scala.Float): scala.Double = value / x
  def /(x: scala.Double): scala.Double = value / x

  def %(x: scala.Byte): scala.Double = value % x
  def %(x: scala.Short): scala.Double = value % x
  def %(x: scala.Char): scala.Double = value % x
  def %(x: scala.Int): scala.Double = value % x
  def %(x: scala.Long): scala.Double = value % x
  def %(x: scala.Float): scala.Double = value % x
  def %(x: scala.Double): scala.Double = value % x

  // Methods of scala.Int that are not defined on scala.Double

  // scalastyle:off disallow.space.before.token
  def unary_~ : scala.Int = ~value.toInt
  // scalastyle:on disallow.space.before.token

  def <<(x: scala.Int): scala.Int = value.toInt << x
  def <<(x: scala.Long): scala.Int = value.toInt << x
  def >>>(x: scala.Int): scala.Int = value.toInt >>> x
  def >>>(x: scala.Long): scala.Int = value.toInt >>> x
  def >>(x: scala.Int): scala.Int = value.toInt >> x
  def >>(x: scala.Long): scala.Int = value.toInt >> x

  def |(x: scala.Byte): scala.Int = value.toInt | x
  def |(x: scala.Short): scala.Int = value.toInt | x
  def |(x: scala.Char): scala.Int = value.toInt | x
  def |(x: scala.Int): scala.Int = value.toInt | x
  def |(x: scala.Long): scala.Long = value.toInt | x

  def &(x: scala.Byte): scala.Int = value.toInt & x
  def &(x: scala.Short): scala.Int = value.toInt & x
  def &(x: scala.Char): scala.Int = value.toInt & x
  def &(x: scala.Int): scala.Int = value.toInt & x
  def &(x: scala.Long): scala.Long = value.toInt & x

  def ^(x: scala.Byte): scala.Int = value.toInt ^ x
  def ^(x: scala.Short): scala.Int = value.toInt ^ x
  def ^(x: scala.Char): scala.Int = value.toInt ^ x
  def ^(x: scala.Int): scala.Int = value.toInt ^ x
  def ^(x: scala.Long): scala.Long = value.toInt ^ x

}
