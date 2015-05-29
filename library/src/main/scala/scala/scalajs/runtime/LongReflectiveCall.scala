package scala.scalajs.runtime

import java.lang.{Long => JLong}

/** Explicit box for longs when doing a reflective call.
 *  This class and its methods are only here to properly support reflective
 *  calls on longs.
 */
class LongReflectiveCall(value: Long) {

  // Methods of java.lang.Long

  def byteValue(): Byte = value.toByte
  def shortValue(): Short = value.toShort
  def intValue(): Int = value.toInt
  def longValue(): Long = value
  def floatValue(): Float = value.toFloat
  def doubleValue(): Double = value.toDouble

  def compareTo(that: JLong): Int =
    new JLong(value).compareTo(that)
  def compareTo(that: AnyRef): Int =
    new JLong(value).compareTo(that.asInstanceOf[JLong])

  // Methods of scala.Long

  def toByte: Byte = value.toByte
  def toShort: Short = value.toShort
  def toChar: Char = value.toChar
  def toInt: Int = value.toInt
  def toLong: Long = value
  def toFloat: Float = value.toFloat
  def toDouble: Double = value.toDouble

  // scalastyle:off disallow.space.before.token
  def unary_~ : Long = ~value
  def unary_+ : Long = value
  def unary_- : Long = -value
  // scalastyle:on disallow.space.before.token

  def <<(y: Int): Long = value << y
  def <<(y: Long): Long = value << y
  def >>>(y: Int): Long = value >>> y
  def >>>(y: Long): Long = value >>> y
  def >>(y: Int): Long = value >> y
  def >>(y: Long): Long = value >> y

  def ==(y: Byte): Boolean = value == y
  def ==(y: Short): Boolean = value == y
  def ==(y: Char): Boolean = value == y
  def ==(y: Int): Boolean = value == y
  def ==(y: Long): Boolean = value == y
  def ==(y: Float): Boolean = value == y
  def ==(y: Double): Boolean = value == y

  def !=(y: Byte): Boolean = value != y
  def !=(y: Short): Boolean = value != y
  def !=(y: Char): Boolean = value != y
  def !=(y: Int): Boolean = value != y
  def !=(y: Long): Boolean = value != y
  def !=(y: Float): Boolean = value != y
  def !=(y: Double): Boolean = value != y

  def <(y: Byte): Boolean = value < y
  def <(y: Short): Boolean = value < y
  def <(y: Char): Boolean = value < y
  def <(y: Int): Boolean = value < y
  def <(y: Long): Boolean = value < y
  def <(y: Float): Boolean = value < y
  def <(y: Double): Boolean = value < y

  def <=(y: Byte): Boolean = value <= y
  def <=(y: Short): Boolean = value <= y
  def <=(y: Char): Boolean = value <= y
  def <=(y: Int): Boolean = value <= y
  def <=(y: Long): Boolean = value <= y
  def <=(y: Float): Boolean = value <= y
  def <=(y: Double): Boolean = value <= y

  def >(y: Byte): Boolean = value > y
  def >(y: Short): Boolean = value > y
  def >(y: Char): Boolean = value > y
  def >(y: Int): Boolean = value > y
  def >(y: Long): Boolean = value > y
  def >(y: Float): Boolean = value > y
  def >(y: Double): Boolean = value > y

  def >=(y: Byte): Boolean = value >= y
  def >=(y: Short): Boolean = value >= y
  def >=(y: Char): Boolean = value >= y
  def >=(y: Int): Boolean = value >= y
  def >=(y: Long): Boolean = value >= y
  def >=(y: Float): Boolean = value >= y
  def >=(y: Double): Boolean = value >= y

  def |(y: Byte): Long = value | y
  def |(y: Short): Long = value | y
  def |(y: Char): Long = value | y
  def |(y: Int): Long = value | y
  def |(y: Long): Long = value | y

  def &(y: Byte): Long = value & y
  def &(y: Short): Long = value & y
  def &(y: Char): Long = value & y
  def &(y: Int): Long = value & y
  def &(y: Long): Long = value & y

  def ^(y: Byte): Long = value ^ y
  def ^(y: Short): Long = value ^ y
  def ^(y: Char): Long = value ^ y
  def ^(y: Int): Long = value ^ y
  def ^(y: Long): Long = value ^ y

  def +(y: Byte): Long = value + y
  def +(y: Short): Long = value + y
  def +(y: Char): Long = value + y
  def +(y: Int): Long = value + y
  def +(y: Long): Long = value + y
  def +(y: Float): Float = value + y
  def +(y: Double): Double = value + y

  def -(y: Byte): Long = value - y
  def -(y: Short): Long = value - y
  def -(y: Char): Long = value - y
  def -(y: Int): Long = value - y
  def -(y: Long): Long = value - y
  def -(y: Float): Float = value - y
  def -(y: Double): Double = value - y

  def *(y: Byte): Long = value - y
  def *(y: Short): Long = value - y
  def *(y: Char): Long = value - y
  def *(y: Int): Long = value - y
  def *(y: Long): Long = value - y
  def *(y: Float): Float = value - y
  def *(y: Double): Double = value - y

  def /(y: Byte): Long = value / y
  def /(y: Short): Long = value / y
  def /(y: Char): Long = value / y
  def /(y: Int): Long = value / y
  def /(y: Long): Long = value / y
  def /(y: Float): Float = value / y
  def /(y: Double): Double = value / y

  def %(y: Byte): Long = value % y
  def %(y: Short): Long = value % y
  def %(y: Char): Long = value % y
  def %(y: Int): Long = value % y
  def %(y: Long): Long = value % y
  def %(y: Float): Float = value % y
  def %(y: Double): Double = value % y

}
