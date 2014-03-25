package java.lang

import scala.scalajs.js

final class Integer(private val value: scala.Int)
    extends Number with Comparable[Integer] {

  def intValue() = value
  def longValue() = value.toLong
  def floatValue() = value.toFloat
  def doubleValue() = value.toDouble

  override def hashCode(): Int = value.##

  override def equals(that: Any) =
    that.isInstanceOf[Integer] && (value == that.asInstanceOf[Integer].value)

  override def compareTo(that: Integer): Int =
    if (value == that.value) 0 else if (value < that.value) -1 else 1

  override def toString = (value:js.Number).toString()

  /*
   * Methods on scala.Int
   * The following methods are only here to properly support reflective calls
   * on boxed primitive values. YOU WILL NOT BE ABLE TO USE THESE METHODS, since
   * we use the true javalib to lookup symbols, this file contains only
   * implementations.
   */
  protected def toByte: scala.Byte     = value.toByte
  protected def toShort: scala.Short   = value.toShort
  protected def toChar: scala.Char     = value.toChar
  protected def toInt: scala.Int       = value
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

  def toBinaryString(i: scala.Int): String = toStringBase(i, 2)
  def toHexString(i: scala.Int): String = toStringBase(i, 16)
  def toOctalString(i: scala.Int): String = toStringBase(i, 8)

  private[this] def toStringBase(i: scala.Int, base: scala.Int): String = {
    if (i > scala.Int.MaxValue) "+" + (i: js.Number).toString(base)
    else if (i < scala.Int.MinValue) (i: js.Number).toString(base) // includes "-"
    else ((i: js.Number) >>> 0).toString(base)
  }
}
