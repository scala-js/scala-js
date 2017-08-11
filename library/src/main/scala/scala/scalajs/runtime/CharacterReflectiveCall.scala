package scala.scalajs.runtime

/** Explicit box for char values when doing a reflective call.
 *  This class and its methods are only here to properly support reflective
 *  calls on characters.
 */
final class CharacterReflectiveCall(value: Char) {

  // Methods of java.lang.Character

  def charValue(): Char = value

  def compareTo(that: Character): Int =
    new Character(value).compareTo(that)
  def compareTo(that: AnyRef): Int =
    new Character(value).compareTo(that.asInstanceOf[Character])

  // Methods of Char

  def toByte: Byte = value.toByte
  def toShort: Short = value.toShort
  def toChar: Char = value.toChar
  def toInt: Int = value
  def toLong: Long = value.toLong
  def toFloat: Float = value.toFloat
  def toDouble: Double = value.toDouble

  // scalastyle:off disallow.space.before.token
  def unary_~ : Int = ~value
  def unary_+ : Int = value
  def unary_- : Int = -value
  // scalastyle:on disallow.space.before.token

  def +(x: String): String = value + x

  def <<(x: Int): Int = value << x
  def <<(x: Long): Int = value << x
  def >>>(x: Int): Int = value >>> x
  def >>>(x: Long): Int = value >>> x
  def >>(x: Int): Int = value >> x
  def >>(x: Long): Int = value >> x

  def ==(x: Byte): Boolean = value == x
  def ==(x: Short): Boolean = value == x
  def ==(x: Char): Boolean = value == x
  def ==(x: Int): Boolean = value == x
  def ==(x: Long): Boolean = value == x
  def ==(x: Float): Boolean = value == x
  def ==(x: Double): Boolean = value == x

  def !=(x: Byte): Boolean = value != x
  def !=(x: Short): Boolean = value != x
  def !=(x: Char): Boolean = value != x
  def !=(x: Int): Boolean = value != x
  def !=(x: Long): Boolean = value != x
  def !=(x: Float): Boolean = value != x
  def !=(x: Double): Boolean = value != x

  def <(x: Byte): Boolean = value < x
  def <(x: Short): Boolean = value < x
  def <(x: Char): Boolean = value < x
  def <(x: Int): Boolean = value < x
  def <(x: Long): Boolean = value < x
  def <(x: Float): Boolean = value < x
  def <(x: Double): Boolean = value < x

  def <=(x: Byte): Boolean = value <= x
  def <=(x: Short): Boolean = value <= x
  def <=(x: Char): Boolean = value <= x
  def <=(x: Int): Boolean = value <= x
  def <=(x: Long): Boolean = value <= x
  def <=(x: Float): Boolean = value <= x
  def <=(x: Double): Boolean = value <= x

  def >(x: Byte): Boolean = value > x
  def >(x: Short): Boolean = value > x
  def >(x: Char): Boolean = value > x
  def >(x: Int): Boolean = value > x
  def >(x: Long): Boolean = value > x
  def >(x: Float): Boolean = value > x
  def >(x: Double): Boolean = value > x

  def >=(x: Byte): Boolean = value >= x
  def >=(x: Short): Boolean = value >= x
  def >=(x: Char): Boolean = value >= x
  def >=(x: Int): Boolean = value >= x
  def >=(x: Long): Boolean = value >= x
  def >=(x: Float): Boolean = value >= x
  def >=(x: Double): Boolean = value >= x

  def |(x: Byte): Int = value | x
  def |(x: Short): Int = value | x
  def |(x: Char): Int = value | x
  def |(x: Int): Int = value | x
  def |(x: Long): Long = value | x

  def &(x: Byte): Int = value & x
  def &(x: Short): Int = value & x
  def &(x: Char): Int = value & x
  def &(x: Int): Int = value & x
  def &(x: Long): Long = value & x

  def ^(x: Byte): Int = value ^ x
  def ^(x: Short): Int = value ^ x
  def ^(x: Char): Int = value ^ x
  def ^(x: Int): Int = value ^ x
  def ^(x: Long): Long = value ^ x

  def +(x: Byte): Int = value + x
  def +(x: Short): Int = value + x
  def +(x: Char): Int = value + x
  def +(x: Int): Int = value + x
  def +(x: Long): Long = value + x
  def +(x: Float): Float = value + x
  def +(x: Double): Double = value + x

  def -(x: Byte): Int = value - x
  def -(x: Short): Int = value - x
  def -(x: Char): Int = value - x
  def -(x: Int): Int = value - x
  def -(x: Long): Long = value - x
  def -(x: Float): Float = value - x
  def -(x: Double): Double = value - x

  def *(x: Byte): Int = value * x
  def *(x: Short): Int = value * x
  def *(x: Char): Int = value * x
  def *(x: Int): Int = value * x
  def *(x: Long): Long = value * x
  def *(x: Float): Float = value * x
  def *(x: Double): Double = value * x

  def /(x: Byte): Int = value / x
  def /(x: Short): Int = value / x
  def /(x: Char): Int = value / x
  def /(x: Int): Int = value / x
  def /(x: Long): Long = value / x
  def /(x: Float): Float = value / x
  def /(x: Double): Double = value / x

  def %(x: Byte): Int = value % x
  def %(x: Short): Int = value % x
  def %(x: Char): Int = value % x
  def %(x: Int): Int = value % x
  def %(x: Long): Long = value % x
  def %(x: Float): Float = value % x
  def %(x: Double): Double = value % x

}
