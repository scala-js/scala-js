package java.lang

import scala.scalajs.js

/* This is a hijacked class. Its instances are primitive numbers.
 * Constructors are not emitted.
 */
final class Integer private () extends Number with Comparable[Integer] {

  def this(value: scala.Int) = this()
  def this(s: String) = this()

  @inline def intValue(): scala.Int =
    this.asInstanceOf[scala.Int]

  @inline override def byteValue(): scala.Byte = intValue.toByte
  @inline override def shortValue(): scala.Short = intValue.toShort
  @inline def longValue(): scala.Long = intValue.toLong
  @inline def floatValue(): scala.Float = intValue.toFloat
  @inline def doubleValue(): scala.Double = intValue.toDouble

  @inline override def equals(that: Any): scala.Boolean =
    this eq that.asInstanceOf[AnyRef]

  @inline override def hashCode(): Int =
    intValue

  @inline override def compareTo(that: Integer): Int =
    Integer.compare(intValue, that.intValue)

  @inline override def toString(): String =
    Integer.toString(intValue)

  /* Methods of java.lang.Byte and java.lang.Short.
   * When calling a method of j.l.Byte or j.l.Short on a primitive value,
   * it appears to be called directly on the primitive value, which has type
   * IntType. Call resolution, by the analyzer and the optimizer, will then
   * look for the method in the class j.l.Integer instead of j.l.Byte or
   * j.l.Short. This is why we add here the methods of these two classes that
   * are not already in j.l.Integer.
   */

  @inline def compareTo(that: Byte): Int =
    Integer.compare(intValue, that.intValue)

  @inline def compareTo(that: Short): Int =
    Integer.compare(intValue, that.intValue)

}

object Integer {
  final val TYPE = classOf[scala.Int]
  final val MIN_VALUE = -2147483648
  final val MAX_VALUE = 2147483647
  final val SIZE = 32

  @inline def valueOf(intValue: scala.Int): Integer = new Integer(intValue)
  @inline def valueOf(s: String): Integer = valueOf(parseInt(s))

  @inline def valueOf(s: String, radix: Int): Integer =
    valueOf(parseInt(s, radix))

  @inline def parseInt(s: String): scala.Int = parseInt(s, 10)

  def parseInt(s: String, radix: scala.Int): scala.Int = {
    def fail = throw new NumberFormatException(s"""For input string: "$s"""")

    if (s == null || s.size == 0 ||
        radix < Character.MIN_RADIX ||
        radix > Character.MAX_RADIX)
      fail
    else {
      var i = if (s(0) == '-' || s(0) == '+') 1 else 0
      // JavaDoc says: We need at least one digit
      if (s.size <= i) fail
      else {
        // Check each character for validity
        while (i < s.size) {
          if (Character.digit(s(i), radix) < 0) fail
          i += 1
        }
        val res = js.Dynamic.global.parseInt(s, radix).asInstanceOf[scala.Double]

        if (res.isNaN || res > MAX_VALUE || res < MIN_VALUE)
          fail
        else
          res.toInt
      }
    }
  }

  @inline def toString(i: scala.Int): String =
    "" + i

  @inline def compare(x: scala.Int, y: scala.Int): scala.Int =
    if (x == y) 0 else if (x < y) -1 else 1

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

  def rotateLeft(i: scala.Int, distance: scala.Int): scala.Int =
    (i << distance) | (i >>> -distance)

  def rotateRight(i: scala.Int, distance: scala.Int): scala.Int =
    (i >>> distance) | (i << -distance)

  @inline def signum(i: scala.Int): scala.Int =
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

  @inline private[this] def toStringBase(i: scala.Int, base: scala.Int): String = {
    import js.JSNumberOps._
    i.toUint.toString(base)
  }
}
