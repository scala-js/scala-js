package java.lang

import scala.scalajs.js

// This class is not emitted, but we need to define its members correctly
final class Integer(value: scala.Int) extends Number with Comparable[Integer] {

  def this(s: String) = this(Integer.parseInt(s))

  override def byteValue(): scala.Byte = sys.error("stub")
  override def shortValue(): scala.Short = sys.error("stub")
  def intValue(): scala.Int = sys.error("stub")
  def longValue(): scala.Long = sys.error("stub")
  def floatValue(): scala.Float = sys.error("stub")
  def doubleValue(): scala.Double = sys.error("stub")

  override def equals(that: Any): scala.Boolean = sys.error("stub")

  override def compareTo(that: Integer): Int = sys.error("stub")

  override def toString(): String = sys.error("stub")

}

object Integer {
  val TYPE = classOf[scala.Int]
  val MIN_VALUE: scala.Int = -2147483648
  val MAX_VALUE: scala.Int = 2147483647
  val SIZE: Int = 32

  def valueOf(intValue: scala.Int): Integer = new Integer(intValue)
  def valueOf(s: String): Integer = valueOf(parseInt(s))
  def valueOf(s: String, radix: Int): Integer = valueOf(parseInt(s, radix))

  def parseInt(s: String): scala.Int =
    // explicitly specify radix to avoid interpretation as octal (by JS)
    parseInt(s, 10)

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
        val res = js.parseInt(s, radix)

        if (js.isNaN(res) || res > MAX_VALUE || res < MIN_VALUE)
          fail
        else
          res.toInt
      }
    }
  }

  def toString(i: scala.Int): String = valueOf(i).toString

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

  private[this] def toStringBase(i: scala.Int, base: scala.Int): String =
    ((i: js.prim.Number) >>> 0).toString(base)
}
