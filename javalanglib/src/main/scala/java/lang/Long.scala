package java.lang

import scala.annotation.tailrec

import scala.scalajs.js

/* This is a hijacked class. Its instances are the representation of scala.Longs.
 * Constructors are not emitted.
 */
final class Long private () extends Number with Comparable[Long] {
  def this(value: scala.Long) = this()
  def this(s: String) = this()

  @inline def longValue(): scala.Long =
    this.asInstanceOf[scala.Long]

  @inline override def byteValue(): scala.Byte = longValue.toByte
  @inline override def shortValue(): scala.Short = longValue.toShort
  @inline def intValue(): scala.Int = longValue.toInt
  @inline def floatValue(): scala.Float = longValue.toFloat
  @inline def doubleValue(): scala.Double = longValue.toDouble

  @inline override def equals(that: Any): scala.Boolean = that match {
    case that: Long => longValue == that.longValue
    case _          => false
  }

  @inline override def hashCode(): Int =
    (longValue ^ (longValue >>> 32)).toInt

  @inline override def compareTo(that: Long): Int =
    Long.compare(longValue, that.longValue)

  @inline override def toString(): String =
    Long.toString(longValue)

}

object Long {
  import scala.scalajs.runtime.RuntimeLong

  final val TYPE = classOf[scala.Long]
  final val MIN_VALUE = -9223372036854775808L
  final val MAX_VALUE = 9223372036854775807L
  final val SIZE = 64

  @inline def valueOf(longValue: scala.Long): Long = new Long(longValue)
  @inline def valueOf(s: String): Long = valueOf(parseLong(s))

  @inline def valueOf(s: String, radix: Int): Long =
    valueOf(parseLong(s, radix))

  @inline def parseLong(s: String): scala.Long =
    parseLong(s, 10)

  def parseLong(s: String, radix: Int): scala.Long = {
    import js.JSStringOps._

    def fail() = throw new NumberFormatException(s"""For input string: "$s"""")

    if (s.isEmpty) {
      fail()
    } else if (s.charAt(0) == '-') {
      -parseLong(s.substring(1), radix)
    } else {
      @inline
      @tailrec
      def fastPow(base: Int, exp: Int, acc: Int = 1): Int =
        if (exp == 0) acc
        else if (exp % 2 == 0) fastPow(base*base, exp/2, acc)
        else fastPow(base, exp-1, acc*base)

      @inline
      @tailrec
      def loop(str0: String, acc: scala.Long): scala.Long = if (str0.length > 0) {
        val MaxLen = 9
        val cur = str0.jsSubstring(0, MaxLen)
        val macc = acc * fastPow(radix, cur.length)
        val ival = js.parseInt(cur, radix)
        if (ival.isNaN)
          fail()
        val cval = ival.toInt.toLong // faster than ival.toLong
        loop(str0.jsSubstring(MaxLen), macc + cval)
      } else acc

      loop(s, 0L)
    }
  }

  def toString(l: scala.Long): String = {
    if (l == 0L) "0"
    // Check for MinValue, because it is not negatable
    else if (l == MIN_VALUE) "-9223372036854775808"
    else if (l < 0L) "-" + toString(-l)
    else {
      @tailrec
      @inline
      def toString0(v: scala.Long, acc: String): String = {
        val quot = v / 1000000000L // 9 zeros
        val rem  = v % 1000000000L

        val digits = rem.toInt.toString

        if (quot == 0L) {
          digits + acc
        } else {
          val padding = "000000000".substring(digits.length) // (9 - digits.length) zeros
          toString0(quot, padding + digits + acc)
        }
      }

      toString0(l, "")
    }
  }

  @inline def compare(x: scala.Long, y: scala.Long): scala.Int =
    if (x == y) 0 else if (x < y) -1 else 1

  def bitCount(i: scala.Long): scala.Int = {
    val lo = i.toInt
    val hi = (i >>> 32).toInt
    Integer.bitCount(lo) + Integer.bitCount(hi)
  }

  def reverseBytes(i: scala.Long): scala.Long = {
    val hiReversed = Integer.reverseBytes((i >>> 32).toInt)
    val loReversed = Integer.reverseBytes(i.toInt)
    (loReversed.toLong << 32) | (hiReversed.toLong & 0xffffffffL)
  }

  def rotateLeft(i: scala.Long, distance: scala.Int): scala.Long =
    (i << distance) | (i >>> -distance)

  def rotateRight(i: scala.Long, distance: scala.Int): scala.Long =
    (i >>> distance) | (i << -distance)

  def signum(i: scala.Long): scala.Long =
    if (i < 0L) -1L else if (i == 0L) 0L else 1L

  def numberOfLeadingZeros(l: scala.Long): Int = {
    val hi = (l >>> 32).toInt
    if (hi != 0) Integer.numberOfLeadingZeros(hi)
    else         Integer.numberOfLeadingZeros(l.toInt) + 32
  }

  def numberOfTrailingZeros(l: scala.Long): Int = {
    val lo = l.toInt
    if (lo != 0) Integer.numberOfTrailingZeros(lo)
    else         Integer.numberOfTrailingZeros((l >>> 32).toInt) + 32
  }

  def toBinaryString(l: scala.Long): String = {
    val zeros = "00000000000000000000000000000000" // 32 zeros
    @inline def padBinary32(i: Int) = {
      val s = Integer.toBinaryString(i)
      zeros.substring(s.length) + s
    }

    val lo = l.toInt
    val hi = (l >>> 32).toInt

    if (hi != 0) Integer.toBinaryString(hi) + padBinary32(lo)
    else Integer.toBinaryString(lo)
  }

  def toHexString(l: scala.Long): String = {
    val zeros = "00000000" // 8 zeros
    @inline def padBinary8(i: Int) = {
      val s = Integer.toHexString(i)
      zeros.substring(s.length) + s
    }

    val lo = l.toInt
    val hi = (l >>> 32).toInt

    if (hi != 0) Integer.toHexString(hi) + padBinary8(lo)
    else Integer.toHexString(lo)
  }

  def toOctalString(l: scala.Long): String = {
    val zeros = "0000000000" // 10 zeros
    @inline def padOctal10(i: Int) = {
      val s = Integer.toOctalString(i)
      zeros.substring(s.length) + s
    }

    val lo = l.toInt
    val hi = (l >>> 32).toInt

    val lp = lo & 0x3fffffff
    val mp = ((lo >>> 30) + (hi << 2)) & 0x3fffffff
    val hp = hi >>> 28

    if (hp != 0) Integer.toOctalString(hp) + padOctal10(mp) + padOctal10(lp)
    else if (mp != 0) Integer.toOctalString(mp) + padOctal10(lp)
    else Integer.toOctalString(lp)
  }
}
