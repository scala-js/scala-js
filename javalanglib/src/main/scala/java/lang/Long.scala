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

  /** Maximal number digits for each radix, so that a number of
   *  that radix can be converted to an Int.
   */
  private lazy val DigitFitInInt = Array(
    -1, -1, 30, 19, 15, 13, 11, 11, 10, 9, 9, 8, 8, 8, 8, 7, 7, 7, 7, 7, 7, 7,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5)

  @inline def valueOf(longValue: scala.Long): Long = new Long(longValue)
  @inline def valueOf(s: String): Long = valueOf(parseLong(s))

  @inline def valueOf(s: String, radix: Int): Long =
    valueOf(parseLong(s, radix))

  @inline def parseLong(s: String): scala.Long =
    parseLong(s, 10)

  def parseLong(s: String, radix: Int): scala.Long = {
    import js.JSStringOps._

    def fail() = throw new NumberFormatException(s"""For input string: "$s"""")

    if (s.isEmpty || radix < Character.MIN_RADIX || radix > Character.MAX_RADIX) {
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

      val MaxLen = DigitFitInInt(radix)

      @inline
      @tailrec
      def loop(str0: String, acc: scala.Long): scala.Long = if (str0.length > 0) {
        val cur = str0.jsSubstring(0, MaxLen)
        val macc = acc * fastPow(radix, cur.length)
        val cval = Integer.parseInt(cur, radix).toLong
        loop(str0.jsSubstring(MaxLen), macc + cval)
      } else acc

      try {
        loop(s, 0L)
      } catch {
        case _: NumberFormatException => fail() // for the correct error message
      }
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

  // Ported from https://github.com/gwtproject/gwt/blob/master/user/super/com/google/gwt/emul/java/lang/Long.java
  def toString(value: scala.Long, intRadix: Int): String = {
    if (intRadix == 10 || intRadix < Character.MIN_RADIX || intRadix > Character.MAX_RADIX) {
      toString(value)
    } else if (Integer.MIN_VALUE <= value && (value <= Integer.MAX_VALUE)) {
      Integer.toString(value.toInt, intRadix)
    } else {
      // Character.forDigit checks the digit range which is not the requirement here - hence the apparent duplication
      def forDigit(digit: Int): Char = {
        val overBaseTen = digit - 10
        val result = if (overBaseTen < 0) '0' + digit else 'a' + overBaseTen
        result.toChar
      }

      // If the value is zero or positive, we can reduce code by negating the value for the string conversion
      // but not adding a '-' at the end

      var _value: scala.Long = if (value >= 0) -value else value
      var res = ""
      var radix: scala.Long = intRadix
      val negRadix: scala.Long = -radix

      while (_value <= negRadix) {
        res = forDigit(-(_value % radix).toInt) + res
        _value /= radix
      }
      res = forDigit(-(_value % radix).toInt) + res

      // But real negative values do need the '-' sign
      if (value < 0) {
        res = '-' + res
      }
      res
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

  def signum(i: scala.Long): Int =
    if (i < 0L) -1 else if (i == 0L) 0 else 1

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
