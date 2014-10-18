package java.lang

import scala.annotation.tailrec

import scala.scalajs.js

// This class is not emitted, but we need to define its members correctly
final class Long(value: scala.Long) extends Number with Comparable[Long] {

  def this(s: String) = this(Long.parseLong(s))

  override def byteValue(): scala.Byte = sys.error("stub")
  override def shortValue(): scala.Short = sys.error("stub")
  def intValue(): scala.Int = sys.error("stub")
  def longValue(): scala.Long = sys.error("stub")
  def floatValue(): scala.Float = sys.error("stub")
  def doubleValue(): scala.Double = sys.error("stub")

  override def equals(that: Any): scala.Boolean = sys.error("stub")

  override def compareTo(that: Long): Int = sys.error("stub")

  override def toString(): String = sys.error("stub")

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
        val cur = (str0: js.prim.String).substring(0, MaxLen): String
        val macc = acc * fastPow(radix, cur.length)
        val ival = js.parseInt(cur, radix): scala.Double
        if (ival.isNaN)
          fail()
        val cval = ival.toInt.toLong // faster than ival.toLong
        loop((str0: js.prim.String).substring(MaxLen), macc + cval)
      } else acc

      loop(s, 0L)
    }
  }

  @inline def toString(l: scala.Long): String = l.toString

  def bitCount(i: scala.Long): scala.Int = {
    val lo = i.toInt
    val hi = (i >>> 32).toInt
    Integer.bitCount(lo) + Integer.bitCount(hi)
  }

  def reverseBytes(i: scala.Long): scala.Long = sys.error("unimplemented")
  def rotateLeft(i: scala.Long, distance: scala.Int): scala.Long = sys.error("unimplemented")
  def rotateRight(i: scala.Long, distance: scala.Int): scala.Long = sys.error("unimplemented")

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
