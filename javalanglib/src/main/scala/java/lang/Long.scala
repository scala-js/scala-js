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

  @inline def bitCount(i: scala.Long): scala.Int = toRuntimeLong(i).bitCount

  def reverseBytes(i: scala.Long): scala.Long = sys.error("unimplemented")
  def rotateLeft(i: scala.Long, distance: scala.Int): scala.Long = sys.error("unimplemented")
  def rotateRight(i: scala.Long, distance: scala.Int): scala.Long = sys.error("unimplemented")

  @inline def signum(i: scala.Long): scala.Long =
    toRuntimeLong(i).signum

  @inline def numberOfLeadingZeros(l: scala.Long): Int =
    toRuntimeLong(l).numberOfLeadingZeros

  @inline def numberOfTrailingZeros(l: scala.Long): Int =
    toRuntimeLong(l).numberOfTrailingZeros

  def toBinaryString(l: scala.Long): String =
    dropLZ(toRuntimeLong(l).toBinaryString)
  def toHexString(l: scala.Long): String =
    dropLZ(toRuntimeLong(l).toHexString)
  def toOctalString(l: scala.Long): String =
    dropLZ(toRuntimeLong(l).toOctalString)

  /* TODO This is a hack.
   * Ideally the javalib should not even know about RuntimeLong. */
  @inline private def toRuntimeLong(x: Long): RuntimeLong =
    x.asInstanceOf[RuntimeLong]

  /** Drop leading zeros
   *
   * This method was:
   *
   *     s.dropWhile(_ == '0').padTo(1, '0')
   *
   * but generated too much JS code
   */
  private def dropLZ(s: String) = {
    var i = 0
    while (i < s.length-1 && s.charAt(i) == '0')
      i += 1
    s.substring(i)
  }
}
