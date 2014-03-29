package java.lang

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
  import RuntimeLong.{fromRuntimeLong, toRuntimeLong}

  val TYPE = classOf[scala.Long]
  val MIN_VALUE: scala.Long = -9223372036854775808L
  val MAX_VALUE: scala.Long = 9223372036854775807L
  val SIZE: scala.Int = 64

  def valueOf(longValue: scala.Long): Long = new Long(longValue)
  def valueOf(s: String): Long = valueOf(parseLong(s))
  def valueOf(s: String, radix: Int): Long = valueOf(parseLong(s, radix))

  def parseLong(s: String): scala.Long =
    fromRuntimeLong(RuntimeLong.fromString(s))

  def parseLong(s: String, radix: Int): scala.Long =
    fromRuntimeLong(RuntimeLong.fromString(s, radix))

  def toString(l: scala.Long): String = toRuntimeLong(l).toString

  def bitCount(i: scala.Long): scala.Int = toRuntimeLong(i).bitCount

  def reverseBytes(i: scala.Long): scala.Long = sys.error("unimplemented")
  def rotateLeft(i: scala.Long, distance: scala.Int): scala.Long = sys.error("unimplemented")
  def rotateRight(i: scala.Long, distance: scala.Int): scala.Long = sys.error("unimplemented")

  def signum(i: scala.Long): scala.Long =
    if (i == 0) 0 else if (i < 0) -1 else 1

  def numberOfLeadingZeros(l: scala.Long) =
    toRuntimeLong(l).numberOfLeadingZeros

  def toBinaryString(l: scala.Long): String =
    dropLZ(toRuntimeLong(l).toBinaryString)
  def toHexString(l: scala.Long): String =
    dropLZ(toRuntimeLong(l).toHexString)
  def toOctalString(l: scala.Long): String =
    dropLZ(toRuntimeLong(l).toOctalString)

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
