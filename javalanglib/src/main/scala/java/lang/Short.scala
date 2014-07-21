package java.lang

import scala.scalajs.js

// This class is not emitted, but we need to define its members correctly
final class Short(value: scala.Short) extends Number with Comparable[Short] {

  def this(s: String) = this(Short.parseShort(s))

  override def byteValue(): scala.Byte = sys.error("stub")
  override def shortValue(): scala.Short = sys.error("stub")
  def intValue(): scala.Int = sys.error("stub")
  def longValue(): scala.Long = sys.error("stub")
  def floatValue(): scala.Float = sys.error("stub")
  def doubleValue(): scala.Double = sys.error("stub")

  override def equals(that: Any): scala.Boolean = sys.error("stub")

  override def compareTo(that: Short): Int = sys.error("stub")

  override def toString(): String = sys.error("stub")

}

object Short {
  val TYPE = classOf[scala.Short]
  val MIN_VALUE: scala.Short = -32768
  val MAX_VALUE: scala.Short = 32767
  val SIZE: Int = 16

  def valueOf(shortValue: scala.Short): Short = new Short(shortValue)
  def valueOf(s: String): Short = valueOf(parseShort(s))
  def valueOf(s: String, radix: Int): Short = valueOf(parseShort(s, radix))

  def parseShort(s: String): scala.Short = parseShort(s, 10)

  def parseShort(s: String, radix: Int): scala.Short = {
    val r = Integer.parseInt(s, radix)
    if (r < MIN_VALUE || r > MAX_VALUE)
      throw new NumberFormatException(s"""For input string: "$s"""")
    else
      r.toShort
  }

  def toString(s: scala.Short): String = Integer.valueOf(s.toInt).toString

  def reverseBytes(i: scala.Short): scala.Short =
    (((i >>> 8) & 0xff) + ((i & 0xff) << 8)).toShort
}
