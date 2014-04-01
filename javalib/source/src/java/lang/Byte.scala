package java.lang

import scala.scalajs.js

// This class is not emitted, but we need to define its members correctly
final class Byte(value: scala.Byte) extends Number with Comparable[Byte] {

  def this(s: String) = this(Byte.parseByte(s))

  override def byteValue(): scala.Byte = sys.error("stub")
  override def shortValue(): scala.Short = sys.error("stub")
  def intValue(): scala.Int = sys.error("stub")
  def longValue(): scala.Long = sys.error("stub")
  def floatValue(): scala.Float = sys.error("stub")
  def doubleValue(): scala.Double = sys.error("stub")

  override def equals(that: Any): scala.Boolean = sys.error("stub")

  override def compareTo(that: Byte): Int = sys.error("stub")

  override def toString(): String = sys.error("stub")

}

object Byte {
  val TYPE = classOf[scala.Byte]
  val MIN_VALUE: scala.Byte = -128
  val MAX_VALUE: scala.Byte = 127
  val SIZE: scala.Int = 8

  def valueOf(byteValue: scala.Byte): Byte = new Byte(byteValue)
  def valueOf(s: String): Byte = valueOf(parseByte(s))
  def valueOf(s: String, radix: Int): Byte = valueOf(parseByte(s, radix))

  def parseByte(s: String): scala.Byte = parseByte(s, 10)

  def parseByte(s: String, radix: Int): scala.Byte = {
    val r = Integer.parseInt(s, radix)
    if (r < MIN_VALUE || r > MAX_VALUE)
      throw new NumberFormatException(s"""For input string: "$s"""")
    else
      r.toByte
  }

  def toString(b: scala.Byte): String = Integer.valueOf(b.toInt).toString
}
