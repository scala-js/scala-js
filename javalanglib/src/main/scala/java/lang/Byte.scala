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
  final val TYPE = classOf[scala.Byte]
  final val SIZE = 8

  /* MIN_VALUE and MAX_VALUE should be 'final val's. But it is impossible to
   * write a proper Byte literal in Scala, that would both considered a Byte
   * and a constant expression (optimized as final val).
   * Since vals and defs are binary-compatible (although they're not strictly
   * speaking source-compatible, because of stability), we implement them as
   * defs. Source-compatibility is not an issue because user code is compiled
   * against the JDK .class files anyway.
   */
  def MIN_VALUE: scala.Byte = -128
  def MAX_VALUE: scala.Byte = 127

  @inline def valueOf(byteValue: scala.Byte): Byte = new Byte(byteValue)
  @inline def valueOf(s: String): Byte = valueOf(parseByte(s))

  @inline def valueOf(s: String, radix: Int): Byte =
    valueOf(parseByte(s, radix))

  @inline def parseByte(s: String): scala.Byte = parseByte(s, 10)

  def parseByte(s: String, radix: Int): scala.Byte = {
    val r = Integer.parseInt(s, radix)
    if (r < MIN_VALUE || r > MAX_VALUE)
      throw new NumberFormatException(s"""For input string: "$s"""")
    else
      r.toByte
  }

  @inline def toString(b: scala.Byte): String = b.toString
}
