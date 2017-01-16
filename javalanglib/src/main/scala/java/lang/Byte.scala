package java.lang

import scala.scalajs.js

/* This is a hijacked class. Its instances are primitive numbers.
 * Constructors are not emitted.
 */
final class Byte private () extends Number with Comparable[Byte] {

  def this(value: scala.Byte) = this()
  def this(s: String) = this()

  @inline override def byteValue(): scala.Byte =
    this.asInstanceOf[scala.Byte]

  @inline override def shortValue(): scala.Short = byteValue.toShort
  @inline def intValue(): scala.Int = byteValue.toInt
  @inline def longValue(): scala.Long = byteValue.toLong
  @inline def floatValue(): scala.Float = byteValue.toFloat
  @inline def doubleValue(): scala.Double = byteValue.toDouble

  @inline override def equals(that: Any): scala.Boolean =
    this eq that.asInstanceOf[AnyRef]

  @inline override def hashCode(): Int =
    byteValue

  @inline override def compareTo(that: Byte): Int =
    Byte.compare(byteValue, that.byteValue)

  @inline override def toString(): String =
    Byte.toString(byteValue)
}

object Byte {
  final val TYPE = classOf[scala.Byte]
  final val SIZE = 8
  final val BYTES = 1

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

  @inline def toString(b: scala.Byte): String =
    "" + b

  @inline def compare(x: scala.Byte, y: scala.Byte): scala.Int =
    x - y
}
