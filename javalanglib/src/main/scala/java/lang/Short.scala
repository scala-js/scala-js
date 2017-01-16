package java.lang

/* This is a hijacked class. Its instances are primitive numbers.
 * Constructors are not emitted.
 */
final class Short private () extends Number with Comparable[Short] {

  def this(value: scala.Short) = this()
  def this(s: String) = this()

  @inline override def shortValue(): scala.Short =
    this.asInstanceOf[scala.Short]

  @inline override def byteValue(): scala.Byte = shortValue.toByte
  @inline def intValue(): scala.Int = shortValue.toInt
  @inline def longValue(): scala.Long = shortValue.toLong
  @inline def floatValue(): scala.Float = shortValue.toFloat
  @inline def doubleValue(): scala.Double = shortValue.toDouble

  @inline override def equals(that: Any): scala.Boolean =
    this eq that.asInstanceOf[AnyRef]

  @inline override def hashCode(): Int =
    shortValue

  @inline override def compareTo(that: Short): Int =
    Short.compare(shortValue, that.shortValue)

  @inline override def toString(): String =
    Short.toString(shortValue)

}

object Short {
  final val TYPE = classOf[scala.Short]
  final val SIZE = 16
  final val BYTES = 2

  /* MIN_VALUE and MAX_VALUE should be 'final val's. But it is impossible to
   * write a proper Short literal in Scala, that would both considered a Short
   * and a constant expression (optimized as final val).
   * Since vals and defs are binary-compatible (although they're not strictly
   * speaking source-compatible, because of stability), we implement them as
   * defs. Source-compatibility is not an issue because user code is compiled
   * against the JDK .class files anyway.
   */
  def MIN_VALUE: scala.Short = -32768
  def MAX_VALUE: scala.Short = 32767

  @inline def valueOf(shortValue: scala.Short): Short = new Short(shortValue)
  @inline def valueOf(s: String): Short = valueOf(parseShort(s))

  @inline def valueOf(s: String, radix: Int): Short =
    valueOf(parseShort(s, radix))

  @inline def parseShort(s: String): scala.Short = parseShort(s, 10)

  def parseShort(s: String, radix: Int): scala.Short = {
    val r = Integer.parseInt(s, radix)
    if (r < MIN_VALUE || r > MAX_VALUE)
      throw new NumberFormatException(s"""For input string: "$s"""")
    else
      r.toShort
  }

  @inline def toString(s: scala.Short): String =
    "" + s

  @inline def compare(x: scala.Short, y: scala.Short): scala.Int =
    x - y

  def reverseBytes(i: scala.Short): scala.Short =
    (((i >>> 8) & 0xff) + ((i & 0xff) << 8)).toShort
}
