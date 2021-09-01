/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package java.lang

import java.lang.constant.Constable

/* This is a hijacked class. Its instances are primitive numbers.
 * Constructors are not emitted.
 */
final class Short private ()
    extends Number with Comparable[Short] with Constable {

  def this(value: scala.Short) = this()
  def this(s: String) = this()

  @inline override def shortValue(): scala.Short =
    this.asInstanceOf[scala.Short]

  @inline override def byteValue(): scala.Byte = shortValue().toByte
  @inline def intValue(): scala.Int = shortValue().toInt
  @inline def longValue(): scala.Long = shortValue().toLong
  @inline def floatValue(): scala.Float = shortValue().toFloat
  @inline def doubleValue(): scala.Double = shortValue().toDouble

  @inline override def equals(that: Any): scala.Boolean =
    this eq that.asInstanceOf[AnyRef]

  @inline override def hashCode(): Int =
    shortValue()

  @inline override def compareTo(that: Short): Int =
    Short.compare(shortValue(), that.shortValue())

  @inline override def toString(): String =
    Short.toString(shortValue())

}

object Short {
  /* TYPE should be a `final val`, but that crashes the JVM back-end, so we
   * use a 'def' instead, which is binary compatible.
   */
  def TYPE: Class[_] = scala.Predef.classOf[scala.Short]

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

  @inline def `new`(value: scala.Short): Short = valueOf(value)

  @inline def `new`(s: String): Short = valueOf(s)

  @inline def valueOf(s: scala.Short): Short = s.asInstanceOf[Short]

  @inline def valueOf(s: String): Short = valueOf(parseShort(s))

  @inline def valueOf(s: String, radix: Int): Short =
    valueOf(parseShort(s, radix))

  @inline def parseShort(s: String): scala.Short = parseShort(s, 10)

  def parseShort(s: String, radix: Int): scala.Short = {
    val r = Integer.parseInt(s, radix)
    if (r < MIN_VALUE || r > MAX_VALUE)
      throw new NumberFormatException("For input string: \"" + s + "\"")
    else
      r.toShort
  }

  @inline def toString(s: scala.Short): String =
    "" + s

  @noinline def decode(nm: String): Short =
    Integer.decodeGeneric(nm, valueOf(_, _))

  @inline def compare(x: scala.Short, y: scala.Short): scala.Int =
    x - y

  def reverseBytes(i: scala.Short): scala.Short =
    (((i >>> 8) & 0xff) + ((i & 0xff) << 8)).toShort
}
