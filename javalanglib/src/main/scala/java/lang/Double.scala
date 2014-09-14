package java.lang

import scala.scalajs.js

// This class is not emitted, but we need to define its members correctly
final class Double(private val value: scala.Double)
    extends Number with Comparable[Double] {

  def this(s: String) = this(Double.parseDouble(s))

  override def byteValue(): scala.Byte = sys.error("stub")
  override def shortValue(): scala.Short = sys.error("stub")
  def intValue(): scala.Int = sys.error("stub")
  def longValue(): scala.Long = sys.error("stub")
  def floatValue(): scala.Float = sys.error("stub")
  def doubleValue(): scala.Double = sys.error("stub")

  override def equals(that: Any): scala.Boolean = sys.error("stub")

  override def compareTo(that: Double): Int = sys.error("stub")

  override def toString(): String = sys.error("stub")

  def isNaN(): scala.Boolean = sys.error("stub")
  def isInfinite(): scala.Boolean = sys.error("stub")

}

object Double {
  final val TYPE = classOf[scala.Double]
  final val POSITIVE_INFINITY = 1.0 / 0.0
  final val NEGATIVE_INFINITY = 1.0 / -0.0
  final val NaN = 0.0 / 0.0
  final val MAX_VALUE = scala.Double.MaxValue
  final val MIN_VALUE = scala.Double.MinPositiveValue
  final val MAX_EXPONENT = 1023
  final val MIN_EXPONENT = -1022
  final val SIZE = 64

  @inline def valueOf(doubleValue: scala.Double): Double =
    new Double(doubleValue)

  @inline def valueOf(s: String): Double = valueOf(parseDouble(s))

  @inline def parseDouble(s: String): scala.Double =
    Float.parseFloat(s).toDouble

  @inline def toString(d: scala.Double): String = d.toString

  @inline def compare(a: scala.Double, b: scala.Double): scala.Int = {
    if (a == b) 0
    else if (a < b) -1
    else 1
  }

  @inline def isNaN(v: scala.Double): scala.Boolean =
    valueOf(v).isNaN()

  @inline def isInfinite(v: scala.Double): scala.Boolean =
    valueOf(v).isInfinite()

  def longBitsToDouble(bits: scala.Long): scala.Double = sys.error("unimplemented")
  def doubleToLongBits(value: scala.Double): scala.Long = sys.error("unimplemented")
}
