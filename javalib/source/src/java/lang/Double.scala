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

}

object Double {
  val TYPE = classOf[scala.Double]
  val POSITIVE_INFINITY = js.Number.POSITIVE_INFINITY.toDouble
  val NEGATIVE_INFINITY = js.Number.NEGATIVE_INFINITY.toDouble
  val NaN = js.Number.NaN.toDouble
  val MAX_VALUE = js.Number.MAX_VALUE // 0x1.fffffffffffffP+1023
  val MIN_NORMAL = 0.0d // 0x1.0p-1022
  val MIN_VALUE = js.Number.MIN_VALUE // 0x0.0000000000001P-1022
  val MAX_EXPONENT = 1023
  val MIN_EXPONENT = -1022
  val SIZE = 64

  def valueOf(doubleValue: scala.Double): Double = new Double(doubleValue)
  def valueOf(s: String): Double = valueOf(parseDouble(s))
  def parseDouble(s: String): scala.Double = Float.parseFloat(s).toDouble
  def toString(d: scala.Double): String = Float.valueOf(d.toFloat).toString

  def compare(a: scala.Double, b: scala.Double): scala.Int = {
    if (a == b) 0
    else if (a < b) -1
    else 1
  }

  def isNaN(v: scala.Double): scala.Boolean = js.isNaN(v)
  def isInfinite(v: scala.Double): scala.Boolean =
    !js.isFinite(v) && !js.isNaN(v)

  def longBitsToDouble(bits: scala.Long): scala.Double = sys.error("unimplemented")
  def doubleToLongBits(value: scala.Double): scala.Long = sys.error("unimplemented")
}
