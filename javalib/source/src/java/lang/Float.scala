package java.lang

import scala.scalajs.js

// This class is not emitted, but we need to define its members correctly
final class Float(value: scala.Float) extends Number with Comparable[Float] {

  def this(s: String) = this(Float.parseFloat(s))

  override def byteValue(): scala.Byte = sys.error("stub")
  override def shortValue(): scala.Short = sys.error("stub")
  def intValue(): scala.Int = sys.error("stub")
  def longValue(): scala.Long = sys.error("stub")
  def floatValue(): scala.Float = sys.error("stub")
  def doubleValue(): scala.Double = sys.error("stub")

  override def equals(that: Any): scala.Boolean = sys.error("stub")

  override def compareTo(that: Float): Int = sys.error("stub")

  override def toString(): String = sys.error("stub")

  def isNaN(): scala.Boolean = sys.error("stub")
  def isInfinite(): scala.Boolean = sys.error("stub")

}

object Float {
  val TYPE = classOf[scala.Float]
  val POSITIVE_INFINITY = js.Number.POSITIVE_INFINITY.toFloat
  val NEGATIVE_INFINITY = js.Number.NEGATIVE_INFINITY.toFloat
  val NaN = js.Number.NaN.toFloat
  val MAX_VALUE = js.Number.MAX_VALUE.toFloat // 0x1.fffffeP+127f
  val MIN_NORMAL = 0.0f // 0x1.0p-126f
  val MIN_VALUE = js.Number.MIN_VALUE.toFloat // 0x0.000002P-126f
  val MAX_EXPONENT = 127
  val MIN_EXPONENT = -126
  val SIZE = 32

  private[this] val floatStrPat = new js.RegExp("^" +
      "[\\x00-\\x20]*"   + // optional whitespace
      "[+-]?"            + // optional sign
      "(NaN|Infinity|"   + // special cases
       "(\\d+\\.?\\d*|"  + // literal w/  leading digit
        "\\.\\d+)"       + // literal w/o leading digit
       "([eE][+-]?\\d+)?"+ // optional exponent
      ")[fFdD]?"         + // optional float / double specifier (ignored)
      "[\\x00-\\x20]*"   + // optional whitespace
      "$")

  def valueOf(floatValue: scala.Float): Float = new Float(floatValue)
  def valueOf(s: String): Float = valueOf(parseFloat(s))

  def parseFloat(s: String): scala.Float = {
    if (floatStrPat.test(s))
      js.parseFloat(s).toFloat
    else
      throw new NumberFormatException(s"""For input string: "$s"""")
  }

  def toString(f: scala.Float): String = valueOf(f).toString

  def compare(a: scala.Float, b: scala.Float): scala.Int = {
    if (a == b) 0
    else if (a < b) -1
    else 1
  }

  def isNaN(v: scala.Float): scala.Boolean = js.isNaN(v)
  def isInfinite(v: scala.Float): scala.Boolean =
    !js.isFinite(v) && !js.isNaN(v)

  def intBitsToFloat(bits: scala.Int): scala.Float = sys.error("unimplemented")
  def floatToIntBits(value: scala.Float): scala.Int = sys.error("unimplemented")
}
