package java.lang

/* This is a hijacked class. Its instances are primitive numbers.
 * Constructors are not emitted.
 */
final class Float private () extends Number with Comparable[Float] {

  def this(value: scala.Float) = this()
  def this(s: String) = this()

  @inline def floatValue(): scala.Float =
    this.asInstanceOf[scala.Float]

  @inline override def byteValue(): scala.Byte = floatValue.toByte
  @inline override def shortValue(): scala.Short = floatValue.toShort
  @inline def intValue(): scala.Int = floatValue.toInt
  @inline def longValue(): scala.Long = floatValue.toLong
  @inline def doubleValue(): scala.Double = floatValue.toDouble

  override def equals(that: Any): scala.Boolean = that match {
    case that: Double => // yes, Double
      val a = doubleValue
      val b = that.doubleValue
      (a == b) || (Double.isNaN(a) && Double.isNaN(b))
    case _ =>
      false
  }

  @inline override def hashCode(): Int =
    intValue

  @inline override def compareTo(that: Float): Int =
    Float.compare(floatValue, that.floatValue)

  @inline override def toString(): String =
    Float.toString(floatValue)

  @inline def isNaN(): scala.Boolean =
    Float.isNaN(floatValue)

  @inline def isInfinite(): scala.Boolean =
    Float.isInfinite(floatValue)

}

object Float {
  final val TYPE = classOf[scala.Float]
  final val POSITIVE_INFINITY = 1.0f / 0.0f
  final val NEGATIVE_INFINITY = 1.0f / -0.0f
  final val NaN = 0.0f / 0.0f
  final val MAX_VALUE = scala.Float.MaxValue
  final val MIN_VALUE = scala.Float.MinPositiveValue
  final val MAX_EXPONENT = 127
  final val MIN_EXPONENT = -126
  final val SIZE = 32

  @inline def valueOf(floatValue: scala.Float): Float = new Float(floatValue)

  @inline def valueOf(s: String): Float = valueOf(parseFloat(s))

  @inline def parseFloat(s: String): scala.Float =
    Double.parseDouble(s).toFloat

  @inline def toString(f: scala.Float): String =
    "" + f

  def compare(a: scala.Float, b: scala.Float): scala.Int = {
    // NaN must equal itself, and be greater than anything else
    if (isNaN(a)) {
      if (isNaN(b)) 0
      else 1
    } else if (isNaN(b)) {
      -1
    } else {
      if (a == b) 0
      else if (a < b) -1
      else 1
    }
  }

  @inline protected def equals(a: scala.Float, b: scala.Float): scala.Boolean =
    a == b || (isNaN(a) && isNaN(b))

  @inline def isNaN(v: scala.Float): scala.Boolean =
    v != v

  @inline def isInfinite(v: scala.Float): scala.Boolean =
    v == POSITIVE_INFINITY || v == NEGATIVE_INFINITY

  def intBitsToFloat(bits: scala.Int): scala.Float = sys.error("unimplemented")
  def floatToIntBits(value: scala.Float): scala.Int = sys.error("unimplemented")
}
