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
    Float.hashCode(floatValue)

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
  final val MIN_NORMAL = 1.17549435e-38f
  final val MIN_VALUE = scala.Float.MinPositiveValue
  final val MAX_EXPONENT = 127
  final val MIN_EXPONENT = -126
  final val SIZE = 32
  final val BYTES = 4

  @inline def valueOf(floatValue: scala.Float): Float = new Float(floatValue)

  @inline def valueOf(s: String): Float = valueOf(parseFloat(s))

  @inline def parseFloat(s: String): scala.Float =
    Double.parseDouble(s).toFloat

  @inline def toString(f: scala.Float): String =
    "" + f

  def toHexString(f: scala.Float): String = {
    val ebits = 8 // exponent size
    val mbits = 23 // mantissa size
    val bias = (1 << (ebits - 1)) - 1

    val bits = floatToIntBits(f)
    val s = bits < 0
    val m = bits & ((1 << mbits) - 1)
    val e = (bits >>> mbits).toInt & ((1 << ebits) - 1) // biased

    val posResult = if (e > 0) {
      if (e == (1 << ebits) - 1) {
        // Special
        if (m != 0) "NaN"
        else "Infinity"
      } else {
        // Normalized
        "0x1." + mantissaToHexString(m) + "p" + (e - bias)
      }
    } else {
      if (m != 0) {
        // Subnormal
        "0x0." + mantissaToHexString(m) + "p-126"
      } else {
        // Zero
        "0x0.0p0"
      }
    }

    if (bits < 0) "-" + posResult else posResult
  }

  @inline
  private def mantissaToHexString(m: Int): String = {
    @inline def padHex6(i: Int): String = {
      val s = Integer.toHexString(i)
      "000000".substring(s.length) + s // 6 zeros
    }

    // The << 1 turns `m` from a 23-bit int into a 24-bit int (multiple of 4)
    val padded = padHex6(m << 1)
    var len = padded.length
    while (len > 1 && padded.charAt(len - 1) == '0')
      len -= 1
    padded.substring(0, len)
  }

  @inline def compare(a: scala.Float, b: scala.Float): scala.Int =
    Double.compare(a, b)

  @inline def isNaN(v: scala.Float): scala.Boolean =
    v != v

  @inline def isInfinite(v: scala.Float): scala.Boolean =
    v == POSITIVE_INFINITY || v == NEGATIVE_INFINITY

  @inline def isFinite(f: scala.Float): scala.Boolean =
    !isNaN(f) && !isInfinite(f)

  // Uses the hashCode of Doubles. See Bits.numberHashCode for the rationale.
  @inline def hashCode(value: scala.Float): Int =
    scala.scalajs.runtime.Bits.numberHashCode(value)

  @inline def intBitsToFloat(bits: scala.Int): scala.Float =
    scala.scalajs.runtime.Bits.intBitsToFloat(bits)

  @inline def floatToIntBits(value: scala.Float): scala.Int =
    scala.scalajs.runtime.Bits.floatToIntBits(value)

  @inline def sum(a: scala.Float, b: scala.Float): scala.Float =
    a + b

  @inline def max(a: scala.Float, b: scala.Float): scala.Float =
    Math.max(a, b)

  @inline def min(a: scala.Float, b: scala.Float): scala.Float =
    Math.min(a, b)
}
