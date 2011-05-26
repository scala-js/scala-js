package java.lang

abstract class Number extends Object {
  //protected[lang] val isInt: scala.Boolean

  def byteValue(): scala.Byte = intValue.toByte
  def shortValue(): scala.Short = intValue.toShort
  def intValue(): scala.Int
  def longValue(): scala.Long
  def floatValue(): scala.Float
  def doubleValue(): scala.Double

  protected def scala_==(other: Object): scala.Boolean = equals(other)
  /*other match {
    case num:Number if isInt == num.isInt =>
      if (isInt)
        this.intValue == num.intValue
      else
        this.floatValue == num.floatValue

    case chr:Character if isInt =>
      this.intValue == chr.charValue.toInt

    case _ => equals(other)
  }*/
}

////////////////// Byte //////////////////

final class Byte(private val value: scala.Byte) extends Number {
  protected[lang] val isInt = true

  override def byteValue() = value
  override def shortValue() = value.toShort
  def intValue() = value.toInt
  def longValue() = value.toLong
  def floatValue() = value.toFloat
  def doubleValue() = value.toDouble

  override def toString = new Integer(value.toInt).toString
}

object Byte {
  val TYPE = null
  val MIN_VALUE: scala.Byte = -128
  val MAX_VALUE: scala.Byte = 127

  def valueOf(byteValue: scala.Byte) = new Byte(byteValue)
  def parseByte(s: String): scala.Byte = Integer.parseInt(s).toByte
  def toString(b: scala.Byte) = Integer.valueOf(b.toInt).toString
}

////////////////// Short //////////////////

final class Short(private val value: scala.Short) extends Number {
  protected[lang] val isInt = true

  override def byteValue() = value.toByte
  override def shortValue() = value
  def intValue() = value.toInt
  def longValue() = value.toLong
  def floatValue() = value.toFloat
  def doubleValue() = value.toDouble

  override def toString = new Integer(value.toInt).toString
}

object Short {
  val TYPE = null
  val MIN_VALUE: scala.Short = -32768
  val MAX_VALUE: scala.Short = 32767

  def valueOf(shortValue: scala.Short) = new Short(shortValue)
  def parseShort(s: String): scala.Short = Integer.parseInt(s).toShort
  def toString(s: scala.Short) = Integer.valueOf(s.toInt).toString

  def reverseBytes(i: scala.Short): scala.Short = sys.error("unimplemented")
}

////////////////// Integer //////////////////

final class Integer(private val value: scala.Int) extends Number {
  protected[lang] val isInt = true

  def intValue() = value
  def longValue() = value.toLong
  def floatValue() = value.toFloat
  def doubleValue() = value.toDouble

  @native override def toString: String = sys.error("stub")
}

object Integer {
  val TYPE = null
  val MIN_VALUE: scala.Int = -2147483648
  val MAX_VALUE: scala.Int = 2147483647

  def valueOf(intValue: scala.Int) = new Integer(intValue)

  @native def parseInt(s: String): scala.Int = sys.error("stub")

  def parseInt(s: String, radix: scala.Int): scala.Int = sys.error("unimplemented")

  def toString(i: scala.Int) = valueOf(i).toString

  def bitCount(i: scala.Int): scala.Int = sys.error("unimplemented")
  def reverseBytes(i: scala.Int): scala.Int = sys.error("unimplemented")

  def toBinaryString(i: scala.Int): String = sys.error("unimplemented")
  def toHexString(i: scala.Int): String = sys.error("unimplemented")
  def toOctalString(i: scala.Int): String = sys.error("unimplemented")
}

////////////////// Long //////////////////

final class Long(private val value: scala.Long) extends Number {
  protected[lang] val isInt = true

  override def byteValue() = value.toByte
  override def shortValue() = value.toByte
  def intValue() = value.toInt
  def longValue() = value
  def floatValue() = value.toFloat
  def doubleValue() = value.toDouble

  override def toString = new Integer(value.toInt).toString
}

object Long {
  val TYPE = null
  val MIN_VALUE: scala.Long = -9223372036854775808L
  val MAX_VALUE: scala.Long = 9223372036854775807L

  def valueOf(longValue: scala.Long) = new Long(longValue)
  def parseLong(s: String): scala.Long = Integer.parseInt(s).toLong
  def toString(l: scala.Long) = Integer.valueOf(l.toInt).toString

  def bitCount(i: scala.Long): scala.Long = sys.error("unimplemented")
  def reverseBytes(i: scala.Long): scala.Long = sys.error("unimplemented")

  def toBinaryString(l: scala.Long): String = sys.error("unimplemented")
  def toHexString(l: scala.Long): String = sys.error("unimplemented")
  def toOctalString(l: scala.Long): String = sys.error("unimplemented")
}

////////////////// Float //////////////////

final class Float(private val value: scala.Float) extends Number {
  protected[lang] val isInt = false

  override def byteValue() = value.toByte
  override def shortValue() = value.toShort
  def intValue() = value.toInt
  def longValue() = value.toLong
  def floatValue() = value
  def doubleValue() = value.toDouble

  @native override def toString: String = sys.error("stub")

  def isNaN: scala.Boolean = Float.isNaN(value)
}

object Float {
  val TYPE = null
  val POSITIVE_INFINITY = 0.0f // 1.0f / 0.0f
  val NEGATIVE_INFINITY = 0.0f // -1.0f / 0.0f
  val NaN = 0.0f // 0.0f / 0.0f
  val MAX_VALUE = 0.0f // 0x1.fffffeP+127f
  val MIN_NORMAL = 0.0f // 0x1.0p-126f
  val MIN_VALUE = 0.0f // 0x0.000002P-126f
  val MAX_EXPONENT = 127
  val MIN_EXPONENT = -126
  val SIZE = 32

  def valueOf(floatValue: scala.Float) = new Float(floatValue)

  @native def parseFloat(s: String): scala.Float = sys.error("stub")

  def toString(f: scala.Float) = valueOf(f).toString

  def compare(a: scala.Float, b: scala.Float): scala.Int = {
    if (a == b) 0
    else if (a < b) -1
    else 1
  }

  def isNaN(v: scala.Float): scala.Boolean = sys.error("unimplemented")
  def isInfinite(v: scala.Float): scala.Boolean = sys.error("unimplemented")
}

////////////////// Double //////////////////

final class Double(private val value: scala.Double) extends Number {
  protected[lang] val isInt = false

  override def byteValue() = value.toByte
  override def shortValue() = value.toShort
  def intValue() = value.toInt
  def longValue() = value.toLong
  def floatValue() = value.toFloat
  def doubleValue() = value

  override def toString = new Float(value.toFloat).toString

  def isNaN: scala.Boolean = Double.isNaN(value)
}

object Double {
  val TYPE = null
  val POSITIVE_INFINITY = 0.0d // 1.0 / 0.0
  val NEGATIVE_INFINITY = 0.0d // -1.0 / 0.0
  val NaN = 0.0d // 0.0d / 0.0
  val MAX_VALUE = 0.0d // 0x1.fffffffffffffP+1023
  val MIN_NORMAL = 0.0d // 0x1.0p-1022
  val MIN_VALUE = 0.0d // 0x0.0000000000001P-1022
  val MAX_EXPONENT = 1023
  val MIN_EXPONENT = -1022
  val SIZE = 64

  def valueOf(doubleValue: scala.Double) = new Double(doubleValue)
  def parseDouble(s: String): scala.Double = Float.parseFloat(s).toDouble
  def toString(d: scala.Double) = Float.valueOf(d.toFloat).toString

  def compare(a: scala.Double, b: scala.Double): scala.Int = {
    if (a == b) 0
    else if (a < b) -1
    else 1
  }

  def isNaN(v: scala.Double): scala.Boolean = sys.error("unimplemented")
  def isInfinite(v: scala.Double): scala.Boolean = sys.error("unimplemented")
}
