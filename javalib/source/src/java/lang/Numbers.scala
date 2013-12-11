package java.lang

import scala.scalajs.js

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

  override def equals(that: Any) =
    that.isInstanceOf[Byte] && (value == that.asInstanceOf[Byte].value)

  override def toString = (value:js.Number).toString()
}

object Byte {
  val TYPE = classOf[scala.Byte]
  val MIN_VALUE: scala.Byte = -128
  val MAX_VALUE: scala.Byte = 127
  val SIZE: Int = 8

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

  override def equals(that: Any) =
    that.isInstanceOf[Short] && (value == that.asInstanceOf[Short].value)

  override def toString = (value:js.Number).toString()
}

object Short {
  val TYPE = classOf[scala.Short]
  val MIN_VALUE: scala.Short = -32768
  val MAX_VALUE: scala.Short = 32767
  val SIZE: Int = 16

  def valueOf(shortValue: scala.Short) = new Short(shortValue)
  def parseShort(s: String): scala.Short = Integer.parseInt(s).toShort
  def toString(s: scala.Short) = Integer.valueOf(s.toInt).toString

  def reverseBytes(i: scala.Short): scala.Short =
    (((i >>> 8) & 0xff) + ((i & 0xff) << 8)).toShort
}

////////////////// Integer //////////////////

final class Integer(private val value: scala.Int) extends Number {
  protected[lang] val isInt = true

  def intValue() = value
  def longValue() = value.toLong
  def floatValue() = value.toFloat
  def doubleValue() = value.toDouble

  override def equals(that: Any) =
    that.isInstanceOf[Integer] && (value == that.asInstanceOf[Integer].value)

  override def toString = (value:js.Number).toString()
}

object Integer {
  val TYPE = classOf[scala.Int]
  val MIN_VALUE: scala.Int = -2147483648
  val MAX_VALUE: scala.Int = 2147483647
  val SIZE: Int = 32

  def valueOf(intValue: scala.Int) = new Integer(intValue)

  def parseInt(s: String): scala.Int =
    js.parseInt(s).toInt

  def parseInt(s: String, radix: scala.Int): scala.Int =
    js.parseInt(s, radix).toInt

  def toString(i: scala.Int) = valueOf(i).toString

  def bitCount(i: scala.Int): scala.Int = {
    // See http://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetParallel
    // The implicit casts to 32-bit ints due to binary ops make this work in JS too
    val t1 = i - ((i >> 1) & 0x55555555)
    val t2 = (t1 & 0x33333333) + ((t1 >> 2) & 0x33333333)
    ((t2 + (t2 >> 4) & 0xF0F0F0F) * 0x1010101) >> 24
  }

  def reverseBytes(i: scala.Int): scala.Int = {
    val byte3 = i >>> 24
    val byte2 = (i >>> 8) & 0xFF00
    val byte1 = (i << 8) & 0xFF0000
    val byte0 = (i << 24)
    byte0 | byte1 | byte2 | byte3
  }

  def rotateLeft(i: scala.Int, distance: scala.Int): scala.Int = {
    (i << distance) | (i >>> (32-distance))
  }

  def rotateRight(i: scala.Int, distance: scala.Int): scala.Int = {
    (i >>> distance) | (i << (32-distance))
  }

  def signum(i: scala.Int): scala.Int =
    if (i == 0) 0 else if (i < 0) -1 else 1

  def numberOfLeadingZeros(i: scala.Int): scala.Int = {
    // See http://aggregate.org/MAGIC/#Leading%20Zero%20Count
    var x = i
    x |= (x >>> 1)
    x |= (x >>> 2)
    x |= (x >>> 4)
    x |= (x >>> 8)
    x |= (x >>> 16)
    32 - bitCount(x)
  }

  def numberOfTrailingZeros(i: scala.Int): scala.Int =
    // See http://aggregate.org/MAGIC/#Trailing%20Zero%20Count
    bitCount((i & -i) - 1)

  def toBinaryString(i: scala.Int): String = (i:js.Number).toString(2)
  def toHexString(i: scala.Int): String = (i:js.Number).toString(16)
  def toOctalString(i: scala.Int): String = (i:js.Number).toString(8)
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

  override def equals(that: Any) =
    that.isInstanceOf[Long] && (value == that.asInstanceOf[Long].value)

  override def toString = (value:js.Number).toString()
}

object Long {
  val TYPE = classOf[scala.Long]
  val MIN_VALUE: scala.Long = -9223372036854775808L
  val MAX_VALUE: scala.Long = 9223372036854775807L
  val SIZE: Int = 64

  def valueOf(longValue: scala.Long) = new Long(longValue)
  def parseLong(s: String): scala.Long = Integer.parseInt(s).toLong
  def toString(l: scala.Long) = Integer.valueOf(l.toInt).toString

  def bitCount(i: scala.Long): scala.Int = {
    val t1 = i - ((i >> 1) & 0x5555555555555555L)
    val t2 = (t1 & 0x3333333333333333L) + ((t1 >> 2) & 0x3333333333333333L)
    val t3 = (((t2 + (t2 >> 4)) & 0xF0F0F0F0F0F0F0FL) * 0x101010101010101L) >> 56
    t3.toInt
  }

  def reverseBytes(i: scala.Long): scala.Long = sys.error("unimplemented")
  def rotateLeft(i: scala.Long, distance: scala.Int): scala.Long = sys.error("unimplemented")
  def rotateRight(i: scala.Long, distance: scala.Int): scala.Long = sys.error("unimplemented")

  def signum(i: scala.Long): scala.Long =
    if (i == 0) 0 else if (i < 0) -1 else 1

  def toBinaryString(l: scala.Long): String = (l: js.Number).toString(2)
  def toHexString(l: scala.Long): String = (l: js.Number).toString(16)
  def toOctalString(l: scala.Long): String = (l: js.Number).toString(8)
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

  override def equals(that: Any) =
    that.isInstanceOf[Float] && (value == that.asInstanceOf[Float].value)

  override def toString = {
    val s = (value: js.Number).toString()
    if (s.indexOf(".") < 0) s + ".0" else s
  }

  def isNaN: scala.Boolean = Float.isNaN(value)
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

  def valueOf(floatValue: scala.Float) = new Float(floatValue)

  def parseFloat(s: String): scala.Float =
    js.parseFloat(s).toFloat

  def toString(f: scala.Float) = valueOf(f).toString

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

////////////////// Double //////////////////

final class Double(private val value: scala.Double) extends Number {
  protected[lang] val isInt = false

  override def byteValue() = value.toByte
  override def shortValue() = value.toShort
  def intValue() = value.toInt
  def longValue() = value.toLong
  def floatValue() = value.toFloat
  def doubleValue() = value

  override def equals(that: Any) =
    that.isInstanceOf[Double] && (value == that.asInstanceOf[Double].value)

  override def toString = {
    val s = (value: js.Number).toString()
    if (s.indexOf(".") < 0) s + ".0" else s
  }

  def isNaN: scala.Boolean = Double.isNaN(value)
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

  def valueOf(doubleValue: scala.Double) = new Double(doubleValue)
  def parseDouble(s: String): scala.Double = Float.parseFloat(s).toDouble
  def toString(d: scala.Double) = Float.valueOf(d.toFloat).toString

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
