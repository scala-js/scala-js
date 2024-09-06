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

import java.lang.constant.{Constable, ConstantDesc}

import scala.scalajs.js
import scala.scalajs.LinkingInfo
import scala.scalajs.LinkingInfo.ESVersion

/* This is a hijacked class. Its instances are primitive numbers.
 * Constructors are not emitted.
 */
final class Integer private ()
    extends Number with Comparable[Integer] with Constable with ConstantDesc {

  def this(value: scala.Int) = this()
  def this(s: String) = this()

  @inline def intValue(): scala.Int =
    this.asInstanceOf[scala.Int]

  @inline override def byteValue(): scala.Byte = intValue().toByte
  @inline override def shortValue(): scala.Short = intValue().toShort
  @inline def longValue(): scala.Long = intValue().toLong
  @inline def floatValue(): scala.Float = intValue().toFloat
  @inline def doubleValue(): scala.Double = intValue().toDouble

  @inline override def equals(that: Any): scala.Boolean =
    this eq that.asInstanceOf[AnyRef]

  @inline override def hashCode(): Int =
    intValue()

  @inline override def compareTo(that: Integer): Int =
    Integer.compare(intValue(), that.intValue())

  @inline override def toString(): String =
    Integer.toString(intValue())
}

object Integer {
  /* TYPE should be a `final val`, but that crashes the JVM back-end, so we
   * use a 'def' instead, which is binary compatible.
   */
  def TYPE: Class[_] = scala.Predef.classOf[scala.Int]

  final val MIN_VALUE = -2147483648
  final val MAX_VALUE = 2147483647
  final val SIZE = 32
  final val BYTES = 4

  @inline def `new`(value: scala.Int): Integer = valueOf(value)

  @inline def `new`(s: String): Integer = valueOf(s)

  @inline def valueOf(i: scala.Int): Integer = i.asInstanceOf[Integer]

  @inline def valueOf(s: String): Integer = valueOf(parseInt(s))

  @inline def valueOf(s: String, radix: Int): Integer =
    valueOf(parseInt(s, radix))

  @inline def parseInt(s: String): scala.Int = parseInt(s, 10)

  @noinline def parseInt(s: String, radix: scala.Int): scala.Int =
    parseIntImpl(s, radix, signed = true)

  @inline def parseUnsignedInt(s: String): scala.Int = parseUnsignedInt(s, 10)

  @noinline def parseUnsignedInt(s: String, radix: scala.Int): scala.Int =
    parseIntImpl(s, radix, signed = false)

  @inline
  private def parseIntImpl(s: String, radix: scala.Int,
      signed: scala.Boolean): scala.Int = {

    def fail(): Nothing =
      throw new NumberFormatException(s"""For input string: "$s"""")

    val len = if (s == null) 0 else s.length

    if (len == 0 || radix < Character.MIN_RADIX || radix > Character.MAX_RADIX)
      fail()

    val firstChar = s.charAt(0)
    val negative = signed && firstChar == '-'

    val maxAbsValue: scala.Double = {
      if (!signed) 0xffffffffL.toDouble
      else if (negative) 0x80000000L.toDouble
      else 0x7fffffffL.toDouble
    }

    var i = if (negative || firstChar == '+') 1 else 0

    // We need at least one digit
    if (i >= s.length)
      fail()

    var result: scala.Double = 0.0
    while (i != len) {
      val digit = Character.digitWithValidRadix(s.charAt(i), radix)
      result = result * radix + digit
      if (digit == -1 || result > maxAbsValue)
        fail()
      i += 1
    }

    if (negative)
      asInt(-result)
    else
      asInt(result)
  }

  @inline def toString(i: scala.Int): String = "" + i

  @inline def toUnsignedString(i: Int, radix: Int): String =
    toStringBase(i, radix)

  @noinline def decode(nm: String): Integer =
    decodeGeneric(nm, valueOf(_, _))

  @inline private[lang] def decodeGeneric[A](nm: String,
      parse: (String, Int) => A): A = {

    val len = nm.length()
    var i = 0

    val negative = if (i != len) {
      nm.charAt(i) match {
        case '+' =>
          i += 1
          false
        case '-' =>
          i += 1
          true
        case _ =>
          false
      }
    } else {
      false
    }

    val base = if (i != len) {
      nm.charAt(i) match {
        case '0' =>
          if (i == len - 1) {
            10
          } else {
            i += 1
            nm.charAt(i) match {
              case 'x' | 'X' =>
                i += 1
                16
              case _ =>
                8
            }
          }
        case '#' =>
          i += 1
          16
        case _ =>
          10
      }
    } else {
      10
    }

    val remaining = nm.substring(i)
    if (remaining.startsWith("+") || remaining.startsWith("-"))
      throw new NumberFormatException("Sign character in wrong position")

    val s = if (negative) "-" + remaining else remaining
    parse(s, base)
  }

  @inline def compare(x: scala.Int, y: scala.Int): scala.Int =
    if (x == y) 0 else if (x < y) -1 else 1

  @inline def compareUnsigned(x: scala.Int, y: scala.Int): scala.Int = {
    import Utils.toUint
    if (x == y) 0
    else if (toUint(x) > toUint(y)) 1
    else -1
  }

  @inline def toUnsignedLong(x: Int): scala.Long =
    x.toLong & 0xffffffffL

  // Wasm intrinsic
  def bitCount(i: scala.Int): scala.Int = {
    /* See http://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetParallel
     *
     * The original algorithm uses *logical* shift rights. Here we use
     * *arithmetic* shift rights instead. >> is shorter than >>>, especially
     * since the latter needs (a >>> b) | 0 in JS. It might also be the case
     * that >>> is a bit slower for that reason on some VMs.
     *
     * Using >> is valid because:
     * * For the 2 first >>, the possible sign bit extension is &'ed away
     * * For (t2 >> 4), t2 cannot be negative because it is at most the result
     *   of 2 * 0x33333333, which does not overflow and is positive.
     * * For the last >> 24, the left operand cannot be negative either.
     *   Assume it was, that means the result of a >>> would be >= 128, but
     *   the correct result must be <= 32. So by contradiction, it is positive.
     */
    val t1 = i - ((i >> 1) & 0x55555555)
    val t2 = (t1 & 0x33333333) + ((t1 >> 2) & 0x33333333)
    (((t2 + (t2 >> 4)) & 0xF0F0F0F) * 0x1010101) >> 24
  }

  // Wasm intrinsic
  @inline def divideUnsigned(dividend: Int, divisor: Int): Int =
    if (divisor == 0) 0 / 0
    else asInt(asUint(dividend) / asUint(divisor))

  // Wasm intrinsic
  @inline def remainderUnsigned(dividend: Int, divisor: Int): Int =
    if (divisor == 0) 0 % 0
    else asInt(asUint(dividend) % asUint(divisor))

  @inline def highestOneBit(i: Int): Int = {
    /* The natural way of implementing this is:
     *   if (i == 0) 0
     *   else (1 << 31) >>> numberOfLeadingZeros(i)
     *
     * We can deal with the 0 case in a branchless fashion by adding `& i` to
     * the else branch:
     *   ((1 << 31) >>> numberOfLeadingZeros(i)) & i
     * Indeed, when i == 0, the `& i` collapses everything to 0. And otherwise,
     * we know that ((1 << 31) >>> numberOfLeadingZeros(i)) is the highest 1
     * bit of i, so &'ing with i is a no-op.
     *
     * Finally, since we're &'ing with i anyway, we can replace the >>> by a
     * >>, which is shorter in JS and does not require the additional `| 0`.
     */
    ((1 << 31) >> numberOfLeadingZeros(i)) & i
  }

  @inline def lowestOneBit(i: Int): Int =
    i & -i

  def reverseBytes(i: scala.Int): scala.Int = {
    val byte3 = i >>> 24
    val byte2 = (i >>> 8) & 0xFF00
    val byte1 = (i << 8) & 0xFF0000
    val byte0 = i << 24
    byte0 | byte1 | byte2 | byte3
  }

  def reverse(i: scala.Int): scala.Int = {
    // From Hacker's Delight, 7-1, Figure 7-1
    val j = (i & 0x55555555) << 1 | (i >> 1) & 0x55555555
    val k = (j & 0x33333333) << 2 | (j >> 2) & 0x33333333
    reverseBytes((k & 0x0F0F0F0F) << 4 | (k >> 4) & 0x0F0F0F0F)
  }

  // Wasm intrinsic
  @inline def rotateLeft(i: scala.Int, distance: scala.Int): scala.Int =
    (i << distance) | (i >>> -distance)

  // Wasm intrinsic
  @inline def rotateRight(i: scala.Int, distance: scala.Int): scala.Int =
    (i >>> distance) | (i << -distance)

  @inline def signum(i: scala.Int): scala.Int =
    if (i == 0) 0 else if (i < 0) -1 else 1

  // Intrinsic, fallback on actual code for non-literal in JS
  @inline def numberOfLeadingZeros(i: scala.Int): scala.Int = {
    if (LinkingInfo.esVersion >= ESVersion.ES2015) js.Math.clz32(i)
    else clz32Dynamic(i)
  }

  private def clz32Dynamic(i: scala.Int) = {
    if (js.typeOf(js.Dynamic.global.Math.clz32) == "function") {
      js.Math.clz32(i)
    } else {
      // See Hacker's Delight, Section 5-3
      var x = i
      if (x == 0) {
        32
      } else {
        var r = 1
        if ((x & 0xffff0000) == 0) { x <<= 16; r += 16 }
        if ((x & 0xff000000) == 0) { x <<= 8; r += 8 }
        if ((x & 0xf0000000) == 0) { x <<= 4; r += 4 }
        if ((x & 0xc0000000) == 0) { x <<= 2; r += 2 }
        r + (x >> 31)
      }
    }
  }

  // Wasm intrinsic
  @inline def numberOfTrailingZeros(i: scala.Int): scala.Int =
    if (i == 0) 32
    else 31 - numberOfLeadingZeros(i & -i)

  def toBinaryString(i: scala.Int): String = toStringBase(i, 2)
  def toHexString(i: scala.Int): String = toStringBase(i, 16)
  def toOctalString(i: scala.Int): String = toStringBase(i, 8)

  @inline // because radix is almost certainly constant at call site
  def toString(i: Int, radix: Int): String = {
    if (radix == 10 || radix < Character.MIN_RADIX || radix > Character.MAX_RADIX) {
      Integer.toString(i)
    } else {
      import js.JSNumberOps.enableJSNumberOps
      i.toString(radix)
    }
  }

  @inline def toUnsignedString(i: scala.Int): String = toUnsignedString(i, 10)

  @inline def hashCode(value: Int): Int = value.hashCode

  @inline def sum(a: Int, b: Int): Int = a + b
  @inline def max(a: Int, b: Int): Int = Math.max(a, b)
  @inline def min(a: Int, b: Int): Int = Math.min(a, b)

  @inline private[this] def toStringBase(i: scala.Int, base: scala.Int): String = {
    import js.JSNumberOps.enableJSNumberOps
    asUint(i).toString(base)
  }

  @inline private def asInt(n: scala.Double): scala.Int = {
    import js.DynamicImplicits.number2dynamic
    (n | 0).asInstanceOf[Int]
  }

  @inline private def asUint(n: scala.Int): scala.Double = {
    import js.DynamicImplicits.number2dynamic
    (n.toDouble >>> 0).asInstanceOf[scala.Double]
  }
}
