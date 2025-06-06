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
import java.util.function._
import java.util.ScalaOps._

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

  private final val SignBit = Int.MinValue

  /** Quantities in this class are interpreted as unsigned values.
   *
   *  - `maxLength`: `toUnsignedString(UInt.MaxValue, radix).length()`.
   *    This is the maximum length of a string that can be parsed into an unsigned
   *    `Int` using the given radix, assuming there is no leading sign or 0 digit.
   *  - `overflowBarrier`: `divideUnsigned(UInt.MaxValue, radix)`.
   *    This is the largest unsigned `x: Int` such that `x * radix` does not overflow.
   */
  private final class StringRadixInfo(val maxLength: Int, val overflowBarrier: Int)

  /** Precomputed table for parseIntInternal. */
  private lazy val StringRadixInfos: Array[StringRadixInfo] = {
    val r = new Array[StringRadixInfo](Character.MAX_RADIX + 1)

    for (radix <- Character.MIN_RADIX to Character.MAX_RADIX) {
      val overflowBarrier = divideUnsigned(-1, radix)
      var radixPower = 1
      var maxLength = 1 // toString(1, radix).length() == "1".length()
      // invariant: maxLength == toString(radixPower, radix).length()
      while ((radixPower ^ SignBit) <= (overflowBarrier ^ SignBit)) { // unsigned comparison
        maxLength += 1
        radixPower *= radix
      }
      // now radixPower is the greatest power of radix <= UInt.MaxValue
      r(radix) = new StringRadixInfo(maxLength, overflowBarrier)
    }

    r
  }

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

  private def parseIntFail(s: String): Nothing =
    throw new NumberFormatException(s"""For input string: "$s"""")

  @inline
  private def parseIntImpl(s: String, radix: scala.Int,
      signed: scala.Boolean): scala.Int = {

    def fail(): Nothing = parseIntFail(s)

    if (s == null || s == "")
      fail()

    val firstChar = s.charAt(0)
    val negative = signed && firstChar == '-'
    val start = if (negative || firstChar == '+') 1 else 0

    val unsignedResult = parseIntInternal(s, radix, start)

    if (signed) {
      if (negative) {
        val result = -unsignedResult
        if (result > 0)
          fail()
        result
      } else {
        if (unsignedResult < 0)
          fail()
        unsignedResult
      }
    } else {
      unsignedResult
    }
  }

  @noinline
  private def parseIntInternal(s: String, radix: Int, start: Int): Int = {
    def fail(): Nothing = parseIntFail(s)

    val len = s.length()

    // We need at least one digit, even if it is a 0 digit; also check the radix
    if (start == len || radix < Character.MIN_RADIX || radix > Character.MAX_RADIX)
      fail()

    val radixInfo = StringRadixInfos(radix)
    val maxLength = radixInfo.maxLength

    // Skip over leading 0 digits
    var i = start
    while (i != len && Character.isZeroDigit(s.charAt(i)))
      i += 1

    // Check that the remaining string is not too long
    val remainingLength = len - i
    if (remainingLength > maxLength)
      fail()

    // Until which index can we safely loop without worrying about overflow?
    val safeLen = if (remainingLength == maxLength) len - 1 else len

    var result: Int = 0

    // Loop until safeLen, during which no overflow can occur
    while (i != safeLen) {
      val digit = Character.digitWithValidRadix(s.charAt(i), radix)
      if (digit == -1)
        fail()
      result = result * radix + digit
      i += 1
    }

    // Possibly handle the last digit, where we have to check for overflow
    if (safeLen != len) {
      val digit = Character.digitWithValidRadix(s.charAt(safeLen), radix)
      if (digit == -1 || (result ^ SignBit) > (radixInfo.overflowBarrier ^ SignBit))
        fail()
      result = result * radix + digit
      if ((result ^ SignBit) < (Character.MAX_RADIX ^ SignBit)) // unsigned comparison
        fail()
    }

    result
  }

  @inline def toString(i: scala.Int): String = "" + i

  @inline def toUnsignedString(i: Int, radix: Int): String =
    toStringBase(i, radix)

  @noinline def decode(nm: String): Integer =
    decodeGeneric(nm, valueOf(_, _))

  @inline private[lang] def decodeGeneric[A](nm: String,
      parse: BiFunction[String, Int, A]): A = {

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

  @inline def divideUnsigned(dividend: Int, divisor: Int): Int =
    throw new Error("stub") // body replaced by the compiler back-end

  @inline def remainderUnsigned(dividend: Int, divisor: Int): Int =
    throw new Error("stub") // body replaced by the compiler back-end

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

  @inline def numberOfLeadingZeros(i: scala.Int): scala.Int =
    throw new Error("stub") // body replaced by the compiler back-end

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

  @inline private def asUint(n: scala.Int): scala.Double = {
    import js.DynamicImplicits.number2dynamic
    (n.toDouble >>> 0).asInstanceOf[scala.Double]
  }
}
