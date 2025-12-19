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

  @inline def `new`(value: scala.Int): Integer = valueOf(value)

  @inline def `new`(s: String): Integer = valueOf(s)

  @inline def valueOf(i: scala.Int): Integer = i.asInstanceOf[Integer]

  @inline def valueOf(s: String): Integer = valueOf(parseInt(s))

  @inline def valueOf(s: String, radix: Int): Integer =
    valueOf(parseInt(s, radix))

  private[lang] def parseIntFail(s: String): Nothing =
    throw new NumberFormatException(s"""For input string: "$s"""")

  @inline def parseInt(s: String): scala.Int =
    parseIntImpl(s, 10, divideUnsigned(Int.MinValue, 10))

  @inline // because radix is almost certainly constant at call site
  def parseInt(s: String, radix: scala.Int): scala.Int = {
    if (Character.isRadixInvalid(radix))
      parseIntFail(s)
    parseIntImpl(s, radix, divideUnsigned(Int.MinValue, radix))
  }

  /* Must be called only with a valid radix.
   *
   * The overflowBarrier must be divideUnsigned(Int.MinValue, radix).
   */
  @noinline
  private def parseIntImpl(s: String, radix: Int, overflowBarrier: Int): Int =
    IntegerLong.parseSignedImpl(s, radix, overflowBarrier)

  @inline def parseUnsignedInt(s: String): scala.Int =
    parseUnsignedIntImpl(s, 10, divideUnsigned(-1, 10))

  @inline // because radix is almost certainly constant at call site
  def parseUnsignedInt(s: String, radix: scala.Int): scala.Int = {
    if (Character.isRadixInvalid(radix))
      parseIntFail(s)
    parseUnsignedIntImpl(s, radix, divideUnsigned(-1, radix))
  }

  /* Must be called only with a valid radix.
   *
   * The overflowBarrier must be divideUnsigned(-1, radix). It will be used to
   * detect overflow during the multiplication.
   */
  @noinline
  private def parseUnsignedIntImpl(s: String, radix: Int,
      overflowBarrier: Int): Int = {
    IntegerLong.parseUnsignedImpl(s, radix, overflowBarrier)
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

  @inline def compare(x: scala.Int, y: scala.Int): scala.Int = {
    if (x == y) 0
    else if (x < y) -1
    else 1
  }

  @inline def compareUnsigned(x: scala.Int, y: scala.Int): scala.Int = {
    if (x == y) 0
    else if (unsigned_<(x, y)) -1
    else 1
  }

  @inline private[java] def unsigned_<(x: scala.Int, y: scala.Int): scala.Boolean =
    (x ^ SignBit) < (y ^ SignBit)

  @inline private[java] def unsigned_<=(x: scala.Int, y: scala.Int): scala.Boolean =
    (x ^ SignBit) <= (y ^ SignBit)

  @inline private[java] def unsigned_>(x: scala.Int, y: scala.Int): scala.Boolean =
    (x ^ SignBit) > (y ^ SignBit)

  @inline private[java] def unsigned_>=(x: scala.Int, y: scala.Int): scala.Boolean =
    (x ^ SignBit) >= (y ^ SignBit)

  @inline def toUnsignedLong(x: Int): scala.Long =
    throw new Error("stub") // body replaced by the compiler back-end

  @inline private[lang] def toUnsignedDouble(x: Int): scala.Double =
    toUnsignedLong(x).toDouble

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

  @inline
  def reverseBytes(i: scala.Int): scala.Int = {
    /* Hacker's Delight, Section 7-1
     * On JS this is no better than the naive algorithm, but on Wasm we exploit
     * the intrinsics for rotate shifts.
     */
    rotateRight(i & 0x00ff00ff, 8) | (rotateLeft(i, 8) & 0x00ff00ff)
  }

  @inline
  def reverse(i: scala.Int): scala.Int = {
    /* Hacker's Delight, Section 7-1, Figure 7-3
     * We use >> instead of >>> because it's shorter in JS. It makes no
     * difference because the bits coming from the sign extension are masked
     * off with the respective & operations.
     */
    val x0 = rotateLeft(i, 15) // 3 instructions in JS; intrinsic in Wasm
    val t1 = (x0 ^ (x0 >> 10)) & 0x003f801f
    val x1 = (t1 | (t1 << 10)) ^ x0
    val t2 = (x1 ^ (x1 >> 4)) & 0x0e038421
    val x2 = (t2 | (t2 << 4)) ^ x1
    val t3 = (x2 ^ (x2 >> 2)) & 0x22488842
    (t3 | (t3 << 2)) ^ x2
  }

  // Wasm intrinsic
  @inline def rotateLeft(i: scala.Int, distance: scala.Int): scala.Int =
    (i << distance) | (i >>> -distance)

  // Wasm intrinsic
  @inline def rotateRight(i: scala.Int, distance: scala.Int): scala.Int =
    (i >>> distance) | (i << -distance)

  @noinline
  def compress(i: scala.Int, mask: scala.Int): scala.Int =
    IntegerLong.compress(i, mask)

  @noinline
  def expand(i: scala.Int, mask: scala.Int): scala.Int =
    IntegerLong.expand(i, mask)

  @inline def signum(i: scala.Int): scala.Int = {
    // Hacker's Delight, Section 2-8
    (i >> 31) | (-i >>> 31)
  }

  @inline def numberOfLeadingZeros(i: scala.Int): scala.Int =
    throw new Error("stub") // body replaced by the compiler back-end

  // Wasm intrinsic
  @inline def numberOfTrailingZeros(i: scala.Int): scala.Int = {
    // Hacker's Delight, Section 5-4
    32 - numberOfLeadingZeros(~i & (i - 1))
  }

  def toBinaryString(i: scala.Int): String = toStringBase(i, 2)
  def toHexString(i: scala.Int): String = toStringBase(i, 16)
  def toOctalString(i: scala.Int): String = toStringBase(i, 8)

  @inline // because radix is almost certainly constant at call site
  def toString(i: Int, radix: Int): String = {
    if (radix == 10 || Character.isRadixInvalid(radix)) {
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
    toUnsignedDouble(i).toString(base)
  }
}
