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

  private def parseIntFail(s: String): Nothing =
    throw new NumberFormatException(s"""For input string: "$s"""")

  @inline def parseInt(s: String): scala.Int =
    parseIntImpl(s, 10, divideUnsigned(Int.MinValue, 10))

  @inline // because radix is almost certainly constant at call site
  def parseInt(s: String, radix: scala.Int): scala.Int = {
    if (radix < Character.MIN_RADIX || radix > Character.MAX_RADIX)
      parseIntFail(s)
    parseIntImpl(s, radix, divideUnsigned(Int.MinValue, radix))
  }

  /* Must be called only with a valid radix.
   *
   * The overflowBarrier must be divideUnsigned(Int.MinValue, radix). It will
   * be used to detect overflow during the multiplication (and, a posteriori,
   * for the addition of `+ digit` from the previous iteration).
   *
   * `Int.MinValue` is clearly the correct value for the negative case.
   *
   * For the positive case, in theory it should be `Int.MaxValue`.
   * The only case where that would give a different quotient is when
   * `MinValue = n * radix`. In that case, we will fail to detect the
   * overflow if `result == (n - 1) * radix` just before the multiplication.
   * After the multiplication, it will be `MinValue`, which is out of bounds.
   * That's fine, though, because that case will be caught either on the
   * next iteration of the loop, or in the final overflow check for the
   * addition.
   *
   * That means we can always use the constant `Int.MinValue` here.
   */
  @noinline
  private def parseIntImpl(s: String, radix: Int, overflowBarrier: Int): Int = {
    def fail(): Nothing = parseIntFail(s)

    // Early checks: s non-null and non-empty
    if (s == null)
      fail()
    val len = s.length
    if (len == 0)
      fail()

    // Load the module instance of Character once, instead of in every loop iteration
    val character = Character

    /* Process the sign character.
     * Set `sign` to `-1` if there is a leading '-', and `0` otherwise.
     * Set `i` to 1 if there was a leading '+' or '-', and 0 otherwise.
     */
    val firstChar = s.charAt(0)
    val negative = firstChar == '-'
    val sign = if (negative) -1 else 0
    var i = if (negative || firstChar == '+') 1 else 0

    // We need at least one digit
    if (i >= len)
      fail()

    var result: Int = 0

    while (i != len) {
      val digit = character.digitWithValidRadix(s.charAt(i), radix)
      if (digit == -1 || (result ^ SignBit) > (overflowBarrier ^ SignBit))
        fail()
      result = result * radix + digit
      /* The above addition can overflow the range of valid results (but it
       * cannot overflow the unsigned int range). If that happens during the
       * last iteration, we catch it with the final overflow check. If it
       * happens during an earlier iteration, we catch it with the
       * `overflowBarrier`-based check.
       */
      i += 1
    }

    /* Final overflow check. So far we computed `result` as an unsigned
     * quantity. If negative, the maximum unsigned value allowed in
     * `Int.MinValue`. If non-negative, it is `Int.MaxValue`. We can compute
     * the right value without branches with `Int.MaxValue - sign`.
     */
    if ((result ^ SignBit) > ((Int.MaxValue - sign) ^ SignBit))
      fail()

    /* Compute the final result. Use the standard trick to do this in a
     * branchless way.
     */
    (result ^ sign) - sign
  }

  @inline def parseUnsignedInt(s: String): scala.Int =
    parseUnsignedIntImpl(s, 10, divideUnsigned(-1, 10))

  @inline // because radix is almost certainly constant at call site
  def parseUnsignedInt(s: String, radix: scala.Int): scala.Int = {
    if (radix < Character.MIN_RADIX || radix > Character.MAX_RADIX)
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

    def fail(): Nothing = parseIntFail(s)

    // Early checks: s non-null and non-empty
    if (s == null)
      fail()
    val len = s.length
    if (len == 0)
      fail()

    // Load the module instance of Character once, instead of in every loop iteration
    val character = Character

    // Process a possible leading '+' sign
    var i = if (s.charAt(0) == '+') 1 else 0

    // We need at least one digit
    if (i >= len)
      fail()

    var result: Int = 0

    while (i != len) {
      val digit = character.digitWithValidRadix(s.charAt(i), radix)
      if (digit == -1 || (result ^ SignBit) > (overflowBarrier ^ SignBit))
        fail()
      result = result * radix + digit
      /* Unlike in `parseInt`, the addition overflows outside of the unsigned
       * int range (obviously, otherwise it wouldn't be considered an overflow
       * for `parseUnsignedInt`). We have to test for it at each iteration,
       * as the `overflowBarrier`-based check cannot detect it.
       */
      if ((result ^ SignBit) < (digit ^ SignBit))
        fail()
      i += 1
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

  @inline def compare(x: scala.Int, y: scala.Int): scala.Int = {
    if (x == y) 0
    else if (x < y) -1
    else 1
  }

  @inline def compareUnsigned(x: scala.Int, y: scala.Int): scala.Int = {
    if (x == y) 0
    else if ((x ^ Int.MinValue) < (y ^ Int.MinValue)) -1
    else 1
  }

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

  def compress(i: scala.Int, mask: scala.Int): scala.Int = {
    // Hacker's Delight, Section 7-4, Figure 7-10

    val LogBitSize = 5 // log_2(32)

    // !!! Verbatim copy-paste of Long.compress

    var m = mask
    var x = i & mask // clear irrelevant bits
    var mk = ~m << 1 // we will count 0's to right

    var j = 0 // i in Hacker's Delight, but we already have an i
    while (j < LogBitSize) {
      val mp = parallelSuffix(mk)
      val mv = mp & m // bits to move
      m = (m ^ mv) | (mv >>> (1 << j)) // compress m
      val t = x & mv
      x = (x ^ t) | (t >>> (1 << j)) // compress x
      mk = mk & ~mp
      j += 1
    }

    x
  }

  def expand(i: scala.Int, mask: scala.Int): scala.Int = {
    // Hacker's Delight, Section 7-5, Figure 7-12

    val LogBitSize = 5 // log_2(32)

    val array = new Array[scala.Int](LogBitSize)

    // !!! Verbatim copy-paste of Long.expand

    var m = mask
    var x = i
    var mk = ~m << 1 // we will count 0's to right

    var j = 0 // i in Hacker's Delight, but we already have an i
    while (j < LogBitSize) {
      val mp = parallelSuffix(mk)
      val mv = mp & m // bits to move
      array(j) = mv
      m = (m ^ mv) | (mv >>> (1 << j)) // compress m
      mk = mk & ~mp
      j += 1
    }

    j = LogBitSize - 1
    while (j >= 0) {
      val mv = array(j)
      val t = x << (1 << j)

      /* See the last line of the section text, but there is a mistake in the
       * book: y should be t. There is no y in this algorithm, so it doesn't
       * make sense. Plugging t instead matches the formula (c) of "Exchanging
       * Corresponding Fields of Registers" in Section 2-20.
       */
      x = ((x ^ t) & mv) ^ x

      j -= 1
    }

    x & mask // clear out extraneous bits
  }

  @inline
  private def parallelSuffix(x: Int): Int = {
    // Hacker's Delight, Section 5-2
    var y = x ^ (x << 1)
    y = y ^ (y << 2)
    y = y ^ (y << 4)
    y = y ^ (y << 8)
    y ^ (y << 16)
  }

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
    toUnsignedDouble(i).toString(base)
  }
}
