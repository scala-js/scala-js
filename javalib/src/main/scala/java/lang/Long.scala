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

import scala.annotation.{switch, tailrec}

import java.lang.constant.{Constable, ConstantDesc}
import java.util.ScalaOps._

import scala.scalajs.js
import scala.scalajs.LinkingInfo

/* This is a hijacked class. Its instances are the representation of scala.Longs.
 * Constructors are not emitted.
 */
final class Long private ()
    extends Number with Comparable[Long] with Constable with ConstantDesc {

  def this(value: scala.Long) = this()
  def this(s: String) = this()

  @inline def longValue(): scala.Long =
    this.asInstanceOf[scala.Long]

  @inline override def byteValue(): scala.Byte = longValue().toByte
  @inline override def shortValue(): scala.Short = longValue().toShort
  @inline def intValue(): scala.Int = longValue().toInt
  @inline def floatValue(): scala.Float = longValue().toFloat
  @inline def doubleValue(): scala.Double = longValue().toDouble

  @inline override def equals(that: Any): scala.Boolean = that match {
    case that: Long => longValue() == that.longValue()
    case _          => false
  }

  @inline override def hashCode(): Int =
    Long.hashCode(longValue())

  @inline override def compareTo(that: Long): Int =
    Long.compare(longValue(), that.longValue())

  @inline override def toString(): String =
    Long.toString(longValue())

}

object Long {
  /* TYPE should be a `final val`, but that crashes the JVM back-end, so we
   * use a 'def' instead, which is binary compatible.
   */
  def TYPE: Class[_] = scala.Predef.classOf[scala.Long]

  final val MIN_VALUE = -9223372036854775808L
  final val MAX_VALUE = 9223372036854775807L
  final val SIZE = 64
  final val BYTES = 8

  private final val SignBit = scala.Long.MinValue

  /** Quantities in this class are interpreted as unsigned values. */
  private final class StringRadixInfo(val chunkLength: Int,
      val radixPowLength: Int, val radixPowLengthInverse: scala.Double,
      val paddingZeros: String, val overflowBarrier: scala.Long)

  /** Precomputed table for toUnsignedStringInternalLarge and
   *  parseUnsignedLongInternal.
   */
  private lazy val StringRadixInfos: js.Array[StringRadixInfo] = {
    val r = new js.Array[StringRadixInfo]()

    for (radix <- 0 until Character.MIN_RADIX)
      r.push(null)

    for (radix <- Character.MIN_RADIX to Character.MAX_RADIX) {
      /* Find the biggest chunk size we can use.
       *
       * - radixPowLength should be the biggest exact power of radix that is <= 2^30.
       * - chunkLength is then log_radix(radixPowLength).
       * - paddingZeros is a string with exactly chunkLength '0's.
       * - overflowBarrier is divideUnsigned(-1L, radixPowLength) so that we
       *   can test whether someValue * radixPowLength will overflow.
       *
       * It holds that 2^30 >= radixPowLength > 2^30 / maxRadix = 2^30 / 36 > 2^24.
       *
       * Therefore, (2^64 - 1) / radixPowLength < 2^(64-24) = 2^40, which
       * comfortably fits in a `Double`. `toUnsignedStringLarge` relies on that
       * property.
       *
       * Also, radixPowLength² < 2^63, and radixPowLength³ > 2^64.
       * `parseUnsignedLongInternal` relies on that property.
       */
      val barrier = Integer.divideUnsigned(1 << 30, radix)
      var radixPowLength = radix
      var chunkLength = 1
      var paddingZeros = "0"
      while (radixPowLength <= barrier) {
        radixPowLength *= radix
        chunkLength += 1
        paddingZeros += "0"
      }
      val overflowBarrier = Long.divideUnsigned(-1L, Integer.toUnsignedLong(radixPowLength))
      r.push(new StringRadixInfo(chunkLength, radixPowLength,
          1.0 / radixPowLength.toDouble, paddingZeros, overflowBarrier))
    }

    r
  }

  @inline // because radix is almost certainly constant at call site
  def toString(i: scala.Long, radix: Int): String = {
    if (radix == 10 || Character.isRadixInvalid(radix))
      toString(i)
    else
      toStringImpl(i, radix)
  }

  @inline // because radix is almost certainly constant at call site
  def toUnsignedString(i: scala.Long, radix: Int): String = {
    (radix: @switch) match {
      case 2  => toBinaryString(i)
      case 8  => toOctalString(i)
      case 16 => toHexString(i)
      case _  =>
        val radix1 =
          if (Character.isRadixInvalid(radix)) 10
          else radix
        toUnsignedStringImpl(i, radix1)
    }
  }

  // Intrinsic
  @inline def toString(i: scala.Long): String = "" + i

  @inline def toUnsignedString(i: scala.Long): String =
    toUnsignedStringImpl(i, 10)

  // Must be called only with valid radix
  private def toStringImpl(i: scala.Long, radix: Int): String = {
    import js.JSNumberOps.enableJSNumberOps

    val lo = i.toInt
    val hi = (i >>> 32).toInt

    if (lo >> 31 == hi) {
      // It's a signed int32
      lo.toString(radix)
    } else if (((hi ^ (hi >> 10)) & 0xffe00000) == 0) { // see RuntimeLong.isSignedSafeDouble
      // (lo, hi) is small enough to be a Double, so toDouble is exact
      i.toDouble.toString(radix)
    } else {
      val abs = Math.abs(i)
      val s = toUnsignedStringInternalLarge(abs.toInt, (abs >>> 32).toInt, radix)
      if (hi < 0) "-" + s else s
    }
  }

  // Must be called only with valid radix
  private def toUnsignedStringImpl(i: scala.Long, radix: Int): String = {
    import js.JSNumberOps.enableJSNumberOps

    val lo = i.toInt
    val hi = (i >>> 32).toInt

    if (hi == 0) {
      // It's an unsigned int32
      Integer.toUnsignedDouble(lo).toString(radix)
    } else if ((hi & 0xffe00000) == 0) { // see RuntimeLong.isUnsignedSafeDouble
      // (lo, hi) is small enough to be a Double, so toDouble is exact
      i.toDouble.toString(radix)
    } else {
      toUnsignedStringInternalLarge(lo, hi, radix)
    }
  }

  // Must be called only with valid radix and with (lo, hi) >= 2^53
  @inline // inlined twice: once in toStringImpl and once in toUnsignedStringImpl
  private def toUnsignedStringInternalLarge(lo: Int, hi: Int, radix: Int): String = {
    import js.JSNumberOps.enableJSNumberOps
    import js.JSStringOps.enableJSStringOps

    @inline def unsignedSafeDoubleLo(n: scala.Double): Int = {
      import js.DynamicImplicits.number2dynamic
      (n | 0).asInstanceOf[Int]
    }

    val TwoPow32 = (1L << 32).toDouble

    /* See RuntimeLong.toUnsignedString for a proof. Although that proof is
     * done in terms of a fixed divisor of 10^9, it generalizes to any
     * divisor that statisfies 2^12 < divisor <= 2^30 and
     * ULong.MaxValue / divisor < 2^53, which is true for `radixPowLength`.
     */

    val radixInfo = StringRadixInfos(radix)
    val divisor = radixInfo.radixPowLength
    val divisorInv = radixInfo.radixPowLengthInverse
    val paddingZeros = radixInfo.paddingZeros

    // initial approximation of the quotient and remainder
    val approxNum =
      Integer.toUnsignedDouble(hi) * TwoPow32 + Integer.toUnsignedDouble(lo)
    var approxQuot = Math.floor(approxNum * divisorInv)
    var approxRem = lo - divisor * unsignedSafeDoubleLo(approxQuot)

    // correct the approximations
    if (approxRem < 0) {
      approxQuot -= 1.0
      approxRem += divisor
    } else if (approxRem >= divisor) {
      approxQuot += 1.0
      approxRem -= divisor
    }

    // build the result string
    val remStr = approxRem.toString(radix)
    approxQuot.toString(radix) + paddingZeros.jsSubstring(remStr.length) + remStr
  }

  private def parseLongFail(s: String): Nothing =
    Integer.parseIntFail(s)

  @inline def parseLong(s: String): scala.Long =
    parseLongPlatform(s, 10)

  @inline // because radix is almost certainly constant at call site
  def parseLong(s: String, radix: Int): scala.Long = {
    if (Character.isRadixInvalid(radix))
      parseLongFail(s)
    parseLongPlatform(s, radix)
  }

  // Must be called only with a valid radix
  @inline def parseLongPlatform(s: String, radix: Int): scala.Long = {
    if (LinkingInfo.isWebAssembly)
      parseLongImplWasm(s, radix, divideUnsigned(Int.MinValue, radix.toLong))
    else
      parseLongImplJS(s, radix)
  }

  /* Must be called only with a valid radix.
   *
   * The overflowBarrier must be divideUnsigned(Long.MinValue, radix.toLong).
   */
  @noinline
  private def parseLongImplWasm(s: String, radix: Int,
      overflowBarrier: scala.Long): scala.Long = {
    IntegerLong.parseSignedImpl(s, radix, overflowBarrier)
  }

  // Must be called only with a valid radix
  @noinline
  def parseLongImplJS(s: String, radix: Int): scala.Long = {
    if (s == "")
      parseLongFail(s)

    var start = 0
    var neg = false

    s.charAt(0) match {
      case '+' =>
        start = 1
      case '-' =>
        start = 1
        neg = true
      case _ =>
    }

    val unsignedResult = parseUnsignedLongJSInternal(s, radix, start)

    if (neg) {
      val result = -unsignedResult
      if (result > 0)
        parseLongFail(s)
      result
    } else {
      if (unsignedResult < 0)
        parseLongFail(s)
      unsignedResult
    }
  }

  @inline def parseUnsignedLong(s: String): scala.Long =
    parseUnsignedLongPlatform(s, 10)

  @inline // because radix is almost certainly constant at call site
  def parseUnsignedLong(s: String, radix: Int): scala.Long = {
    if (Character.isRadixInvalid(radix))
      parseLongFail(s)
    parseUnsignedLongPlatform(s, radix)
  }

  // Must be called only with a valid radix
  @inline def parseUnsignedLongPlatform(s: String, radix: Int): scala.Long = {
    if (LinkingInfo.isWebAssembly)
      parseUnsignedLongImplWasm(s, radix, divideUnsigned(-1L, radix.toLong))
    else
      parseUnsignedLongImplJS(s, radix)
  }

  /* Must be called only with a valid radix.
   *
   * The overflowBarrier must be divideUnsigned(-1L, radix.toLong). It will be
   * used to detect overflow during the multiplication.
   */
  @noinline
  private def parseUnsignedLongImplWasm(s: String, radix: Int,
      overflowBarrier: scala.Long): scala.Long = {
    IntegerLong.parseUnsignedImpl(s, radix, overflowBarrier)
  }

  // Must be called only with a valid radix
  @noinline
  def parseUnsignedLongImplJS(s: String, radix: Int): scala.Long = {
    if (s == "")
      parseLongFail(s)

    val start =
      if (s.charAt(0) == '+') 1
      else 0

    parseUnsignedLongJSInternal(s, radix, start)
  }

  // Must be called only with a valid radix
  def parseUnsignedLongJSInternal(s: String, radix: Int, start: Int): scala.Long = {
    val length = s.length

    if (start >= length) {
      parseLongFail(s)
    } else {
      val radixInfo = StringRadixInfos(radix)
      val chunkLen = radixInfo.chunkLength

      /* Skip leading 0's - important because we have an assumption on the
       * number of chunks that are necessary to parse any string.
       */
      var firstChunkStart = start
      while (firstChunkStart < length &&
          Character.isZeroDigit(s.charAt(firstChunkStart))) {
        firstChunkStart += 1
      }

      /* After that, if more than 3 chunks are necessary, it means the value
       * is too large, and does not fit in an unsigned Long.
       */
      if (length - firstChunkStart > 3 * chunkLen)
        parseLongFail(s)

      @noinline def parseChunkAsUInt(chunkStart: Int, chunkEnd: Int): Int = {
        var result = 0 // This is an *unsigned* integer
        var i = chunkStart
        while (i != chunkEnd) {
          val digit = Character.digitWithValidRadix(s.charAt(i), radix)
          if (digit == -1)
            parseLongFail(s)
          result = result * radix + digit // cannot overflow
          i += 1
        }
        result
      }

      @inline def parseChunk(chunkStart: Int, chunkEnd: Int): scala.Long =
        Integer.toUnsignedLong(parseChunkAsUInt(chunkStart, chunkEnd))

      /* The first chunk is sized so that all subsequent chunks are of size
       * chunkLen. Note also that the first chunk cannot overflow.
       * For small strings (length <= MaxLen), this first chunk is all there
       * is.
       */
      val firstChunkLength = ((length - firstChunkStart) - 1) % chunkLen + 1
      val firstChunkEnd = firstChunkStart + firstChunkLength
      val firstResult = parseChunk(firstChunkStart, firstChunkEnd)

      if (firstChunkEnd == length) {
        firstResult
      } else {
        // Second chunk. Still cannot overflow.
        val multiplier = Integer.toUnsignedLong(radixInfo.radixPowLength)
        val secondChunkEnd = firstChunkEnd + chunkLen
        val secondResult =
          firstResult * multiplier + parseChunk(firstChunkEnd, secondChunkEnd)

        if (secondChunkEnd == length) {
          secondResult
        } else {
          // Third and final chunk. This one can overflow
          // Assert: secondChunkEnd + chunkLen == length

          val overflowBarrier = radixInfo.overflowBarrier
          val thirdChunk = parseChunk(secondChunkEnd, length)

          if (secondResult > overflowBarrier) // both positive so signed > is OK
            parseLongFail(s) // * will overflow
          val thirdResult = secondResult * multiplier + thirdChunk
          if (unsigned_<(thirdResult, thirdChunk))
            parseLongFail(s) // + overflowed

          thirdResult
        }
      }
    }
  }

  @inline def `new`(value: scala.Long): Long = valueOf(value)

  @inline def `new`(s: String): Long = valueOf(s)

  @inline def valueOf(l: scala.Long): Long = l.asInstanceOf[Long]

  @inline def valueOf(s: String): Long = valueOf(parseLong(s))

  @inline def valueOf(s: String, radix: Int): Long =
    valueOf(parseLong(s, radix))

  @noinline def decode(nm: String): Long =
    Integer.decodeGeneric(nm, valueOf(_, _))

  @inline def hashCode(value: scala.Long): Int =
    value.toInt ^ (value >>> 32).toInt

  // RuntimeLong intrinsic
  @inline def compare(x: scala.Long, y: scala.Long): scala.Int = {
    if (x == y) 0
    else if (x < y) -1
    else 1
  }

  // TODO RuntimeLong intrinsic?
  @inline def compareUnsigned(x: scala.Long, y: scala.Long): scala.Int = {
    if (x == y) 0
    else if (unsigned_<(x, y)) -1
    else 1
  }

  @inline private[java] def unsigned_<(x: scala.Long, y: scala.Long): scala.Boolean =
    (x ^ SignBit) < (y ^ SignBit)

  @inline private[java] def unsigned_<=(x: scala.Long, y: scala.Long): scala.Boolean =
    (x ^ SignBit) <= (y ^ SignBit)

  @inline private[java] def unsigned_>(x: scala.Long, y: scala.Long): scala.Boolean =
    (x ^ SignBit) > (y ^ SignBit)

  @inline private[java] def unsigned_>=(x: scala.Long, y: scala.Long): scala.Boolean =
    (x ^ SignBit) >= (y ^ SignBit)

  @inline def divideUnsigned(dividend: scala.Long, divisor: scala.Long): scala.Long =
    throw new Error("stub") // body replaced by the compiler back-end

  @inline def remainderUnsigned(dividend: scala.Long, divisor: scala.Long): scala.Long =
    throw new Error("stub") // body replaced by the compiler back-end

  @inline
  def highestOneBit(i: scala.Long): scala.Long = {
    if (LinkingInfo.isWebAssembly) {
      // See Integer.highestOneBit
      ((1L << 63) >> numberOfLeadingZeros(i)) & i
    } else {
      /* With RuntimeLong, the above algorithm results in 3 branches, so we
       * decompose lo and hi.
       */
      val lo = i.toInt
      val hi = (i >>> 32).toInt
      makeLongFromLoHi(
          if (hi != 0) 0 else Integer.highestOneBit(lo),
          Integer.highestOneBit(hi))
    }
  }

  @inline
  def lowestOneBit(i: scala.Long): scala.Long = {
    /* With RuntimeLong, this produces 7 instructions without branches.
     * An algorithm based on separating lo and hi takes 5 instructions plus
     * one branch.
     */
    i & -i
  }

  // Wasm intrinsic
  @inline
  def bitCount(i: scala.Long): scala.Int = {
    val lo = i.toInt
    val hi = (i >>> 32).toInt
    Integer.bitCount(lo) + Integer.bitCount(hi)
  }

  @inline
  def reverseBytes(i: scala.Long): scala.Long = {
    if (LinkingInfo.isWebAssembly) {
      /* We first reverse the 4 blocks of 16 bits, in the same way that
       * Integer.reverseBytes reverses the 4 blocks of 8 bits. Then, we swap
       * all the pairs of 8-bit blocks in parallel.
       *
       * This algorithm uses 10 primitive operations on Longs.
       * The else branch would take 17 instructions mixing Ints and Longs.
       */
      val x1 = rotateRight(i & 0x0000ffff0000ffffL, 16) | (rotateLeft(i, 16) & 0x0000ffff0000ffffL)
      ((x1 >>> 8) & 0x00ff00ff00ff00ffL) | ((x1 & 0x00ff00ff00ff00ffL) << 8)
    } else {
      /* On JS, with RuntimeLong, swapping the ints is free, so nothing beats
       * applying Integer.reverseBytes twice.
       */
      makeLongFromLoHi(
          Integer.reverseBytes((i >>> 32).toInt),
          Integer.reverseBytes(i.toInt))
    }
  }

  @inline
  def reverse(i: scala.Long): scala.Long = {
    if (LinkingInfo.isWebAssembly) {
      // Hacker's Delight, Section 7-1, Figure 7-4
      val swapped = rotateLeft(i, 32)
      val x0 = ((swapped & 0x0001ffff0001ffffL) << 15) | ((swapped & 0xfffe0000fffe0000L) >>> 17)
      val t1 = (x0 ^ (x0 >>> 10)) & 0x003f801f003f801fL
      val x1 = (t1 | (t1 << 10)) ^ x0
      val t2 = (x1 ^ (x1 >>> 4)) & 0x0e0384210e038421L
      val x2 = (t2 | (t2 << 4)) ^ x1
      val t3 = (x2 ^ (x2 >>> 2)) & 0x2248884222488842L
      (t3 | (t3 << 2)) ^ x2
    } else {
      /* On JS, with RuntimeLong, swapping the ints is free, but shifts on
       * Long values by amounts less than 32 require 4 instructions each, so
       * nothing beats applying Integer.reverse twice.
       */
      makeLongFromLoHi(
          Integer.reverse((i >>> 32).toInt),
          Integer.reverse(i.toInt))
    }
  }

  /** Make a `Long` value from its lo and hi 32-bit parts.
   *  When the optimizer is enabled, this operation is free.
   */
  @inline
  private def makeLongFromLoHi(lo: Int, hi: Int): scala.Long =
    (lo.toLong & 0xffffffffL) | (hi.toLong << 32)

  // Wasm intrinsic
  @inline
  def rotateLeft(i: scala.Long, distance: scala.Int): scala.Long =
    (i << distance) | (i >>> -distance)

  // Wasm intrinsic
  @inline
  def rotateRight(i: scala.Long, distance: scala.Int): scala.Long =
    (i >>> distance) | (i << -distance)

  @noinline
  def compress(i: scala.Long, mask: scala.Long): scala.Long =
    IntegerLong.compress(i, mask)

  @noinline
  def expand(i: scala.Long, mask: scala.Long): scala.Long =
    IntegerLong.expand(i, mask)

  @inline
  def signum(i: scala.Long): Int = {
    /* Hacker's Delight, Section 2-8
     * With RuntimeLong, this yields 8 int operations, which seems to be as
     * good as it gets.
     */
    ((i >> 63) | (-i >>> 63)).toInt
  }

  @inline
  def numberOfLeadingZeros(l: scala.Long): Int =
    throw new Error("stub") // body replaced by the compiler back-end

  // Wasm intrinsic
  @inline
  def numberOfTrailingZeros(l: scala.Long): Int = {
    /* Warning to the next adventurer to come here: read the comment in
     * RuntimeLong.clz. There does not appear to a solution better than the
     * naive algorithm below.
     */
    val lo = l.toInt
    if (lo != 0) Integer.numberOfTrailingZeros(lo)
    else Integer.numberOfTrailingZeros((l >>> 32).toInt) + 32
  }

  @inline def toBinaryString(l: scala.Long): String =
    toBinaryString(l.toInt, (l >>> 32).toInt)

  private def toBinaryString(lo: Int, hi: Int): String = {
    val zeros = "00000000000000000000000000000000" // 32 zeros
    @inline def padBinary32(i: Int) = {
      val s = Integer.toBinaryString(i)
      zeros.substring(s.length) + s
    }

    if (hi != 0) Integer.toBinaryString(hi) + padBinary32(lo)
    else Integer.toBinaryString(lo)
  }

  @inline def toHexString(l: scala.Long): String =
    toHexString(l.toInt, (l >>> 32).toInt)

  private def toHexString(lo: Int, hi: Int): String = {
    val zeros = "00000000" // 8 zeros
    @inline def padBinary8(i: Int) = {
      val s = Integer.toHexString(i)
      zeros.substring(s.length) + s
    }

    if (hi != 0) Integer.toHexString(hi) + padBinary8(lo)
    else Integer.toHexString(lo)
  }

  @inline def toOctalString(l: scala.Long): String =
    toOctalString(l.toInt, (l >>> 32).toInt)

  private def toOctalString(lo: Int, hi: Int): String = {
    val zeros = "0000000000" // 10 zeros
    @inline def padOctal10(i: Int) = {
      val s = Integer.toOctalString(i)
      zeros.substring(s.length) + s
    }

    val lp = lo & 0x3fffffff
    val mp = ((lo >>> 30) + (hi << 2)) & 0x3fffffff
    val hp = hi >>> 28

    if (hp != 0) Integer.toOctalString(hp) + padOctal10(mp) + padOctal10(lp)
    else if (mp != 0) Integer.toOctalString(mp) + padOctal10(lp)
    else Integer.toOctalString(lp)
  }

  @inline def sum(a: scala.Long, b: scala.Long): scala.Long =
    a + b

  @inline def max(a: scala.Long, b: scala.Long): scala.Long =
    Math.max(a, b)

  @inline def min(a: scala.Long, b: scala.Long): scala.Long =
    Math.min(a, b)
}
