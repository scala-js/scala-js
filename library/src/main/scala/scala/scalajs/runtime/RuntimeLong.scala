package scala.scalajs.runtime

import scala.annotation.tailrec

import scala.scalajs.js
import js.|
import js.JSNumberOps._
import js.JSStringOps._

/* IMPORTANT NOTICE about this file
 *
 * The code of RuntimeLong is code-size- and performance critical. The methods
 * of this class are used for every single primitive operation on Longs, and
 * must therefore be as fast as they can.
 *
 * This means that this implementation is oriented for performance over
 * readability and idiomatic code. Some examples of idiomatic code that are
 * avoided are:
 *
 * val (x, y) = something
 *
 *   unapply of a tuple is not optimized as well as it should be. The tuple is
 *   stack-allocated alright, but there are more temporary variables than
 *   necessary. Instead we use:
 *
 *   val t = something
 *   val x = t._1
 *   val y = t._2
 *
 * val pair = if (...) (x1, y1) else (x2, y2)
 *
 *   Multi-path merging of (specialized) tuples creates more code and more
 *   temporary variables than necessary.
 *
 * DRY is applied as much as possible but is bounded by the performance and
 * code size requirements. We use a lot of inline_xyz helpers meant to be used
 * when we already have the parameters on stack, but they are generally
 * duplicated for entry points.
 *
 * Also, we typically extract the lo and hi fields from the heap into local
 * variables once, then pass them around as parameters to inlineable methods.
 * This reduces heap accesses, and allows the JIT to know that we indeed always
 * have the same value (it does not know that fields are immutable).
 */

/** Emulates a Long on the JavaScript platform. */
final class RuntimeLong(val lo: Int, val hi: Int)
    extends java.lang.Number with java.io.Serializable
    with java.lang.Comparable[java.lang.Long] { a =>

  import RuntimeLong._
  import Utils._

  /** Constructs a Long from an Int. */
  def this(value: Int) = this(value, value >> 31)

  // Binary compatibility for the old (l, m, h) encoding

  @deprecated("Use the constructor with (lo, hi) instead.", "0.6.6")
  def this(l: Int, m: Int, h: Int) =
    this(l | (m << 22), (m >> 10) | (h << 12))

  @deprecated("Use lo and hi instead.", "0.6.6")
  def l: Int = lo & ((1 << 22) - 1)

  @deprecated("Use lo and hi instead.", "0.6.6")
  def m: Int = (lo >>> 22) & ((hi & ((1 << 12) - 1)) << 10)

  @deprecated("Use lo and hi instead.", "0.6.6")
  def h: Int = hi >>> 12

  // Universal equality

  override def equals(that: Any): Boolean = that match {
    case b: RuntimeLong => inline_equals(b)
    case _              => false
  }

  override def hashCode(): Int =
    lo ^ hi

  // String operations

  override def toString(): String = {
    val lo = this.lo
    val hi = this.hi

    if (isInt32(lo, hi)) {
      lo.toString()
    } else if (hi < 0) {
      val (absLo, absHi) = inline_unary_-(lo, hi)
      "-" + toUnsignedString(absLo, absHi)
    } else {
      toUnsignedString(lo, hi)
    }
  }

  private def toUnsignedString(lo: Int, hi: Int): String = {
    // This is called only if (lo, hi) is not an Int32

    if (isUnsignedSafeDouble(hi)) {
      // (lo, hi) is small enough to be a Double, use that directly
      asUnsignedSafeDouble(lo, hi).toString
    } else {
      /* We divide (lo, hi) once by 10^9 and keep the remainder.
       *
       * The remainder must then be < 10^9, and is therefore an int32.
       *
       * The quotient must be <= ULong.MaxValue / 10^9, which is < 2^53, and
       * is therefore a valid double. It must also be non-zero, since we tested
       * previously for cases where (lo, hi) < 2^53, but 2^10 is itself < 2^53.
       */
      val TenPow9Lo = 1000000000L.toInt
      val TenPow9Hi = (1000000000L >>> 32).toInt

      val quotRem = unsignedDivModHelper(lo, hi, TenPow9Lo, TenPow9Hi,
          AskBoth).asInstanceOf[js.Tuple4[Int, Int, Int, Int]]
      val quotLo = quotRem._1
      val quotHi = quotRem._2
      val rem = quotRem._3 // remHi must be 0 by construction

      val quot = asUnsignedSafeDouble(quotLo, quotHi)

      val remStr = rem.toString
      quot.toString + "000000000".jsSubstring(remStr.length) + remStr
    }
  }

  // Conversions

  def toByte: Byte = lo.toByte
  def toShort: Short = lo.toShort
  def toChar: Char = lo.toChar
  def toInt: Int = lo
  def toLong: Long = this.asInstanceOf[Long]
  def toFloat: Float = toDouble.toFloat

  def toDouble: Double = {
    val lo = this.lo
    val hi = this.hi

    if (hi < 0) {
      val (abslo, abshi) = inline_unary_-(lo, hi)
      -(abshi.toUint * TwoPow32 + abslo.toUint) // abshi.toUint for MinValue
    } else {
      hi * TwoPow32 + lo.toUint
    }
  }

  // java.lang.Number

  override def byteValue(): Byte = toByte
  override def shortValue(): Short = toShort
  def intValue(): Int = toInt
  def longValue(): Long = toLong
  def floatValue(): Float = toFloat
  def doubleValue(): Double = toDouble

  // Comparisons and java.lang.Comparable interface

  def compareTo(b: RuntimeLong): Int = {
    val ahi = a.hi
    val bhi = b.hi
    if (ahi == bhi) {
      val alo = a.lo
      val blo = b.lo
      if (alo == blo) 0
      else if (inlineUnsignedInt_<(alo, blo)) -1
      else 1
    } else {
      if (ahi < bhi) -1
      else 1
    }
  }

  def compareTo(that: java.lang.Long): Int =
    compareTo(that.asInstanceOf[RuntimeLong])

  @inline
  private def inline_equals(b: RuntimeLong): Boolean =
    a.lo == b.lo && a.hi == b.hi

  def equals(b: RuntimeLong): Boolean =
    inline_equals(b)

  def notEquals(b: RuntimeLong): Boolean =
    !inline_equals(b)

  def <(b: RuntimeLong): Boolean = {
    val ahi = a.hi
    val bhi = b.hi
    if (ahi == bhi) inlineUnsignedInt_<(a.lo, b.lo)
    else ahi < bhi
  }

  def <=(b: RuntimeLong): Boolean = {
    val ahi = a.hi
    val bhi = b.hi
    if (ahi == bhi) inlineUnsignedInt_<=(a.lo, b.lo)
    else ahi < bhi
  }

  def >(b: RuntimeLong): Boolean = {
    /* Work around https://code.google.com/p/v8/issues/detail?id=3304
     * 0x7fffffff > 0x80000000 is broken, so use < instead.
     * This happens when comparing MaxValue to MinValue.
     */
    val ahi = a.hi
    val bhi = b.hi
    if (ahi == bhi) inlineUnsignedInt_>(a.lo, b.lo)
    else bhi < ahi // workaround here
  }

  def >=(b: RuntimeLong): Boolean = {
    /* Work around https://code.google.com/p/v8/issues/detail?id=3304
     * 0x7fffffff > 0x80000000 is broken, so use < instead.
     * This happens when comparing MaxValue to MinValue.
     */
    val ahi = a.hi
    val bhi = b.hi
    if (ahi == bhi) inlineUnsignedInt_>=(a.lo, b.lo)
    else bhi < ahi // workaround here
  }

  // Bitwise operations

  def unary_~ : RuntimeLong = // scalastyle:ignore
    new RuntimeLong(~lo, ~hi)

  def |(b: RuntimeLong): RuntimeLong =
    new RuntimeLong(a.lo | b.lo, a.hi | b.hi)

  def &(b: RuntimeLong): RuntimeLong =
    new RuntimeLong(a.lo & b.lo, a.hi & b.hi)

  def ^(b: RuntimeLong): RuntimeLong =
    new RuntimeLong(a.lo ^ b.lo, a.hi ^ b.hi)

  // Shifts

  /** Shift left */
  def <<(n0: Int): RuntimeLong = {
    val n = n0 & 63
    val lo = this.lo

    if (n == 0) this
    else if (n < 32) new RuntimeLong(lo << n, (lo >>> -n) | (hi << n))
    else new RuntimeLong(0, lo << n)
  }

  @inline
  private def inline_<<(lo: Int, hi: Int, n: Int): (Int, Int) = {
    if (n == 0) (lo, hi)
    else if (n < 32) (lo << n, (lo >>> -n) | (hi << n))
    else (0, lo << n)
  }

  /** Logical shift right */
  def >>>(n0: Int): RuntimeLong = {
    val n = n0 & 63
    val hi = this.hi

    if (n == 0) this
    else if (n < 32) new RuntimeLong((lo >>> n) | (hi << -n), hi >>> n)
    else new RuntimeLong(hi >>> n, 0)
  }

  @inline
  private def inline_>>>(lo: Int, hi: Int, n: Int): (Int, Int) = {
    if (n == 0) (lo, hi)
    else if (n < 32) ((lo >>> n) | (hi << -n), hi >>> n)
    else (hi >>> n, 0)
  }

  /** Arithmetic shift right */
  def >>(n0: Int): RuntimeLong = {
    val n = n0 & 63
    val hi = this.hi

    if (n == 0) this
    else if (n < 32) new RuntimeLong((lo >>> n) | (hi << -n), hi >> n)
    else new RuntimeLong(hi >> n, hi >> 31)
  }

  // Arithmetic operations

  def unary_- : RuntimeLong = // scalastyle:ignore
    inlineLongUnary_-(lo, hi)

  @inline
  private def inline_abs(lo: Int, hi: Int): (Boolean, Int, Int) = {
    val neg = hi < 0
    var absLo = lo
    var absHi = hi
    if (neg) {
      absLo = -lo
      absHi = if (lo != 0) ~hi else -hi
    }
    (neg, absLo, absHi)
  }

  def +(b: RuntimeLong): RuntimeLong = {
    val result = inline_+(a.lo, a.hi, b.lo, b.hi)
    new RuntimeLong(result._1, result._2)
  }

  @inline
  private def inline_+(alo: Int, ahi: Int, blo: Int, bhi: Int): (Int, Int) = {
    val lo = alo + blo
    (lo, ahi + bhi + (if (inlineUnsignedInt_<(lo, alo)) 1 else 0))
  }

  def -(b: RuntimeLong): RuntimeLong = {
    val result = inline_-(a.lo, a.hi, b.lo, b.hi)
    new RuntimeLong(result._1, result._2)
  }

  @inline
  private def inline_-(alo: Int, ahi: Int, blo: Int, bhi: Int): (Int, Int) = {
    val lo = alo - blo
    (lo, ahi - bhi + (if (inlineUnsignedInt_>(lo, alo)) -1 else 0))
  }

  def *(b: RuntimeLong): RuntimeLong = {
    val alo = a.lo
    val ahi = a.hi
    val blo = b.lo
    val bhi = b.hi

    val a0 = alo & 0xffff
    val a1 = alo >>> 16
    val a2 = ahi & 0xffff
    val a3 = ahi >>> 16
    val b0 = blo & 0xffff
    val b1 = blo >>> 16
    val b2 = bhi & 0xffff
    val b3 = bhi >>> 16

    var c0 = a0 * b0

    var c1 = c0 >>> 16
    c1 = c1 + a1 * b0

    var c2 = c1 >>> 16
    c1 = (c1 & 0xffff) + a0 * b1
    c2 = c2 + (c1 >>> 16)

    var c3 = c2 >>> 16
    c2 = (c2 & 0xffff) + a2 * b0
    c3 = c3 + (c2 >>> 16)
    c2 = (c2 & 0xffff) + a1 * b1
    c3 = c3 + (c2 >>> 16)
    c2 = (c2 & 0xffff) + a0 * b2
    c3 = c3 + (c2 >>> 16)
    c3 = c3 + a3 * b0 + a2 * b1 + a1 * b2 + a0 * b3

    new RuntimeLong(
        (c0 & 0xffff) | (c1 << 16),
        (c2 & 0xffff) | (c3 << 16))
  }

  def /(b: RuntimeLong): RuntimeLong = {
    val alo = a.lo
    val ahi = a.hi
    val blo = b.lo
    val bhi = b.hi

    if (isZero(blo, bhi))
      throw new ArithmeticException("/ by zero")

    if (isInt32(alo, ahi)) {
      if (isInt32(blo, bhi)) {
        if (alo == Int.MinValue && blo == -1) new RuntimeLong(Int.MinValue, 0)
        else new RuntimeLong(alo / blo)
      } else {
        // Either a == Int.MinValue && b == (Int.MaxValue + 1), or (abs(b) > abs(a))
        if (alo == Int.MinValue && (blo == 0x80000000 && bhi == 0)) MinusOne
        else Zero // because abs(b) > abs(a)
      }
    } else {
      val (aNeg, aAbsLo, aAbsHi) = inline_abs(alo, ahi)
      val (bNeg, bAbsLo, bAbsHi) = inline_abs(blo, bhi)
      val absR = unsigned_/(aAbsLo, aAbsHi, bAbsLo, bAbsHi)
      if (aNeg == bNeg) absR
      else inlineLongUnary_-(absR.lo, absR.hi)
    }
  }

  /** `java.lang.Long.divideUnsigned(a, b)` */
  def divideUnsigned(b: RuntimeLong): RuntimeLong = {
    val alo = a.lo
    val ahi = a.hi
    val blo = b.lo
    val bhi = b.hi

    if (isZero(blo, bhi))
      throw new ArithmeticException("/ by zero")

    if (isUInt32(ahi)) {
      if (isUInt32(bhi)) {
        // Integer.divideUnsigned(alo, blo), inaccessible when compiling on JDK < 8
        new RuntimeLong(rawToInt(alo.toUint / blo.toUint), 0)
      } else {
        // a < b
        Zero
      }
    } else {
      unsigned_/(alo, ahi, blo, bhi)
    }
  }

  private def unsigned_/(alo: Int, ahi: Int, blo: Int, bhi: Int): RuntimeLong = {
    // This method is not called if isInt32(alo, ahi) nor if isZero(blo, bhi)
    if (isUnsignedSafeDouble(ahi)) {
      if (isUnsignedSafeDouble(bhi)) {
        val aDouble = asUnsignedSafeDouble(alo, ahi)
        val bDouble = asUnsignedSafeDouble(blo, bhi)
        val rDouble = aDouble / bDouble
        fromUnsignedSafeDouble(rDouble)
      } else {
        Zero // because b > a
      }
    } else {
      if (bhi == 0 && isPowerOfTwo_IKnowItsNot0(blo)) {
        val pow = log2OfPowerOfTwo(blo)
        if (pow == 0) new RuntimeLong(alo, ahi)
        else new RuntimeLong((alo >>> pow) | (ahi << -pow), ahi >>> pow)
      } else if (blo == 0 && isPowerOfTwo_IKnowItsNot0(bhi)) {
        val pow = log2OfPowerOfTwo(bhi)
        new RuntimeLong(ahi >>> pow, 0)
      } else {
        unsignedDivModHelper(alo, ahi, blo, bhi,
            AskQuotient).asInstanceOf[RuntimeLong]
      }
    }
  }

  def %(b: RuntimeLong): RuntimeLong = {
    val alo = a.lo
    val ahi = a.hi
    val blo = b.lo
    val bhi = b.hi

    if (isZero(blo, bhi))
      throw new ArithmeticException("/ by zero")

    if (isInt32(alo, ahi)) {
      if (isInt32(blo, bhi)) {
        if (blo != -1) new RuntimeLong(alo % blo)
        else Zero // Work around https://github.com/ariya/phantomjs/issues/12198
      } else {
        // Either a == Int.MinValue && b == (Int.MaxValue + 1), or (abs(b) > abs(a))
        if (alo == Int.MinValue && (blo == 0x80000000 && bhi == 0)) Zero
        else a // because abs(b) > abs(a)
      }
    } else {
      val (aNeg, aAbsLo, aAbsHi) = inline_abs(alo, ahi)
      val (_, bAbsLo, bAbsHi) = inline_abs(blo, bhi)
      val absR = unsigned_%(aAbsLo, aAbsHi, bAbsLo, bAbsHi)
      if (aNeg) inlineLongUnary_-(absR.lo, absR.hi)
      else absR
    }
  }

  /** `java.lang.Long.divideUnsigned(a, b)` */
  def remainderUnsigned(b: RuntimeLong): RuntimeLong = {
    val alo = a.lo
    val ahi = a.hi
    val blo = b.lo
    val bhi = b.hi

    if (isZero(blo, bhi))
      throw new ArithmeticException("/ by zero")

    if (isUInt32(ahi)) {
      if (isUInt32(bhi)) {
        // Integer.remainderUnsigned(alo, blo), inaccessible when compiling on JDK < 8
        new RuntimeLong(rawToInt(alo.toUint % blo.toUint), 0)
      } else {
        // a < b
        a
      }
    } else {
      unsigned_%(alo, ahi, blo, bhi)
    }
  }

  private def unsigned_%(alo: Int, ahi: Int, blo: Int, bhi: Int): RuntimeLong = {
    // This method is not called if isInt32(alo, ahi) nor if isZero(blo, bhi)
    if (isUnsignedSafeDouble(ahi)) {
      if (isUnsignedSafeDouble(bhi)) {
        val aDouble = asUnsignedSafeDouble(alo, ahi)
        val bDouble = asUnsignedSafeDouble(blo, bhi)
        val rDouble = aDouble % bDouble
        fromUnsignedSafeDouble(rDouble)
      } else {
        new RuntimeLong(alo, ahi) // because b > a
      }
    } else {
      if (bhi == 0 && isPowerOfTwo_IKnowItsNot0(blo)) {
        new RuntimeLong(alo & (blo - 1), 0)
      } else if (blo == 0 && isPowerOfTwo_IKnowItsNot0(bhi)) {
        new RuntimeLong(alo, ahi & (bhi - 1))
      } else {
        unsignedDivModHelper(alo, ahi, blo, bhi,
            AskRemainder).asInstanceOf[RuntimeLong]
      }
    }
  }

  private def unsignedDivModHelper(alo: Int, ahi: Int, blo: Int, bhi: Int,
      ask: Int): RuntimeLong | js.Tuple4[Int, Int, Int, Int] = {

    var shift =
      inlineNumberOfLeadingZeros(blo, bhi) - inlineNumberOfLeadingZeros(alo, ahi)
    val initialBShift = inline_<<(blo, bhi, shift)
    var bShiftLo = initialBShift._1
    var bShiftHi = initialBShift._2
    var remLo = alo
    var remHi = ahi
    var quotLo = 0
    var quotHi = 0

    /* Invariants:
     *   bShift == b << shift == b * 2^shift
     *   quot >= 0
     *   0 <= rem < 2 * bShift
     *   quot * b + rem == a
     *
     * The loop condition should be
     *   while (shift >= 0 && isUnsignedSafeDouble(remHi))
     * but we manually inline isUnsignedSafeDouble because remHi is a var. If
     * we let the optimizer inline it, it will first store remHi in a temporary
     * val, which will explose the while condition as a while(true) + if +
     * break, and we don't want that.
     */
    while (shift >= 0 && (remHi & UnsignedSafeDoubleHiMask) != 0) {
      if (inlineUnsigned_>=(remLo, remHi, bShiftLo, bShiftHi)) {
        val newRem = inline_-(remLo, remHi, bShiftLo, bShiftHi)
        remLo = newRem._1
        remHi = newRem._2
        if (shift < 32)
          quotLo |= (1 << shift)
        else
          quotHi |= (1 << shift) // == (1 << (shift - 32))
      }
      shift -= 1
      val newBShift = inline_>>>(bShiftLo, bShiftHi, 1)
      bShiftLo = newBShift._1
      bShiftHi = newBShift._2
    }

    // Now rem < 2^53, we can finish with a double division
    if (inlineUnsigned_>=(remLo, remHi, blo, bhi)) {
      val remDouble = asUnsignedSafeDouble(remLo, remHi)
      val bDouble = asUnsignedSafeDouble(blo, bhi)

      if (ask != AskRemainder) {
        val rem_div_bDouble = remDouble / bDouble
        val newQuot = inline_+(quotLo, quotHi,
            unsignedSafeDoubleLo(rem_div_bDouble),
            unsignedSafeDoubleHi(rem_div_bDouble))
        quotLo = newQuot._1
        quotHi = newQuot._2
      }

      if (ask != AskQuotient) {
        val rem_mod_bDouble = remDouble % bDouble
        remLo = unsignedSafeDoubleLo(rem_mod_bDouble)
        remHi = unsignedSafeDoubleHi(rem_mod_bDouble)
      }
    }

    if (ask == AskQuotient) new RuntimeLong(quotLo, quotHi)
    else if (ask == AskRemainder) new RuntimeLong(remLo, remHi)
    else js.Tuple4(quotLo, quotHi, remLo, remHi)
  }

  // Support for intrinsics

  def toBinaryString: String = {
    val zeros = "00000000000000000000000000000000" // 32 zeros
    @inline def padBinary32(i: Int) = {
      val s = Integer.toBinaryString(i)
      zeros.substring(s.length) + s
    }

    val lo = this.lo
    val hi = this.hi

    if (hi != 0) Integer.toBinaryString(hi) + padBinary32(lo)
    else Integer.toBinaryString(lo)
  }

  def toHexString: String = {
    val zeros = "00000000" // 8 zeros
    @inline def padHex8(i: Int) = {
      val s = Integer.toHexString(i)
      zeros.substring(s.length) + s
    }

    val lo = this.lo
    val hi = this.hi

    if (hi != 0) Integer.toHexString(hi) + padHex8(lo)
    else Integer.toHexString(lo)
  }

  def toOctalString: String = {
    val zeros = "0000000000" // 10 zeros
    @inline def padOctal10(i: Int) = {
      val s = Integer.toOctalString(i)
      zeros.substring(s.length) + s
    }

    val lo = this.lo
    val hi = this.hi

    val lp = lo & 0x3fffffff
    val mp = ((lo >>> 30) + (hi << 2)) & 0x3fffffff
    val hp = hi >>> 28

    if (hp != 0) Integer.toOctalString(hp) + padOctal10(mp) + padOctal10(lp)
    else if (mp != 0) Integer.toOctalString(mp) + padOctal10(lp)
    else Integer.toOctalString(lp)
  }

  def bitCount: Int =
    Integer.bitCount(lo) + Integer.bitCount(hi)

  def signum: RuntimeLong = {
    val hi = this.hi
    if (hi < 0) MinusOne
    else if (isZero(lo, hi)) Zero
    else One
  }

  def numberOfLeadingZeros: Int = {
    val hi = this.hi
    if (hi != 0) Integer.numberOfLeadingZeros(hi)
    else Integer.numberOfLeadingZeros(lo) + 32
  }

  def numberOfTrailingZeros: Int = {
    val lo = this.lo
    if (lo != 0) Integer.numberOfTrailingZeros(lo)
    else Integer.numberOfTrailingZeros(hi) + 32
  }

  // TODO Remove those. There are remnant of before we had LongReflectiveCall

  @deprecated("Just use `this` instead.", "0.6.6")
  def unary_+ : RuntimeLong = this // scalastyle:ignore

  @deprecated("Use `this.toString + y` instead.", "0.6.6")
  def +(y: String): String = this.toString + y

}

object RuntimeLong {
  private final val TwoPow32 = 4294967296.0
  private final val TwoPow53 = 9223372036854775808.0

  /** The magical mask that allows to test whether an unsigned long is a safe
   *  double.
   *  @see Utils.isUnsignedSafeDouble
   */
  private final val UnsignedSafeDoubleHiMask = 0xffe00000

  private final val AskQuotient = 0
  private final val AskRemainder = 1
  private final val AskBoth = 2

  // Cache the instances for some "literals" used in this implementation
  val Zero = new RuntimeLong(0, 0)
  val One = new RuntimeLong(1, 0)
  val MinusOne = new RuntimeLong(-1, -1)
  val MinValue = new RuntimeLong(0, 0x80000000)
  val MaxValue = new RuntimeLong(0xffffffff, 0x7fffffff)

  def fromDouble(value: Double): RuntimeLong = {
    import Utils._

    if (value.isNaN) {
      Zero
    } else if (value < -TwoPow53) {
      MinValue
    } else if (value >= TwoPow53) {
      MaxValue
    } else {
      val neg = value < 0
      val absValue = if (neg) -value else value
      val lo = rawToInt(absValue)
      val hi = rawToInt(absValue / TwoPow32)
      if (neg) inlineLongUnary_-(lo, hi)
      else new RuntimeLong(lo, hi)
    }
  }

  // In a different object so they can be inlined without cost
  private object Utils {
    /** Tests whether the long (lo, hi) is 0. */
    @inline def isZero(lo: Int, hi: Int): Boolean =
      (lo | hi) == 0

    /** Tests whether the long (lo, hi)'s mathematic value fits in a signed Int. */
    @inline def isInt32(lo: Int, hi: Int): Boolean =
      hi == (lo >> 31)

    /** Tests whether the long (_, hi)'s mathematic value fits in an unsigned Int. */
    @inline def isUInt32(hi: Int): Boolean =
      hi == 0

    /** Tests whether an unsigned long (lo, hi) is a safe Double.
     *  This test is in fact slightly stricter than necessary, as it tests
     *  whether `x < 2^53`, although x == 2^53 would be a perfectly safe
     *  Double. The reason we do this is that testing `x <= 2^53` is much
     *  slower, as `x == 2^53` basically has to be treated specially.
     *  Since there is virtually no gain to treating 2^53 itself as a safe
     *  Double, compared to all numbers smaller than it, we don't bother, and
     *  stay on the fast side.
     */
    @inline def isUnsignedSafeDouble(hi: Int): Boolean =
      (hi & UnsignedSafeDoubleHiMask) == 0

    /** Converts an unsigned safe double into its Double representation. */
    @inline def asUnsignedSafeDouble(lo: Int, hi: Int): Double =
      hi * TwoPow32 + lo.toUint

    /** Converts an unsigned safe double into its RuntimeLong representation. */
    @inline def fromUnsignedSafeDouble(x: Double): RuntimeLong =
      new RuntimeLong(unsignedSafeDoubleLo(x), unsignedSafeDoubleHi(x))

    /** Computes the lo part of a long from an unsigned safe double. */
    @inline def unsignedSafeDoubleLo(x: Double): Int =
      rawToInt(x)

    /** Computes the hi part of a long from an unsigned safe double. */
    @inline def unsignedSafeDoubleHi(x: Double): Int =
      rawToInt(x / TwoPow32)

    /** Performs the JavaScript operation `(x | 0)`. */
    @inline def rawToInt(x: Double): Int =
      (x.asInstanceOf[js.Dynamic] | 0.asInstanceOf[js.Dynamic]).asInstanceOf[Int]

    /** Tests whether the given non-zero unsigned Int is an exact power of 2. */
    @inline def isPowerOfTwo_IKnowItsNot0(i: Int): Boolean =
      (i & (i - 1)) == 0

    /** Returns the log2 of the given unsigned Int assuming it is an exact power of 2. */
    @inline def log2OfPowerOfTwo(i: Int): Int =
      31 - Integer.numberOfLeadingZeros(i)

    /** Returns the number of leading zeros in the given long (lo, hi). */
    @inline def inlineNumberOfLeadingZeros(lo: Int, hi: Int): Int =
      if (hi != 0) Integer.numberOfLeadingZeros(hi)
      else Integer.numberOfLeadingZeros(lo) + 32

    /** Tests whether the unsigned long (alo, ahi) is >= (blo, bhi). */
    @inline
    def inlineUnsigned_>=(alo: Int, ahi: Int, blo: Int, bhi: Int): Boolean =
      if (ahi == bhi) inlineUnsignedInt_>=(alo, blo)
      else inlineUnsignedInt_>=(ahi, bhi)

    @inline
    def inlineUnsignedInt_<(a: Int, b: Int): Boolean =
      (a ^ 0x80000000) < (b ^ 0x80000000)

    @inline
    def inlineUnsignedInt_<=(a: Int, b: Int): Boolean = {
      /* Work around https://code.google.com/p/v8/issues/detail?id=3304
       * 0x7fffffff <= 0x80000000, so use >= here instead.
       * This case is common because it happens when a == -1 and b == 0.
       */
      (b ^ 0x80000000) >= (a ^ 0x80000000)
    }

    @inline
    def inlineUnsignedInt_>(a: Int, b: Int): Boolean = {
      /* Work around https://code.google.com/p/v8/issues/detail?id=3304
       * 0x7fffffff > 0x80000000, so use < here instead.
       * This case is common because it happens when a == -1 and b == 0.
       */
      (b ^ 0x80000000) < (a ^ 0x80000000)
    }

    @inline
    def inlineUnsignedInt_>=(a: Int, b: Int): Boolean =
      (a ^ 0x80000000) >= (b ^ 0x80000000)

    @inline
    def inlineLongUnary_-(lo: Int, hi: Int): RuntimeLong =
      new RuntimeLong(-lo, if (lo != 0) ~hi else -hi)

    @inline
    def inline_unary_-(lo: Int, hi: Int): (Int, Int) =
      (-lo, if (lo != 0) ~hi else -hi)
  }

}
