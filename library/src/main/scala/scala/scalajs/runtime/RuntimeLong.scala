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
 * readability and idiomatic code.
 *
 * DRY is applied as much as possible but is bounded by the performance and
 * code size requirements. We use a lot of inline_xyz helpers meant to be used
 * when we already have the parameters on stack, but they are sometimes
 * duplicated in entry points to avoid the explicit extraction of heap fields
 * into temporary variables when they are used only once.
 *
 * Otherwise, we typically extract the lo and hi fields from the heap into
 * local variables once, whether explicitly in vals or implicitly when passed
 * as arguments to inlineable methods. This reduces heap/record accesses, and
 * allows both our optimizer and the JIT to know that we indeed always have the
 * same value (the JIT does not even know that fields are immutable, but even
 * our optimizer does not make use of that information).
 */

/** Emulates a Long on the JavaScript platform. */
@inline
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

  @inline
  override def equals(that: Any): Boolean = that match {
    case b: RuntimeLong => inline_equals(b)
    case _              => false
  }

  @inline override def hashCode(): Int = lo ^ hi

  // String operations

  @inline override def toString(): String =
    RuntimeLong.toString(lo, hi)

  // Conversions

  @inline def toByte: Byte = lo.toByte
  @inline def toShort: Short = lo.toShort
  @inline def toChar: Char = lo.toChar
  @inline def toInt: Int = lo
  @inline def toLong: Long = this.asInstanceOf[Long]
  @inline def toFloat: Float = toDouble.toFloat
  @inline def toDouble: Double = RuntimeLong.toDouble(lo, hi)

  // java.lang.Number

  @inline override def byteValue(): Byte = toByte
  @inline override def shortValue(): Short = toShort
  @inline def intValue(): Int = toInt
  @inline def longValue(): Long = toLong
  @inline def floatValue(): Float = toFloat
  @inline def doubleValue(): Double = toDouble

  // Comparisons and java.lang.Comparable interface

  @inline
  def compareTo(b: RuntimeLong): Int =
    RuntimeLong.compare(a.lo, a.hi, b.lo, b.hi)

  @inline
  def compareTo(that: java.lang.Long): Int =
    compareTo(that.asInstanceOf[RuntimeLong])

  @inline
  private def inline_equals(b: RuntimeLong): Boolean =
    a.lo == b.lo && a.hi == b.hi

  @inline
  def equals(b: RuntimeLong): Boolean =
    inline_equals(b)

  @inline
  def notEquals(b: RuntimeLong): Boolean =
    !inline_equals(b)

  @inline
  def <(b: RuntimeLong): Boolean = {
    /* We should use `inlineUnsignedInt_<(a.lo, b.lo)`, but that first extracts
     * a.lo and b.lo into local variables, which cause the if/else not to be
     * a valid JavaScript expression anymore. This causes useless explosion of
     * JavaScript code at call site, when inlined. So we manually inline
     * `inlineUnsignedInt_<(a.lo, b.lo)` to avoid that problem.
     */
    val ahi = a.hi
    val bhi = b.hi
    if (ahi == bhi) (a.lo ^ 0x80000000) < (b.lo ^ 0x80000000)
    else ahi < bhi
  }

  @inline
  def <=(b: RuntimeLong): Boolean = {
    /* Manually inline `inlineUnsignedInt_<=(a.lo, b.lo)`.
     * See the comment in `<` for the rationale.
     */
    val ahi = a.hi
    val bhi = b.hi
    if (ahi == bhi) (a.lo ^ 0x80000000) <= (b.lo ^ 0x80000000)
    else ahi < bhi
  }

  @inline
  def >(b: RuntimeLong): Boolean = {
    /* Manually inline `inlineUnsignedInt_>a.lo, b.lo)`.
     * See the comment in `<` for the rationale.
     */
    val ahi = a.hi
    val bhi = b.hi
    if (ahi == bhi) (a.lo ^ 0x80000000) > (b.lo ^ 0x80000000)
    else ahi > bhi
  }

  @inline
  def >=(b: RuntimeLong): Boolean = {
    /* Manually inline `inlineUnsignedInt_>=(a.lo, b.lo)`.
     * See the comment in `<` for the rationale.
     */
    val ahi = a.hi
    val bhi = b.hi
    if (ahi == bhi) (a.lo ^ 0x80000000) >= (b.lo ^ 0x80000000)
    else ahi > bhi
  }

  // Bitwise operations

  @inline
  def unary_~ : RuntimeLong = // scalastyle:ignore
    new RuntimeLong(~lo, ~hi)

  @inline
  def |(b: RuntimeLong): RuntimeLong =
    new RuntimeLong(a.lo | b.lo, a.hi | b.hi)

  @inline
  def &(b: RuntimeLong): RuntimeLong =
    new RuntimeLong(a.lo & b.lo, a.hi & b.hi)

  @inline
  def ^(b: RuntimeLong): RuntimeLong =
    new RuntimeLong(a.lo ^ b.lo, a.hi ^ b.hi)

  // Shifts

  /** Shift left */
  @inline
  def <<(n0: Int): RuntimeLong = {
    val n = n0 & 63
    val lo = this.lo

    if (n < 32)
      new RuntimeLong(lo << n, if (n == 0) hi else (lo >>> -n) | (hi << n))
    else
      new RuntimeLong(0, lo << n)
  }

  /** Logical shift right */
  @inline
  def >>>(n0: Int): RuntimeLong = {
    val n = n0 & 63
    val hi = this.hi

    if (n < 32)
      new RuntimeLong(if (n == 0) lo else (lo >>> n) | (hi << -n), hi >>> n)
    else
      new RuntimeLong(hi >>> n, 0)
  }

  /** Arithmetic shift right */
  @inline
  def >>(n0: Int): RuntimeLong = {
    val n = n0 & 63
    val hi = this.hi

    if (n < 32)
      new RuntimeLong(if (n == 0) lo else (lo >>> n) | (hi << -n), hi >> n)
    else
      new RuntimeLong(hi >> n, hi >> 31)
  }

  // Arithmetic operations

  @inline
  def unary_- : RuntimeLong = { // scalastyle:ignore
    val lo = this.lo
    val hi = this.hi
    new RuntimeLong(inline_lo_unary_-(lo), inline_hi_unary_-(lo, hi))
  }

  @inline
  def +(b: RuntimeLong): RuntimeLong =
    inline_+(a.lo, a.hi, b.lo, b.hi)

  @inline
  def -(b: RuntimeLong): RuntimeLong =
    inline_-(a.lo, a.hi, b.lo, b.hi)

  @inline
  def *(b: RuntimeLong): RuntimeLong = {
    val alo = a.lo
    val blo = b.lo
    new RuntimeLong(alo * blo, RuntimeLong.timesHi(alo, a.hi, blo, b.hi))
  }

  @inline
  def /(b: RuntimeLong): RuntimeLong =
    RuntimeLong.divide(a, b)

  /** `java.lang.Long.divideUnsigned(a, b)` */
  @inline
  def divideUnsigned(b: RuntimeLong): RuntimeLong =
    RuntimeLong.divideUnsigned(a, b)

  @inline
  def %(b: RuntimeLong): RuntimeLong =
    RuntimeLong.remainder(a, b)

  /** `java.lang.Long.remainderUnsigned(a, b)` */
  @inline
  def remainderUnsigned(b: RuntimeLong): RuntimeLong =
    RuntimeLong.remainderUnsigned(a, b)

  // TODO Remove these. They were support for intrinsics before 0.6.6.

  @deprecated("Use java.lang.Long.toBinaryString instead.", "0.6.6")
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

  @deprecated("Use java.lang.Long.toHexString instead.", "0.6.6")
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

  @deprecated("Use java.lang.Long.toOctalString instead.", "0.6.6")
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

  @deprecated("Use java.lang.Long.bitCount instead.", "0.6.6")
  def bitCount: Int =
    Integer.bitCount(lo) + Integer.bitCount(hi)

  @deprecated("Use java.lang.Long.signum instead.", "0.6.6")
  def signum: RuntimeLong = {
    val hi = this.hi
    if (hi < 0) MinusOne
    else if (isZero(lo, hi)) Zero
    else One
  }

  @deprecated("Use java.lang.Long.numberOfLeadingZeros instead.", "0.6.6")
  def numberOfLeadingZeros: Int = {
    val hi = this.hi
    if (hi != 0) Integer.numberOfLeadingZeros(hi)
    else Integer.numberOfLeadingZeros(lo) + 32
  }

  @deprecated("Use java.lang.Long.numberOfTrailingZeros instead.", "0.6.6")
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
  import Utils._

  private final val TwoPow32 = 4294967296.0
  private final val TwoPow63 = 9223372036854775808.0

  /** The magical mask that allows to test whether an unsigned long is a safe
   *  double.
   *  @see Utils.isUnsignedSafeDouble
   */
  private final val UnsignedSafeDoubleHiMask = 0xffe00000

  private final val AskQuotient = 0
  private final val AskRemainder = 1
  private final val AskBoth = 2

  /** The hi part of a (lo, hi) return value. */
  private[this] var hiReturn: Int = _

  /** The instance of 0L, which is used by the `Emitter` in various places. */
  val Zero = new RuntimeLong(0, 0)

  @deprecated("Use new RuntimeLong(1, 0) instead.", "0.6.11")
  def One: RuntimeLong = new RuntimeLong(1, 0)

  @deprecated("Use new RuntimeLong(-1, -1) instead.", "0.6.11")
  def MinusOne: RuntimeLong = new RuntimeLong(-1, -1)

  @deprecated("Use new RuntimeLong(0, 0x80000000) instead.", "0.6.11")
  def MinValue: RuntimeLong = new RuntimeLong(0, 0x80000000)

  @deprecated("Use new RuntimeLong(0xffffffff, 0x7fffffff) instead.", "0.6.11")
  def MaxValue: RuntimeLong = new RuntimeLong(0xffffffff, 0x7fffffff)

  private def toString(lo: Int, hi: Int): String = {
    if (isInt32(lo, hi)) {
      lo.toString()
    } else if (hi < 0) {
      "-" + toUnsignedString(inline_lo_unary_-(lo), inline_hi_unary_-(lo, hi))
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
      /* At this point, (lo, hi) >= 2^53.
       * We divide (lo, hi) once by 10^9 and keep the remainder.
       *
       * The remainder must then be < 10^9, and is therefore an int32.
       *
       * The quotient must be <= ULong.MaxValue / 10^9, which is < 2^53, and
       * is therefore a valid double. It must also be non-zero, since
       * (lo, hi) >= 2^53 > 10^9.
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

  private def toDouble(lo: Int, hi: Int): Double = {
    if (hi < 0) {
      // We do .toUint on the hi part specifically for MinValue
      -(inline_hi_unary_-(lo, hi).toUint * TwoPow32 + inline_lo_unary_-(lo).toUint)
    } else {
      hi * TwoPow32 + lo.toUint
    }
  }

  @inline
  def fromDouble(value: Double): RuntimeLong = {
    val lo = fromDoubleImpl(value)
    new RuntimeLong(lo, hiReturn)
  }

  private def fromDoubleImpl(value: Double): Int = {
    /* When value is NaN, the conditions of the 3 `if`s are false, and we end
     * up returning (NaN | 0, (NaN / TwoPow32) | 0), which is correctly (0, 0).
     */

    if (value < -TwoPow63) {
      hiReturn = 0x80000000
      0
    } else if (value >= TwoPow63) {
      hiReturn = 0x7fffffff
      0xffffffff
    } else {
      val rawLo = rawToInt(value)
      val rawHi = rawToInt(value / TwoPow32)

      /* Magic!
       *
       * When value < 0, this should *reasonably* be:
       *   val absValue = -value
       *   val absLo = rawToInt(absValue)
       *   val absHi = rawToInt(absValue / TwoPow32)
       *   val lo = -absLo
       *   hiReturn = if (absLo != 0) ~absHi else -absHi
       *   return lo
       *
       * Using the fact that rawToInt(-x) == -rawToInt(x), we can rewrite
       * absLo and absHi without absValue as:
       *   val absLo = -rawToInt(value)
       *             = -rawLo
       *   val absHi = -rawToInt(value / TwoPow32)
       *             = -rawHi
       *
       * Now, we can replace absLo in the definition of lo and get:
       *   val lo = -(-rawLo)
       *          = rawLo
       *
       * The `hiReturn` definition can be rewritten as
       *   hiReturn = if (lo != 0) -absHi - 1 else -absHi
       *            = if (rawLo != 0) -(-rawHi) - 1 else -(-rawHi)
       *            = if (rawLo != 0) rawHi - 1 else rawHi
       *
       * Now that we do not need absValue, absLo nor absHi anymore, we end
       * end up with:
       *   hiReturn = if (rawLo != 0) rawHi - 1 else rawHi
       *   return rawLo
       *
       * When value >= 0, the definitions are simply
       *   hiReturn = rawToInt(value / TwoPow32) = rawHi
       *   lo = rawToInt(value) = rawLo
       *
       * Combining the negative and positive cases, we get:
       */
      hiReturn = if (value < 0 && rawLo != 0) rawHi - 1 else rawHi
      rawLo
    }
  }

  private def compare(alo: Int, ahi: Int, blo: Int, bhi: Int): Int = {
    if (ahi == bhi) {
      if (alo == blo) 0
      else if (inlineUnsignedInt_<(alo, blo)) -1
      else 1
    } else {
      if (ahi < bhi) -1
      else 1
    }
  }

  private def timesHi(alo: Int, ahi: Int, blo: Int, bhi: Int): Int = {
    val a0 = alo & 0xffff
    val a1 = alo >>> 16
    val a2 = ahi & 0xffff
    val a3 = ahi >>> 16
    val b0 = blo & 0xffff
    val b1 = blo >>> 16
    val b2 = bhi & 0xffff
    val b3 = bhi >>> 16

    val c1part = ((a0 * b0) >>> 16) + (a1 * b0)
    var c2 = (c1part >>> 16) + (((c1part & 0xffff) + (a0 * b1)) >>> 16)
    var c3 = c2 >>> 16
    c2 = (c2 & 0xffff) + a2 * b0
    c3 = c3 + (c2 >>> 16)
    c2 = (c2 & 0xffff) + a1 * b1
    c3 = c3 + (c2 >>> 16)
    c2 = (c2 & 0xffff) + a0 * b2
    c3 = c3 + (c2 >>> 16)
    c3 = c3 + a3 * b0 + a2 * b1 + a1 * b2 + a0 * b3

    (c2 & 0xffff) | (c3 << 16)
  }

  @inline
  def divide(a: RuntimeLong, b: RuntimeLong): RuntimeLong = {
    val lo = divideImpl(a.lo, a.hi, b.lo, b.hi)
    new RuntimeLong(lo, hiReturn)
  }

  def divideImpl(alo: Int, ahi: Int, blo: Int, bhi: Int): Int = {
    if (isZero(blo, bhi))
      throw new ArithmeticException("/ by zero")

    if (isInt32(alo, ahi)) {
      if (isInt32(blo, bhi)) {
        if (alo == Int.MinValue && blo == -1) {
          hiReturn = 0
          Int.MinValue
        } else {
          val lo = alo / blo
          hiReturn = lo >> 31
          lo
        }
      } else {
        // Either a == Int.MinValue && b == (Int.MaxValue + 1), or (abs(b) > abs(a))
        if (alo == Int.MinValue && (blo == 0x80000000 && bhi == 0)) {
          hiReturn = -1
          -1
        } else {
          // 0L, because abs(b) > abs(a)
          hiReturn = 0
          0
        }
      }
    } else {
      val (aNeg, aAbs) = inline_abs(alo, ahi)
      val (bNeg, bAbs) = inline_abs(blo, bhi)
      val absRLo = unsigned_/(aAbs.lo, aAbs.hi, bAbs.lo, bAbs.hi)
      if (aNeg == bNeg) absRLo
      else inline_hiReturn_unary_-(absRLo, hiReturn)
    }
  }

  @inline
  def divideUnsigned(a: RuntimeLong, b: RuntimeLong): RuntimeLong = {
    val lo = divideUnsignedImpl(a.lo, a.hi, b.lo, b.hi)
    new RuntimeLong(lo, hiReturn)
  }

  def divideUnsignedImpl(alo: Int, ahi: Int, blo: Int, bhi: Int): Int = {
    if (isZero(blo, bhi))
      throw new ArithmeticException("/ by zero")

    if (isUInt32(ahi)) {
      if (isUInt32(bhi)) {
        hiReturn = 0
        // Integer.divideUnsigned(alo, blo), inaccessible when compiling on JDK < 8
        rawToInt(alo.toUint / blo.toUint)
      } else {
        // a < b
        hiReturn = 0
        0
      }
    } else {
      unsigned_/(alo, ahi, blo, bhi)
    }
  }

  private def unsigned_/(alo: Int, ahi: Int, blo: Int, bhi: Int): Int = {
    // This method is not called if isInt32(alo, ahi) nor if isZero(blo, bhi)
    if (isUnsignedSafeDouble(ahi)) {
      if (isUnsignedSafeDouble(bhi)) {
        val aDouble = asUnsignedSafeDouble(alo, ahi)
        val bDouble = asUnsignedSafeDouble(blo, bhi)
        val rDouble = aDouble / bDouble
        hiReturn = unsignedSafeDoubleHi(rDouble)
        unsignedSafeDoubleLo(rDouble)
      } else {
        // 0L, because b > a
        hiReturn = 0
        0
      }
    } else {
      if (bhi == 0 && isPowerOfTwo_IKnowItsNot0(blo)) {
        val pow = log2OfPowerOfTwo(blo)
        if (pow == 0) {
          hiReturn = ahi
          alo
        } else {
          hiReturn = ahi >>> pow
          (alo >>> pow) | (ahi << -pow)
        }
      } else if (blo == 0 && isPowerOfTwo_IKnowItsNot0(bhi)) {
        val pow = log2OfPowerOfTwo(bhi)
        hiReturn = 0
        ahi >>> pow
      } else {
        unsignedDivModHelper(alo, ahi, blo, bhi, AskQuotient).asInstanceOf[Int]
      }
    }
  }

  @inline
  def remainder(a: RuntimeLong, b: RuntimeLong): RuntimeLong = {
    val lo = remainderImpl(a.lo, a.hi, b.lo, b.hi)
    new RuntimeLong(lo, hiReturn)
  }

  def remainderImpl(alo: Int, ahi: Int, blo: Int, bhi: Int): Int = {
    if (isZero(blo, bhi))
      throw new ArithmeticException("/ by zero")

    if (isInt32(alo, ahi)) {
      if (isInt32(blo, bhi)) {
        if (blo != -1) {
          val lo = alo % blo
          hiReturn = lo >> 31
          lo
        } else {
          // Work around https://github.com/ariya/phantomjs/issues/12198
          hiReturn = 0
          0
        }
      } else {
        // Either a == Int.MinValue && b == (Int.MaxValue + 1), or (abs(b) > abs(a))
        if (alo == Int.MinValue && (blo == 0x80000000 && bhi == 0)) {
          hiReturn = 0
          0
        } else {
          // a, because abs(b) > abs(a)
          hiReturn = ahi
          alo
        }
      }
    } else {
      val (aNeg, aAbs) = inline_abs(alo, ahi)
      val (_, bAbs) = inline_abs(blo, bhi)
      val absRLo = unsigned_%(aAbs.lo, aAbs.hi, bAbs.lo, bAbs.hi)
      if (aNeg) inline_hiReturn_unary_-(absRLo, hiReturn)
      else absRLo
    }
  }

  @inline
  def remainderUnsigned(a: RuntimeLong, b: RuntimeLong): RuntimeLong = {
    val lo = remainderUnsignedImpl(a.lo, a.hi, b.lo, b.hi)
    new RuntimeLong(lo, hiReturn)
  }

  def remainderUnsignedImpl(alo: Int, ahi: Int, blo: Int, bhi: Int): Int = {
    if (isZero(blo, bhi))
      throw new ArithmeticException("/ by zero")

    if (isUInt32(ahi)) {
      if (isUInt32(bhi)) {
        hiReturn = 0
        // Integer.remainderUnsigned(alo, blo), inaccessible when compiling on JDK < 8
        rawToInt(alo.toUint % blo.toUint)
      } else {
        // a < b
        hiReturn = ahi
        alo
      }
    } else {
      unsigned_%(alo, ahi, blo, bhi)
    }
  }

  private def unsigned_%(alo: Int, ahi: Int, blo: Int, bhi: Int): Int = {
    // This method is not called if isInt32(alo, ahi) nor if isZero(blo, bhi)
    if (isUnsignedSafeDouble(ahi)) {
      if (isUnsignedSafeDouble(bhi)) {
        val aDouble = asUnsignedSafeDouble(alo, ahi)
        val bDouble = asUnsignedSafeDouble(blo, bhi)
        val rDouble = aDouble % bDouble
        hiReturn = unsignedSafeDoubleHi(rDouble)
        unsignedSafeDoubleLo(rDouble)
      } else {
        // a, because b > a
        hiReturn = ahi
        alo
      }
    } else {
      if (bhi == 0 && isPowerOfTwo_IKnowItsNot0(blo)) {
        hiReturn = 0
        alo & (blo - 1)
      } else if (blo == 0 && isPowerOfTwo_IKnowItsNot0(bhi)) {
        hiReturn = ahi & (bhi - 1)
        alo
      } else {
        unsignedDivModHelper(alo, ahi, blo, bhi, AskRemainder).asInstanceOf[Int]
      }
    }
  }

  private def unsignedDivModHelper(alo: Int, ahi: Int, blo: Int, bhi: Int,
      ask: Int): Int | js.Tuple4[Int, Int, Int, Int] = {

    var shift =
      inlineNumberOfLeadingZeros(blo, bhi) - inlineNumberOfLeadingZeros(alo, ahi)
    val initialBShift = inline_<<(blo, bhi, shift)
    var bShiftLo = initialBShift.lo
    var bShiftHi = initialBShift.hi
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
     *   while (shift >= 0 && !isUnsignedSafeDouble(remHi))
     * but we manually inline isUnsignedSafeDouble because remHi is a var. If
     * we let the optimizer inline it, it will first store remHi in a temporary
     * val, which will explose the while condition as a while(true) + if +
     * break, and we don't want that.
     */
    while (shift >= 0 && (remHi & UnsignedSafeDoubleHiMask) != 0) {
      if (inlineUnsigned_>=(remLo, remHi, bShiftLo, bShiftHi)) {
        val newRem = inline_-(remLo, remHi, bShiftLo, bShiftHi)
        remLo = newRem.lo
        remHi = newRem.hi
        if (shift < 32)
          quotLo |= (1 << shift)
        else
          quotHi |= (1 << shift) // == (1 << (shift - 32))
      }
      shift -= 1
      val newBShift = inline_>>>(bShiftLo, bShiftHi, 1)
      bShiftLo = newBShift.lo
      bShiftHi = newBShift.hi
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
        quotLo = newQuot.lo
        quotHi = newQuot.hi
      }

      if (ask != AskQuotient) {
        val rem_mod_bDouble = remDouble % bDouble
        remLo = unsignedSafeDoubleLo(rem_mod_bDouble)
        remHi = unsignedSafeDoubleHi(rem_mod_bDouble)
      }
    }

    if (ask == AskQuotient) {
      hiReturn = quotHi
      quotLo
    } else if (ask == AskRemainder) {
      hiReturn = remHi
      remLo
    } else {
      js.Tuple4(quotLo, quotHi, remLo, remHi)
    }
  }

  @inline
  private def inline_hiReturn_unary_-(lo: Int, hi: Int): Int = {
    hiReturn = inline_hi_unary_-(lo, hi)
    inline_lo_unary_-(lo)
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
    def inlineUnsignedInt_<=(a: Int, b: Int): Boolean =
      (a ^ 0x80000000) <= (b ^ 0x80000000)

    @inline
    def inlineUnsignedInt_>(a: Int, b: Int): Boolean =
      (a ^ 0x80000000) > (b ^ 0x80000000)

    @inline
    def inlineUnsignedInt_>=(a: Int, b: Int): Boolean =
      (a ^ 0x80000000) >= (b ^ 0x80000000)

    @inline
    def inline_lo_unary_-(lo: Int): Int =
      -lo

    @inline
    def inline_hi_unary_-(lo: Int, hi: Int): Int =
      if (lo != 0) ~hi else -hi

    @inline
    def inline_abs(lo: Int, hi: Int): (Boolean, RuntimeLong) = {
      val neg = hi < 0
      val abs =
        if (neg) new RuntimeLong(inline_lo_unary_-(lo), inline_hi_unary_-(lo, hi))
        else new RuntimeLong(lo, hi)
      (neg, abs)
    }

    @inline
    def inline_<<(lo: Int, hi: Int, n: Int): RuntimeLong = {
      if (n < 32)
        new RuntimeLong(lo << n, if (n == 0) hi else (lo >>> -n) | (hi << n))
      else
        new RuntimeLong(0, lo << n)
    }

    @inline
    def inline_>>>(lo: Int, hi: Int, n: Int): RuntimeLong = {
      if (n < 32)
        new RuntimeLong(if (n == 0) lo else (lo >>> n) | (hi << -n), hi >>> n)
      else
        new RuntimeLong(hi >>> n, 0)
    }

    @inline
    def inline_+(alo: Int, ahi: Int, blo: Int, bhi: Int): RuntimeLong = {
      val lo = alo + blo
      new RuntimeLong(lo,
          if (inlineUnsignedInt_<(lo, alo)) ahi + bhi + 1 else ahi + bhi)
    }

    @inline
    def inline_-(alo: Int, ahi: Int, blo: Int, bhi: Int): RuntimeLong = {
      val lo = alo - blo
      new RuntimeLong(lo,
          if (inlineUnsignedInt_>(lo, alo)) ahi - bhi - 1 else ahi - bhi)
    }
  }

}
