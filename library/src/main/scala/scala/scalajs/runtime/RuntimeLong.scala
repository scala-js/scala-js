package scala.scalajs.runtime

import scala.annotation.tailrec

/**
 * emulate a Java-Long using three integers.
 * taken from gwt LongLib:
 * com.google.gwt.lang.LongLib
 *
 * only used by runtime
 *
 * holds values l, m, h (low, middle, high)
 * s.t. (x.l + ((long) x.m << 22) + ((long) x.h << 44)) is equal to
 * the original value
 */
final class RuntimeLong private (
  val l: Int,
  val m: Int,
  val h: Int
) extends Number with Comparable[java.lang.Long] { x =>

  import RuntimeLong.{MinValue => _, MaxValue => _, _}
  import CachedConstants._

  /** Construct from an Int.
   *  This is the implementation of RuntimeLong.fromInt() in a way that does not
   *  require to load to module RuntimeLong.
   */
  def this(value: Int) = this(
      value & RuntimeLong.MASK,
      (value >> RuntimeLong.BITS) & RuntimeLong.MASK,
      if (value < 0) RuntimeLong.MASK_2 else 0)

  def toByte: Byte = toInt.toByte
  def toShort: Short = toInt.toShort
  def toChar: Char = toInt.toChar
  def toInt: Int = l | (m << BITS)
  def toLong: Long = fromRuntimeLong(x)
  def toFloat: Float = toDouble.toFloat
  def toDouble: Double =
    if (isMinValue) -9223372036854775808.0
    else if (isNegative) -((-x).toDouble)
    else l + m * TWO_PWR_22_DBL + h * TWO_PWR_44_DBL

  // java.lang.Number
  override def byteValue(): Byte = toByte
  override def shortValue(): Short = toShort
  def intValue(): Int = toInt
  def longValue(): Long = fromRuntimeLong(x)
  def floatValue(): Float = toFloat
  def doubleValue(): Double = toDouble

  // java.lang.Comparable + overload taking scala.Long
  def compareTo(that: RuntimeLong): Int =
    if (this equals that) 0 else if (this > that) 1 else -1
  def compareTo(that: java.lang.Long): Int =
    compareTo(toRuntimeLong(that.longValue()))

  def unary_~ : RuntimeLong = masked(~x.l, ~x.m, ~x.h)
  def unary_+ : RuntimeLong = x
  def unary_- : RuntimeLong = {
    val neg0 = (~x.l + 1) & MASK
    val neg1 = (~x.m + (if (neg0 == 0) 1 else 0)) & MASK
    val neg2 = (~x.h + (if (neg0 == 0 && neg1 == 0) 1 else 0)) & MASK_2
    RuntimeLong(neg0, neg1, neg2)
  }

  def +(y: String): String = x.toString + y

  def <<(n_in: Int): RuntimeLong = {
    /* crop MSB. Note: This will cause (2L << 65 == 2L << 1)
     * apparently this is as specified
     */
    val n = n_in & 63

    if (n < BITS) {
      val remBits = BITS - n
      masked(x.l << n,
             (x.m << n) | (x.l >> remBits),
             (x.h << n) | (x.m >> remBits))
    } else if (n < BITS01) {
      val shfBits = n - BITS
      val remBits = BITS01 - n
      masked(0, x.l << shfBits, (x.m << shfBits) | (x.l >> remBits))
    } else {
      masked(0, 0, x.l << (n - BITS01))
    }

  }

  /**
   * logical right shift
   */
  def >>>(n_in: Int): RuntimeLong = {
    val n = n_in & 63
    if (n < BITS) {
      val remBits = BITS - n
      masked((x.l >> n) | (x.m << remBits),
             // FIXME is this really >> and not >>>??
             (x.m >> n) | (x.h << remBits),
             x.h >>> n)
    } else if (n < BITS01) {
      val shfBits = n - BITS
      val remBits = BITS01 - n
             // FIXME is this really >> and not >>>??
      masked((x.m >> shfBits) | (x.h << remBits),
             x.h >>> shfBits, 0)
    } else {
      masked(x.h >>> (n - BITS01), 0, 0)
    }
  }

  /**
   * arithmetic right shift
   */
  def >>(n_in: Int): RuntimeLong = {
    val n = n_in & 63;

    // Sign extend x.h
    val negative = (x.h & SIGN_BIT_VALUE) != 0
    val xh = if (negative) x.h | ~MASK_2 else x.h

    if (n < BITS) {
      val remBits = BITS - n
      // FIXME IMHO the first two >> should be >>>
      masked((x.l >> n) | (x.m << remBits),
             (x.m >> n) | (xh  << remBits),
             xh >> n)
    } else if (n < BITS01) {
      val shfBits = n - BITS
      val remBits = BITS01 - n
      // FIXME IMHO the first >> should be >>>
      masked((x.m >> shfBits) | (xh << remBits),
              xh  >> shfBits,
             if (negative) MASK_2 else 0)
    } else {
      masked(xh >> (n - BITS01),
             if (negative) MASK   else 0,
             if (negative) MASK_2 else 0)
    }

  }

  def equals(y: RuntimeLong): Boolean =
    x.l == y.l && x.m == y.m && x.h == y.h

  override def equals(that: Any): Boolean = that match {
    case y: RuntimeLong => x.equals(y)
    case _ => false
  }

  def notEquals(that: RuntimeLong) = !equals(that)
  def notEquals(that: Any): Boolean = !equals(that)

  @inline
  def <(y: RuntimeLong): Boolean = y > x
  @inline
  def <=(y: RuntimeLong): Boolean = y >= x

  def >(y: RuntimeLong): Boolean = {
    if (!x.isNegative)
      y.isNegative ||
      x.h >  y.h ||
      x.h == y.h && x.m >  y.m ||
      x.h == y.h && x.m == y.m && x.l >  y.l
    else !(
      !y.isNegative ||
      x.h <  y.h ||
      x.h == y.h && x.m <  y.m ||
      x.h == y.h && x.m == y.m && x.l <= y.l
    )
  }

  def >=(y: RuntimeLong): Boolean = {
    if (!x.isNegative)
      y.isNegative ||
      x.h >  y.h ||
      x.h == y.h && x.m >  y.m ||
      x.h == y.h && x.m == y.m && x.l >= y.l
    else !(
      !y.isNegative ||
      x.h <  y.h ||
      x.h == y.h && x.m <  y.m ||
      x.h == y.h && x.m == y.m && x.l <  y.l
    )
  }

  def |(y: RuntimeLong): RuntimeLong =
    RuntimeLong(x.l | y.l, x.m | y.m, x.h | y.h)
  def &(y: RuntimeLong): RuntimeLong =
    RuntimeLong(x.l & y.l, x.m & y.m, x.h & y.h)
  def ^(y: RuntimeLong): RuntimeLong =
    RuntimeLong(x.l ^ y.l, x.m ^ y.m, x.h ^ y.h)

  def +(y: RuntimeLong): RuntimeLong = {
    val sum0 = x.l + y.l
    val sum1 = x.m + y.m + (sum0 >> BITS)
    val sum2 = x.h + y.h + (sum1 >> BITS)
    masked(sum0, sum1, sum2)
  }

  /**
   * subtraction
   * note: gwt implements this individually
   */
  def -(y: RuntimeLong): RuntimeLong = x + (-y)

  // This assumes that BITS == 22
  def *(y: RuntimeLong): RuntimeLong = {

    /** divides v in 13bit chunks */
    @inline def chunk13(v: RuntimeLong) = (
      v.l & 0x1fff,
      (v.l >> 13) | ((v.m & 0xf) << 9),
      (v.m >> 4) & 0x1fff,
      (v.m >> 17) | ((v.h & 0xff) << 5),
      (v.h & 0xfff00) >> 8
    )

    val (a0, a1, a2, a3, a4) = chunk13(x)
    val (b0, b1, b2, b3, b4) = chunk13(y)

    // Compute partial products
    // Optimization: if b is small, avoid multiplying by parts that are 0
    var p0 = a0 * b0; // << 0
    var p1 = a1 * b0; // << 13
    var p2 = a2 * b0; // << 26
    var p3 = a3 * b0; // << 39
    var p4 = a4 * b0; // << 52

    if (b1 != 0) {
      p1 += a0 * b1;
      p2 += a1 * b1;
      p3 += a2 * b1;
      p4 += a3 * b1;
    }
    if (b2 != 0) {
      p2 += a0 * b2;
      p3 += a1 * b2;
      p4 += a2 * b2;
    }
    if (b3 != 0) {
      p3 += a0 * b3;
      p4 += a1 * b3;
    }
    if (b4 != 0) {
      p4 += a0 * b4;
    }

    // Accumulate into 22-bit chunks:
    // .........................................c10|...................c00|
    // |....................|..................xxxx|xxxxxxxxxxxxxxxxxxxxxx| p0
    // |....................|......................|......................|
    // |....................|...................c11|......c01.............|
    // |....................|....xxxxxxxxxxxxxxxxxx|xxxxxxxxx.............| p1
    // |....................|......................|......................|
    // |.................c22|...............c12....|......................|
    // |..........xxxxxxxxxx|xxxxxxxxxxxxxxxxxx....|......................| p2
    // |....................|......................|......................|
    // |.................c23|..c13.................|......................|
    // |xxxxxxxxxxxxxxxxxxxx|xxxxx.................|......................| p3
    // |....................|......................|......................|
    // |.........c24........|......................|......................|
    // |xxxxxxxxxxxx........|......................|......................| p4

    val c00 = p0 & 0x3fffff;
    val c01 = (p1 & 0x1ff) << 13;
    val c0 = c00 + c01;

    val c10 = p0 >> 22;
    val c11 = p1 >> 9;
    val c12 = (p2 & 0x3ffff) << 4;
    val c13 = (p3 & 0x1f) << 17;
    val c1 = c10 + c11 + c12 + c13;

    val c22 = p2 >> 18;
    val c23 = p3 >> 5;
    val c24 = (p4 & 0xfff) << 8;
    val c2 = c22 + c23 + c24;

    // Propagate high bits from c0 -> c1, c1 -> c2
    val c1n = c1 + (c0 >> BITS)

    masked(c0, c1n, c2 + (c1n >> BITS))
  }

  def /(y: RuntimeLong): RuntimeLong = (x divMod y)(0)
  def %(y: RuntimeLong): RuntimeLong = (x divMod y)(1)

  //override def getClass(): Class[Long] = null

  def toBinaryString: String = {
    val zeros = "0000000000000000000000" // 22 zeros
    @inline def padBinary22(i: Int) = {
      val s = Integer.toBinaryString(i)
      zeros.substring(s.length) + s
    }
    (padBinary22(h) + padBinary22(m) + padBinary22(l)).substring(2)
  }

  def toHexString: String = {
    val mp = m >> 2
    val lp = l | ((m & 0x3) << BITS)
    f"$h%05x$mp%05x$lp%06x"
  }

  def toOctalString: String = {
    val lp = l & (MASK >> 1)
    val mp = ((m & (MASK >> 2)) << 1) | (l >> (BITS - 1))
    val hp = (h << 2) | (m >> (BITS - 2))
    f"$hp%08o$mp%07o$lp%07o"
  }

  // Any API //

  override def toString: String = {
    if (isZero) "0"
    // Check for MinValue, because its not negatable
    else if (isMinValue) "-9223372036854775808"
    else if (isNegative) "-" + (-x).toString
    else {
      val tenPow9 = TenPow9 // local copy to access CachedConstants only once

      @tailrec
      @inline
      def toString0(v: RuntimeLong, acc: String): String =
        if (v.isZero) acc
        else {
          val quotRem = v.divMod(tenPow9)
          val quot = quotRem(0)
          val rem = quotRem(1)

          val digits = rem.toInt.toString
          val zeroPrefix =
            if (quot.isZero) ""
            else "000000000".substring(digits.length) // (9 - digits.length) zeros

          toString0(quot, zeroPrefix + digits + acc)
        }

      toString0(x, "")
    }
  }

  def bitCount: Int =
    Integer.bitCount(l) + Integer.bitCount(m) + Integer.bitCount(h)

  // helpers //

  @inline private def isZero = l == 0 && m == 0 && h == 0
  @inline private def isMinValue = x.equals(MinValue)
  @inline private def isNegative = (h & SIGN_BIT_VALUE) != 0
  @inline private def abs = if (isNegative) -x else x

  def signum: Int =
    if (isNegative) -1 else if (isZero) 0 else 1

  def numberOfLeadingZeros: Int =
    if (h != 0)      Integer.numberOfLeadingZeros(h) - (32 - BITS2)
    else if (m != 0) Integer.numberOfLeadingZeros(m) - (32 - BITS) + (64 - BITS01)
    else             Integer.numberOfLeadingZeros(l) - (32 - BITS) + (64 - BITS)

  def numberOfTrailingZeros: Int =
    if      (l != 0) Integer.numberOfTrailingZeros(l)
    else if (m != 0) Integer.numberOfTrailingZeros(m) + BITS
    else             Integer.numberOfTrailingZeros(h) + BITS01

  /** return log_2(x) if power of 2 or -1 otherwise */
  private def powerOfTwo =
    if      (h == 0 && m == 0 && l != 0 && (l & (l - 1)) == 0)
      Integer.numberOfTrailingZeros(l)
    else if (h == 0 && m != 0 && l == 0 && (m & (m - 1)) == 0)
      Integer.numberOfTrailingZeros(m) + BITS
    else if (h != 0 && m == 0 && l == 0 && (h & (h - 1)) == 0)
      Integer.numberOfTrailingZeros(h) + BITS01
    else
      -1

  private def setBit(bit: Int) =
    if (bit < BITS)
      RuntimeLong(l | (1 << bit), m, h)
    else if (bit < BITS01)
      RuntimeLong(l, m | (1 << (bit - BITS)), h)
    else
      RuntimeLong(l, m, h | (1 << (bit - BITS01)))

  private def divMod(y: RuntimeLong): scala.scalajs.js.Array[RuntimeLong] = {
    import scala.scalajs.js
    if (y.isZero) throw new ArithmeticException("/ by zero")
    else if (x.isZero) js.Array(zero, zero)
    else if (y.isMinValue) {
      // MinValue / MinValue == 1, rem = 0
      // otherwise == 0, rem x
      if (x.isMinValue) js.Array(one, zero)
      else js.Array(zero, x)
    } else {
      val xNegative = x.isNegative
      val yNegative = y.isNegative

      val xMinValue = x.isMinValue

      val pow = y.powerOfTwo
      if (pow >= 0) {
        if (xMinValue) {
          val z = x >> pow
          js.Array(if (yNegative) -z else z, zero)
        } else {
          // x is not min value, so we can calculate absX
          val absX = x.abs
          val absZ = absX >> pow
          val z = if (xNegative ^ yNegative) -absZ else absZ
          val remAbs = absX.maskRight(pow)
          val rem = if (xNegative) -remAbs else remAbs
          js.Array(z, rem)
        }
      } else {
        val absY = y.abs

        val newX = {
          if (xMinValue)
            MaxValue
          else {
            val absX = x.abs
            if (absX < absY)
              return js.Array(zero, x) // <-- ugly but fast
            else
              absX
          }
        }
        divModHelper(newX, absY, xNegative, yNegative, xMinValue)
      }
    }
  }

  @inline
  private def maskRight(bits: Int) = {
    if (bits <= BITS)
      RuntimeLong(l & ((1 << bits) - 1), 0, 0)
    else if (bits <= BITS01)
      RuntimeLong(l, m & ((1 << (bits - BITS)) - 1), 0)
    else
      RuntimeLong(l, m, h & ((1 << (bits - BITS01)) - 1))
  }

  /**
   * performs division in "normal cases"
   * @param x absolute value of the numerator
   * @param y absolute value of the denominator
   * @param xNegative whether numerator was negative
   * @param yNegative whether denominator was negative
   * @param xMinValue whether numerator was Long.minValue
   */
  @inline
  private def divModHelper(x: RuntimeLong, y: RuntimeLong,
      xNegative: Boolean, yNegative: Boolean,
      xMinValue: Boolean): scala.scalajs.js.Array[RuntimeLong] = {
    import scala.scalajs.js

    @inline
    @tailrec
    def divide0(shift: Int, yShift: RuntimeLong, curX: RuntimeLong,
        quot: RuntimeLong): (RuntimeLong, RuntimeLong) =
      if (shift < 0 || curX.isZero) (quot, curX) else {
        val newX = curX - yShift
        if (!newX.isNegative)
          divide0(shift-1, yShift >> 1, newX, quot.setBit(shift))
        else
          divide0(shift-1, yShift >> 1, curX, quot)
      }

    val shift = y.numberOfLeadingZeros - x.numberOfLeadingZeros
    val yShift = y << shift

    val (absQuot, absRem) = divide0(shift, yShift, x, zero)

    val quot = if (xNegative ^ yNegative) -absQuot else absQuot
    val rem  =
      if (xNegative && xMinValue) -absRem - one
      else if (xNegative)         -absRem
      else                         absRem

    js.Array(quot, rem)
  }

  /*
   * Methods of scala.Long
   * The following methods are only here to properly support reflective calls
   * on longs. YOU MUST NOT USE THESE METHODS.
   */

  //protected def unary_~ : Long = ~toLong // already defined
  //protected def unary_+ : Long = toLong  // already defined
  //protected def unary_- : Long = -toLong // already defined

  //protected def <<(y: Int): Long = toLong << y         // already defined
  protected def <<(y: Long): Long = toLong << y
  //protected def >>>(y: Int): Long = toLong >>> y       // already defined
  protected def >>>(y: Long): Long = toLong >>> y
  //protected def >>(y: Int): Long = toLong >> y         // already defined
  protected def >>(y: Long): Long = toLong >> y

  protected def ==(y: Byte): Boolean = toLong == y
  protected def ==(y: Short): Boolean = toLong == y
  protected def ==(y: Char): Boolean = toLong == y
  protected def ==(y: Int): Boolean = toLong == y
  protected def ==(y: Long): Boolean = toLong == y
  protected def ==(y: Float): Boolean = toLong == y
  protected def ==(y: Double): Boolean = toLong == y

  protected def !=(y: Byte): Boolean = toLong != y
  protected def !=(y: Short): Boolean = toLong != y
  protected def !=(y: Char): Boolean = toLong != y
  protected def !=(y: Int): Boolean = toLong != y
  protected def !=(y: Long): Boolean = toLong != y
  protected def !=(y: Float): Boolean = toLong != y
  protected def !=(y: Double): Boolean = toLong != y

  protected def <(y: Byte): Boolean = toLong < y
  protected def <(y: Short): Boolean = toLong < y
  protected def <(y: Char): Boolean = toLong < y
  protected def <(y: Int): Boolean = toLong < y
  protected def <(y: Long): Boolean = toLong < y
  protected def <(y: Float): Boolean = toLong < y
  protected def <(y: Double): Boolean = toLong < y

  protected def <=(y: Byte): Boolean = toLong <= y
  protected def <=(y: Short): Boolean = toLong <= y
  protected def <=(y: Char): Boolean = toLong <= y
  protected def <=(y: Int): Boolean = toLong <= y
  protected def <=(y: Long): Boolean = toLong <= y
  protected def <=(y: Float): Boolean = toLong <= y
  protected def <=(y: Double): Boolean = toLong <= y

  protected def >(y: Byte): Boolean = toLong > y
  protected def >(y: Short): Boolean = toLong > y
  protected def >(y: Char): Boolean = toLong > y
  protected def >(y: Int): Boolean = toLong > y
  protected def >(y: Long): Boolean = toLong > y
  protected def >(y: Float): Boolean = toLong > y
  protected def >(y: Double): Boolean = toLong > y

  protected def >=(y: Byte): Boolean = toLong >= y
  protected def >=(y: Short): Boolean = toLong >= y
  protected def >=(y: Char): Boolean = toLong >= y
  protected def >=(y: Int): Boolean = toLong >= y
  protected def >=(y: Long): Boolean = toLong >= y
  protected def >=(y: Float): Boolean = toLong >= y
  protected def >=(y: Double): Boolean = toLong >= y

  protected def |(y: Byte): Long = toLong | y
  protected def |(y: Short): Long = toLong | y
  protected def |(y: Char): Long = toLong | y
  protected def |(y: Int): Long = toLong | y
  protected def |(y: Long): Long = toLong | y

  protected def &(y: Byte): Long = toLong & y
  protected def &(y: Short): Long = toLong & y
  protected def &(y: Char): Long = toLong & y
  protected def &(y: Int): Long = toLong & y
  protected def &(y: Long): Long = toLong & y

  protected def ^(y: Byte): Long = toLong ^ y
  protected def ^(y: Short): Long = toLong ^ y
  protected def ^(y: Char): Long = toLong ^ y
  protected def ^(y: Int): Long = toLong ^ y
  protected def ^(y: Long): Long = toLong ^ y

  protected def +(y: Byte): Long = toLong + y
  protected def +(y: Short): Long = toLong + y
  protected def +(y: Char): Long = toLong + y
  protected def +(y: Int): Long = toLong + y
  protected def +(y: Long): Long = toLong + y
  protected def +(y: Float): Float = toLong + y
  protected def +(y: Double): Double = toLong + y

  protected def -(y: Byte): Long = toLong - y
  protected def -(y: Short): Long = toLong - y
  protected def -(y: Char): Long = toLong - y
  protected def -(y: Int): Long = toLong - y
  protected def -(y: Long): Long = toLong - y
  protected def -(y: Float): Float = toLong - y
  protected def -(y: Double): Double = toLong - y

  protected def *(y: Byte): Long = toLong - y
  protected def *(y: Short): Long = toLong - y
  protected def *(y: Char): Long = toLong - y
  protected def *(y: Int): Long = toLong - y
  protected def *(y: Long): Long = toLong - y
  protected def *(y: Float): Float = toLong - y
  protected def *(y: Double): Double = toLong - y

  protected def /(y: Byte): Long = toLong / y
  protected def /(y: Short): Long = toLong / y
  protected def /(y: Char): Long = toLong / y
  protected def /(y: Int): Long = toLong / y
  protected def /(y: Long): Long = toLong / y
  protected def /(y: Float): Float = toLong / y
  protected def /(y: Double): Double = toLong / y

  protected def %(y: Byte): Long = toLong % y
  protected def %(y: Short): Long = toLong % y
  protected def %(y: Char): Long = toLong % y
  protected def %(y: Int): Long = toLong % y
  protected def %(y: Long): Long = toLong % y
  protected def %(y: Float): Float = toLong % y
  protected def %(y: Double): Double = toLong % y

}

object RuntimeLong {

  /** number of relevant bits in each Long.l and Long.m */
  private final val BITS   = 22
  /** number of relevant bits in Long.l and Long.m together */
  private final val BITS01 = 2 * BITS
  /** number of relevant bits in Long.h */
  private final val BITS2  = 64 - BITS01
  /** bitmask for Long.l and Long.m */
  private final val MASK   = (1 << BITS) - 1
  /** bitmask for Long.h */
  private final val MASK_2 = (1 << BITS2) - 1

  private final val SIGN_BIT       = BITS2 - 1
  private final val SIGN_BIT_VALUE = 1 << SIGN_BIT
  private final val TWO_PWR_15_DBL = 0x8000   * 1.0
  private final val TWO_PWR_16_DBL = 0x10000  * 1.0
  private final val TWO_PWR_22_DBL = 0x400000 * 1.0
  private final val TWO_PWR_31_DBL = TWO_PWR_16_DBL * TWO_PWR_15_DBL
  private final val TWO_PWR_32_DBL = TWO_PWR_16_DBL * TWO_PWR_16_DBL
  private final val TWO_PWR_44_DBL = TWO_PWR_22_DBL * TWO_PWR_22_DBL
  private final val TWO_PWR_63_DBL = TWO_PWR_32_DBL * TWO_PWR_31_DBL

  @inline def zero: RuntimeLong = CachedConstants.Zero
  @inline def one:  RuntimeLong = CachedConstants.One

  def toRuntimeLong(x: scala.Long): RuntimeLong = sys.error("stub")
  def fromRuntimeLong(x: RuntimeLong): scala.Long = sys.error("stub")

  @inline def fromString(str: String): RuntimeLong = fromString(str, 10)

  def fromString(str: String, radix: Int): RuntimeLong = {
    if (str.isEmpty) {
      throw new java.lang.NumberFormatException(
          s"""For input string: "$str"""")
    } else if (str.charAt(0) == '-') {
      -fromString(str.substring(1), radix)
    } else {
      import scalajs.js

      val maxLen = 9
      @tailrec
      def fromString0(str0: String, acc: RuntimeLong): RuntimeLong = if (str0.length > 0) {
        val cur = (str0: js.prim.String).substring(0, maxLen): String
        val macc = acc * fromInt(math.pow(radix, cur.length).toInt)
        val ival = js.parseInt(cur, radix)
        if (js.isNaN(ival)) {
          throw new java.lang.NumberFormatException(
            s"""For input string: "$str"""")
        }
        val cval = fromInt(ival.toInt)
        fromString0((str0: js.prim.String).substring(maxLen), macc + cval)
      } else acc

      fromString0(str, zero)
    }
  }

  @inline def fromByte(value: Byte): RuntimeLong = fromInt(value.toInt)
  @inline def fromShort(value: Short): RuntimeLong = fromInt(value.toInt)
  @inline def fromChar(value: Char): RuntimeLong = fromInt(value.toInt)
  @inline def fromInt(value: Int): RuntimeLong = new RuntimeLong(value)
  @inline def fromFloat(value: Float): RuntimeLong = fromDouble(value.toDouble)

  def fromDouble(value: Double): RuntimeLong =
    if (java.lang.Double.isNaN(value)) zero
    else if (value < -TWO_PWR_63_DBL) CachedConstants.MinValue
    else if (value >= TWO_PWR_63_DBL) CachedConstants.MaxValue
    else if (value < 0) -fromDouble(-value)
    else {
      var acc = value
      val a2 = if (acc >= TWO_PWR_44_DBL) (acc / TWO_PWR_44_DBL).toInt else 0
      acc -= a2 * TWO_PWR_44_DBL
      val a1 = if (acc >= TWO_PWR_22_DBL) (acc / TWO_PWR_22_DBL).toInt else 0
      acc -= a1 * TWO_PWR_22_DBL
      val a0 = acc.toInt
      RuntimeLong(a0, a1, a2)
    }

  /**
   * creates a new long but masks bits as follows:
   * l & MASK, m & MASK, h & MASK_2
   */
  @inline private def masked(l: Int, m: Int, h: Int) =
    RuntimeLong(l & MASK, m & MASK, h & MASK_2)

  /** Creates a new long from its three underlying components. */
  @inline def apply(l: Int, m: Int, h: Int): RuntimeLong =
    new RuntimeLong(l, m, h)

  // Public Long API

  /** The smallest value representable as a Long. */
  @deprecated("Implementation detail. Will be removed in 0.6.0.", "0.5.5")
  def MinValue: RuntimeLong = CachedConstants.MinValue

  /** The largest value representable as a Long. */
  @deprecated("Implementation detail. Will be removed in 0.6.0.", "0.5.5")
  def MaxValue: RuntimeLong = CachedConstants.MaxValue

  /** Independent object holding cached constants.
   *  These constants are extracted here so that the constructor of the
   *  RuntimeLong module itself is elideable.
   */
  private object CachedConstants {
    // Do not make these 'final' vals. The goal is to cache the instances.
    val Zero     = toRuntimeLong(0L)
    val One      = toRuntimeLong(1L)
    val MinValue = toRuntimeLong(Long.MinValue)
    val MaxValue = toRuntimeLong(Long.MaxValue)
    val TenPow9  = toRuntimeLong(1000000000L) // 9 zeros
  }

}
