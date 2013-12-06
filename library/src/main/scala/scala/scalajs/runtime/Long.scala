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
final class Long private (
  val l: Int,
  val m: Int,
  val h: Int
) { x =>

  import Long._

  def toByte: Byte = toInt.toByte
  def toShort: Short = toInt.toShort
  def toChar: Char = toInt.toChar
  def toInt: Int = l | (m << BITS)
  def toLong: Long = x
  def toFloat: Float = toDouble.toFloat
  def toDouble: Double =
    if (isMinValue) -9223372036854775808.0
    else if (isNegative) -((-x).toDouble)
    else l + m * TWO_PWR_22_DBL + h * TWO_PWR_44_DBL

  def unary_~ : Long = masked(~x.l, ~x.m, ~x.h)
  def unary_+ : Long = x
  def unary_- : Long = {
    val neg0 = (~x.l + 1) & MASK
    val neg1 = (~x.m + (if (neg0 == 0) 1 else 0)) & MASK
    val neg2 = (~x.h + (if (neg0 == 0 && neg1 == 0) 1 else 0)) & MASK_2
    Long(neg0, neg1, neg2)
  }

  def +(y: String): String = x.toString + y

  def <<(n_in: Int): Long = {
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
  def >>>(n_in: Int): Long = {
    // Check that h is correctly masked, otherwise we'll shift values in
    assert(x.h == (x.h & MASK_2))
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
    } else
      masked(x.h >>> (n - BITS01), 0, 0)

  }

  /**
   * arithmetic right shift
   */
  def >>(n_in: Int): Long = {
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

  override def equals(that: Any): Boolean = that match {
    case y: Long =>
      x.l == y.l && x.m == y.m && x.h == y.h
    case _ => false
  }
  def notEquals(that: Any): Boolean = !equals(that)

  def < (y: Long): Boolean = !(x >= y)
  def <=(y: Long): Boolean = !(x >  y)
  def > (y: Long): Boolean = {
    val signx = x.sign
    val signy = y.sign
    if (signx == 0)
      signy != 0 ||
      x.h >  y.h ||
      x.h == y.h && x.m >  y.m ||
      x.h == y.h && x.m == y.m && x.l >  y.l
    else !(
      signy == 0 ||
      x.h <  y.h ||
      x.h == y.h && x.m <  y.m ||
      x.h == y.h && x.m == y.m && x.l <= y.l
    )
  }

  /**
   * greater or equal.
   * note: gwt implements this individually
   */
  def >=(y: Long) : Boolean = x == y || x > y

  def |(y: Long): Long = Long(x.l | y.l, x.m | y.m, x.h | y.h)
  def &(y: Long): Long = Long(x.l & y.l, x.m & y.m, x.h & y.h)
  def ^(y: Long): Long = Long(x.l ^ y.l, x.m ^ y.m, x.h ^ y.h)

  def +(y: Long): Long = {
    val sum0 = x.l + y.l
    val sum1 = x.m + y.m + (sum0 >> BITS)
    val sum2 = x.h + y.h + (sum1 >> BITS)
    masked(sum0, sum1, sum2)
  }

  /**
   * subtraction
   * note: gwt implements this individually
   */
  def -(y: Long): Long = x + (-y)

  // This assumes that BITS == 22
  def *(y: Long): Long = {

    /** divides v in 13bit chunks */
    def chunk13(v: Long) = (
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

  def /(y: Long): Long = (x divMod y)._1
  def %(y: Long): Long = (x divMod y)._2

  //override def getClass(): Class[Long] = null

  def toBinaryString: String =
    f"${h.toBinaryString}%020s${m.toBinaryString}%022s${l.toBinaryString}%022s"

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
      assert(!isNegative)

      val tenPowZeros = 9
      val tenPow = 1000000000
      val tenPowL = fromInt(tenPow)

      @tailrec
      def toString0(v: Long, acc: String): String =
        if (v.isZero) acc
        else {
          val (quot, rem) = v.divMod(tenPowL)

          val digits = rem.toInt.toString
          val zeroPrefix = if (!quot.isZero) {
            "0" * (tenPowZeros - digits.length)
          } else ""

          toString0(quot, zeroPrefix + digits + acc)
        }

      toString0(x, "")
    }
  }

  def bitCount: Int =
    Integer.bitCount(l) + Integer.bitCount(m) + Integer.bitCount(h)
  
  // helpers //

  /** sign *bit* of long (0 for positive, 1 for negative) */
  private def sign = h >> (BITS2 - 1)

  private def isZero = l == 0 && m == 0 && h == 0
  private def isMinValue = x == MinValue
  private def isNegative = sign != 0
  private def abs = if (sign == 1) -x else x
  private def numberOfLeadingZeros =
    if (h == 0 && m == 0)
      Integer.numberOfLeadingZeros(l) - (32 - BITS) + (64 - BITS)
    else if (h == 0)
      Integer.numberOfLeadingZeros(m) - (32 - BITS) + (64 - BITS01)
    else
      Integer.numberOfLeadingZeros(h) - (32 - BITS2)

  /** return Some(log_2(x)) if power of 2 or None othwerise */
  private def powerOfTwo = 
    if      (h == 0 && m == 0 && l != 0 && (l & (l - 1)) == 0)
      Some(Integer.numberOfTrailingZeros(l))
    else if (h == 0 && m != 0 && l == 0 && (m & (m - 1)) == 0)
      Some(Integer.numberOfTrailingZeros(m) + BITS)
    else if (h != 0 && m == 0 && l == 0 && (h & (h - 1)) == 0)
      Some(Integer.numberOfTrailingZeros(h) + BITS01)
    else
      None

  private def setBit(bit: Int) =
    if (bit < BITS)
      Long(l | (1 << bit), m, h)
    else if (bit < BITS01)
      Long(l, m | (1 << (bit - BITS)), h)
    else
      Long(l, m, h | (1 << (bit - BITS01)))

  private def divMod(y: Long): (Long, Long) = {
    if (y.isZero) throw new ArithmeticException("/ by zero")
    else if (x.isZero) (zero, zero)
    else if (y.isMinValue) {
      // MinValue / MinValue == 1, rem = 0
      // otherwise == 0, rem x
      if (x.isMinValue) (one, zero)
      else (zero, x)
    } else {
      val xNegative = x.isNegative
      val yNegative = y.isNegative
      
      val xMinValue = x.isMinValue

      val absX = x.abs  // this may be useless if x.isMinValue
      val absY = y.abs
      
      y.powerOfTwo match {
        case Some(pow) if xMinValue =>
          val z = x >> pow
          (if (yNegative) -z else z, zero)
        case Some(pow) =>
          // x is not min value, so we can use absX
          val absZ = absX >> pow
          val z = if (xNegative ^ yNegative) -absZ else absZ
          val remAbs = absX.maskRight(pow)
          val rem = if (xNegative) -remAbs else remAbs
          (z, rem)
        case None if xMinValue =>
          divModHelper(MaxValue, absY, xNegative, yNegative, xMinValue = true)
        // here we know that x is not min value, so absX makes sense to use
        case None if absX < absY =>
          (zero, x)
        case None =>
          divModHelper(absX, absY, xNegative, yNegative, xMinValue = false)
      }

    }
  }

  private def maskRight(bits: Int) = {
    if (bits <= BITS)
      Long(l & ((1 << bits) - 1), 0, 0)
    else if (bits <= BITS01)
      Long(l, m & ((1 << (bits - BITS)) - 1), 0)
    else
      Long(l, m, h & ((1 << (bits - BITS01)) - 1))
  }

}

object Long {

  /** number of relevant bits in each Long.l and Long.m */
  private val BITS:    Int = 22
  /** number of relevant bits in Long.l and Long.m together */
  private val BITS01:  Int = 2 * BITS
  /** number of relevant bits in Long.h */
  private val BITS2:   Int = 64 - BITS01
  /** bitmask for Long.l and Long.m */
  private val MASK:    Int = (1 << BITS) - 1
  /** bitmask for Long.h */
  private val MASK_2:  Int = (1 << BITS2) - 1

  private val SIGN_BIT:       Int    = BITS2 - 1
  private val SIGN_BIT_VALUE: Int    = 1 << SIGN_BIT
  private val TWO_PWR_15_DBL: Double = 0x8000
  private val TWO_PWR_16_DBL: Double = 0x10000
  private val TWO_PWR_22_DBL: Double = 0x400000
  private val TWO_PWR_31_DBL: Double = TWO_PWR_16_DBL * TWO_PWR_15_DBL
  private val TWO_PWR_32_DBL: Double = TWO_PWR_16_DBL * TWO_PWR_16_DBL
  private val TWO_PWR_44_DBL: Double = TWO_PWR_22_DBL * TWO_PWR_22_DBL
  private val TWO_PWR_63_DBL: Double = TWO_PWR_32_DBL * TWO_PWR_31_DBL

  val zero = Long(0,0,0)
  val one  = Long(1,0,0)

  def fromHexString(str: String) = {
	import scalajs.js.parseInt
    assert(str.size == 16)
    val l = parseInt(str.substring(10), 16).toInt
    val m = parseInt(str.substring(6, 7), 16).toInt >> 2
    val h = parseInt(str.substring(0, 5), 16).toInt
    masked(l, m, h)
  }
  
  def fromString(str: String): Long =
    if (str.head == '-') -fromString(str.tail) else {
      import scalajs.js.parseInt
      val maxLen = 9
      @tailrec
      def fromString0(str: String, acc: Long): Long = if (str.size > 0) {
        val (cur, next) = str.splitAt(maxLen)
        val macc = acc * fromInt(math.pow(10, cur.size).toInt)
        // explicitly specify radix to avoid intepreation as octal
        val cval = fromInt(parseInt(cur, 10).toInt)
        fromString0(next, macc + cval)
      } else acc
    
      fromString0(str, zero)
    }
  
  def fromByte(value: Byte): Long = fromInt(value.toInt)
  def fromShort(value: Short): Long = fromInt(value.toInt)
  def fromChar(value: Char): Long = fromInt(value.toInt)
  def fromInt(value: Int): Long = {
    val a0 = value & MASK
    val a1 = (value >> BITS) & MASK
    val a2 = if (value < 0) MASK_2 else 0
    new Long(a0, a1, a2)
  }

  def fromFloat(value: Float): Long = fromDouble(value.toDouble)
  def fromDouble(value: Double): Long = 
    if (value.isNaN) zero
    else if (value < -TWO_PWR_63_DBL) MinValue
    else if (value >= TWO_PWR_63_DBL) MaxValue
    else if (value < 0) -fromDouble(-value)
    else {
      var acc = value
      val a2 = if (acc >= TWO_PWR_44_DBL) (acc / TWO_PWR_44_DBL).toInt else 0
      acc -= a2 * TWO_PWR_44_DBL
      val a1 = if (acc >= TWO_PWR_22_DBL) (acc / TWO_PWR_22_DBL).toInt else 0
      acc -= a1 * TWO_PWR_22_DBL
      val a0 = acc.toInt
      Long(a0, a1, a2)
    }

  /**
   * creates a new long but masks bits as follows:
   * l & MASK, m & MASK, h & MASK_2
   */
  protected def masked(l: Int, m: Int, h: Int) =
    Long(l & MASK, m & MASK, h & MASK_2)
  def apply(l: Int, m: Int, h: Int) = new Long(l, m, h)

  /**
   * performs division in "normal cases"
   * @param x absolute value of the numerator
   * @param y absolute value of the denominator
   * @param xNegative whether numerator was negative
   * @param yNegative whether denominator was negative
   * @param xMinValue whether numerator was Long.minValue
   */
  private def divModHelper(x: Long, y: Long,
                           xNegative: Boolean, yNegative: Boolean,
                           xMinValue: Boolean) = {

    @tailrec
    def divide0(shift: Int, yShift: Long,
                curX: Long, quot: Long): (Long,Long) =
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

    (quot, rem)
    
  }

  // Public Long API

  /** The smallest value representable as a Long. */
  final val MinValue = Long(0, 0, SIGN_BIT_VALUE)

  /** The largest value representable as a Long. */
  final val MaxValue = Long(MASK, MASK, MASK_2 >> 1)
  
}
