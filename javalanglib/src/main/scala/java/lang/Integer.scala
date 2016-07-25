package java.lang

import scala.scalajs.js

/* This is a hijacked class. Its instances are primitive numbers.
 * Constructors are not emitted.
 */
final class Integer private () extends Number with Comparable[Integer] {

  def this(value: scala.Int) = this()
  def this(s: String) = this()

  @inline def intValue(): scala.Int =
    this.asInstanceOf[scala.Int]

  @inline override def byteValue(): scala.Byte = intValue.toByte
  @inline override def shortValue(): scala.Short = intValue.toShort
  @inline def longValue(): scala.Long = intValue.toLong
  @inline def floatValue(): scala.Float = intValue.toFloat
  @inline def doubleValue(): scala.Double = intValue.toDouble

  @inline override def equals(that: Any): scala.Boolean =
    this eq that.asInstanceOf[AnyRef]

  @inline override def hashCode(): Int =
    intValue

  @inline override def compareTo(that: Integer): Int =
    Integer.compare(intValue, that.intValue)

  @inline override def toString(): String =
    Integer.toString(intValue)

  /* Methods of java.lang.Byte and java.lang.Short.
   * When calling a method of j.l.Byte or j.l.Short on a primitive value,
   * it appears to be called directly on the primitive value, which has type
   * IntType. Call resolution, by the analyzer and the optimizer, will then
   * look for the method in the class j.l.Integer instead of j.l.Byte or
   * j.l.Short. This is why we add here the methods of these two classes that
   * are not already in j.l.Integer.
   */

  @inline def compareTo(that: Byte): Int =
    Integer.compare(intValue, that.intValue)

  @inline def compareTo(that: Short): Int =
    Integer.compare(intValue, that.intValue)

}

object Integer {
  final val TYPE = classOf[scala.Int]
  final val MIN_VALUE = -2147483648
  final val MAX_VALUE = 2147483647
  final val SIZE = 32
  final val BYTES = 4

  @inline def valueOf(intValue: scala.Int): Integer = new Integer(intValue)
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
    def fail = throw new NumberFormatException(s"""For input string: "$s"""")

    if (s == null || s.size == 0 ||
        radix < Character.MIN_RADIX ||
        radix > Character.MAX_RADIX)
      fail
    else {
      var i = if ((signed && s(0) == '-') || s(0) == '+') 1 else 0
      // JavaDoc says: We need at least one digit
      if (s.size <= i) fail
      else {
        // Check each character for validity
        while (i < s.size) {
          if (Character.digit(s(i), radix) < 0) fail
          i += 1
        }
        val res = js.Dynamic.global.parseInt(s, radix).asInstanceOf[scala.Double]

        @inline def isOutOfBounds: scala.Boolean = {
          if (signed) res > MAX_VALUE || res < MIN_VALUE
          else res > 0xFFFFFFFFL || res < 0
        }

        if (res.isNaN || isOutOfBounds) {
          fail
        } else if (signed) {
          res.toInt
        } else {
          asInt(res)
        }
      }
    }
  }

  @inline def toString(i: scala.Int): String = "" + i

  @inline def toUnsignedString(i: Int, radix: Int): String =
    toStringBase(i, radix)

  @inline def compare(x: scala.Int, y: scala.Int): scala.Int =
    if (x == y) 0 else if (x < y) -1 else 1

  @inline def compareUnsigned(x: scala.Int, y: scala.Int): scala.Int = {
    import js.JSNumberOps._
    if (x == y) 0
    else if (x.toUint > y.toUint) 1
    else -1
  }

  @inline def toUnsignedLong(x: Int): scala.Long =
    x.toLong & 0xffffffffL

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

  @inline def divideUnsigned(dividend: Int, divisor: Int): Int = {
    import js.JSNumberOps._
    asInt(dividend.toUint / divisor.toUint)
  }

  @inline def remainderUnsigned(dividend: Int, divisor: Int): Int = {
    import js.JSNumberOps._
    asInt(dividend.toUint % divisor.toUint)
  }

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

  @inline def rotateLeft(i: scala.Int, distance: scala.Int): scala.Int =
    (i << distance) | (i >>> -distance)

  @inline def rotateRight(i: scala.Int, distance: scala.Int): scala.Int =
    (i >>> distance) | (i << -distance)

  @inline def signum(i: scala.Int): scala.Int =
    if (i == 0) 0 else if (i < 0) -1 else 1

  // Intrinsic
  def numberOfLeadingZeros(i: scala.Int): scala.Int = {
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
    import js.JSNumberOps._
    i.toUint.toString(base)
  }

  @inline private def asInt(n: scala.Double): scala.Int =
    (n.asInstanceOf[js.Dynamic] | 0.asInstanceOf[js.Dynamic]).asInstanceOf[Int]
}
