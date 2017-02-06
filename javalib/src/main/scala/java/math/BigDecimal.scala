/*
 * Ported by Alistair Johnson from
 * https://android.googlesource.com/platform/libcore/+/master/luni/src/main/java/java/math/BigDecimal.java
 * Original license copied below:
 */

/*
 *  Licensed to the Apache Software Foundation (ASF) under one or more
 *  contributor license agreements.  See the NOTICE file distributed with
 *  this work for additional information regarding copyright ownership.
 *  The ASF licenses this file to You under the Apache License, Version 2.0
 *  (the "License"); you may not use this file except in compliance with
 *  the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package java.math

import java.util.Arrays
import scala.annotation.tailrec

object BigDecimal {

  final val ZERO = new BigDecimal(0, 0)

  final val ONE = new BigDecimal(1, 0)

  final val TEN = new BigDecimal(10, 0)

  final val ROUND_UP = 0

  final val ROUND_DOWN = 1

  final val ROUND_CEILING = 2

  final val ROUND_FLOOR = 3

  final val ROUND_HALF_UP = 4

  final val ROUND_HALF_DOWN = 5

  final val ROUND_HALF_EVEN = 6

  final val ROUND_UNNECESSARY = 7

  /** The double closest to {@code Log10(2)}. */
  private final val Log2 = 0.3010299956639812

  private final val LongFivePows = newArrayOfPows(28, 5)

  private final val LongFivePowsBitLength =
    Array.tabulate[Int](LongFivePows.length)(i => bitLength(LongFivePows(i)))

  /** An array of longs with powers of ten.
   *
   *  An array with powers of ten that fit in the type <code>long</code>
   *  (<code>10^0,10^1,...,10^18</code>).
   */
  private[math] final val LongTenPows = newArrayOfPows(19, 10)

  private final val LongTenPowsBitLength =
    Array.tabulate[Int](LongTenPows.length)(i => bitLength(LongTenPows(i)))

  private final val BigIntScaledByZeroLength = 11

  /** An array with the first <code>BigInteger</code> scaled by zero.
   *
   *  (<code>[0,0],[1,0],...,[10,0]</code>).
   */
  private final val BigIntScaledByZero =
    Array.tabulate[BigDecimal](BigIntScaledByZeroLength)(new BigDecimal(_, 0))

  /** An array with the zero number scaled by the first positive scales.
   *
   *  (<code>0*10^0, 0*10^1, ..., 0*10^10</code>).
   */
  private final val ZeroScaledBy =
    Array.tabulate[BigDecimal](BigIntScaledByZeroLength)(new BigDecimal(0, _))

  /** An array filled with characters <code>'0'</code>. */
  private final val CharZeros = Array.fill[Char](100)('0')

  def valueOf(unscaledVal: Long, scale: Int): BigDecimal = {
    if (scale == 0)
      valueOf(unscaledVal)
    else if (unscaledVal == 0 && scale >= 0 && scale < ZeroScaledBy.length)
      ZeroScaledBy(scale)
    else
      new BigDecimal(unscaledVal, scale)
  }

  def valueOf(unscaledVal: Long): BigDecimal = {
    if (unscaledVal >= 0 && unscaledVal < BigIntScaledByZeroLength)
      BigIntScaledByZero(unscaledVal.toInt)
    else
      new BigDecimal(unscaledVal, 0)
  }

  def valueOf(d: Double): BigDecimal = {
    if (d.isInfinite || d.isNaN)
      throw new NumberFormatException("Infinity or NaN: " + d)

    new BigDecimal(d.toString)
  }

  private def addAndMult10(thisValue: BigDecimal, augend: BigDecimal,
      diffScale: Int): BigDecimal = {
    def powLen = LongTenPowsBitLength(diffScale)
    def augPlusPowLength = augend._bitLength + powLen
    def maxLen = Math.max(thisValue._bitLength, augPlusPowLength) + 1

    if (diffScale < LongTenPows.length && maxLen < 64) {
      val augPlusPowLength = augend._smallValue * LongTenPows(diffScale)
      val unscaled = thisValue._smallValue + augPlusPowLength
      valueOf(unscaled, thisValue._scale)
    } else {
      val bi = Multiplication.multiplyByTenPow(augend.getUnscaledValue(), diffScale)
      new BigDecimal(thisValue.getUnscaledValue().add(bi), thisValue.scale)
    }
  }

  private def divideBigIntegers(scaledDividend: BigInteger, scaledDivisor: BigInteger,
      scale: Int, roundingMode: RoundingMode): BigDecimal = {
    val qr = scaledDividend.divideAndRemainderImpl(scaledDivisor)
    // If after division there is a remainder...

    if (qr.rem.signum() == 0) {
      new BigDecimal(qr.quot, scale)
    } else {
      val sign = scaledDividend.signum() * scaledDivisor.signum()
      // 'compare to remainder'
      val compRem: Int = {
        val parityBit = if (qr.quot.testBit(0)) 1 else 0
        if (scaledDivisor.bitLength() < 63) {
          // 63 in order to avoid out of long after *2
          val rem = qr.rem.longValue()
          val divisor = scaledDivisor.longValue()
          val compRem = longCompareTo(Math.abs(rem) * 2, Math.abs(divisor))
          // To look if there is a carry
          roundingBehavior(parityBit, sign * (5 + compRem), roundingMode)
        } else {
          // Checking if:  remainder * 2 >= scaledDivisor
          val compRem = qr.rem.abs().shiftLeftOneBit().compareTo(scaledDivisor.abs())
          roundingBehavior(parityBit, sign * (5 + compRem), roundingMode)
        }
      }

      if (compRem != 0) {
        if (qr.quot.bitLength() < 63) {
          valueOf(qr.quot.longValue() + compRem, scale)
        } else {
          val quotient2 = qr.quot.add(BigInteger.valueOf(compRem))
          new BigDecimal(quotient2, scale)
        }
      } else {
        // Constructing the result with the appropriate unscaled value
        new BigDecimal(qr.quot, scale)
      }
    }
  }

  private def dividePrimitiveLongs(scaledDividend: Long, scaledDivisor: Long,
      scale: Int, roundingMode: RoundingMode): BigDecimal = {
    import java.lang.{Long => JLong}

    val remainder = scaledDividend % scaledDivisor
    val sign = JLong.signum(scaledDividend) * JLong.signum(scaledDivisor)
    val quotient = {
      val q = scaledDividend / scaledDivisor
      if (remainder != 0) {
        // Checking if:  remainder * 2 >= scaledDivisor
        val compRem = longCompareTo(Math.abs(remainder) * 2, Math.abs(scaledDivisor))
        // To look if there is a carry
        q + roundingBehavior(q.toInt & 1, sign * (5 + compRem), roundingMode)
      } else {
        q
      }
    }

    // Constructing the result with the appropriate unscaled value
    valueOf(quotient, scale)
  }

  private def longCompareTo(value1: Long, value2: Long): Int = {
    if (value1 > value2) 1
    else if (value1 < value2) -1
    else 0
  }

  private[math] def newArrayOfPows(len: Int, pow: Int): Array[Long] =
    new Array[Long](len - 1).scanLeft[Long, Array[Long]](1)((z, e) => z * pow)

  /** Return an increment that can be -1,0 or 1, depending on {@code roundingMode}.
   *
   *  @param parityBit can be 0 or 1, it's only used in the case {@code HALF_EVEN}
   *  @param fraction the mantissa to be analyzed
   *  @param roundingMode the type of rounding
   *  @return the carry propagated after rounding.
   */
  private def roundingBehavior(parityBit: Int, fraction: Int,
      roundingMode: RoundingMode): Int = {
    import RoundingMode._

    val absFraction = Math.abs(fraction)
    val sigFraction = java.lang.Integer.signum(fraction)
    // the carry after rounding
    roundingMode match {
      case UP          => sigFraction
      case DOWN        => 0
      case CEILING     => Math.max(sigFraction, 0)
      case FLOOR       => Math.min(sigFraction, 0)
      case HALF_UP     => if (absFraction >= 5) sigFraction else 0
      case HALF_DOWN   => if (absFraction > 5) sigFraction else 0
      case HALF_EVEN   => if (absFraction + parityBit > 5) sigFraction else 0
      case UNNECESSARY =>
        if (fraction == 0) 0
        else throw new ArithmeticException("Rounding necessary")
    }
  }

  private def safeLongToInt(longValue: Long): Int = {
    if (longValue < Int.MinValue || longValue > Int.MaxValue)
      throw new ArithmeticException("Out of int range: " + longValue)

    longValue.toInt
  }

  /** The value 0 with the most approximated scale of type {@code int}.
   *
   *  If {@code longScale > Integer.MAX_VALUE} the scale will be
   *  {@code Integer.MAX_VALUE}.
   *
   *  If {@code longScale < Integer.MIN_VALUE} the scale will be
   *  {@code Integer.MIN_VALUE}.
   *
   *  Otherwise {@code longScale} is casted to the type {@code int}.
   *
   *  @param longScale the scale to which the value 0 will be scaled.
   *  @return the value 0 scaled by the closer scale of type {@code int}.
   *  @see #scale
   */
  private def zeroScaledBy(longScale: Long): BigDecimal = {
    if (longScale == longScale.toInt)
      valueOf(0, longScale.toInt)
    else if (longScale >= 0)
      new BigDecimal(0, Int.MaxValue)
    else
      new BigDecimal(0, Int.MinValue)
  }

  protected def bitLength(sValue: Long): Int = {
    val smallValue = if (sValue < 0) ~sValue else sValue
    64 - java.lang.Long.numberOfLeadingZeros(smallValue)
  }

  private def bitLength(sValue: Int): Int = {
    val smallValue = if (sValue < 0) ~sValue else sValue
    32 - java.lang.Integer.numberOfLeadingZeros(smallValue)
  }

  @inline
  private def charNotEqualTo(c: Char, cs: Char*): Boolean = !cs.contains(c)

  @inline
  private def charEqualTo(c: Char, cs: Char*): Boolean = cs.contains(c)

  @inline
  private def insertString(s: String, pos: Int, s2: String): String =
    s.substring(0, pos) + s2 + s.substring(pos)

  @inline
  private def insertString(s: String, pos: Int, s2: String, s2Start: Int,
      s2Len: Int): String = {
    insertString(s, pos, s2.substring(s2Start, s2Start + s2Len))
  }

  private implicit class StringOps(val s: String) extends AnyVal {
    @inline
    def insert(pos: Int, s2: String): String = insertString(s, pos, s2)

    @inline
    def insert(pos: Int, s2: String, s2Start: Int, s2Len: Int): String =
      insertString(s, pos, s2, s2Start, s2Len)
  }

  @inline
  private final class QuotAndRem(val quot: BigDecimal, val rem: BigDecimal) {
    def toArray(): Array[BigDecimal] = Array[BigDecimal](quot, rem)
  }
}

class BigDecimal() extends Number with Comparable[BigDecimal] {
  import BigDecimal._
  import Multiplication._

  /** The <code>String</code> representation is cached. */
  private var _toStringImage: String = null

  /** Cache for the hash code. */
  private var _hashCode: Int = 0

  /** The internal representation of {@code BigDecimal}.
   *
   *  The arbitrary precision integer (unscaled value) in the internal
   *  representation of {@code BigDecimal}.
   */
  private var _intVal: BigInteger = _

  private var _bitLength: Int = 0

  private var _smallValue: Long = 0

  /** The 32-bit integer scale in the internal representation of {@code BigDecimal}. */
  private var _scale: Int = 0

  /** Represent the number of decimal digits in the unscaled value.
   *
   *  This precision is calculated the first time, and used in the following calls
   *  of method <code>precision()</code>. Note that some call to the private
   *  method <code>inplaceRound()</code> could update this field.
   *
   *  @see #precision()
   *  @see #inplaceRound(MathContext)
   */
  private var _precision: Int = 0

  private def this(smallValue: Long, scale: Int) = {
    this()
    _smallValue = smallValue
    _scale = scale
    _bitLength = bitLength(smallValue)
  }

  private def this(smallValue: Int, scale: Int) = {
    this()
    _smallValue = smallValue
    _scale = scale
    _bitLength = bitLength(smallValue)
  }

  def this(in: Array[Char], offset: Int, len: Int) = {
    this()

    val last = offset + len - 1 // last index to be copied

    if (in == null)
      throw new NullPointerException("in == null")

    if (last >= in.length || offset < 0 || len <= 0 || last < 0) {
      throw new NumberFormatException(
          s"Bad offset/length: offset=${offset} len=$len in.length=${in.length}")
    }

    var index = offset
    // To skip a possible '+' symbol
    if (offset <= last && in(offset) == '+') {
      index += 1
      // Fail if the next character is another sign.
      if (index < last && charEqualTo(in(index), '+', '-'))
        throw new NumberFormatException("For input string: " + in.toString)
    } else {
      // check that '-' is not followed by another sign
      val isMinus = index <= last && in(index) == '-'
      val nextIsSign = index + 1 < last && charEqualTo(in(index + 1), '+', '-')
      if (isMinus && nextIsSign)
        throw new NumberFormatException("For input string: " + in.toString)
    }

    val begin = index   // first index to be copied
    var counter = 0
    var wasNonZero = false
    // Accumulating all digits until a possible decimal point
    while (index <= last && charNotEqualTo(in(index), '.', 'e', 'E')) {
      if (!wasNonZero) {
        if (in(index) == '0') counter += 1
        else wasNonZero = true
      }
      index += 1
    }

    val (unscaled, bufLength) = {
      val u = in.subSequence(begin, index).toString
      val b = index - begin
      // A decimal point was found
      if ((index <= last) && (in(index) == '.')) {
        index += 1
        // Accumulating all digits until a possible exponent
        val begin = index
        while (index <= last && charNotEqualTo(in(index), 'e', 'E')) {
          if (!wasNonZero) {
            if (in(index) == '0') counter += 1
            else wasNonZero = true
          }
          index += 1
        }
        _scale = index - begin
        (u + in.subSequence(begin, begin + _scale).toString, b + _scale)
      } else {
        _scale = 0
        (u,b)
      }
    }

    // An exponent was found
    if ((index <= last) && charEqualTo(in(index), 'e', 'E')) {
      index += 1
      // Checking for a possible sign of scale
      val indexIsPlus = index <= last && in(index) == '+'
      val nextIsNotMinus = (index + 1) <= last && in(index + 1) != '-'
      val begin = if (indexIsPlus && nextIsNotMinus) index + 1 else index

      // Accumulating all remaining digits
      val scaleString = String.valueOf(in, begin, last + 1 - begin)
      // Checking if the scale is defined
      val newScale: Long = _scale.toLong - java.lang.Integer.parseInt(scaleString)
      _scale = newScale.toInt
      if (newScale != _scale)
        throw new NumberFormatException("Scale out of range")
    }
    // Parsing the unscaled value
    if (bufLength < 19) {
      _smallValue = java.lang.Long.parseLong(unscaled)
      _bitLength = bitLength(_smallValue)
    } else {
      setUnscaledValue(new BigInteger(unscaled))
    }
  }

  def this(in: Array[Char], offset: Int, len: Int, mc: MathContext) = {
    this(in, offset, len)
    inplaceRound(mc)
  }

  def this(in: Array[Char]) = {
    this(in, 0, in.length)
  }

  def this(in: Array[Char], mc: MathContext) = {
    this(in, 0, in.length)
    inplaceRound(mc)
  }

  def this(sVal: String) = {
    this(sVal.toCharArray(), 0, sVal.length)
  }

  def this(sVal: String, mc: MathContext) = {
    this(sVal.toCharArray(), 0, sVal.length)
    inplaceRound(mc)
  }

  def this(dVal: Double) = {
    this()
    if (dVal.isInfinite || dVal.isNaN)
      throw new NumberFormatException("Infinity or NaN: " + dVal)

    val bits = java.lang.Double.doubleToLongBits(dVal)
    // Extracting the exponent, note that the bias is 1023
    _scale = 1075 - ((bits >> 52) & 2047).toInt
    // Extracting the 52 bits of the mantissa.
    val mantissa =
      if (_scale == 1075) (bits & 0xFFFFFFFFFFFFFL) << 1
      else (bits & 0xFFFFFFFFFFFFFL) | 0x10000000000000L

    if (mantissa == 0) {
      _scale = 0
      _precision = 1
    }
    // To simplify all factors '2' in the mantissa
    val mantissa2 = {
      if (_scale > 0) {
        val trailingZeros = Math.min(_scale, java.lang.Long.numberOfTrailingZeros(mantissa))
        _scale -= trailingZeros
        mantissa >>> trailingZeros
      } else {
        mantissa
      }
    }
    // Calculating the new unscaled value and the new scale
    val mantissa3 = if ((bits >> 63) != 0) -mantissa2 else mantissa2
    val mantissaBits = bitLength(mantissa3)
    if (_scale < 0) {
      _bitLength = if (mantissaBits == 0) 0 else mantissaBits - _scale
      if (_bitLength < 64)
        _smallValue = mantissa3 << (-_scale)
      else
        _intVal = new BigInteger(1, mantissa3).shiftLeft(-_scale)
      _scale = 0
    } else if (_scale > 0) {
      def mSum = mantissaBits + LongFivePowsBitLength(_scale)
      if (_scale < LongFivePows.length && mSum < 64) {
        _smallValue = mantissa3 * LongFivePows(_scale)
        _bitLength = bitLength(_smallValue)
      } else {
        setUnscaledValue(multiplyByFivePow(BigInteger.valueOf(mantissa3), _scale))
      }
    } else {
      _smallValue = mantissa3
      _bitLength = mantissaBits
    }
  }

  def this(dVal: Double, mc: MathContext) = {
    this(dVal)
    inplaceRound(mc)
  }

  def this(unscaledVal: BigInteger, scale: Int) = {
    this()
    if (unscaledVal == null)
      throw new NullPointerException("unscaledVal == null")

    _scale = scale
    setUnscaledValue(unscaledVal)
  }

  def this(bi: BigInteger) = {
    this(bi, 0)
  }

  def this(bi: BigInteger, mc: MathContext) = {
    this(bi)
    inplaceRound(mc)
  }

  def this(unscaledVal: BigInteger, scale: Int, mc: MathContext) = {
    this(unscaledVal, scale)
    inplaceRound(mc)
  }

  def this(iVal: Int) = {
    this(iVal, 0)
  }

  def this(iVal: Int, mc: MathContext) = {
    this(iVal, 0)
    inplaceRound(mc)
  }

  def this(lVal: Long) = {
    this(lVal, 0)
  }

  def this(lVal: Long, mc: MathContext) = {
    this(lVal)
    inplaceRound(mc)
  }

  def add(augend: BigDecimal): BigDecimal = {
    val diffScale = this._scale - augend._scale
    // Fast return when some operand is zero
    if (this.isZero && diffScale <= 0) {
      augend
    } else if (augend.isZero && (this.isZero || diffScale >= 0)) {
      this
    } else if (diffScale == 0) {
      if (Math.max(this._bitLength, augend._bitLength) + 1 < 64)
        valueOf(this._smallValue + augend._smallValue, this._scale)
      else
        new BigDecimal(this.getUnscaledValue.add(augend.getUnscaledValue), this._scale)
    } else if (diffScale > 0) {
        addAndMult10(this, augend, diffScale)
    } else {
        addAndMult10(augend, this, -diffScale)
    }
  }

  def add(augend: BigDecimal, mc: MathContext): BigDecimal = {
    // scalastyle:off return
    if (augend.isZero || this.isZero || mc.precision == 0) {
      add(augend).round(mc)
    } else {
      val diffScale = this._scale.toLong - augend._scale
      // Cases where there is room for optimizations
      val (larger, smaller) =
        if (this.approxPrecision() < diffScale - 1) (augend, this)
        else if (augend.approxPrecision() < -diffScale - 1) (this, augend)
        else return add(augend).round(mc) // No optimization is done

      if (mc.precision >= larger.approxPrecision())
        return add(augend).round(mc)// No optimization is done

      // Cases where it's unnecessary to add two numbers with very different scales
      val largerSignum = larger.signum()
      val tempBI: BigInteger = {
        val biLarger = BigInteger.valueOf(largerSignum)
        if (largerSignum == smaller.signum()) {
          multiplyByPosInt(larger.getUnscaledValue, 10).add(biLarger)
        } else {
          val tempBI2 = larger.getUnscaledValue.subtract(biLarger)
          multiplyByPosInt(tempBI2, 10).add(BigInteger.valueOf(largerSignum * 9))
        }
      }
      // Rounding the improved adding
      val result = new BigDecimal(tempBI, larger._scale + 1)
      result.round(mc)
    }
    // scalastyle:on return
  }

  def subtract(subtrahend: BigDecimal): BigDecimal = {
    val diffScale = _scale - subtrahend._scale
    // Fast return when some operand is zero

    if (this.isZero && diffScale <= 0) {
      subtrahend.negate()
    } else if (subtrahend.isZero && (this.isZero || diffScale >= 0)) {
      this
    } else if (diffScale == 0) {
      if (Math.max(this._bitLength, subtrahend._bitLength) + 1 < 64)
        valueOf(this._smallValue - subtrahend._smallValue, this._scale)
      else
        new BigDecimal(getUnscaledValue.subtract(subtrahend.getUnscaledValue), _scale)
    } else if (diffScale > 0) {
      def powTenLen = LongTenPowsBitLength(diffScale)
      def maxLen = Math.max(this._bitLength, subtrahend._bitLength + powTenLen) + 1

      if (diffScale < LongTenPows.length && maxLen < 64) {
        val powTen = LongTenPows(diffScale)
        valueOf(this._smallValue - subtrahend._smallValue * powTen, this._scale)
      } else {
        val mult = multiplyByTenPow(subtrahend.getUnscaledValue, diffScale)
        new BigDecimal(getUnscaledValue.subtract(mult), this._scale)
      }
    } else {
      val negDiffScale = -diffScale
      def powTenLen = LongTenPowsBitLength(negDiffScale)
      def maxLen = Math.max(this._bitLength + powTenLen, subtrahend._bitLength) + 1

      if (negDiffScale < LongTenPows.length && maxLen < 64) {
        val powTen = LongTenPows(negDiffScale)
        valueOf(_smallValue * powTen - subtrahend._smallValue, subtrahend._scale)
      } else {
        val mult = multiplyByTenPow(this.getUnscaledValue, negDiffScale)
        val multSub = mult.subtract(subtrahend.getUnscaledValue)
        new BigDecimal(multSub, subtrahend._scale)
      }
    }
  }

  def subtract(subtrahend: BigDecimal, mc: MathContext): BigDecimal = {
    val diffScale = subtrahend._scale - this._scale.toLong
    val precLessDiff = subtrahend.approxPrecision() < diffScale - 1
     // Some operand is zero or the precision is infinity
    if (subtrahend.isZero || this.isZero || mc.precision == 0) {
      subtract(subtrahend).round(mc)
    } else if (precLessDiff && (mc.precision < this.approxPrecision())) {
      // Cases where it is unnecessary to subtract two numbers with very different scales
      val thisSignum: Int = this.signum()
      val biSignum = BigInteger.valueOf(thisSignum)
      val tempBI: BigInteger = {
        if (thisSignum != subtrahend.signum()) {
          multiplyByPosInt(getUnscaledValue, 10).add(biSignum)
        } else {
          val bi = this.getUnscaledValue.subtract(biSignum)
          multiplyByPosInt(bi, 10).add(BigInteger.valueOf(thisSignum * 9))
        }
      }
      new BigDecimal(tempBI, this._scale + 1).round(mc)
    } else {
      // No optimization is done
      subtract(subtrahend).round(mc)
    }
  }

  def multiply(multiplicand: BigDecimal): BigDecimal = {
    val newScale = this._scale.toLong + multiplicand._scale
    if (this.isZero || multiplicand.isZero) {
      zeroScaledBy(newScale)
    } else if (this._bitLength + multiplicand._bitLength < 64) {
      val smallResult = this._smallValue * multiplicand._smallValue
      if (smallResult == Long.MinValue &&
          this._smallValue < 0L && multiplicand._smallValue < 0L) {
        // Corner case #2587: the result should be -Long.MinValue
        new BigDecimal(BigInteger.getPowerOfTwo(63), safeLongToInt(newScale))
      } else {
        valueOf(smallResult, safeLongToInt(newScale))
      }
    } else {
      val unscaled = this.getUnscaledValue.multiply(multiplicand.getUnscaledValue)
      new BigDecimal(unscaled, safeLongToInt(newScale))
    }
  }

  def multiply(multiplicand: BigDecimal, mc: MathContext): BigDecimal = {
    val result = multiply(multiplicand)
    result.inplaceRound(mc)
    result
  }

  def divide(divisor: BigDecimal, scale: Int, roundingMode: Int): BigDecimal =
    divide(divisor, scale, RoundingMode.valueOf(roundingMode))

  def divide(divisor: BigDecimal, scale: Int, roundingMode: RoundingMode): BigDecimal = {
    if (roundingMode == null)
      throw new NullPointerException("roundingMode == null")
    else if (divisor.isZero)
      throw new ArithmeticException("Division by zero")

    val diffScale = {
      val diffScaleLong = (this._scale.toLong - divisor._scale) - scale
      val diffScale = diffScaleLong.toInt

      // Check whether the diffScale will fit into an Int
      if (diffScale.toLong != diffScaleLong) {
        throw new ArithmeticException(
            s"Unable to scale as difference is too big ($diffScaleLong)")
      }

      diffScale
    }

    def default(): BigDecimal = {
      val scaledDividend0 = this.getUnscaledValue
      val scaledDivisor0 = divisor.getUnscaledValue

      val (scaledDividend, scaledDivisor) =
        if (diffScale > 0)
          (scaledDividend0, multiplyByTenPow(scaledDivisor0, diffScale))
        else if (diffScale < 0)
          (multiplyByTenPow(scaledDividend0, -diffScale), scaledDivisor0)
        else
          (scaledDividend0, scaledDivisor0)

      divideBigIntegers(scaledDividend, scaledDivisor, scale, roundingMode)
    }

    if (this._bitLength < 64 && divisor._bitLength < 64) {
      val lptLen = LongTenPows.length

      if (diffScale == 0) {
        val div = divisor._smallValue
        dividePrimitiveLongs(_smallValue, div, scale, roundingMode)
      } else if (diffScale > 0) {
        if (diffScale < lptLen &&
            divisor._bitLength + LongTenPowsBitLength(diffScale) < 64) {
          val div = divisor._smallValue * LongTenPows(diffScale)
          dividePrimitiveLongs(_smallValue, div, scale, roundingMode)
        } else {
          default()
        }
      } else {
        if (diffScale > -lptLen && // `-diffScale < lptLen` without overflow
            this._bitLength + LongTenPowsBitLength(-diffScale) < 64) {
          val div = _smallValue * LongTenPows(-diffScale)
          dividePrimitiveLongs(div, divisor._smallValue, scale, roundingMode)
        } else {
          default()
        }
      }
    } else {
      default()
    }
  }

  def divide(divisor: BigDecimal, roundingMode: Int): BigDecimal =
    divide(divisor, _scale, RoundingMode.valueOf(roundingMode))

  def divide(divisor: BigDecimal, roundingMode: RoundingMode): BigDecimal =
    divide(divisor, _scale, roundingMode)

  def divide(divisor: BigDecimal): BigDecimal = {
    val thisUnscaled = this.getUnscaledValue
    val diffScale: Long = _scale.toLong - divisor._scale

    if (divisor.isZero) {
      throw new ArithmeticException("Division by zero")
    } else if (thisUnscaled.signum() == 0) {
      zeroScaledBy(diffScale)
    } else {
      val divisorUnscaled = divisor.getUnscaledValue
      val lastPow = BigFivePows.length - 1
      val gcd = thisUnscaled.gcd(divisorUnscaled) // To divide both by the GCD
      val p = thisUnscaled.divide(gcd)
      val q1 = divisorUnscaled.divide(gcd)
      // To simplify all "2" factors of q, dividing by 2^k
      val k = q1.getLowestSetBit // number of factors "2" in 'q'

      @inline
      @tailrec
      def loop(i: Int, q: BigInteger, l: Int): (BigInteger, Int) = {
        val qr = q.divideAndRemainderImpl(BigFivePows(i))
        if (qr.rem.signum() == 0)
          loop(if (i < lastPow) i + 1 else i, qr.quot, l + i)
        else if (i != 1)
          loop(1, q, l)
        else
          (q, l)
      }

      //q simplifies all "5" factors of q1, dividing by 5^l
      //l number of factors "5" in divisorUnscaled
      val (q, l) = loop(1, q1.shiftRight(k), 0)

      // If  abs(q) != 1  then the quotient is periodic
      if (q.abs() != BigInteger.ONE) {
        throw new ArithmeticException(
            "Non-terminating decimal expansion; no exact representable decimal result")
      }

      // The sign of the is fixed and the quotient will be saved in 'p2'
      val p2 = if (q.signum() < 0) p.negate() else p

      // Checking if the new scale is out of range
      val newScale = safeLongToInt(diffScale + Math.max(k, l))
      // k >= 0  and  l >= 0  implies that  k - l  is in the 32-bit range
      val i = k - l
      val p3 = if (i > 0) multiplyByFivePow(p2, i) else p2.shiftLeft(-i)
      new BigDecimal(p3, newScale)
    }
  }

  def divide(divisor: BigDecimal, mc: MathContext): BigDecimal = {
    // Calculating how many zeros must be append to 'dividend'
    // to obtain a  quotient with at least 'mc.precision()' digits

    // In special cases it reduces the problem to call the dual method
    if (mc.precision == 0 || this.isZero || divisor.isZero)
      return this.divide(divisor) // scalastyle:ignore

    val diffScale: Long = _scale.toLong - divisor._scale
    val trailingZeros = mc.precision + 2L + divisor.approxPrecision() - approxPrecision()

    val (quot, newScale0) = {
      if (trailingZeros > 0) {
        // To append trailing zeros at end of dividend
        val q  = getUnscaledValue.multiply(powerOf10(trailingZeros))
        (q, diffScale + trailingZeros)
      } else {
        (getUnscaledValue, diffScale)
      }
    }

    val qr = quot.divideAndRemainderImpl(divisor.getUnscaledValue)
    val (integerQuot, newScale) = {
      // Calculating the exact quotient with at least 'mc.precision()' digits
      if (qr.rem.signum() != 0) {
        // Checking if:   2 * remainder >= divisor ?
        val compRem = qr.rem.shiftLeftOneBit().compareTo(divisor.getUnscaledValue)
        val bi = BigInteger.valueOf(qr.quot.signum() * (5 + compRem))
        (qr.quot.multiply(BigInteger.TEN).add(bi), newScale0 + 1)
      } else {
        // To strip trailing zeros until the preferred scale is reached
        val lastPow = BigTenPows.length - 1

        @inline
        @tailrec
        def loop(i: Int, iq: BigInteger, scale: Long): (BigInteger, Long) = {
          if (!iq.testBit(0)) {
            val qr = iq.divideAndRemainderImpl(BigTenPows(i))
            if ((qr.rem.signum() == 0) && (scale - i >= diffScale))
              loop(if (i < lastPow) i + 1 else i, qr.quot, scale - i)
            else if (i != 1)
              loop(1, iq, scale)
            else
              (iq, scale)
          } else {
            (iq, scale)
          }
        }

        loop(1, qr.quot, newScale0)
      }
    }
    // To perform rounding
    new BigDecimal(integerQuot, safeLongToInt(newScale), mc)
  }

  def divideToIntegralValue(divisor: BigDecimal): BigDecimal = {
    if (divisor.isZero)
      throw new ArithmeticException("Division by zero")

    val newScale: Long = this._scale.toLong - divisor._scale
    val lastPow = BigTenPows.length - 1
    val (integralValue, varScale) = {
      if ((divisor.approxPrecision() + newScale > this.approxPrecision() + 1L) || this.isZero) {
        // If the divisor's integer part is greater than this's integer part,
        // the result must be zero with the appropriate scale
        (BigInteger.ZERO, 0L)
      } else if (newScale == 0) {
        (getUnscaledValue.divide(divisor.getUnscaledValue), 0L)
      } else if (newScale > 0) {
        val powerOfTen = powerOf10(newScale)
        val iv = getUnscaledValue.divide(divisor.getUnscaledValue.multiply(powerOfTen))
        (iv.multiply(powerOfTen), newScale)
      } else {
        // (newScale < 0)
        val powerOfTen = powerOf10(-newScale)
        val integralValue0 = getUnscaledValue.multiply(powerOfTen).divide(divisor.getUnscaledValue)

        // To strip trailing zeros approximating to the preferred scale
        @inline
        @tailrec
        def loop(i: Int, iv: BigInteger, vs: Long): (BigInteger, Long) = {
          if (!iv.testBit(0)) {
            val qr = iv.divideAndRemainderImpl(BigTenPows(i))
            if ((qr.rem.signum() == 0) && (vs - i >= newScale)) {
              loop(if (i < lastPow) i + 1 else i, qr.quot, vs - i)
            } else if (i != 1) {
              loop(1, iv, vs)
            } else {
              (iv, vs)
            }
          } else {
            (iv, vs)
          }
        }

        loop(1, integralValue0, 0)
      }
    }

    if (integralValue.signum() == 0) zeroScaledBy(varScale)
    else new BigDecimal(integralValue, safeLongToInt(varScale))
  }

  def divideToIntegralValue(divisor: BigDecimal, mc: MathContext): BigDecimal = {
    // scalastyle:off return
    val mcPrecision = mc.precision
    val diffPrecision = this.precision() - divisor.precision()
    val lastPow = BigTenPows.length - 1
    val diffScale = this._scale.toLong - divisor._scale
    val quotPrecision = diffPrecision - diffScale + 1

    // In special cases it call the dual method
    if (mcPrecision == 0 || this.isZero || divisor.isZero)
      return this.divideToIntegralValue(divisor)

    val (quot, newScale) = {
      if (quotPrecision <= 0) {
        (BigInteger.ZERO, diffScale)
      } else if (diffScale == 0) {
        (this.getUnscaledValue.divide(divisor.getUnscaledValue), diffScale)
      } else if (diffScale > 0) {
        val div = divisor.getUnscaledValue.multiply(powerOf10(diffScale))
        val q = this.getUnscaledValue.divide(div)
        // To chose  10^newScale  to get a quotient with at least 'mc.precision()' digits
        val ns = Math.min(diffScale, Math.max(mcPrecision - quotPrecision + 1, 0))
        (q.multiply(powerOf10(ns)), ns)
      } else {
        /* To calculate the minimum power of ten, such that the quotient
         * (u1 * 10^exp) / u2   has at least 'mc.precision()' digits. */
        val exp = Math.min(-diffScale, Math.max(mcPrecision.toLong - diffPrecision, 0))
        val mult = this.getUnscaledValue.multiply(powerOf10(exp))
        val qr = mult.divideAndRemainderImpl(divisor.getUnscaledValue)
        val ns = diffScale + exp // To fix the scale
        val exp2 = -ns // The remaining power of ten
        // If after division there is a remainder...
        if ((qr.rem.signum() != 0) && (exp2 > 0)) {
          val bi = new BigDecimal(qr.rem)
          val compRemDiv0 = bi.precision() + exp2 - divisor.precision()
          val compRemDiv = {
            if (compRemDiv0 == 0) {
              val bi = qr.rem.multiply(powerOf10(exp2))
              val rem = bi.divide(divisor.getUnscaledValue)
              Math.abs(rem.signum())
            } else {
              compRemDiv0
            }
          }
          if (compRemDiv > 0)
            throw new ArithmeticException("Division impossible")
        }
        (qr.quot, ns)
      }
    }

    // Fast return if the quotient is zero
    if (quot.signum() == 0)
      return zeroScaledBy(diffScale)

    val integralValue = new BigDecimal(quot)

    // To strip trailing zeros until the specified precision is reached
    @inline
    @tailrec
    def loop(i: Int, ns: Long, q: BigInteger, prec: Int): (Long, BigInteger, Int) = {
      if (!q.testBit(0)) {
        val qr = q.divideAndRemainderImpl(BigTenPows(i))
        val cond1 = {
          (qr.rem.signum() == 0) &&
          ((prec - i >= mcPrecision) || (ns - i >= diffScale))
        }

        if (cond1) loop(if (i < lastPow) i + 1 else i, ns - i, qr.quot, prec - i)
        else if (i != 1) loop(1, ns, q, prec)
        else (ns, q, prec)
      } else {
        (ns, q, prec)
      }
    }

    val (finalScale, strippedBI, resultPrecision) =
      loop(1, newScale, quot, integralValue.precision())

    // To check if the result fit in 'mc.precision()' digits
    if (resultPrecision > mcPrecision)
      throw new ArithmeticException("Division impossible")

    integralValue._scale = safeLongToInt(finalScale)
    integralValue.setUnscaledValue(strippedBI)
    integralValue
    // scalastyle:on return
  }

  def remainder(divisor: BigDecimal): BigDecimal =
    divideAndRemainderImpl(divisor).rem

  def remainder(divisor: BigDecimal, mc: MathContext): BigDecimal =
    divideAndRemainderImpl(divisor, mc).rem

  def divideAndRemainder(divisor: BigDecimal): Array[BigDecimal] =
    divideAndRemainderImpl(divisor).toArray()

  def divideAndRemainder(divisor: BigDecimal, mc: MathContext): Array[BigDecimal] =
    divideAndRemainderImpl(divisor, mc).toArray()

  def pow(n: Int): BigDecimal = {
    if (n == 0) {
      ONE
    } else if (n < 0 || n > 999999999) {
      throw new ArithmeticException("Invalid operation")
    } else {
      val newScale = _scale * n.toLong
      if (isZero) zeroScaledBy(newScale)
      else new BigDecimal(getUnscaledValue.pow(n), safeLongToInt(newScale))
    }
  }

  def pow(n: Int, mc: MathContext): BigDecimal = {
    val m = Math.abs(n)
    val mcPrec = mc.precision
    val elength = Math.log10(m).toInt + 1
    val mcError = mcPrec > 0 && elength > mcPrec

    // In particular cases, it reduces the problem to call the other 'pow()'
    if (n == 0 || (isZero && n > 0)) {
      pow(n)
    } else if (m > 999999999 || (mcPrec == 0 && n < 0) || mcError) {
      throw new ArithmeticException("Invalid operation")
    } else {
      val newPrecision = {
        if (mcPrec > 0) new MathContext(mcPrec + elength + 1, mc.roundingMode)
        else mc
      }
      // The result is calculated as if 'n' were positive
      var accum: BigDecimal = round(newPrecision)
      var oneBitMask: Int = java.lang.Integer.highestOneBit(m) >> 1

      while (oneBitMask > 0) {
        accum = accum.multiply(accum, newPrecision)
        if ((m & oneBitMask) == oneBitMask)
          accum = accum.multiply(this, newPrecision)
        oneBitMask >>= 1
      }
      // If 'n' is negative, the value is divided into 'ONE'
      if (n < 0)
        accum = ONE.divide(accum, newPrecision)
      // The final value is rounded to the destination precision
      accum.inplaceRound(mc)
      accum
    }
  }

  def abs(): BigDecimal = {
    if (signum() < 0) negate()
    else this
  }

  def abs(mc: MathContext): BigDecimal = {
    val result =
      if (signum() < 0) negate()
      else new BigDecimal(getUnscaledValue, _scale)
    result.inplaceRound(mc)
    result
  }

  def negate(): BigDecimal = {
    if (_bitLength < 63 || (_bitLength == 63 && _smallValue != Long.MinValue))
      valueOf(-_smallValue, _scale)
    else
      new BigDecimal(getUnscaledValue.negate(), _scale)
  }

  def negate(mc: MathContext): BigDecimal = {
    val result = negate()
    result.inplaceRound(mc)
    result
  }

  def plus(): BigDecimal = this

  def plus(mc: MathContext): BigDecimal = round(mc)

  def signum(): Int = {
    if (_bitLength < 64) {
      if (_smallValue < 0) -1
      else if (_smallValue > 0) 1
      else 0
    } else {
      getUnscaledValue().signum()
    }
  }

  def precision(): Int = {
    if (_precision == 0) {
      _precision = {
        if (_bitLength == 0) {
          1
        } else if (_bitLength < 64) {
          decimalDigitsInLong(_smallValue)
        } else {
          val decimalDigits = 1 + ((_bitLength - 1) * Log2).toInt
          // If after division the number isn't zero, there exists an additional digit
          if (getUnscaledValue.divide(powerOf10(decimalDigits)).signum() != 0)
            decimalDigits + 1
          else
            decimalDigits
        }
      }
    }
    _precision
  }

  def unscaledValue(): BigInteger = getUnscaledValue

  def round(mc: MathContext): BigDecimal = {
    val thisBD = new BigDecimal(getUnscaledValue, _scale)
    thisBD.inplaceRound(mc)
    thisBD
  }

  def setScale(newScale: Int, roundingMode: RoundingMode): BigDecimal = {
    if (roundingMode == null)
      throw new NullPointerException("roundingMode == null")

    val diffScale = newScale - _scale.toLong
    if (diffScale == 0) {
      this
    } else if (diffScale > 0) {
      def cmp = this._bitLength + LongTenPowsBitLength(diffScale.toInt)
      if (diffScale < LongTenPows.length && cmp < 64) {
        valueOf(this._smallValue * LongTenPows(diffScale.toInt), newScale)
      } else {
        new BigDecimal(multiplyByTenPow(getUnscaledValue, diffScale.toInt), newScale)
      }
    } else if (this._bitLength < 64 && -diffScale < LongTenPows.length) {
      val lpt = LongTenPows(-diffScale.toInt)
      dividePrimitiveLongs(this._smallValue, lpt, newScale, roundingMode)
    } else {
      val powTen = powerOf10(-diffScale)
      divideBigIntegers(this.getUnscaledValue, powTen, newScale, roundingMode)
    }
  }

  def scale(): Int = _scale

  def setScale(newScale: Int, roundingMode: Int): BigDecimal =
    setScale(newScale, RoundingMode.valueOf(roundingMode))

  def setScale(newScale: Int): BigDecimal =
    setScale(newScale, RoundingMode.UNNECESSARY)

  def movePointLeft(n: Int): BigDecimal = movePoint(_scale + n.toLong)

  def movePointRight(n: Int): BigDecimal = movePoint(_scale - n.toLong)

  def scaleByPowerOfTen(n: Int): BigDecimal = {
    val newScale = _scale - n.toLong
    if (_bitLength < 64) {
      //Taking care when a 0 is to be scaled
      if (_smallValue == 0) zeroScaledBy(newScale)
      else valueOf(_smallValue, safeLongToInt(newScale))
    } else {
      new BigDecimal(getUnscaledValue, safeLongToInt(newScale))
    }
  }

  def stripTrailingZeros(): BigDecimal = {
    if (isZero) {
      // Preserve RI compatibility, so BigDecimal.equals (which checks
      // value *and* scale) continues to work.
      this
    } else {
      val lastPow = BigTenPows.length - 1

      // while the number is even...
      @inline
      @tailrec
      def loop(i: Int, strippedBI: BigInteger, scale: Long): (BigInteger, Long) = {
        if (!strippedBI.testBit(0)) {
          // To divide by 10^i
          val qr = strippedBI.divideAndRemainderImpl(BigTenPows(i))
          // To look the remainder
          if (qr.rem.signum() == 0) // To adjust the scale
            loop(if (i < lastPow) i + 1 else i, qr.quot, scale - i)
          else if (i != 1)
            loop(1, strippedBI, scale)
          else
            (strippedBI, scale)
        } else {
          (strippedBI, scale)
        }
      }

      val (strippedBI, newScale) = loop(1, getUnscaledValue, _scale)
      new BigDecimal(strippedBI, safeLongToInt(newScale))
    }
  }

  def compareTo(bi: BigDecimal): Int = {
    val thisSign = signum()
    val valueSign = bi.signum()
    if (thisSign == valueSign) {
      if (this._scale == bi._scale && this._bitLength < 64 && bi._bitLength < 64) {
        if (_smallValue < bi._smallValue) -1
        else if (_smallValue > bi._smallValue) 1
        else 0
      } else {
        val diffScale = this._scale.toLong - bi._scale
        val diffPrecision = this.approxPrecision() - bi.approxPrecision()
        if (diffPrecision > diffScale + 1) {
          thisSign
        } else if (diffPrecision < diffScale - 1) {
          -thisSign
        } else { // thisSign equals val.signum() and diffPrecision is approx. diffScale
          val (thisUnscaled, valUnscaled) = {
            val t = this.getUnscaledValue
            val v = bi.getUnscaledValue
            if (diffScale < 0)
              (t.multiply(powerOf10(-diffScale)), v)
            else if (diffScale > 0)
              (t, v.multiply(powerOf10(diffScale)))
            else
              (t, v)
          }
          thisUnscaled.compareTo(valUnscaled)
        }
      }
    } else if (thisSign < valueSign) {
      -1
    } else {
      1
    }
  }

  override def equals(x: Any): Boolean = x match {
    case that: BigDecimal =>
      that._scale == this._scale && (
          if (_bitLength < 64) that._smallValue == this._smallValue
          else this._intVal == that._intVal)
    case _ => false
  }

  def min(bd: BigDecimal): BigDecimal =
    if (compareTo(bd) <= 0) this
    else bd

  def max(bd: BigDecimal): BigDecimal =
    if (compareTo(bd) >= 0) this
    else bd

  override def hashCode(): Int = {
    if (_hashCode != 0) {
      _hashCode
    } else if (_bitLength < 64) {
      _hashCode = _smallValue.toInt
      _hashCode = 33 * _hashCode + (_smallValue >> 32).toInt
      _hashCode = 17 * _hashCode + _scale
      _hashCode
    } else {
      _hashCode = 17 * _intVal.hashCode + _scale
      _hashCode
    }
  }

  override def toString(): String = {
    if (_toStringImage != null) {
      _toStringImage
    } else if (_bitLength < 32) {
      _toStringImage = Conversion.toDecimalScaledString(_smallValue, _scale)
      _toStringImage
    } else {
      val intString: String = getUnscaledValue.toString
      if (_scale == 0) {
        intString
      } else {
        val begin = if (getUnscaledValue.signum() < 0) 2 else 1
        val end = intString.length
        val exponent: Long = -_scale.toLong + end - begin
        val result =
          if (_scale > 0 && exponent >= -6) {
            if (exponent >= 0) {
              intString.insert(end - _scale, ".")
            } else {
              intString.insert(begin - 1, "0.").insert(
                  begin + 1, CharZeros.mkString, 0, -exponent.toInt - 1)
            }
          } else {
            val r0 =
              if (end - begin >= 1) intString.insert(begin, ".")
              else intString
            val r1 = r0 + "E"
            val r2 = if (exponent > 0) r1 + "+" else r1
            r2 + java.lang.Long.toString(exponent)
          }
        _toStringImage = result
        _toStringImage
      }
    }
  }

  def toEngineeringString(): String = {
    val intString = getUnscaledValue.toString
    if (_scale == 0) {
      intString
    } else {
      val begin = if (getUnscaledValue.signum() < 0) 2 else 1
      var end = intString.length
      val exponent0: Long = -_scale.toLong + end - begin

      val result = {
        if ((_scale > 0) && (exponent0 >= -6)) {
          if (exponent0 >= 0) {
            intString.insert(end - _scale, ".")
          } else {
            intString.insert(begin - 1, "0.").insert(begin + 1,
                CharZeros.mkString, 0, -exponent0.toInt - 1)
          }
        } else {
          val delta = end - begin
          val rem = (exponent0 % 3).toInt
          var res = intString
          val (e, b) = {
            if (rem != 0) {
              val (rem1, exp, beg) = {
                if (getUnscaledValue.signum() == 0) {
                  val r = if (rem < 0) -rem else 3 - rem
                  (r, exponent0 + r, begin)
                } else {
                  val r = if (rem < 0) rem + 3 else rem
                  (r, exponent0 - r, begin + r)
                }
              }
              if (delta < 3) {
                for (i <- 0 until rem1 - delta) {
                  res += "0"
                  end += 1
                }
              }
              (exp, beg)
            } else {
              (exponent0, begin)
            }
          }
          if (end - b >= 1)
            res = res.insert(b, ".")
          if (e != 0) {
            res += "E"
            if (e > 0)
              res += "+"
            res += java.lang.Long.toString(e)
          }
          res
        }
      }
      result
    }
  }

  def toPlainString(): String = {
    val intStr = getUnscaledValue.toString
    if (_scale == 0 || (isZero && _scale < 0)) {
      intStr
    } else {
      val begin = if (signum() < 0) 1 else 0
      var delta = _scale
      // We take space for all digits, plus a possible decimal point, plus 'scale'
      var result = if (begin == 1) "-" else ""
      val charZerosStr = CharZeros.mkString

      if (_scale > 0) {
        delta -= intStr.length - begin
        if (delta >= 0) {
          result += "0."
          // To append zeros after the decimal point
          while (delta > CharZeros.length) {
            result += charZerosStr
            delta -= CharZeros.length
          }
          result += charZerosStr.substring(0, delta) + intStr.substring(begin)
        } else {
          delta = begin - delta
          result += intStr.substring(begin, delta) + "." + intStr.substring(delta)
        }
      } else { // (scale <= 0)
        result += intStr.substring(begin)
        // To append trailing zeros
        while (delta < -CharZeros.length) {
          result += charZerosStr
          delta += CharZeros.length
        }
        result += charZerosStr.substring(0, -delta)
      }
      result
    }
  }

  def toBigInteger(): BigInteger = {
    if (_scale == 0 || isZero)
      getUnscaledValue
    else if (_scale < 0)
      getUnscaledValue.multiply(powerOf10(-_scale.toLong))
    else
      getUnscaledValue.divide(powerOf10(_scale))
  }

  def toBigIntegerExact(): BigInteger = {
    if (_scale == 0 || isZero) {
      getUnscaledValue
    } else if (_scale < 0) {
      getUnscaledValue.multiply(powerOf10(-_scale.toLong))
    } else { // (scale > 0)
      // An optimization before do a heavy division
      if (_scale > approxPrecision() || _scale > getUnscaledValue.getLowestSetBit)
        throw new ArithmeticException("Rounding necessary")

      val integerAndFraction = getUnscaledValue.divideAndRemainder(powerOf10(_scale))
      if (integerAndFraction(1).signum() != 0) {
        // It exists a non-zero fractional part
        throw new ArithmeticException("Rounding necessary")
      }
      integerAndFraction(0)
    }
  }

  override def longValue(): Long = {
    /*
     * If scale <= -64 there are at least 64 trailing bits zero in
     * 10^(-scale). If the scale is positive and very large the long value
     * could be zero.
     */
    if (_scale <= -64 || _scale > approxPrecision()) 0L
    else toBigInteger().longValue()
  }

  def longValueExact(): Long = valueExact(64)

  override def intValue(): Int = {
    /*
     * If scale <= -32 there are at least 32 trailing bits zero in
     * 10^(-scale). If the scale is positive and very large the long value
     * could be zero.
     */
    if (_scale <= -32 || _scale > approxPrecision()) 0
    else toBigInteger().intValue()
  }

  def intValueExact(): Int = valueExact(32).toInt

  def shortValueExact(): Short = valueExact(16).toShort

  def byteValueExact(): Byte = valueExact(8).toByte

  override def floatValue(): Float = {
    /* A similar code like in doubleValue() could be repeated here,
     * but this simple implementation is quite efficient. */
    val powerOfTwo = this._bitLength - (_scale / Log2).toLong
    val floatResult0: Float = signum()
    val floatResult: Float = {
      if (powerOfTwo < -149 || floatResult0 == 0.0f) // 'this' is very small
        floatResult0 * 0.0f
      else if (powerOfTwo > 129) // 'this' is very large
        floatResult0 * Float.PositiveInfinity
      else
        doubleValue().toFloat
    }
    floatResult
  }

  override def doubleValue(): Double = {
    val sign = signum()
    val powerOfTwo = this._bitLength - (_scale / Log2).toLong

    if (powerOfTwo < -1074 || sign == 0) {
      // Cases which 'this' is very small
      sign * 0.0d
    } else if (powerOfTwo > 1025) {
      // Cases which 'this' is very large
      sign * Double.PositiveInfinity
    } else {
      val mantissa0 = getUnscaledValue.abs()
      var exponent = 1076  // bias + 53

      val mantissa = {
        if (_scale <= 0) {
          mantissa0.multiply(powerOf10(-_scale))
        } else {
          val powerOfTen: BigInteger = powerOf10(_scale)
          val k = 100 - powerOfTwo.toInt
          val m = {
            if (k > 0) {
              /* Computing (mantissa * 2^k) , where 'k' is a enough big
               * power of '2' to can divide by 10^s */
              exponent -= k
              mantissa0.shiftLeft(k)
            } else {
              mantissa0
            }
          }
          // Computing (mantissa * 2^k) / 10^s
          val qr = m.divideAndRemainderImpl(powerOfTen)
          // To check if the fractional part >= 0.5
          val compRem = qr.rem.shiftLeftOneBit().compareTo(powerOfTen)
          // To add two rounded bits at end of mantissa
          exponent -= 2
          qr.quot.shiftLeft(2).add(BigInteger.valueOf((compRem * (compRem + 3)) / 2 + 1))
        }
      }

      val lowestSetBit = mantissa.getLowestSetBit
      val discardedSize = mantissa.bitLength() - 54
      var bits: Long = 0L // IEEE-754 Standard
      var tempBits: Long = 0L // for temporal calculations
      if (discardedSize > 0) { // (#bits > 54)
        bits = mantissa.shiftRight(discardedSize).longValue()
        tempBits = bits
        if (((bits & 1) == 1 && lowestSetBit < discardedSize) || (bits & 3) == 3)
          bits += 2
      } else { // (#bits <= 54)
        bits = mantissa.longValue() << -discardedSize
        tempBits = bits
        if ((bits & 3) == 3)
          bits += 2
      }
      // Testing bit 54 to check if the carry creates a new binary digit
      if ((bits & 0x40000000000000L) == 0) {
        // To drop the last bit of mantissa (first discarded)
        bits >>= 1
        exponent += discardedSize
      } else {
        // #bits = 54
        bits >>= 2
        exponent += (discardedSize + 1)
      }
      // To test if the 53-bits number fits in 'double'
      if (exponent > 2046) {
        // (exponent - bias > 1023)
        sign * Double.PositiveInfinity
      } else if (exponent < -53) {
        sign * 0.0d
      } else {
        if (exponent <= 0) {
          bits = tempBits >> 1
          tempBits = bits & (-1L >>> (63 + exponent))
          bits >>= (-exponent)
          // To test if after discard bits, a new carry is generated
          if (((bits & 3) == 3) ||
              (((bits & 1) == 1) && (tempBits != 0) && (lowestSetBit < discardedSize))) {
            bits += 1
          }
          exponent = 0
          bits >>= 1
        }

        // Construct the 64 double bits: [sign(1), exponent(11), mantissa(52)]
        val resultBits =
          (sign & 0x8000000000000000L) |
          (exponent.toLong << 52)      |
          (bits & 0xFFFFFFFFFFFFFL)
        java.lang.Double.longBitsToDouble(resultBits)
      }
    }
  }

  def ulp(): BigDecimal = valueOf(1, _scale)

  private def decimalDigitsInLong(value: Long): Int = {
    if (value == Long.MinValue) {
      19 // special case required because abs(MIN_VALUE) == MIN_VALUE
    } else {
      val index = Arrays.binarySearch(LongTenPows, Math.abs(value))
      if (index < 0) -index - 1
      else index + 1
    }
  }

  @inline
  private def divideAndRemainderImpl(divisor: BigDecimal): QuotAndRem = {
    val quot = this.divideToIntegralValue(divisor)
    val rem = this.subtract(quot.multiply(divisor))
    new QuotAndRem(quot, rem)
  }

  @inline
  private def divideAndRemainderImpl(divisor: BigDecimal, mc: MathContext): QuotAndRem = {
    val quot = this.divideToIntegralValue(divisor, mc)
    val rem = this.subtract(quot.multiply(divisor))
    new QuotAndRem(quot, rem)
  }

  /** Performs in place rounding.
   *
   *  It does all rounding work of the public method
   *  {@code round(MathContext)}, performing an inplace rounding
   *  without creating a new object.
   *
   *  @param mc the {@code MathContext} for perform the rounding.
   *  @see #round(MathContext)
   */
  private def inplaceRound(mc: MathContext): Unit = {
    val mcPrecision = mc.precision
    val discardedPrecision = precision() - mcPrecision
    val mcPrecGood = approxPrecision() < mcPrecision || mcPrecision == 0
    if (mcPrecGood || discardedPrecision <= 0) {
      // do nothing
    } else if (this._bitLength < 64) {
      // When the number is small perform an efficient rounding
      smallRound(mc, discardedPrecision)
    } else {
      // Getting the integer part and the discarded fraction
      val sizeOfFraction: BigInteger = powerOf10(discardedPrecision)
      val integerAndFraction = getUnscaledValue.divideAndRemainder(sizeOfFraction)
      val newScale0 = _scale.toLong - discardedPrecision
      // If the discarded fraction is non-zero, perform rounding
      val newScale = {
        if (integerAndFraction(1).signum() != 0) {
          // To check if the discarded fraction >= 0.5
          val absBi = integerAndFraction(1).abs()
          val compRem = absBi.shiftLeftOneBit().compareTo(sizeOfFraction)
          // To look if there is a carry
          val parityBit = if (integerAndFraction(0).testBit(0)) 1 else 0
          val frac = integerAndFraction(1).signum() * (5 + compRem)
          val carry = roundingBehavior(parityBit, frac, mc.roundingMode)
          if (carry != 0) {
            val bi = BigInteger.valueOf(carry)
            integerAndFraction(0) = integerAndFraction(0).add(bi)
          }
          val tempBD: BigDecimal = new BigDecimal(integerAndFraction(0))
          // If after to add the increment the precision changed, we normalize the size
          if (tempBD.precision() > mcPrecision) {
            integerAndFraction(0) = integerAndFraction(0).divide(BigInteger.TEN)
            newScale0 - 1
          } else {
            newScale0
          }
        } else {
          newScale0
        }
      }
      // To update all internal fields
      _scale = safeLongToInt(newScale)
      _precision = mcPrecision
      setUnscaledValue(integerAndFraction(0))
    }
  }

  private def isZero(): Boolean = _bitLength == 0 && this._smallValue != -1

  private def movePoint(newScale: Long): BigDecimal = {
    def lptbLen = LongTenPowsBitLength(-newScale.toInt)

    if (isZero) {
      zeroScaledBy(Math.max(newScale, 0))
    } else if (newScale >= 0) {
      // When: 'n'== Integer.MIN_VALUE isn't possible to call to movePointRight(-n)
      // since -Integer.MIN_VALUE == Integer.MIN_VALUE
      if (_bitLength < 64) valueOf(_smallValue, safeLongToInt(newScale))
      else new BigDecimal(getUnscaledValue, safeLongToInt(newScale))
    } else if (-newScale < LongTenPows.length && _bitLength + lptbLen < 64) {
      valueOf(_smallValue * LongTenPows(-newScale.toInt), 0)
    } else {
      new BigDecimal(multiplyByTenPow(getUnscaledValue, safeLongToInt(-newScale)), 0)
    }
  }

  /** Rounds for numbers which unscaled value fits in the type {@code long}.
   *
   *  This method implements an efficient rounding for numbers which unscaled
   *  value fits in the type {@code long}.
   *
   *  @param mc the context to use
   *  @param discardedPrecision the number of decimal digits that are discarded
   *  @see #round(MathContext)
   */
  private def smallRound(mc: MathContext, discardedPrecision: Int): Unit = {
    val sizeOfFraction: Long = LongTenPows(discardedPrecision)
    val newScale0: Long = _scale.toLong - discardedPrecision
    val unscaledVal: Long = _smallValue
    // Getting the integer part and the discarded fraction
    val intPart0: Long = unscaledVal / sizeOfFraction
    val fraction: Long = unscaledVal % sizeOfFraction
    // If the discarded fraction is non-zero perform rounding
    val (newScale, intPart) = {
      if (fraction != 0) {
        // To check if the discarded fraction >= 0.5
        val compRem = longCompareTo(Math.abs(fraction) * 2, sizeOfFraction)
        // To look if there is a carry
        val frac = java.lang.Long.signum(fraction) * (5 + compRem)
        val intPart1 = intPart0 + roundingBehavior(intPart0.toInt & 1, frac, mc.roundingMode)
        // If after to add the increment the precision changed, we normalize the size
        if (Math.log10(Math.abs(intPart1)) >= mc.precision)
          (newScale0 - 1, intPart1 / 10)
        else
          (newScale0, intPart1)
      } else {
        (newScale0, intPart0)
      }
    }
    // To update all internal fields
    _scale = safeLongToInt(newScale)
    _precision = mc.precision
    _smallValue = intPart
    _bitLength = bitLength(intPart)
    _intVal = null
  }

  /** Returns an exact value or throws an exception.
   *
   *  If {@code intVal} has a fractional part throws an exception,
   *  otherwise it counts the number of bits of value and checks if it's out of
   *  the range of the primitive type. If the number fits in the primitive type
   *  returns this number as {@code long}, otherwise throws an
   *  exception.
   *
   *  @param bitLengthOfType number of bits of the type whose value will be
   *                         calculated exactly
   *  @return the exact value of the integer part of {@code BigDecimal}
   *          when is possible
   *  @throws ArithmeticException when rounding is necessary or the
   *          number don't fit in the primitive type
   */
  private def valueExact(bitLengthOfType: Int): Long = {
    // Fast path to avoid some large BigInteger creations by toBigIntegerExact
    if (-scale.toLong + approxPrecision() > 19) {
      /* If there are more digits than the number of digits of Long.MaxValue in
       * base 10, this BigDecimal cannot possibly be an exact Long.
       */
      throw new ArithmeticException("Rounding necessary")
    }

    val bigInteger = toBigIntegerExact()
    if (bigInteger.bitLength() < bitLengthOfType)
      bigInteger.longValue()
    else
      throw new ArithmeticException("Rounding necessary")
  }

  /** Calculates an approximation of {@code precision()} value.
   *
   *  If the precision already was calculated it returns that value, otherwise
   *  it calculates a very good approximation efficiently . Note that this
   *  value will be {@code precision()} or {@code precision()-1}
   *  in the worst case.
   *
   *  @return an approximation of {@code precision()} value
   */
  private def approxPrecision(): Int = {
    if (_precision > 0) _precision
    else ((this._bitLength - 1) * Log2).toInt + 1
  }

  private def getUnscaledValue(): BigInteger = {
    if (_intVal == null)
      _intVal = BigInteger.valueOf(_smallValue)
    _intVal
  }

  private def setUnscaledValue(unscaledVal: BigInteger): Unit = {
    _intVal = unscaledVal
    _bitLength = unscaledVal.bitLength()
    if (_bitLength < 64)
      _smallValue = unscaledVal.longValue()
  }
}
