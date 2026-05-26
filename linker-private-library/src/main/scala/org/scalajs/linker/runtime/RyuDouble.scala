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

/*
 * Ported from
 * https://github.com/ulfjack/ryu/blob/1264a946ba66eab320e927bfd2362e0c8580c42f/src/main/java/info/adams/ryu/RyuDouble.java
 * Original license copied below:
 */

/* Copyright 2018 Ulf Adams
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.scalajs.linker.runtime

/** An implementation of Ryu for double. */
object RyuDouble {
  private val MantissaBits = 52
  private val MantissaMask = (1L << MantissaBits) - 1

  private val ExponentBits = 11
  private val ExponentMask = (1 << ExponentBits) - 1
  private val ExponentBias = (1 << (ExponentBits - 1)) - 1

  @inline private final class DecimalInterval(
      val aq: Long,
      val bq: Long,
      val cq: Long,
      val e10: Int,
      val za: Boolean,
      val zb: Boolean
  )

  @inline private final class ShortestResult(
      val output: Long,
      val removed: Int
  )

  @inline // inline into the static forwarder, which will be the entry point
  def doubleToString(value: Double): String = {
    /* Step 1: Decode the floating point number, and unify normalized and subnormal cases.
     * First, handle all the trivial cases.
     * Otherwise, convert floating point value into the intermediate form
     * f = (-1)^s * m2 * 2^(e2) such that m2 is an unsigned integer
     * by moving the decimal dot all the way to right.
     * m = if (e != 0) 2^(len(m)) + m else m
     * e = if (e != 0) e - bias - len(m) else 1 - bias - len(m)
     */
    if (java.lang.Double.isNaN(value)) return "NaN"
    if (value == Double.PositiveInfinity) return "Infinity"
    if (value == Double.NegativeInfinity) return "-Infinity"

    val bits = java.lang.Double.doubleToLongBits(value)
    if (bits == 0 || bits == 0x8000000000000000L) return "0"

    // Otherwise extract the mantissa and exponent bits and run the full algorithm.
    val exponent = ((bits >>> MantissaBits) & ExponentMask).toInt
    val mantissa = bits & MantissaMask

    val (e, m) = if (exponent == 0) {
      (1 - ExponentBias - MantissaBits, mantissa)
    } else {
      (exponent - ExponentBias - MantissaBits, mantissa | (1L << MantissaBits))
    }
    val sign = bits < 0

    /* Step 2: Determine the interval of information-preserving outputs.
     * Compute smaller halfway point, f, and larger halfway point as
     * u*2^e2, v*2^e2, and w*2^e2. Where e2 = e - 2 to make sure u,v,w are unsigned integers.
     *
     * Given f- and f+ are smaller and larger floating point number to f,
     * (f + f-)/2 = (m*2^e + (m-1)*2^e)/2 = m*2^e - (1/2)*2^e = (4m - 2)*e2.
     * Therefore, u=4m-2, but if the mantissa is zero, the next smaller numbers are
     * m'=(1...1), e'=e-1, and resulting in (4m-1)*e2
     *
     *    f-                f                 f+
     * ---|--------|--------|--------|--------|--->
     *          u*2^e2   v*2^e2   w*2^e2
     */
    val even = (m & 1) == 0
    /* Is it "general case" for calculating the lower boundary u.
     * This will be false if mantissa is all zeros, and the exponent > 1.
     */
    val isGeneralLowerBoundCase = (m != 1L << MantissaBits) || exponent <= 1
    val u = 4 * m - (if (isGeneralLowerBoundCase) 2 else 1)
    val v = 4 * m
    val w = 4 * m + 2
    var e2 = e - 2

    /* Step 3: Convert the binary representation (u,v,w) * 2^e2 to a
     * decimal representation (a,b,c) * 10^e10.
     *
     * We also combine the main Ryu optimization (Step 4' in the paper)
     * by removing 'q' digits in one efficient step. This is done by
     * multiplying with precomputed values, which keeps the result
     * within the information-preserving interval.
     *
     * The result of removing 'q' digits is (aq,bq,cq), which
     * represents the final interval (aq,bq,cq) * 10^e10.
     *
     * The returning flags flags (za, zb, and zc) track if the 'q' digits
     * removed during the conversion step were all zeros.
     */
    val interval = convertBinaryToDecimalInterval(u, v, w, e2, isGeneralLowerBoundCase, even)

    // Step 4: Find the shortest decimal representation in the interval of legal representations.
    val vplength = decimalLength(interval.cq)
    val exp = interval.e10 + vplength - 1

    /* Per ECMA-262 Number::toString specification:
     * Decimal notation is used if the exponent n is in the [-5, 21],
     * otherwise, scientific notation must be used.
     * Here, n in the ECMA spec equals to the `exp + 1`,
     * and the scientific notation condition is `!(exp ∈ [-6, -20])`.
     */
    val scientificNotation = (exp < -6) || (exp > 20)
    val shortest = computeShortest(interval.aq, interval.bq, interval.cq,
        interval.za, interval.zb, interval.e10, even, scientificNotation)
    val olength = vplength - shortest.removed

    // Step 5: Print the decimal representation.
    // We follow Double.toString semantics here.
    printString(shortest.output, olength, exp, sign, scientificNotation)
  }

  @inline
  private def convertBinaryToDecimalInterval(u: Long, v: Long, w: Long, e2: Int,
      isGeneralLowerBoundCase: Boolean, even: Boolean): DecimalInterval = {
    if (e2 >= 0) {
      // q = floor(e2 * log10(2)) - 1
      // We approximate log10(2) as a fraction: 78913 / 2^18 to avoid floating-point operations.
      // (78913 / 262144 = 0.30102996... which is a very close approximation)
      val qTmp = ((e2 * 78913) >>> 18) - 1
      val q = if (qTmp > 0) qTmp else 0
      // k = constant + floor(log_2(5^q))
      val k = Pow5InvBitCount + pow5bits(q) - 1
      val i = -e2 + q + k
      // (a_q, b_q, c_q) = floor[(u,v,w) * (floor(2^k/5^q)+1) / 2^(-e2+q+k)]
      val aq = mulPow5InvDivPow2(u, q, i)
      val bq = mulPow5InvDivPow2(v, q, i)
      val cq = mulPow5InvDivPow2(w, q, i)

      val (za, zb) = if (q <= 21) {
        if (v % 5 == 0) (false, multipleOfPowerOf5(v, q))
        else if (even) (multipleOfPowerOf5(u, q), false)
        else (false, false)
      } else {
        (false, false)
      }
      val cqAdjusted = if (q <= 21 && !even && multipleOfPowerOf5(w, q)) cq - 1 else cq
      new DecimalInterval(aq, bq, cqAdjusted, q, za, zb)
    } else {
      // q = floor(-e2 * log10(5)) - 1
      // We approximate log10(5) as a fraction: 732923 / 2^20
      // (732923 / 1048576 = 0.69897007... which is very close)
      val qTmp = ((-e2 * 732923) >>> 20) - 1
      val q = if (qTmp > 0) qTmp else 0
      val i = -e2 - q
      val k = pow5bits(i) - Pow5BitCount
      val j = q - k
      val aq = mulPow5divPow2(u, i, j)
      val bq = mulPow5divPow2(v, i, j)
      val cq = mulPow5divPow2(w, i, j)

      val (za, zb, cqAdjusted) = if (q <= 1) {
        if (even) (isGeneralLowerBoundCase, true, cq)
        else (false, true, cq - 1)
      } else if (q < 63) {
        (false, (v & ((1L << (q - 1)) - 1)) == 0, cq)
      } else {
        (false, false, cq)
      }
      new DecimalInterval(aq, bq, cqAdjusted, q + e2, za, zb)
    }
  }

  @inline
  private def computeShortest(aq0: Long, bq0: Long, cq0: Long, za0: Boolean, zb0: Boolean,
      e10: Int, even: Boolean, scientificNotation: Boolean): ShortestResult = {
    var removed = 0
    var lastRemovedDigit = 0
    var (aq, bq, cq) = (aq0, bq0, cq0)
    var (za, zb) = (za0, zb0)

    val output = if (za || zb) {
      while (cq / 10 > aq / 10) {
        za &= aq % 10 == 0
        zb &= lastRemovedDigit == 0
        lastRemovedDigit = (bq % 10).toInt
        aq /= 10
        bq /= 10
        cq /= 10
        removed += 1
      }

      if (za && even) {
        while (aq % 10 == 0) {
          zb &= lastRemovedDigit == 0
          lastRemovedDigit = (bq % 10).toInt
          aq /= 10
          bq /= 10
          cq /= 10
          removed += 1
        }
      }
      if (zb && (lastRemovedDigit == 5) && (bq % 2 == 0)) {
        // Round even if the exact numbers is .....50..0.
        lastRemovedDigit = 4
      }
      bq + (if ((bq == aq && !(za && even)) || (lastRemovedDigit >= 5)) 1 else 0)
    } else {
      while (cq / 10 > aq / 10) {
        lastRemovedDigit = (bq % 10).toInt
        aq /= 10
        bq /= 10
        cq /= 10
        removed += 1
      }
      bq + (if ((bq == aq) || (lastRemovedDigit >= 5)) 1 else 0)
    }
    new ShortestResult(output, removed)
  }

  @inline
  private def printString(output0: Long, olength: Int, exp0: Int,
      sign: Boolean, scientificNotation: Boolean): String = {
    val result = new Array[Char](24)
    var index = 0
    var output = output0
    var exp = exp0

    if (sign) {
      result(index) = '-'
      index += 1
    }

    if (scientificNotation) {
      // Print in the format x.xxxxxe-yy or xe-yy for single digit.
      if (olength == 1) {
        // print just "x" without decimal point (e.g., "1e+21" not "1.0e+21")
        result(index) = ('0' + output).toChar
        index += 1
      } else {
        // Multiple digits: print "x.xxxxx" with decimal point
        var i = 0
        while (i < olength - 1) {
          val c = (output % 10).toInt
          output /= 10
          result(index + olength - i) = ('0' + c).toChar
          i += 1
        }
        result(index) = ('0' + output % 10).toChar
        result(index + 1) = '.'
        index += olength + 1
      }

      // Print 'e', the exponent sign, and the exponent, which has at most three digits.
      result(index) = 'e'
      index += 1
      if (exp < 0) {
        result(index) = '-'
        exp = -exp
      } else {
        result(index) = '+'
      }
      index += 1
      if (exp >= 100) {
        result(index) = ('0' + exp / 100).toChar
        index += 1
        exp %= 100
        result(index) = ('0' + exp / 10).toChar
        index += 1
      } else if (exp >= 10) {
        result(index) = ('0' + exp / 10).toChar
        index += 1
      }
      result(index) = ('0' + exp % 10).toChar
      index += 1
      new String(result, 0, index)
    } else {
      // Otherwise follow the Java spec for values in the interval [1E-3, 1E7).
      if (exp < 0) {
        // Decimal dot is before any of the digits.
        result(index) = '0'
        index += 1
        result(index) = '.'
        index += 1
        var j = 1
        while (j < -exp) {
          result(index) = '0'
          index += 1
          j += 1
        }
        val current = index
        var i = 0
        while (i < olength) {
          result(current + olength - i - 1) = ('0' + output % 10).toChar
          output /= 10
          index += 1
          i += 1
        }
      } else if (exp + 1 >= olength) {
        // Decimal dot is after any of the digits.
        var i = 0
        while (i < olength) {
          result(index + olength - i - 1) = ('0' + output % 10).toChar
          output /= 10
          i += 1
        }
        index += olength
        var k = olength
        while (k < exp + 1) {
          result(index) = '0'
          index += 1
          k += 1
        }
      } else {
        // Decimal dot is somewhere between the digits.
        var current = index + 1
        var i = 0
        while (i < olength) {
          if (olength - i - 1 == exp) {
            result(current + olength - i - 1) = '.'
            current -= 1
          }
          result(current + olength - i - 1) = ('0' + output % 10).toChar
          output /= 10
          i += 1
        }
        index += olength + 1
      }
      new String(result, 0, index)
    }
  }

  private def pow5bits(e: Int): Int =
    ((e * 1217359) >>> 19) + 1

  private def decimalLength(v: Long): Int = {
    if (v >= 1000000000000000000L) 19
    else if (v >= 100000000000000000L) 18
    else if (v >= 10000000000000000L) 17
    else if (v >= 1000000000000000L) 16
    else if (v >= 100000000000000L) 15
    else if (v >= 10000000000000L) 14
    else if (v >= 1000000000000L) 13
    else if (v >= 100000000000L) 12
    else if (v >= 10000000000L) 11
    else if (v >= 1000000000L) 10
    else if (v >= 100000000L) 9
    else if (v >= 10000000L) 8
    else if (v >= 1000000L) 7
    else if (v >= 100000L) 6
    else if (v >= 10000L) 5
    else if (v >= 1000L) 4
    else if (v >= 100L) 3
    else if (v >= 10L) 2
    else 1
  }

  private def multipleOfPowerOf5(value: Long, q: Int): Boolean =
    pow5Factor(value) >= q

  private def pow5Factor(value: Long): Int = {
    // We want to find the largest power of 5 that divides value.
    if ((value % 5) != 0) return 0
    if ((value % 25) != 0) return 1
    if ((value % 125) != 0) return 2
    if ((value % 625) != 0) return 3
    var count = 4
    var v = value / 625
    while (v > 0) {
      if (v % 5 != 0) {
        return count
      }
      v /= 5
      count += 1
    }
    throw new AssertionError(s"unreachable code reached for value $value")
  }

  /** Compute the high digits of m * 5^p / 10^q = m * 5^(p - q) / 2^q = m * 5^i / 2^j, with q chosen
   *  such that m * 5^i / 2^j has sufficiently many decimal digits to represent the original floating
   *  point number.
   */
  private def mulPow5divPow2(m: Long, i: Int, j: Int): Long = {
    // m has at most 55 bits.
    val mHigh = m >>> 31
    val mLow = m & 0x7fffffffL
    val idx = i * 4
    val bits13 = mHigh * Pow5Split(idx) // 124
    val bits03 = mLow * Pow5Split(idx) // 93
    val bits12 = mHigh * Pow5Split(idx + 1) // 93
    val bits02 = mLow * Pow5Split(idx + 1) // 62
    val bits11 = mHigh * Pow5Split(idx + 2) // 62
    val bits01 = mLow * Pow5Split(idx + 2) // 31
    val bits10 = mHigh * Pow5Split(idx + 3) // 31
    val bits00 = mLow * Pow5Split(idx + 3) // 0
    val actualShift = j - 3 * 31 - 21

    if (actualShift < 0) {
      throw new AssertionError(s"actualShift is negative: $actualShift")
    }

    ((((((
      ((bits00 >>> 31) + bits01 + bits10) >>> 31)
      + bits02 + bits11) >>> 31)
      + bits03 + bits12) >>> 21)
      + (bits13 << 10)) >>> actualShift
  }

  /** Compute the high digits of m / 5^i / 2^j such that the result is accurate to at least 9
   *  decimal digits. i and j are already chosen appropriately.
   */
  private def mulPow5InvDivPow2(m: Long, i: Int, j: Int): Long = {
    // m has at most 55 bits.
    val mHigh = m >>> 31
    val mLow = m & 0x7fffffffL
    val idx = i * 4
    val bits13 = mHigh * Pow5InvSplit(idx)
    val bits03 = mLow * Pow5InvSplit(idx)
    val bits12 = mHigh * Pow5InvSplit(idx + 1)
    val bits02 = mLow * Pow5InvSplit(idx + 1)
    val bits11 = mHigh * Pow5InvSplit(idx + 2)
    val bits01 = mLow * Pow5InvSplit(idx + 2)
    val bits10 = mHigh * Pow5InvSplit(idx + 3)
    val bits00 = mLow * Pow5InvSplit(idx + 3)

    val actualShift = j - 3 * 31 - 21

    if (actualShift < 0) {
      throw new AssertionError(s"actualShift is negative: $actualShift")
    }

    ((((((
      ((bits00 >>> 31) + bits01 + bits10) >>> 31)
      + bits02 + bits11) >>> 31)
      + bits03 + bits12) >>> 21)
      + (bits13 << 10)) >>> actualShift
  }

  /* Pre-computed tables for RyuDouble.
   * Generated from the following script:
   *
   * ```
   * import java.math.BigInteger
   *
   * val PosTableSize = 326
   * val NegTableSize = 291
   * val Pow5BitCount = 121 // max 3*31 = 124
   * val Pow5QuarterBitCount = 31
   * val Pow5Split = new Array[Int](PosTableSize * 4)
   * val Pow5InvBitCount = 122 // max 3*31 = 124
   * val Pow5InvQuarterBitCount = 31
   * val Pow5InvSplit = new Array[Int](NegTableSize * 4)
   *
   * def pow5bits(e: Int): Int =
   *   ((e * 2.321928094887362) + 0.0).toInt + 1
   *
   * def formatArray(arr: Array[Int], name: String, size: Int): String = {
   *   val sb = new StringBuilder
   *   sb.append(s"private val $name = Array(\n")
   *   for (i <- 0 until size) {
   *     val idx = i * 4
   *     sb.append(f"  ${arr(idx)}%10d, ${arr(idx+1)}%10d, ${arr(idx+2)}%10d, ${arr(idx+3)}%10d")
   *     if (i < size - 1) sb.append(",")
   *     sb.append("\n")
   *   }
   *   sb.append(")\n")
   *   sb.toString
   * }
   *
   * @main def main(): Unit =
   *   val mask = BigInteger.valueOf(1).shiftLeft(Pow5QuarterBitCount).subtract(BigInteger.ONE)
   *   val invMask = BigInteger.valueOf(1).shiftLeft(Pow5InvQuarterBitCount).subtract(BigInteger.ONE)
   *
   *   for (i <- 0 until PosTableSize) {
   *     val pow = BigInteger.valueOf(5).pow(i)
   *     val pow5len = pow.bitLength()
   *     val expectedPow5Bits = pow5bits(i)
   *     if (i < PosTableSize) {
   *       val idx = i * 4
   *       Pow5Split(idx) = pow.shiftRight(pow5len - Pow5BitCount + 3 * Pow5QuarterBitCount).and(mask).intValueExact()
   *       Pow5Split(idx+1) = pow.shiftRight(pow5len - Pow5BitCount + 2 * Pow5QuarterBitCount).and(mask).intValueExact()
   *       Pow5Split(idx+2) = pow.shiftRight(pow5len - Pow5BitCount + 1 * Pow5QuarterBitCount).and(mask).intValueExact()
   *       Pow5Split(idx+3) = pow.shiftRight(pow5len - Pow5BitCount + 0 * Pow5QuarterBitCount).and(mask).intValueExact()
   *     }
   *     if (i < NegTableSize) {
   *       val j = pow5len - 1 + Pow5InvBitCount
   *       val inv = BigInteger.ONE.shiftLeft(j).divide(pow).add(BigInteger.ONE)
   *       val idx = i * 4
   *       Pow5InvSplit(idx) = inv.shiftRight(3 * Pow5InvQuarterBitCount).intValueExact()
   *       Pow5InvSplit(idx+1) = inv.shiftRight(2 * Pow5InvQuarterBitCount).and(invMask).intValueExact()
   *       Pow5InvSplit(idx+2) = inv.shiftRight(1 * Pow5InvQuarterBitCount).and(invMask).intValueExact()
   *       Pow5InvSplit(idx+3) = inv.shiftRight(0 * Pow5InvQuarterBitCount).and(invMask).intValueExact()
   *     }
   *   }
   *
   *   println(s"private val PosTableSize = $PosTableSize")
   *   println(s"private val NegTableSize = $NegTableSize")
   *   println(s"private val Pow5BitCount = $Pow5BitCount")
   *   println(s"private val Pow5QuarterBitCount = $Pow5QuarterBitCount")
   *   println(s"private val Pow5InvBitCount = $Pow5InvBitCount")
   *   println(s"private val Pow5InvQuarterBitCount = $Pow5InvQuarterBitCount")
   *   println()
   *   println("/** Pow5Split contains pre-computed 5^q values split into 4 31-bit parts. */")
   *   println(formatArray(Pow5Split, "Pow5Split", PosTableSize))
   *   println()
   *   println("/** Pow5InvSplit contains pre-computed floor(2^k/5^q) + 1 values split into 4 31-bit parts. */")
   *   println(formatArray(Pow5InvSplit, "Pow5InvSplit", NegTableSize))
   * ```
   */

  private val PosTableSize = 326
  private val NegTableSize = 291
  private val Pow5BitCount = 121
  private val Pow5QuarterBitCount = 31
  private val Pow5InvBitCount = 122
  private val Pow5InvQuarterBitCount = 31

  /** Pow5Split contains pre-computed 5^q values split into 4 31-bit parts. */
  private val Pow5Split = Array(
      134217728, 0, 0, 0,
      167772160, 0, 0, 0,
      209715200, 0, 0, 0,
      262144000, 0, 0, 0,
      163840000, 0, 0, 0,
      204800000, 0, 0, 0,
      256000000, 0, 0, 0,
      160000000, 0, 0, 0,
      200000000, 0, 0, 0,
      250000000, 0, 0, 0,
      156250000, 0, 0, 0,
      195312500, 0, 0, 0,
      244140625, 0, 0, 0,
      152587890, 1342177280, 0, 0,
      190734863, 603979776, 0, 0,
      238418579, 218103808, 0, 0,
      149011611, 2015363072, 0, 0,
      186264514, 1982332928, 0, 0,
      232830643, 1404174336, 0, 0,
      145519152, 609173504, 0, 0,
      181898940, 761466880, 0, 0,
      227373675, 951833600, 0, 0,
      142108547, 326460544, 0, 0,
      177635683, 2018688416, 0, 0,
      222044604, 1986489608, 0, 0,
      138777878, 167814181, 0, 0,
      173472347, 1283509550, 536870912, 0,
      216840434, 1067516025, 1744830464, 0,
      135525271, 1204068428, 285212672, 0,
      169406589, 968214623, 356515840, 0,
      211758236, 1747139190, 2056257536, 0,
      264697796, 36440340, 1496580096, 0,
      165436122, 1096517036, 2009104384, 0,
      206795153, 296904472, 363896832, 0,
      258493941, 908001502, 454871040, 0,
      161558713, 835936394, 1894907136, 0,
      201948391, 1581791405, 1294892096, 0,
      252435489, 1440368345, 8002384, 0,
      157772181, 94923847, 1347178770, 0,
      197215226, 655525721, 1147102550, 1073741824,
      246519032, 1893148975, 1970749100, 268435456,
      154074395, 1183218109, 2037024555, 1241513984,
      192592994, 942151725, 935667958, 1015021568,
      240741243, 103947832, 1706455860, 195035136,
      150463276, 1944015587, 1066534912, 1195638784,
      188079096, 282535836, 796297728, 1494548480,
      235098870, 353169795, 995372160, 1868185600,
      146936793, 1831343858, 353672144, 1167616000,
      183670992, 678567086, 1515832004, 1459520000,
      229588740, 848208858, 821048181, 1824400000,
      143492962, 1603872360, 1050026025, 1408685456,
      179366203, 931098626, 1312532532, 150244084,
      224207754, 627002371, 566923841, 187805105,
      140129846, 928747394, 85891944, 1459555470,
      175162308, 87192418, 1181106754, 1824444338,
      218952885, 108990523, 402641619, 1206813598,
      136845553, 336554532, 2130699204, 485823043,
      171056941, 957564078, 515890357, 607278804,
      213821176, 1733826009, 1718604770, 1295969417,
      267276471, 19798864, 537643227, 546219947,
      167047794, 817680658, 336027017, 72952011,
      208809742, 2095842646, 1493775595, 628060925,
      261012178, 1546061484, 793477670, 248205245,
      163132611, 1503159339, 1569665367, 1765741014,
      203915764, 1342078262, 1425210797, 1670305355,
      254894705, 1677597828, 707771673, 477268958,
      159309191, 243192274, 1516099119, 1640470379,
      199136488, 1914603079, 821382075, 1513717062,
      248920611, 245770201, 489856682, 1355275415,
      155575381, 2032654567, 1648337706, 1383918046,
      194469227, 930205473, 1523551221, 656155734,
      243086534, 625885930, 293826290, 1357065580,
      151929083, 2001791442, 720512343, 1385036899,
      189911354, 1965368390, 1974382253, 1194425212,
      237389193, 1382968664, 1394235992, 2029902427,
      148368246, 59049047, 871397495, 1268689017,
      185460307, 1147553133, 552375957, 1048990359,
      231825384, 897570504, 1227340858, 1848108861,
      144890865, 560981565, 767088036, 1691938950,
      181113581, 1238097868, 1495730957, 2114923688,
      226391976, 2084493247, 1869663697, 1033041874,
      141494985, 1302808279, 1973846178, 1987828451,
      176868732, 17897613, 1930436811, 1411043740,
      221085915, 22372017, 802433278, 1226933763,
      138178696, 1893030702, 1843698079, 229962689,
      172723371, 218804730, 1230880774, 1898066098,
      215904213, 1884118649, 464859144, 1298840799,
      134940133, 1446009611, 1632714245, 811775499,
      168675167, 196899278, 1504021894, 1551590286,
      210843958, 1856736834, 806285544, 865746033,
      263554948, 1247179218, 2081598754, 1082182542,
      164721842, 1853228835, 1837870133, 1213235000,
      205902303, 1242794220, 1760466754, 2053414663,
      257377879, 1016621864, 53099795, 1493026504,
      160861174, 1440695033, 33187372, 664706109,
      201076468, 727126967, 578355127, 830882636,
      251345585, 908908709, 186072997, 501732384,
      157090990, 1910245223, 384731079, 582018196,
      196363738, 1314064704, 2091526585, 190651833,
      245454673, 568839057, 466924583, 775185703,
      153409170, 1697701690, 1634005144, 1289797432,
      191761463, 1048385289, 968764606, 1612246790,
      239701829, 773610699, 1747826670, 941566664,
      149813643, 751942143, 823956213, 51608253,
      187267054, 403056767, 493074354, 601381228,
      234083817, 1577562783, 79472030, 1825468359,
      146302386, 180670371, 854976387, 604046812,
      182877982, 1299579788, 531849572, 218187604,
      228597478, 550732911, 664811965, 272734505,
      142873423, 1954820805, 1220813846, 438894521,
      178591779, 1906655094, 2062888219, 1622359976,
      223239724, 1846447956, 1504868450, 1491079058,
      139524828, 80288148, 2014284605, 1468795323,
      174406035, 100360186, 370372109, 225381418,
      218007543, 1736062968, 1536706960, 818597684,
      136254714, 1890345723, 960441850, 511623552,
      170318393, 1289190330, 663681400, 1713271265,
      212897992, 875176, 1903343574, 2141589081,
      266122490, 1093971, 231695820, 1603244527,
      166326556, 537554643, 2023858079, 2075769653,
      207908195, 671943304, 1992951687, 2057841155,
      259885244, 303058219, 343705961, 2035430532,
      162428277, 1263153210, 2093864418, 466837714,
      203035346, 2115812425, 1543588698, 1657288967,
      253794183, 1571023708, 318873137, 997869385,
      158621364, 1787196185, 1273037534, 1965845645,
      198276706, 86511583, 2128167830, 1383565233,
      247845882, 1181881303, 2123338876, 655714717,
      154903676, 1275546726, 2132393165, 1483563522,
      193629595, 1594433408, 1591749633, 243841667,
      242036994, 1456170848, 1989687041, 841672995,
      151273121, 1446977692, 1243554400, 1868222902,
      189091402, 198109379, 1554443001, 187794979,
      236364252, 1321378548, 1406182839, 771614636,
      147727657, 1899603416, 1952606098, 1287565516,
      184659572, 763891535, 293273975, 535715071,
      230824465, 954864418, 1977205205, 132772926,
      144265290, 1938967541, 1772624165, 351418535,
      180331613, 1349967603, 605167470, 976144081,
      225414517, 76846768, 219588426, 146438277,
      140884073, 316464686, 137242766, 628394835,
      176105091, 932451769, 1245295281, 1859235368,
      220131364, 628693799, 2093490014, 713431474,
      137582102, 1466675448, 2113737626, 2056507407,
      171977628, 759602487, 494688385, 1496892435,
      214972035, 949503109, 81489570, 260502808,
      134357522, 325003987, 319366437, 699685167,
      167946902, 1479996807, 2009820782, 1411477370,
      209933628, 776254185, 1975405066, 690604889,
      262417035, 970317732, 858643596, 1936997935,
      164010647, 338013126, 1610394072, 136881885,
      205013308, 2033129144, 939250766, 171102357,
      256266636, 393927782, 1174063457, 1287619770,
      160166647, 1319946688, 196918748, 2146939636,
      200208309, 1113062448, 246148436, 536190897,
      250260386, 1928198972, 307685545, 670238622,
      156412741, 1741995269, 1266045289, 1761076419,
      195515927, 566881350, 2119427524, 590732787,
      244394909, 171730776, 1575542581, 738415984,
      152746818, 375767191, 984714113, 729945446,
      190933522, 1543450813, 694021729, 1449302720,
      238666903, 855571692, 1404398074, 201015664,
      149166814, 1340038675, 1951490620, 662505702,
      186458518, 601306520, 1902492363, 828132127,
      233073147, 1825374975, 230631806, 498294247,
      145670717, 872423903, 949451246, 1922046640,
      182088396, 1627400791, 649943146, 1328816476,
      227610495, 2034250989, 275558021, 587278772,
      142256559, 2076713236, 440659219, 635484688,
      177820699, 2059020633, 550824024, 257484948,
      222275874, 2036904879, 1225400942, 321856185,
      138922421, 1809936461, 1571181956, 1811772852,
      173653027, 651807841, 353364710, 117232417,
      217066284, 277888889, 978576799, 1220282345,
      135666427, 1247422379, 1953787779, 1567982833,
      169583034, 1022407062, 1905363812, 1423107630,
      211978793, 204267004, 1307962941, 1778884537,
      264973491, 792204667, 1634953677, 612992936,
      165608432, 226692461, 753410592, 651556041,
      207010540, 283365576, 1478634152, 814445051,
      258763175, 354206970, 1848292690, 1018056314,
      161726984, 1026685724, 1692053843, 1173156108,
      202158730, 1283357155, 2115067304, 929574223,
      252698413, 530454620, 2106963218, 1161967779,
      157936508, 599969594, 243110187, 1263100774,
      197420635, 749961992, 1377629558, 1042005055,
      246775794, 400581578, 1722036948, 228764495,
      154234871, 787234398, 1613144004, 1216719633,
      192793589, 447172086, 942688181, 1520899542,
      240991986, 1095836020, 104618403, 290511691,
      150619991, 1221768424, 1139128325, 2060617999,
      188274989, 990339618, 1423910407, 965159763,
      235343736, 1774795435, 706146185, 669578792,
      147089835, 1109247147, 172905909, 1760664025,
      183862294, 849688021, 1826745123, 590217295,
      229827867, 2135851851, 672818668, 200900707,
      143642417, 1066471951, 152076211, 1199304766,
      179553021, 1869960850, 1800708000, 962260045,
      224441277, 726838327, 1177143176, 1202825056,
      140275798, 722709410, 1541020853, 751765660,
      175344747, 1977128587, 852534242, 1476577987,
      219180934, 1934539822, 528796891, 771980660,
      136988084, 672216476, 1941110793, 214052456,
      171235105, 840270596, 278904843, 804436483,
      214043881, 1587209157, 348631054, 468674691,
      267554852, 373398710, 972659729, 1659585188,
      167221782, 1307116018, 71041419, 231934375,
      209027228, 560153198, 1162543597, 1900530704,
      261284035, 700191498, 379437673, 765050645,
      163302522, 169184230, 774019457, 1820333933,
      204128152, 1285222111, 2041266146, 664804680,
      255160190, 1606527639, 2014711770, 1904747674,
      159475119, 467208862, 2064501224, 1727338208,
      199343899, 47140166, 1506884707, 11689112,
      249179873, 1669537944, 809864059, 1625224126,
      155737421, 238154847, 506165037, 747329623,
      194671776, 834564471, 95835384, 1471032941,
      243339720, 1043205588, 1730406966, 1838791176,
      152087325, 652003493, 7762530, 612373573,
      190109156, 1351875278, 546574074, 1839208790,
      237636445, 1689844097, 1756959417, 1225269164,
      148522778, 1324588017, 292793267, 2107970507,
      185653473, 581993197, 902862496, 2098092222,
      232066841, 1264362408, 1665449033, 475131630,
      145041775, 2132403785, 1040905645, 1639134548,
      181302219, 2128633819, 1838002969, 438305450,
      226627774, 2123921362, 1760632799, 1084752724,
      141642359, 790579939, 1637266411, 1483276820,
      177052949, 451354012, 1509712102, 1317225114,
      221316186, 1101063427, 1887140128, 572789568,
      138322616, 1225035554, 911027124, 357993480,
      172903270, 1531294443, 65042081, 447491850,
      216129088, 840376229, 1691915337, 1096235725,
      135080680, 525235143, 1325882541, 2027324608,
      168850850, 656543929, 1120482265, 923543024,
      211063562, 1894421735, 1937473743, 1691299692,
      263829453, 1294285345, 1884971267, 1577253703,
      164893408, 1077363797, 372800674, 717348108,
      206116760, 1346704746, 1002871754, 1970426959,
      257645950, 1683380933, 179847869, 1389291875,
      161028719, 515242171, 380840374, 1136742878,
      201285899, 107181801, 2086663204, 347186773,
      251607373, 1744589988, 997716269, 433983467,
      157254608, 1358804198, 1697314492, 539675123,
      196568260, 1698505248, 1047901291, 674593903,
      245710325, 2123131560, 1309876614, 306371467,
      153568953, 1595392681, 818672883, 1802094903,
      191961192, 383628115, 1560212016, 1715747717,
      239951490, 479535144, 1413394108, 2144684646,
      149969681, 836580377, 883371318, 266686080,
      187462101, 1582596383, 1641085059, 1407099424,
      234327627, 367632743, 1514485412, 1222003368,
      146454766, 2108818656, 1751859750, 1837493929,
      183068458, 1562281497, 42341040, 1223125587,
      228835573, 879110047, 589797212, 1528906984,
      143022233, 817879235, 1173929625, 2029308689,
      178777791, 1559219956, 930541120, 926023125,
      223472239, 1412154033, 1163176400, 1157528906,
      139670149, 1687902638, 2069162530, 723455566,
      174587687, 499265562, 1512711338, 1978061282,
      218234609, 87211041, 817147349, 1398834779,
      136396630, 1396684180, 1852894373, 1142707192,
      170495788, 672113402, 168634318, 1965254903,
      213119735, 840141752, 1284534722, 1382826804,
      266399669, 513306278, 1605668403, 654791682,
      166499793, 589251880, 466671840, 140809345,
      208124741, 1273435762, 583339800, 176011681,
      260155926, 2128665614, 1802916574, 220014602,
      162597454, 793545097, 589951946, 1748121862,
      203246817, 2065673195, 1274310845, 1111410503,
      254058522, 971478758, 1056017644, 1926134041,
      158786576, 1144045136, 123140116, 130091952,
      198483220, 1430056420, 153925145, 162614940,
      248104025, 1787570525, 192406431, 740139587,
      155065016, 311925210, 388689475, 1267893610,
      193831270, 389906512, 1559603668, 1047996100,
      242289087, 1561124964, 1949504585, 1309995125,
      151430679, 1781009471, 144698542, 13440585,
      189288349, 1689390926, 1791485913, 1090542555,
      236610437, 501125922, 1165615567, 1900049106,
      147881523, 581639157, 1265380641, 1992837059,
      184851904, 190178034, 2118596714, 880433588,
      231064880, 237722543, 1574504069, 26800161,
      144415550, 148576589, 1789371411, 285185557,
      180519437, 1259462561, 626101527, 1967094682,
      225649296, 2111199113, 1319497821, 1921997441,
      141030810, 1319499446, 19379770, 1469683856,
      176288513, 575632483, 1097966537, 763362996,
      220360641, 1256411516, 835587259, 1491074658,
      137725400, 2127434477, 1595983861, 663486205,
      172156751, 511809449, 384367090, 1366228668,
      215195939, 102890899, 1017329775, 634044011,
      134497461, 1943355004, 367395653, 1201583875,
      168121827, 818581019, 459244566, 2038850756,
      210152284, 486355362, 37184796, 1474821621,
      262690355, 607944202, 1120222819, 1843527026,
      164181472, 111529670, 1237010174, 883768935,
      205226840, 139412088, 472520894, 30969345,
      256533550, 174265110, 590651117, 1112453505,
      160333468, 1719528429, 1979769684, 963718897,
      200416836, 1926889, 864099369, 1204648621,
      250521045, 2408611, 1616995123, 2042681688,
      156575653, 269940838, 742186496, 1008240599,
      195719566, 874296959, 2001474944, 1260300749,
      244649458, 19129375, 1964972768, 1575375936,
      152905911, 548826771, 2033414348, 984609960,
      191132389, 149162552, 2004897023, 1230762450,
      238915486, 723324103, 358637631, 1001582151,
      149322178, 2062690300, 1029454887, 1431295212,
      186652723, 1504621051, 1286818609, 1252248103,
      233315904, 1343905402, 1071652349, 2102181041,
      145822440, 839940876, 1206653630, 1582298607,
      182278050, 1049926095, 1508317038, 904131434,
      227847563, 238665795, 1348525386, 56422469,
      142404726, 2028214314, 574392910, 572134955,
      178005908, 1461526068, 1791732961, 1788910518,
      222507385, 1826907586, 92182554, 625525411,
      139067116, 336510873, 594485008, 927824294,
      173833895, 420638591, 1279977172, 1159780368,
      217292368, 2136410975, 1063100553, 1449725460,
      135807730, 1335256859, 1469744214, 100772044,
      169759663, 595329250, 1300309355, 1199706879,
      212199579, 207290651, 551644870, 962762687,
      265249473, 1869726050, 152685176, 129711535,
      165780921, 363272413, 632299147, 81069709,
      207226151, 990961428, 1327244845, 1711949873
  )

  /** Pow5InvSplit contains pre-computed floor(2^k/5^q) + 1 values split into 4 31-bit parts. */
  private val Pow5InvSplit = Array(
      536870912, 0, 0, 1,
      429496729, 1288490188, 1717986918, 858993460,
      343597383, 1460288880, 1374389534, 1546188227,
      274877906, 2027224563, 1529008357, 807453852,
      439804651, 237082194, 1587419912, 1291926163,
      351843720, 1907652674, 410942470, 1892534390,
      281474976, 1526122139, 758250706, 655034053,
      450359962, 1582801963, 1642697859, 1477551214,
      360287970, 407248111, 1743655017, 752544241,
      288230376, 325798489, 965427284, 602035393,
      461168601, 1809767771, 1974180384, 963256629,
      368934881, 1018317487, 2008841037, 341108574,
      295147905, 385157260, 1607072829, 1561377048,
      472236648, 616251617, 423832879, 1209713087,
      377789318, 1351994752, 2057053222, 108777011,
      302231454, 1940589261, 1216145848, 87021609,
      483570327, 1816452629, 1516336627, 568731303,
      386856262, 594168644, 1213069301, 1743475232,
      309485009, 1763825104, 970455441, 965283456,
      495176015, 1533629978, 693735247, 255963340,
      396140812, 1226903982, 1413981656, 1922757591,
      316912650, 122529726, 1990178784, 1538206073,
      507060240, 196047563, 177808948, 743142797,
      405648192, 156838050, 1001240617, 1883004427,
      324518553, 1413960629, 371495764, 1506403542,
      519229685, 1832840277, 164896493, 1980748937,
      415383748, 1466272221, 1420407383, 2014095879,
      332306998, 2032011236, 1136325907, 322786514,
      531691198, 674237600, 1818121451, 945955152,
      425352958, 1398383539, 1883993890, 1615757581,
      340282366, 1977700291, 218704923, 1722102795,
      272225893, 1152663503, 604460668, 1377682236,
      435561429, 1414764875, 1396633799, 915801388,
      348449143, 1561308630, 258313580, 732641111,
      278759314, 2108040363, 636147593, 1874603077,
      446014903, 2084374392, 1017836150, 422384546,
      356811923, 379009325, 384772190, 1196901096,
      285449538, 1162200919, 737314482, 98527418,
      456719261, 1430024741, 750206441, 1446134057,
      365375409, 714523063, 1029661882, 2015900705,
      292300327, 1001115180, 823729506, 753727105,
      467680523, 2031281018, 458973750, 2064956827,
      374144419, 336534625, 1655669189, 1222468732,
      299315535, 698724430, 465541892, 977974986,
      478904856, 1117959088, 744867027, 1994256706,
      383123885, 464870541, 166396892, 1595405365,
      306499108, 371896432, 1851104432, 1276324292,
      490398573, 165537562, 2102773632, 2042118867,
      392318858, 991423509, 1252722176, 1633695094,
      313855086, 1652132266, 1861171200, 1306956075,
      502168138, 1784418167, 1689383732, 373142802,
      401734511, 139044345, 922010256, 298514241,
      321387608, 1829222394, 1596601664, 238811393,
      514220174, 349775453, 2125065932, 2100085147,
      411376139, 709317092, 1700052746, 821074659,
      329100911, 996950403, 1789538926, 1515853186,
      526561458, 736127186, 2004268823, 1136874909,
      421249166, 1447895208, 1603415058, 1768493386,
      336999333, 728819437, 853235317, 985297980,
      269599466, 1442049009, 253091524, 788238384,
      431359146, 1448284955, 834443168, 1261181414,
      345087317, 729131234, 1526547994, 149951672,
      276069853, 1871795176, 1221238395, 549458067,
      441711766, 417891904, 1953981432, 879132907,
      353369412, 2052300442, 704191686, 1562299785,
      282695530, 782846894, 1422346808, 1249839828,
      452312848, 1252555031, 987264704, 1999743725,
      361850278, 1861037484, 789811763, 2029291709,
      289480223, 200339798, 1490842870, 764439908,
      463168356, 2038530596, 667361674, 364110394,
      370534685, 1201327747, 963386068, 2009275234,
      296427748, 961062197, 2059199043, 2036916917,
      474284397, 1108202787, 288241363, 252589959,
      379427518, 27568770, 1089586549, 1490562156,
      303542014, 881048475, 1301165969, 762952996,
      485667223, 121187372, 363878632, 1220724793,
      388533778, 955943356, 2009089824, 976579834,
      310827022, 1623748144, 1607271859, 1210760597,
      497323236, 880010113, 424151327, 648726766,
      397858589, 274511360, 2057307980, 518981413,
      318286871, 649105818, 786852924, 2133172049,
      509258994, 179575850, 399971220, 1695088360,
      407407195, 573157409, 1608467165, 926573958,
      325925756, 458525927, 1716270461, 2029749355,
      521481209, 2022131673, 598549091, 241121861,
      417184967, 2047202068, 478839272, 1910884407,
      333747974, 778768195, 812568147, 1958204256,
      533996758, 2105022571, 1729605766, 556146431,
      427197407, 395527868, 1383684613, 15420415,
      341757925, 1604912483, 1536444420, 12336332,
      273406340, 1283929986, 2088148995, 439365796,
      437450144, 2054287979, 334561285, 273488543,
      349960115, 2072927112, 1985635946, 1077784294,
      279968092, 1658341690, 729515298, 3233976,
      447948948, 935359786, 308231017, 1293664550,
      358359158, 1607281288, 246584814, 175938181,
      286687326, 2144818489, 1485758040, 140750545,
      458699723, 425232476, 659225945, 1513691060,
      366959778, 1199179440, 527380756, 1210952848,
      293567822, 1818337011, 851401334, 1827755738,
      469708516, 1191352299, 1791738865, 776925532,
      375766813, 523585110, 574397633, 192043696,
      300613450, 1277861547, 889014836, 153634957,
      480981520, 2044578475, 1851920467, 675312661,
      384785216, 1635662780, 1481536373, 1828740318,
      307828173, 879033494, 2044222558, 603998795,
      492525077, 976956862, 693775715, 1395894801,
      394020061, 2070055678, 1414014031, 1546212571,
      315216049, 1226547813, 701714495, 1666466786,
      504345679, 673986312, 1122743193, 518863210,
      403476543, 968685779, 1327691284, 415090568,
      322781234, 1633942082, 1921146486, 1191065914,
      516449975, 1325817143, 1785344189, 1476208732,
      413159980, 1060653715, 139785162, 2039960445,
      330527984, 848522972, 111828130, 772974897,
      528844775, 69146566, 1037918467, 1666256564,
      423075820, 55317253, 400838044, 1333005251,
      338460656, 44253802, 1179663894, 1925397660,
      270768524, 1753389960, 943731115, 1969814858,
      433229639, 1516933747, 1939466515, 145226665,
      346583711, 1643043727, 1981069941, 1404671521,
      277266969, 884938252, 1584855953, 694240487,
      443627151, 127411015, 1247279336, 1110784780,
      354901720, 1819915730, 1856816928, 888627824,
      283921376, 1455932584, 1485453542, 1569895718,
      454274202, 1470498676, 658738749, 2082336419,
      363419362, 317405481, 1815481188, 1665869136,
      290735489, 1542414574, 593391491, 1762192038,
      465176783, 1179373130, 90432927, 1531017072,
      372141426, 1802491963, 501843071, 1654310387,
      297713141, 1012496840, 2119461375, 1752945040,
      476341026, 761001486, 814157823, 1516221874,
      381072821, 179304459, 1080822988, 1212977499,
      304858256, 1861430486, 5664931, 1399878729,
      487773210, 2119295318, 868057349, 1810309237,
      390218568, 1695436254, 1553439339, 159757201,
      312174855, 67858814, 2101744930, 986799220,
      499479768, 108574103, 2074301699, 2008375481,
      399583814, 945852742, 800447900, 1606700385,
      319667051, 1186178923, 1069855050, 426366849,
      511467282, 1038892818, 852774621, 252690228,
      409173825, 2119604443, 1111716426, 1061145642,
      327339060, 1695683554, 1748366600, 848916514,
      523742497, 565610039, 1508896371, 1787763151,
      418993997, 1740978220, 1207117097, 1000713792,
      335195198, 533789117, 536196948, 800571033,
      536312317, 424565857, 2146405306, 421920194,
      429049853, 1628142875, 428634056, 337536155,
      343239883, 14024111, 772403974, 1129022383,
      274591906, 870212748, 617923179, 1332714636,
      439347050, 533346938, 129683628, 414356500,
      351477640, 426677550, 962740361, 1619975389,
      281182112, 341342040, 770192289, 866483581,
      449891379, 975643994, 373314203, 1815870459,
      359913103, 1210011924, 2016638281, 1023199638,
      287930482, 1827002999, 324820436, 818559711,
      460688772, 1205217880, 519712698, 450702077,
      368551018, 105180844, 2133757076, 2078548580,
      294840814, 943138135, 418515472, 1662838864,
      471745303, 220530827, 1099121486, 83561805,
      377396242, 1035418121, 449800459, 496346174,
      301916993, 2116824685, 1648330556, 397076939,
      483067190, 809939119, 1348838701, 205826373,
      386453752, 647951295, 1508567690, 1023654558,
      309163001, 1806851225, 777357422, 1677917105,
      494660802, 2031968501, 814275146, 1825673909,
      395728642, 766581341, 1939910306, 601545668,
      316582913, 1901755262, 692934785, 1769726723,
      506532662, 465828042, 249702198, 254582380,
      405226129, 1661152622, 1058755217, 1492156093,
      324180903, 1758418827, 1276500903, 1623221604,
      518689446, 236489746, 1183407986, 1738161106,
      414951556, 1907178715, 1376223119, 102038696,
      331961245, 1096246242, 1959971954, 940624416,
      531137992, 1753993988, 1417968208, 1504999066,
      424910394, 544201731, 1563871296, 1203999253,
      339928315, 864858114, 2110090496, 963199402,
      271942652, 691886491, 2117569126, 1629552981,
      435108243, 1536515116, 1670123684, 889297851,
      348086594, 2088205552, 1336098947, 1140935011,
      278469275, 2100061171, 1498375887, 1342244738,
      445550841, 1212614226, 1538407961, 107933,
      356440673, 540594651, 1660223098, 859079806,
      285152538, 1291469180, 1328178478, 1546257304,
      456244061, 1636853959, 836595377, 326528038,
      364995249, 879986437, 1957766490, 1120215890,
      291996199, 1133485879, 1995709922, 37179253,
      467193919, 525087219, 186658768, 59486804,
      373755135, 849566504, 1867313932, 1765576362,
      299004108, 679653203, 1923347875, 1841957819,
      478406573, 657948396, 1359369682, 2088139051,
      382725258, 1385352176, 1087495746, 811517782,
      306180206, 1967275200, 869996597, 219717496,
      489888331, 141163213, 962497825, 1640038182,
      391910664, 1830917489, 340501531, 23540357,
      313528531, 1894230720, 1990388143, 448329015,
      501645651, 24292046, 607640651, 1146823153,
      401316520, 1737420555, 915609250, 1776451982,
      321053216, 1389936444, 732487400, 1421161586,
      513685146, 1364904851, 1601476570, 1414865078,
      410948117, 662427151, 1710677986, 272898603,
      328758493, 1818431910, 509548929, 1506809071,
      526013590, 332510678, 1674271746, 1551901055,
      420810872, 266008543, 50927208, 1241520844,
      336648697, 1501297023, 470238496, 993216675,
      269318958, 342044159, 805687526, 1653566799,
      430910333, 117773925, 859603313, 498223231,
      344728266, 953212599, 1117179380, 398578585,
      275782613, 333073350, 34750044, 2036849786,
      441252181, 103420630, 914593531, 252482550,
      353001744, 1800723422, 1590668284, 201986040,
      282401395, 1870075467, 1702031356, 1879575751,
      451842233, 844637100, 1005263252, 1289334283,
      361473786, 1534703139, 1233707331, 1460964156,
      289179029, 798265782, 127972406, 309777866,
      462686446, 2136218710, 1063749309, 66147855,
      370149157, 1279478238, 1709992906, 911911743,
      296119326, 164589131, 1797491054, 1588522854,
      473790921, 1551832799, 1587495498, 1682643107,
      379032737, 811969510, 411002939, 1775611215,
      303226189, 1938065796, 2046789270, 561495513,
      485161903, 1812415086, 697882454, 1757386280,
      388129523, 161441880, 558305963, 1835405753,
      310503618, 988146963, 876141500, 1468324603,
      496805789, 1151538411, 1831323130, 1490325905,
      397444631, 1350727459, 176568315, 1621757454,
      317955705, 651085237, 1429744841, 867909234,
      508729128, 1041736380, 569604827, 1818151503,
      406983302, 1692382563, 885180591, 1884017932,
      325586642, 494912591, 1137641203, 218724157,
      520938627, 1221356876, 102239006, 1208952110,
      416750902, 118092041, 1370281394, 108168229,
      333400721, 1382963822, 237231656, 86534583,
      533441154, 1353748656, 379570649, 1426945522,
      426752923, 1512495654, 1162649978, 2000549877,
      341402338, 2068989982, 1789113442, 741446442,
      273121871, 366701797, 1001794024, 593157154,
      436994993, 1875213064, 1602870438, 1808044905,
      349595995, 211680262, 2141289810, 587442465,
      279676796, 169344210, 854038389, 40457242,
      447482873, 1559440925, 936964692, 1782718506,
      357986298, 2106546199, 1179068483, 1855671535,
      286389039, 396746770, 1802248246, 625543769,
      458222462, 1493788292, 1165610275, 1430366759,
      366577970, 336037174, 1791481679, 1573790137,
      293262376, 268829739, 1862682073, 829535380,
      469219801, 1718617772, 1262304399, 38766419,
      375375841, 945397488, 1009843519, 460509865,
      300300673, 326821261, 378378085, 1656898081,
      480481077, 93417288, 605404937, 503553281,
      384384861, 1363224019, 913820679, 832339354,
      307507889, 661082485, 2019546732, 665871484,
      492012622, 1916725436, 1513287853, 635897644,
      393610098, 674386890, 351636823, 938214845,
      314888078, 1398502971, 710806188, 750571876,
      503820925, 1808108024, 1137289901, 771418272,
      403056740, 1446486419, 1339328650, 1476128077,
      322445392, 1157189135, 1500959650, 321909002,
      515912628, 133515698, 1542541981, 85557674,
      412730102, 965806018, 375040125, 1356936328,
      330184081, 2061135003, 729528830, 226555603,
      528294531, 291338898, 308252668, 2080475883,
      422635624, 1951058036, 1964589053, 1234883977,
      338108499, 1990343159, 283181054, 128913723,
      270486799, 2021771256, 1944531761, 1391621167,
      432778879, 1946343822, 534270441, 79110219,
      346223103, 1986571787, 856913082, 922281634,
      276978483, 300767241, 256033736, 737825308,
      443165573, 51730856, 409653978, 321527033,
      354532458, 900378144, 327723182, 1116215085,
      283625966, 1579295974, 1121172005, 463475339,
      453801546, 1667880100, 75888289, 2030050731,
      363041237, 904807350, 919704091, 335550396,
      290432989, 2012336069, 306266543, 697937046,
      464692783, 1931247521, 1778516658, 257705815,
      371754227, 256507828, 1422813326, 1065158111,
      297403381, 1493696451, 1567747390, 1711119948,
      475845410, 1530920863, 1219905636, 1019804998,
      380676328, 1224736690, 1834917968, 815843999,
      304541062, 1838782811, 1897431104, 652675199,
      487265700, 1224065580, 1317902848, 1044280318,
      389812560, 979252464, 1054322278, 1694417714,
      311850048, 783401971, 1272954552, 1355534171,
      498960077, 823946424, 2036727284, 450867755,
      399168061, 1947647328, 1629381827, 790190934,
      319334449, 1128621133, 874008732, 632152747,
      510935119, 517303624, 1398413971, 1440941125,
      408748095, 843339629, 689234447, 1582249629,
      326998476, 674671703, 980884287, 1695296433,
      523197562, 220481266, 710421401, 564990645,
      418558049, 1464875201, 1856827309, 1740482705
  )
}
