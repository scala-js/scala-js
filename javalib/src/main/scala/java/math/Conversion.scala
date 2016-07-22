/*
 * Ported by Alistair Johnson from
 * https://android.googlesource.com/platform/libcore/+/master/luni/src/main/java/java/math/Conversion.java
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

import scala.annotation.tailrec

/** Provides {@link BigInteger} base conversions.
 *
 *  Static library that provides {@link BigInteger} base conversion from/to any
 *  integer represented in a {@link java.lang.String} Object.
 */
private[math] object Conversion {

  /** Holds the maximal exponent for each radix.
   *
   *  Holds the maximal exponent for each radix, so that
   *  radix<sup>digitFitInInt[radix]</sup> fit in an {@code int} (32 bits).
   */
  final val DigitFitInInt = Array[Int](
      -1, -1, 31, 19, 15, 13, 11, 11, 10, 9, 9, 8, 8, 8, 8, 7, 7, 7, 7, 7, 7, 7,
      6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5)

  /** Precomputed maximal powers of radices.
   *
   *  BigRadices values are precomputed maximal powers of radices (integer
   *  numbers from 2 to 36) that fit into unsigned int (32 bits). bigRadices[0] =
   *  2 ^ 31, bigRadices[8] = 10 ^ 9, etc.
   */
  final val BigRadices = Array[Int](
      -2147483648, 1162261467, 1073741824, 1220703125, 362797056, 1977326743,
      1073741824, 387420489, 1000000000, 214358881, 429981696, 815730721,
      1475789056, 170859375, 268435456, 410338673, 612220032, 893871739,
      1280000000, 1801088541, 113379904, 148035889, 191102976, 244140625,
      308915776, 387420489, 481890304, 594823321, 729000000, 887503681,
      1073741824, 1291467969, 1544804416, 1838265625, 60466176)

  /** @see BigInteger#toString(int) */
  def bigInteger2String(bi: BigInteger, radix: Int): String = {
    val sign = bi.sign
    val numberLength = bi.numberLength
    val digits = bi.digits
    val radixOutOfBounds =
      radix < Character.MIN_RADIX || radix > Character.MAX_RADIX

    if (sign == 0) {
      "0"
    } else if (numberLength == 1) {
      val highDigit = digits(numberLength - 1)
      var v = highDigit & 0xFFFFFFFFL
      if (sign < 0)
        v = -v
      java.lang.Long.toString(v, radix)
    } else if (radix == 10 || radixOutOfBounds) {
      bi.toString
    } else {
      var bitsForRadixDigit: Double = 0.0
      bitsForRadixDigit = Math.log(radix) / Math.log(2)
      val addForSign = if (sign < 0) 1 else 0
      val biAbsLen = bi.abs().bitLength()
      val resLenInChars = (biAbsLen / bitsForRadixDigit + addForSign).toInt + 1
      var result: String = ""
      var currentChar = resLenInChars
      var resDigit: Int = 0

      if (radix != 16) {
        val temp = new Array[Int](numberLength)
        System.arraycopy(digits, 0, temp, 0, numberLength)
        var tempLen = numberLength
        val charsPerInt = DigitFitInInt(radix)
        val bigRadix = BigRadices(radix - 2)

        @inline
        @tailrec
        def loop(): Unit = {
          resDigit = Division.divideArrayByInt(temp, temp, tempLen, bigRadix)
          val previous = currentChar

          @inline
          @tailrec
          def innerLoop(): Unit = {
            currentChar -= 1
            result = Character.forDigit(resDigit % radix, radix) + result
            resDigit /= radix
            if(resDigit != 0 && currentChar != 0)
              innerLoop()
          }
          innerLoop()

          val delta = charsPerInt - previous + currentChar
          var i: Int = 0
          while (i < delta && currentChar > 0) {
            currentChar -= 1
            result = '0' + result
            i += 1
          }
          i = tempLen - 1
          while (i > 0 && temp(i) == 0) {
            i -= 1
          }
          tempLen = i + 1
          if (!(tempLen == 1 && temp(0) == 0))
            loop()
        }

        loop()
      } else {
        for (i <- 0 until numberLength) {
          var j = 0
          while (j < 8 && currentChar > 0) {
            resDigit = digits(i) >> (j << 2) & 0xf
            currentChar -= 1
            result = java.lang.Character.forDigit(resDigit, 16) + result
            j += 1
          }
        }
      }
      // strip leading zero's
      result = result.dropWhile(_ == '0')
      if (sign == -1) '-' + result
      else result
    }
  }


  /** The string representation scaled by zero.
   *
   *  Builds the correspondent {@code String} representation of {@code val} being
   *  scaled by 0.
   *
   *  @see BigInteger#toString()
   *  @see BigDecimal#toString()
   */
  def toDecimalScaledString(bi: BigInteger): String = {
    val sign: Int = bi.sign
    val numberLength: Int = bi.numberLength
    val digits: Array[Int] = bi.digits

    if (sign == 0) {
      "0"
    } else if (numberLength == 1) {
      val absStr = MathJDK8Bridge.toUnsignedString(digits(0))
      if (sign < 0) "-" + absStr
      else absStr
    } else {
      var result: String = ""

      val temp = new Array[Int](numberLength)
      var tempLen = numberLength
      System.arraycopy(digits, 0, temp, 0, tempLen)

      do {
        // Divide the array of digits by 1000000000 and compute the remainder
        var rem: Int = 0
        var i: Int = tempLen - 1
        while (i >= 0) {
          val temp1 = (rem.toLong << 32) + (temp(i) & 0xFFFFFFFFL)
          val quot = MathJDK8Bridge.divideUnsigned(temp1, 1000000000L).toInt
          temp(i) = quot
          rem = (temp1 - quot * 1000000000L).toInt
          i -= 1
        }

        // Convert the remainder to string, and add it to the result
        val remStr = rem.toString()
        val padding = "000000000".substring(remStr.length)
        result = padding + remStr + result

        while ((tempLen != 0) && (temp(tempLen - 1) == 0))
          tempLen -= 1
      } while (tempLen != 0)

      result = dropLeadingZeros(result)

      if (sign < 0) '-' + result
      else result
    }
  }

  private def dropLeadingZeros(s: String): String = {
    var zeroPrefixLength = 0
    val len = s.length
    while (zeroPrefixLength < len && s.charAt(zeroPrefixLength) == '0')
      zeroPrefixLength += 1
    s.substring(zeroPrefixLength)
  }

  /* can process only 32-bit numbers */
  def toDecimalScaledString(value: Long, scale: Int): String = {
    if (value == 0) {
      scale match {
        case 0 => "0"
        case 1 => "0.0"
        case 2 => "0.00"
        case 3 => "0.000"
        case 4 => "0.0000"
        case 5 => "0.00000"
        case 6 => "0.000000"
        case _ =>
          val scaleVal =
            if (scale == Int.MinValue) "2147483648"
            else java.lang.Integer.toString(-scale)

          val result  = if (scale < 0) "0E+" else "0E"
          result + scaleVal
      }
    } else {
      // one 32-bit unsigned value may contains 10 decimal digits
      // Explanation why 10+1+7:
      // +1 - one char for sign if needed.
      // +7 - For "special case 2" (see below) we have 7 free chars for inserting necessary scaled digits.
      val resLengthInChars = 18
      val negNumber = value < 0
      var result = ""
      //  Allocated [resLengthInChars+1] characters.
      // a free latest character may be used for "special case 1" (see below)
      var currentChar = resLengthInChars

      var v: Long = if (negNumber) -value else value
      do {
        val prev = v
        v /= 10
        currentChar -= 1
        result = (48 + (prev - v * 10)).toChar + result
      } while (v != 0)

      val exponent = resLengthInChars - currentChar - scale - 1

      if (scale > 0 && exponent >= -6) {
        val index = exponent + 1
        if (index > 0) {
          // special case 1
          result = result.substring(0, index) + "." + result.substring(index)
        } else {
          // special case 2
          for (j <- 0 until -index) {
            result = '0' + result
          }
          result = "0." + result
        }
      } else if (scale !=0) {
        var result1 =  exponent.toString
        if (exponent > 0)
          result1 = '+' + result1
        result1 = 'E' + result1

        result =
          if (resLengthInChars - currentChar > 1)
            result(0) + "." + result.substring(1) + result1
          else
            result + result1
      }

      if (negNumber) '-' + result
      else result
    }
  }

  def bigInteger2Double(bi: BigInteger): Double = {
    if (bi.numberLength < 2 || ((bi.numberLength == 2) && (bi.digits(1) > 0))) {
      bi.longValue()
    } else if (bi.numberLength > 32) {
      if (bi.sign > 0) Double.PositiveInfinity
      else Double.NegativeInfinity
    } else {
      val bitLen = bi.abs().bitLength()
      var exponent: Long = bitLen - 1
      val delta = bitLen - 54
      val lVal = bi.abs().shiftRight(delta).longValue()
      var mantissa = lVal & 0x1FFFFFFFFFFFFFL

      if (exponent == 1023 && mantissa == 0X1FFFFFFFFFFFFFL) {
        if (bi.sign > 0) Double.PositiveInfinity
        else Double.NegativeInfinity
      } else if (exponent == 1023 && mantissa == 0x1FFFFFFFFFFFFEL) {
        if (bi.sign > 0) Double.MaxValue
        else -Double.MaxValue
      } else {
        val droppedBits = BitLevel.nonZeroDroppedBits(delta, bi.digits)
        if (((mantissa & 1) == 1) && (((mantissa & 2) == 2) || droppedBits))
          mantissa += 2

        mantissa >>= 1
        val resSign = if (bi.sign < 0) 0x8000000000000000L else 0
        exponent = ((1023 + exponent) << 52) & 0x7FF0000000000000L
        val result = resSign | exponent | mantissa
        java.lang.Double.longBitsToDouble(result)
      }
    }
  }
}
