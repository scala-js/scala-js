/*
 * Ported by Alistair Johnson from
 * https://github.com/gwtproject/gwt/blob/master/user/super/com/google/gwt/emul/java/math/BitLevel.java
 * Original license copied below:
 */

/*
 * Copyright 2009 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements. See the NOTICE file distributed with this
 * work for additional information regarding copyright ownership. The ASF
 * licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 *
 * INCLUDES MODIFICATIONS BY RICHARD ZSCHECH AS WELL AS GOOGLE.
 */

package java.math

/** Object that provides all the <b>bit level</b> operations for {@link BigInteger}.
 *
 *  The operations are: <ul type="circle"> <li>Left Shifting</li>
 *  <li>Right Shifting</li> <li>Bit clearing</li> <li>Bit setting</li> <li>Bit
 *  counting</li> <li>Bit testing</li> <li>Getting of the lowest bit set</li>
 *  </ul> All operations are provided in immutable way, and some in both mutable
 *  and immutable.
 */
private[math] object BitLevel {

  /** @see BigInteger#bitCount()
   *
   *  @param bi
   *  @return
   */
  def bitCount(bi: BigInteger): Int = {
    var bCount = 0
    if (bi.sign == 0) {
      0
    } else {
      var i = bi.getFirstNonzeroDigit
      if (bi.sign > 0) {
        while (i < bi.numberLength) {
          bCount += java.lang.Integer.bitCount(bi.digits(i))
          i += 1
        }
      } else {
        // (sign < 0) this digit absorbs the carry
        bCount += java.lang.Integer.bitCount(-bi.digits(i))
        i += 1
        while (i < bi.numberLength) {
          bCount += java.lang.Integer.bitCount(~bi.digits(i))
          i += 1
        }
        // We take the complement sum:
        bCount = (bi.numberLength << 5) - bCount
      }
      bCount
    }
  }

  /** @see BigInteger#bitLength()
   *
   *  @param bi
   *  @return
   */
  def bitLength(bi: BigInteger): Int = {
    if (bi.sign == 0) {
      0
    } else {
      var bLength = bi.numberLength << 5
      var highDigit = bi.digits(bi.numberLength - 1)
      if (bi.sign < 0) {
        val i = bi.getFirstNonzeroDigit
        // We reduce the problem to the positive case.
        if (i == bi.numberLength - 1)
          highDigit -= 1
      }
      // Subtracting all sign bits
      bLength -= java.lang.Integer.numberOfLeadingZeros(highDigit)
      bLength
    }
  }

  /** Performs a flipBit on the BigInteger.
   *
   *  Returns a BigInteger with the specified bit flipped.
   *
   *  @param bi BigInteger to operate on
   *  @param n the bit to flip
   */
  def flipBit(bi: BigInteger, n: Int): BigInteger = {
    val resSign = if (bi.sign == 0) 1 else bi.sign
    val intCount = n >> 5
    val bitN = n & 31
    val resLength = Math.max(intCount + 1, bi.numberLength) + 1
    val resDigits = new Array[Int](resLength)
    var i: Int = 0
    val bitNumber = 1 << bitN
    System.arraycopy(bi.digits, 0, resDigits, 0, bi.numberLength)
    if (bi.sign < 0) {
      if (intCount >= bi.numberLength) {
        resDigits(intCount) = bitNumber
      } else {
        val firstNonZeroDigit = bi.getFirstNonzeroDigit
        if (intCount > firstNonZeroDigit) {
          resDigits(intCount) ^= bitNumber
        } else if (intCount < firstNonZeroDigit) {
          resDigits(intCount) = -bitNumber
          i = intCount + 1
          while (i < firstNonZeroDigit) {
            resDigits(i) = -1
            i += 1
          }
          resDigits(i) -= 1
        } else {
          i = intCount
          resDigits(i) = -((-resDigits(intCount)) ^ bitNumber)
          if (resDigits(i) == 0) {
            i += 1
            while (resDigits(i) == -1) {
              resDigits(i) = 0
              i += 1
            }
            resDigits(i) += 1
          }
        }
      }
    } else {
      // case where val is positive
      resDigits(intCount) ^= bitNumber
    }
    val result = new BigInteger(resSign, resLength, resDigits)
    result.cutOffLeadingZeroes()
    result
  }

  /** Performs {@code val <<= count}. */
  def inplaceShiftLeft(bi: BigInteger, count: Int): Unit = {
    val intCount = count >> 5
    val numZeros = Integer.numberOfLeadingZeros(bi.digits(bi.numberLength - 1))
    val offset = if ((numZeros - (count & 31)) >= 0) 0 else 1

    bi.numberLength += intCount + offset
    shiftLeft(bi.digits, bi.digits, intCount, count & 31)
    bi.cutOffLeadingZeroes()
    bi.unCache()
  }

  /** Performs {@code val >>= count} where {@code val} is a positive number. */
  def inplaceShiftRight(bi: BigInteger, count: Int): Unit =  {
    val sign = bi.signum()
    if (!(count == 0 || bi.signum() == 0)) {
      val intCount = count >> 5 // count of integers
      bi.numberLength -= intCount
      val shift =
        shiftRight(bi.digits, bi.numberLength, bi.digits, intCount, count & 31)

      if (!shift && sign < 0) {
        // remainder not zero: add one to the result
        var i = 0
        while (i < bi.numberLength && (bi.digits(i) == -1)) {
          bi.digits(i) = 0
          i += 1
        }
        if (i == bi.numberLength)
          bi.numberLength += 1

        bi.digits(i) += 1
      }
      bi.cutOffLeadingZeroes()
      bi.unCache()
    }
  }

  /** Check if there are 1s in the lowest bits of this BigInteger.
   *
   *  @param numberOfBits the number of the lowest bits to check
   *  @return false if all bits are 0s, true otherwise
   */
  def nonZeroDroppedBits(numberOfBits: Int, digits: Array[Int]): Boolean = {
    val intCount = numberOfBits >> 5
    val bitCount = numberOfBits & 31
    var i = 0
    while (i < intCount && digits(i) == 0) {
      i += 1
    }
    (i != intCount) || (digits(i) << (32 - bitCount) != 0)
  }

  /** @see BigInteger#shiftLeft(int).
   *
   *  @param source
   *  @param count
   *  @return
   */
  def shiftLeft(source: BigInteger, count: Int): BigInteger = {
    val intCount: Int = count >> 5
    val andCount: Int = count & 31
    val offset = if (andCount == 0) 0 else 1
    val resLength: Int = source.numberLength + intCount + offset
    val resDigits = new Array[Int](resLength)
    shiftLeft(resDigits, source.digits, intCount, andCount)
    val result = new BigInteger(source.sign, resLength, resDigits)
    result.cutOffLeadingZeroes()
    result
  }

  /** Abstractly shifts left an array of integers in little endian.
   *
   *  (i.e. shift it right). Total shift distance in bits is intCount * 32 + count
   *
   *  @param result the destination array
   *  @param source the source array
   *  @param intCount the shift distance in integers
   *  @param count an additional shift distance in bits
   */
  def shiftLeft(result: Array[Int], source: Array[Int],
        intCount: Int, count: Int): Unit =  {
    if (count == 0) {
      System.arraycopy(source, 0, result, intCount, result.length - intCount)
    } else {
      val rightShiftCount: Int = 32 - count
      result(result.length - 1) = 0
      var i = result.length - 1
      while (i > intCount) {
        result(i) |= (source(i - intCount - 1) >>> rightShiftCount)
        result(i - 1) = (source(i - intCount - 1) << count)
        i -= 1
      }
    }
    for (i <- 0 until intCount) {
      result(i) = 0
    }
  }

  def shiftLeftOneBit(source: BigInteger): BigInteger = {
    val srcLen = source.numberLength
    val resLen = srcLen + 1
    val resDigits = new Array[Int](resLen)
    shiftLeftOneBit(resDigits, source.digits, srcLen)
    val result = new BigInteger(source.sign, resLen, resDigits)
    result.cutOffLeadingZeroes()
    result
  }

  /** Shifts the source digits left one bit.
   *
   *  Creates a value whose magnitude is doubled.
   *
   *  @param result an array of digits that will hold the computed result when
   *                this method returns. The size of this array is {@code srcLen + 1},
   *                and the format is the same as {@link BigInteger#digits}.
   *  @param source the array of digits to shift left, in the same format as
   *                {@link BigInteger#digits}.
   *  @param srcLen the length of {@code source}; may be less than {@code source.length}
   */
  def shiftLeftOneBit(result: Array[Int], source: Array[Int], srcLen: Int): Unit =  {
    var carry = 0
    for (i <- 0 until srcLen) {
      val iVal = source(i)
      result(i) = (iVal << 1) | carry
      carry = iVal >>> 31
    }
    if (carry != 0)
      result(srcLen) = carry
  }

  /** @see BigInteger#shiftRight(int).
   *
   *  @param source
   *  @param count
   *  @return
   */
  def shiftRight(source: BigInteger, count: Int): BigInteger = {
    val intCount: Int = count >> 5
    val andCount: Int = count & 31 // count of remaining bits

    if (intCount >= source.numberLength) {
      if (source.sign < 0) BigInteger.MINUS_ONE
      else BigInteger.ZERO
    } else {
      var resLength: Int = source.numberLength - intCount
      val resDigits = new Array[Int](resLength + 1)

      shiftRight(resDigits, resLength, source.digits, intCount, andCount)
      if (source.sign < 0) {
        // Checking if the dropped bits are zeros (the remainder equals to 0)
        var i: Int = 0
        while ((i < intCount) && (source.digits(i) == 0)) {
          i += 1
        }
        // If the remainder is not zero, add 1 to the result
        val cmp = (source.digits(i) << (32 - andCount)) != 0
        if (i < intCount || (andCount > 0 && cmp)) {
          i = 0
          while (i < resLength && resDigits(i) == -1) {
            resDigits(i) = 0
            i += 1
          }
          if (i == resLength)
            resLength += 1
          resDigits(i) += 1
        }
      }
      val result = new BigInteger(source.sign, resLength, resDigits)
      result.cutOffLeadingZeroes()
      result
    }
  }

  /** Shifts right an array of integers.
   *
   *  Total shift distance in bits is intCount * 32 + count.
   *
   *  @param result the destination array
   *  @param resultLen the destination array's length
   *  @param source the source array
   *  @param intCount the number of elements to be shifted
   *  @param count the number of bits to be shifted
   *  @return dropped bit's are all zero (i.e. remaider is zero)
   */
  def shiftRight(result: Array[Int], resultLen: Int, source: Array[Int],
        intCount: Int, count: Int): Boolean = {
    var i: Int = 0
    var allZero = true
    while (i < intCount) {
      allZero &= (source(i) == 0)
      i += 1
    }
    if (count == 0) {
      System.arraycopy(source, intCount, result, 0, resultLen)
    } else {
      val leftShiftCount = 32 - count
      allZero &= ((source(i) << leftShiftCount) == 0)
      i = 0
      while (i < resultLen - 1) {
        result(i) = (source(i + intCount) >>> count) | (source(i + intCount + 1) << leftShiftCount)
        i += 1
      }
      result(i) = source(i + intCount) >>> count
      i += 1
    }
    allZero
  }

  /** Performs a fast bit testing for positive numbers.
   *
   *  The bit to to be tested must be in the range {@code [0, val.bitLength()-1]}
   */
  def testBit(bi: BigInteger, n: Int): Boolean =
    (bi.digits(n >> 5) & (1 << (n & 31))) != 0
}
