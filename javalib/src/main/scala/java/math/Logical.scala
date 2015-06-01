/*
 * Ported by Alistair Johnson from
 * https://android.googlesource.com/platform/libcore/+/master/luni/src/main/java/java/math/Logical.java
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

/** Logical operations over {@code BigInteger}.
 *
 *  The library implements some logical operations over {@code BigInteger}. The
 *  operations provided are listed below.
 *  <ul type="circle">
 *  <li>not</li>
 *  <li>and</li>
 *  <li>andNot</li>
 *  <li>or</li>
 *  <li>xor</li>
 *  </ul>
 */
private[math] object Logical {

  /** @see BigInteger#not() */
  def not(bi: BigInteger): BigInteger = {
    // scalastyle:off return
    if (bi.sign == 0) {
      BigInteger.MINUS_ONE
    } else if (bi == BigInteger.MINUS_ONE) {
      BigInteger.ZERO
    } else {
      val resDigits = new Array[Int](bi.numberLength + 1)
      var i: Int = 0
      if (bi.sign > 0) {
        if (bi.digits(bi.numberLength - 1) != -1) {
          while (bi.digits(i) == -1) {
            i += 1
          }
        } else {
          while ((i < bi.numberLength) && (bi.digits(i) == -1)) {
            i += 1
          }
          if (i == bi.numberLength) {
            resDigits(i) = 1
            return new BigInteger(-bi.sign, i + 1, resDigits)
          }
        }
        // Here a carry 1 was generated
      } else {
        while (bi.digits(i) == 0) {
          resDigits(i) = -1
          i += 1
        }
        // Here a borrow -1 was generated
      }
      // Now, the carry/borrow can be absorbed
      resDigits(i) = bi.digits(i) + bi.sign
      // Copying the remaining unchanged digit
      i += 1
      while (i < bi.numberLength) {
        resDigits(i) = bi.digits(i)
        i += 1
      }
      new BigInteger(-bi.sign, i, resDigits)
    }
    // scalastyle:on return
  }

  /** @see BigInteger#and(BigInteger) */
  def and(bi: BigInteger, that: BigInteger): BigInteger = {
    if (that.sign == 0 || bi.sign == 0)
      BigInteger.ZERO
    else if (that == BigInteger.MINUS_ONE)
      bi
    else if (bi == BigInteger.MINUS_ONE)
      that
    else if (bi.sign > 0 && that.sign > 0)
      andPositive(bi, that)
    else if (bi.sign > 0)
      andDiffSigns(bi, that)
    else if (that.sign > 0)
      andDiffSigns(that, bi)
    else if (bi.numberLength > that.numberLength)
      andNegative(bi, that)
    else
      andNegative(that, bi)
  }

  /** @return sign = 1, magnitude = val.magnitude & that.magnitude */
  def andPositive(bi: BigInteger, that: BigInteger): BigInteger = {
    // PRE: both arguments are positive
    val resLength = Math.min(bi.numberLength, that.numberLength)
    var i = Math.max(bi.getFirstNonzeroDigit, that.getFirstNonzeroDigit)

    if (i >= resLength) {
      BigInteger.ZERO
    } else {
      val resDigits = new Array[Int](resLength)
      while (i < resLength) {
        resDigits(i) = bi.digits(i) & that.digits(i)
        i += 1
      }

      val result = new BigInteger(1, resLength, resDigits)
      result.cutOffLeadingZeroes()
      result
    }
  }

  /** @return sign = positive.magnitude & magnitude = -negative.magnitude */
  def andDiffSigns(positive: BigInteger, negative: BigInteger): BigInteger = {
    // PRE: positive is positive and negative is negative
    val iPos = positive.getFirstNonzeroDigit
    val iNeg = negative.getFirstNonzeroDigit

    // Look if the trailing zeros of the negative will "blank" all
    // the positive digits
    if (iNeg >= positive.numberLength) {
      BigInteger.ZERO
    } else {
      val resLength = positive.numberLength
      val resDigits = new Array[Int](resLength)

      // Must start from max(iPos, iNeg)
      var i = Math.max(iPos, iNeg)
      if (i == iNeg) {
        resDigits(i) = -negative.digits(i) & positive.digits(i)
        i += 1
      }
      val limit = Math.min(negative.numberLength, positive.numberLength)
      while (i < limit) {
        resDigits(i) = ~negative.digits(i) & positive.digits(i)
        i += 1
      }
      // if the negative was shorter must copy the remaining digits
      // from positive
      if (i >= negative.numberLength) {
        while (i < positive.numberLength) {
          resDigits(i) = positive.digits(i)
          i += 1
        }
      } // else positive ended and must "copy" virtual 0's, do nothing then

      val result = new BigInteger(1, resLength, resDigits)
      result.cutOffLeadingZeroes()
      result
    }
  }

  /** @return sign = -1, magnitude = -(-longer.magnitude & -shorter.magnitude) */
  def andNegative(longer: BigInteger, shorter: BigInteger): BigInteger = {
    // scalastyle:off return
    // PRE: longer and shorter are negative
    // PRE: longer has at least as many digits as shorter
    val iLonger = longer.getFirstNonzeroDigit
    val iShorter = shorter.getFirstNonzeroDigit

    // Does shorter matter?
    if (iLonger >= shorter.numberLength) {
      longer
    } else {
      var i = Math.max(iShorter, iLonger)
      var digit: Int =
        if (iShorter > iLonger) -shorter.digits(i) & ~longer.digits(i)
        else if (iShorter < iLonger) ~shorter.digits(i) & -longer.digits(i)
        else -shorter.digits(i) & -longer.digits(i)

      if (digit == 0) {
        i += 1

        @inline
        @tailrec
        def loop(bi1: BigInteger,bi2: BigInteger): Unit = {
          if (i < bi1.numberLength) {
            digit = ~(bi2.digits(i) | bi1.digits(i))
            if (digit == 0) {
              i += 1
              loop(bi1, bi2)
            }
          }
        }
        loop(shorter, longer)

        if (digit == 0) {
          // shorter has only the remaining virtual sign bits
          loop(longer, shorter)

          if (digit == 0) {
            val resLength = longer.numberLength + 1
            val resDigits = new Array[Int](resLength)
            resDigits(resLength - 1) = 1
            return new BigInteger(-1, resLength, resDigits)
          }
        }
      }
      val resLength = longer.numberLength
      val resDigits = new Array[Int](resLength)
      resDigits(i) = -digit
      i += 1
      while (i < shorter.numberLength) {
        resDigits(i) = longer.digits(i) | shorter.digits(i)
        i += 1
      }
      // shorter has only the remaining virtual sign bits
      while (i < longer.numberLength) {
        resDigits(i) = longer.digits(i)
        i += 1
      }
      new BigInteger(-1, resLength, resDigits)
    }
    // scalastyle:on return
  }

  /** @see BigInteger#andNot(BigInteger) */
  def andNot(bi: BigInteger, that: BigInteger): BigInteger = {
    if (that.sign == 0)
      bi
    else if (bi.sign == 0)
      BigInteger.ZERO
    else if (bi == BigInteger.MINUS_ONE)
      that.not()
    else if (that == BigInteger.MINUS_ONE)
      BigInteger.ZERO
    else if (bi.sign > 0 && that.sign > 0)
      andNotPositive(bi, that)
    else if (bi.sign > 0)
      andNotPositiveNegative(bi, that)
    else if (that.sign > 0)
      andNotNegativePositive(bi, that)
    else
      andNotNegative(bi, that)
  }

  /** @return sign = 1, magnitude = val.magnitude & ~that.magnitude */
  def andNotPositive(bi: BigInteger, that: BigInteger): BigInteger = {
    // PRE: both arguments are positive
    val resDigits = new Array[Int](bi.numberLength)
    val limit = Math.min(bi.numberLength, that.numberLength)
    var i: Int = 0
    i = bi.getFirstNonzeroDigit
    while (i < limit) {
      resDigits(i) = bi.digits(i) & ~that.digits(i)
      i += 1
    }
    while (i < bi.numberLength) {
      resDigits(i) = bi.digits(i)
      i += 1
    }

    val result = new BigInteger(1, bi.numberLength, resDigits)
    result.cutOffLeadingZeroes()
    result
  }

  /** @return sign = 1, magnitude = positive.magnitude & ~(-negative.magnitude) */
  def andNotPositiveNegative(positive: BigInteger, negative: BigInteger): BigInteger = {
    // PRE: positive > 0 && negative < 0
    val iNeg = negative.getFirstNonzeroDigit
    val iPos = positive.getFirstNonzeroDigit
    if (iNeg >= positive.numberLength) {
      positive
    } else {
      val resLength = Math.min(positive.numberLength, negative.numberLength)
      val resDigits = new Array[Int](resLength)

      // Always start from first non zero of positive
      var i = iPos
      while (i < iNeg) {
        resDigits(i) = positive.digits(i)
        i += 1
      }
      if (i == iNeg) {
        resDigits(i) = positive.digits(i) & (negative.digits(i) - 1)
        i += 1
      }
      while (i < resLength) {
        resDigits(i) = positive.digits(i) & negative.digits(i)
        i += 1
      }

      val result = new BigInteger(1, resLength, resDigits)
      result.cutOffLeadingZeroes()
      result
    }
  }

  /** @return sign = -1, magnitude = -(-negative.magnitude & ~positive.magnitude) */
  def andNotNegativePositive(negative: BigInteger, positive: BigInteger): BigInteger = {
    // scalastyle:off return
    // PRE: negative < 0 && positive > 0
    var limit: Int = 0
    var digit: Int = 0
    val iNeg = negative.getFirstNonzeroDigit
    val iPos = positive.getFirstNonzeroDigit
    if (iNeg >= positive.numberLength) {
      negative
    } else {
      val resLength = Math.max(negative.numberLength, positive.numberLength)
      val resDigits: Array[Int] = new Array[Int](resLength)
      var i = iNeg
      if (iPos > iNeg) {
        limit = Math.min(negative.numberLength, iPos)
        while (i < limit) {
          resDigits(i) = negative.digits(i)
          i += 1
        }
        if (i == negative.numberLength) {
          i = iPos
          while (i < positive.numberLength) {
            resDigits(i) = positive.digits(i)
            i += 1
          }
        }
      } else {
        digit = -negative.digits(i) & ~positive.digits(i)
        if (digit == 0) {
          limit = Math.min(positive.numberLength, negative.numberLength)
          i += 1

          @inline
          @tailrec
          def loop(): Unit = {
            if (i < limit) {
              digit = ~(negative.digits(i) | positive.digits(i))
              if(digit == 0) {
                i += 1
                loop()
              }
            }
          }
          loop()

          if (digit == 0) {
            @inline
            @tailrec
            def loop2(bi: BigInteger): Unit = {
              if (i < bi.numberLength)
                digit = ~bi.digits(i)
              if (digit == 0) {
                i += 1
                loop2(bi)
              }
            }
            loop2(positive)
            loop2(negative)

            if (digit == 0) {
              val resLength2 = resLength + 1
              val resDigits2 = new Array[Int](resLength2)
              resDigits2(resLength2 - 1) = 1
              return new BigInteger(-1, resLength2, resDigits2)
            }
          }
        }
        resDigits(i) = -digit
        i += 1
      }
      limit = Math.min(positive.numberLength, negative.numberLength)
      while (i < limit) {
        resDigits(i) = negative.digits(i) | positive.digits(i)
        i += 1
      }
      // Actually one of the next two cycles will be executed
      while (i < negative.numberLength) {
        resDigits(i) = negative.digits(i)
        i += 1
      }
      while (i < positive.numberLength) {
        resDigits(i) = positive.digits(i)
        i += 1
      }
      new BigInteger(-1, resLength, resDigits)
    }
    // scalastyle:on return
  }

  /** @return sign = 1, magnitude = -val.magnitude & ~(-that.magnitude) */
  def andNotNegative(bi: BigInteger, that: BigInteger): BigInteger = {
    // PRE: val < 0 && that < 0
    val iVal = bi.getFirstNonzeroDigit
    val iThat = that.getFirstNonzeroDigit
    if (iVal >= that.numberLength) {
      BigInteger.ZERO
    } else {
      val resLength = that.numberLength
      val resDigits = new Array[Int](resLength)
      var limit: Int = 0
      var i = iVal
      if (iVal < iThat) {
        resDigits(i) = -bi.digits(i)
        limit = Math.min(bi.numberLength, iThat)
        i += 1
        while (i < limit) {
          resDigits(i) = ~bi.digits(i)
          i += 1
        }
        if (i == bi.numberLength) {
          while (i < iThat) {
            resDigits(i) = -1
            i += 1
          }
          resDigits(i) = that.digits(i) - 1
        } else {
          resDigits(i) = ~bi.digits(i) & (that.digits(i) - 1)
        }
      } else  {
        resDigits(i) =
          if (iThat < iVal) -bi.digits(i) & that.digits(i)
          else -bi.digits(i) & (that.digits(i) - 1)
      }

      limit = Math.min(bi.numberLength, that.numberLength)
      i += 1
      while (i < limit) {
        resDigits(i) = ~bi.digits(i) & that.digits(i)
        i += 1
      }
      while (i < that.numberLength) {
        resDigits(i) = that.digits(i)
        i += 1
      }

      val result = new BigInteger(1, resLength, resDigits)
      result.cutOffLeadingZeroes()
      result
    }
  }

  /** @see BigInteger#or(BigInteger) */
  def or(bi: BigInteger, that: BigInteger): BigInteger = {
    if (that == BigInteger.MINUS_ONE || bi == BigInteger.MINUS_ONE) {
      BigInteger.MINUS_ONE
    } else if (that.sign == 0) {
      bi
    } else if (bi.sign == 0) {
      that
    } else if (bi.sign > 0) {
      if (that.sign > 0) {
        if (bi.numberLength > that.numberLength) orPositive(bi, that)
        else orPositive(that, bi)
      } else {
        orDiffSigns(bi, that)
      }
    } else if (that.sign > 0) {
      orDiffSigns(that, bi)
    } else if (that.getFirstNonzeroDigit > bi.getFirstNonzeroDigit) {
      orNegative(that, bi)
    } else {
      orNegative(bi, that)
    }
  }

  /** @return sign = 1, magnitude = longer.magnitude | shorter.magnitude */
  def orPositive(longer: BigInteger, shorter: BigInteger): BigInteger = {
    // PRE: longer and shorter are positive;
    // PRE: longer has at least as many digits as shorter
    val resLength = longer.numberLength
    val resDigits = new Array[Int](resLength)
    var i = 0
    while (i < shorter.numberLength) {
      resDigits(i) = longer.digits(i) | shorter.digits(i)
      i += 1
    }
    while (i < resLength) {
      resDigits(i) = longer.digits(i)
      i += 1
    }
    new BigInteger(1, resLength, resDigits)
  }

  /** @return sign = -1, magnitude = -(-val.magnitude | -that.magnitude) */
  def orNegative(bi: BigInteger, that: BigInteger): BigInteger = {
    // PRE: val and that are negative;
    // PRE: val has at least as many trailing zeros digits as that
    val iThat = that.getFirstNonzeroDigit
    val iVal = bi.getFirstNonzeroDigit
    var i = 0
    if (iVal >= that.numberLength) {
      that
    } else if (iThat >= bi.numberLength) {
      bi
    } else {
      val resLength = Math.min(bi.numberLength, that.numberLength)
      val resDigits = new Array[Int](resLength)

      //Looking for the first non-zero digit of the result
      if (iThat == iVal) {
        resDigits(iVal) = -(-bi.digits(iVal) | -that.digits(iVal))
        i = iVal
      } else {
        i = iThat
        while (i < iVal) {
          resDigits(i) = that.digits(i)
          i += 1
        }
        resDigits(i) = that.digits(i) & (bi.digits(i) - 1)
      }
      i += 1
      while (i < resLength) {
        resDigits(i) = bi.digits(i) & that.digits(i)
        i += 1
      }
      val result = new BigInteger(-1, resLength, resDigits)
      result.cutOffLeadingZeroes()
      result
    }
  }

  /** @return sign = -1, magnitude = -(positive.magnitude | -negative.magnitude) */
  def orDiffSigns(positive: BigInteger, negative: BigInteger): BigInteger = {
    // Jumping over the least significant zero bits
    val iNeg = negative.getFirstNonzeroDigit
    val iPos = positive.getFirstNonzeroDigit

    // Look if the trailing zeros of the positive will "copy" all
    // the negative digits
    if (iPos >= negative.numberLength) {
      negative
    } else {
      val resLength = negative.numberLength
      val resDigits = new Array[Int](resLength)
      var i = 0
      if (iNeg < iPos) {
        // We know for sure that this will be the first non zero digit in the result
        i = iNeg
        while (i < iPos) {
          resDigits(i) = negative.digits(i)
          i += 1
        }
      } else if (iPos < iNeg) {
        i = iPos
        resDigits(i) = -positive.digits(i)
        val limit = Math.min(positive.numberLength, iNeg)
        i += 1
        while (i < limit) {
          resDigits(i) = ~positive.digits(i)
          i += 1
        }
        if (i != positive.numberLength) {
          resDigits(i) = ~(-negative.digits(i) | positive.digits(i))
        } else {
          while (i < iNeg) {
            resDigits(i) = -1
            i += 1
          }
          resDigits(i) = negative.digits(i) - 1
        }
        i += 1
      } else {
        // Applying two complement to negative and to result
        i = iPos
        resDigits(i) = -(-negative.digits(i) | positive.digits(i))
        i += 1
      }
      val limit = Math.min(negative.numberLength, positive.numberLength)
      while (i < limit) {
        // Applying two complement to negative and to result
        resDigits(i) = negative.digits(i) & ~positive.digits(i)
        i += 1
      }
      while (i < negative.numberLength) {
        resDigits(i) = negative.digits(i)
        i += 1
      }

      val result = new BigInteger(-1, resLength, resDigits)
      result.cutOffLeadingZeroes()
      result
    }
  }

  /** @see BigInteger#xor(BigInteger) */
  def xor(bi: BigInteger, that: BigInteger): BigInteger = {
    if (that.sign == 0) {
      bi
    } else if (bi.sign == 0) {
      that
    } else if (that == BigInteger.MINUS_ONE) {
      bi.not()
    } else if (bi == BigInteger.MINUS_ONE) {
      that.not()
    } else if (bi.sign > 0) {
      if (that.sign > 0) {
        if (bi.numberLength > that.numberLength) xorPositive(bi, that)
        else xorPositive(that, bi)
      } else {
        xorDiffSigns(bi, that)
      }
    } else if (that.sign > 0) {
      xorDiffSigns(that, bi)
    } else if (that.getFirstNonzeroDigit > bi.getFirstNonzeroDigit) {
      xorNegative(that, bi)
    } else {
      xorNegative(bi, that)
    }
  }

  /** @return sign = 0, magnitude = longer.magnitude | shorter.magnitude */
  def xorPositive(longer: BigInteger, shorter: BigInteger): BigInteger = {
    // PRE: longer and shorter are positive;
    // PRE: longer has at least as many digits as shorter
    val resLength = longer.numberLength
    val resDigits = new Array[Int](resLength)
    var i = Math.min(longer.getFirstNonzeroDigit, shorter.getFirstNonzeroDigit)
    while (i < shorter.numberLength) {
      resDigits(i) = longer.digits(i) ^ shorter.digits(i)
      i += 1
    }
    while (i < longer.numberLength) {
      resDigits(i) = longer.digits(i)
      i += 1
    }
    val result = new BigInteger(1, resLength, resDigits)
    result.cutOffLeadingZeroes()
    result
  }

  /** @return sign = 0, magnitude = -val.magnitude ^ -that.magnitude */
  def xorNegative(bi: BigInteger, that: BigInteger): BigInteger = {
    // PRE: val and that are negative
    // PRE: val has at least as many trailing zero digits as that
    val resLength = Math.max(bi.numberLength, that.numberLength)
    val resDigits = new Array[Int](resLength)
    val iVal = bi.getFirstNonzeroDigit
    val iThat = that.getFirstNonzeroDigit
    var i = iThat

    if (iVal == iThat) {
      resDigits(i) = -bi.digits(i) ^ -that.digits(i)
    } else {
      resDigits(i) = -that.digits(i)
      val limit = Math.min(that.numberLength, iVal)
      i += 1
      while (i < limit) {
        resDigits(i) = ~that.digits(i)
        i += 1
      }
      // Remains digits in that?
      if (i == that.numberLength) {
        //Jumping over the remaining zero to the first non one
        while (i < iVal) {
          resDigits(i) = -1
          i += 1
        }
        resDigits(i) = bi.digits(i) - 1
      } else {
        resDigits(i) = -bi.digits(i) ^ ~that.digits(i)
      }
    }
    val limit = Math.min(bi.numberLength, that.numberLength)
    //Perform ^ between that al val until that ends
    i += 1
    while (i < limit) {
      resDigits(i) = bi.digits(i) ^ that.digits(i)
      i += 1
    }
    //Perform ^ between val digits and -1 until val ends
    while (i < bi.numberLength) {
      resDigits(i) = bi.digits(i)
      i += 1
    }
    while (i < that.numberLength) {
      resDigits(i) = that.digits(i)
      i += 1
    }
    val result = new BigInteger(1, resLength, resDigits)
    result.cutOffLeadingZeroes()
    result
  }

  /** @return sign = 1, magnitude = -(positive.magnitude ^ -negative.magnitude) */
  def xorDiffSigns(positive: BigInteger, negative: BigInteger): BigInteger = {
    // scalastyle:off return
    val resLength = Math.max(negative.numberLength, positive.numberLength)
    val resDigits: Array[Int] = new Array[Int](resLength)
    val iNeg = negative.getFirstNonzeroDigit
    val iPos = positive.getFirstNonzeroDigit
    var i = 0

    //The first
    if (iNeg < iPos) {
      i = iNeg
      resDigits(i) = negative.digits(i)
      val limit = Math.min(negative.numberLength, iPos)

      //Skip the positive digits while they are zeros
      i += 1
      while (i < limit) {
        resDigits(i) = negative.digits(i)
        i += 1
      }
      //if the negative has no more elements, must fill the
      //result with the remaining digits of the positive
      if (i == negative.numberLength) {
        while (i < positive.numberLength) {
          resDigits(i) = positive.digits(i)
          i += 1
        }
      }
    } else if (iPos < iNeg) {
      i = iPos
      //Applying two complement to the first non-zero digit of the result
      resDigits(i) = -positive.digits(i)
      val limit = Math.min(positive.numberLength, iNeg)
      i += 1
      while (i < limit) {
        //Continue applying two complement the result
        resDigits(i) = ~positive.digits(i)
        i += 1
      }
      //When the first non-zero digit of the negative is reached, must apply
      //two complement (arithmetic negation) to it, and then operate
      if (i == iNeg) {
        resDigits(i) = ~(positive.digits(i) ^ -negative.digits(i))
        i += 1
      } else {
        //if the positive has no more elements must fill the remaining digits with
        //the negative ones
        while (i < iNeg) {
          resDigits(i) = -1
          i += 1
        }
        while (i < negative.numberLength) {
          resDigits(i) = negative.digits(i)
          i += 1
        }
      }
    } else {
      //The first non-zero digit of the positive and negative are the same
      i = iNeg
      var digit = positive.digits(i) ^ -negative.digits(i)
      if (digit == 0) {
        val limit = Math.min(positive.numberLength, negative.numberLength)
        i += 1
        while (i < limit && {digit = positive.digits(i) ^ ~negative.digits(i);digit} == 0) {
          i += 1
        }
        if (digit == 0) {
          // shorter has only the remaining virtual sign bits
          def loop(bi: BigInteger): Unit = {
            if (i < bi.numberLength) {
              digit = ~bi.digits(i)
              if (digit == 0) {
                i += 1
                loop(bi)
              }
            }
          }
          loop(positive)
          loop(negative)
          if (digit == 0) {
            val newResLength = resLength + 1
            val resDigits = new Array[Int](newResLength)
            resDigits(resLength - 1) = 1
            return new BigInteger(-1, newResLength, resDigits)
          }
        }
      }

      resDigits(i) = -digit
      i += 1
    }
    val limit = Math.min(negative.numberLength, positive.numberLength)
    while (i < limit) {
      resDigits(i) = ~(~negative.digits(i) ^ positive.digits(i))
      i += 1
    }
    while (i < positive.numberLength) {
      resDigits(i) = positive.digits(i)
      i += 1
    }
    while (i < negative.numberLength) {
      resDigits(i) = negative.digits(i)
      i += 1
    }
    val result = new BigInteger(-1, resLength, resDigits)
    result.cutOffLeadingZeroes()
    result
    // scalastyle:on return
  }
}
