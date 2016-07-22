/*
 * Ported by Alistair Johnson from
 * https://github.com/gwtproject/gwt/blob/master/user/super/com/google/gwt/emul/java/math/Elementary.java
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

/** Provides the basic arithmetic mutable operations for {@link BigInteger}.
 *
 *  Object that provides the basic arithmetic mutable operations for
 *  {@link BigInteger}. The operations provided are listed below. <ul
 *  type="circle"> <li>Addition.</li> <li>Subtraction.</li> <li>Comparison.</li>
 *  </ul> In addition to this, some <i><b>Inplace</b></i> (mutable) methods are
 *  provided.
 */
private[math] object Elementary {

  private final val UINT_MAX = 0xffffffffL

  /** Adds two {@link BigInteger}.
   *
   *  @see BigInteger#add(BigInteger) .
   *  @param op1
   *  @param op2
   *  @return the sum.
   */
  def add(op1: BigInteger, op2: BigInteger): BigInteger = {
    // scalastyle:off return
    val op1Sign = op1.sign
    val op2Sign = op2.sign
    val op1Len = op1.numberLength
    val op2Len = op2.numberLength

    if (op1Sign == 0) {
      op2
    } else if (op2Sign == 0) {
      op1
    } else if (op1Len + op2Len == 2) {
      val a: Long = op1.digits(0) & UINT_MAX
      val b: Long = op2.digits(0) & UINT_MAX
      if (op1Sign == op2Sign) {
        val res = a + b
        val valueLo = res.toInt
        val valueHi = (res >>> 32).toInt
        if (valueHi == 0) new BigInteger(op1Sign, valueLo)
        else new BigInteger(op1Sign, 2, Array(valueLo, valueHi))
      } else {
        BigInteger.valueOf(if (op1Sign < 0) (b - a) else (a - b))
      }
    } else {
      val (resSign, resDigits) = {
        if (op1Sign == op2Sign) {
          // an augend should not be shorter than addend
          val res =
            if (op1Len >= op2Len) add(op1.digits, op1Len, op2.digits, op2Len)
            else add(op2.digits, op2Len, op1.digits, op1Len)
          (op1Sign, res)
        } else {
          // signs are different
          val cmp = {
            if (op1Len != op2Len) {
              if (op1Len > op2Len) 1
              else -1
            } else {
              compareArrays(op1.digits, op2.digits, op1Len)
            }
          }

          if (cmp == BigInteger.EQUALS)
            return BigInteger.ZERO
          else if (cmp == BigInteger.GREATER) // a minuend should not be shorter than subtrahend
            (op1Sign, subtract(op1.digits, op1Len, op2.digits, op2Len))
          else
            (op2Sign, subtract(op2.digits, op2Len, op1.digits, op1Len))
        }
      }

      val res = new BigInteger(resSign, resDigits.length, resDigits)
      res.cutOffLeadingZeroes()
      res
    }
    // scalastyle:on return
  }

  def compareArrays(a: Array[Int], b: Array[Int], size: Int): Int = {
    var i: Int = size - 1
    while ((i >= 0) && (a(i) == b(i))) {
      i -= 1
    }
    if (i < 0) BigInteger.EQUALS
    else if ((a(i) & UINT_MAX) < (b(i) & UINT_MAX)) BigInteger.LESS
    else BigInteger.GREATER
  }

  /** In place add on positive or negative {@link BigInteger}.
   *
   *  Same as @link #inplaceAdd(BigInteger, BigInteger), but without the
   *  restriction of non-positive values.
   *
   *  @param op1 any number
   *  @param op2 any number
   */
  def completeInPlaceAdd(op1: BigInteger, op2: BigInteger): Unit = {
    // scalastyle:off return
    if (op1.sign == 0) {
      System.arraycopy(op2.digits, 0, op1.digits, 0, op2.numberLength)
    } else if (op2.sign == 0) {
      return
    } else if (op1.sign == op2.sign) {
      add(op1.digits, op1.digits, op1.numberLength, op2.digits, op2.numberLength)
    } else {
      val sign =
        unsignedArraysCompare(op1.digits, op2.digits, op1.numberLength, op2.numberLength)
      if (sign > 0) {
        subtract(op1.digits, op1.digits, op1.numberLength, op2.digits, op2.numberLength)
      } else {
        inverseSubtract(op1.digits, op1.digits, op1.numberLength, op2.digits, op2.numberLength)
        op1.sign = -op1.sign
      }
    }
    op1.numberLength = Math.max(op1.numberLength, op2.numberLength) + 1
    op1.cutOffLeadingZeroes()
    op1.unCache()
    // scalastyle:on return
  }

  /** In place subtract of positive or negative {@link BigInteger}.
   *
   *  Same as @link #inplaceSubtract(BigInteger, BigInteger), but without the
   *  restriction of non-positive values.
   *
   *  @param op1 should have enough space to save the result
   *  @param op2
   */
  def completeInPlaceSubtract(op1: BigInteger, op2: BigInteger): Unit = {
    val resultSign = op1.compareTo(op2)
    if (op1.sign == 0) {
      System.arraycopy(op2.digits, 0, op1.digits, 0, op2.numberLength)
      op1.sign = -op2.sign
    } else if (op1.sign != op2.sign) {
      add(op1.digits, op1.digits, op1.numberLength, op2.digits, op2.numberLength)
      op1.sign = resultSign
    } else {
      val sign =
        unsignedArraysCompare(op1.digits, op2.digits, op1.numberLength, op2.numberLength)
      if (sign > 0) {
        subtract(op1.digits, op1.digits, op1.numberLength, op2.digits, op2.numberLength)
      } else {
        inverseSubtract(op1.digits, op1.digits, op1.numberLength, op2.digits, op2.numberLength)
        op1.sign = -op1.sign
      }
    }
    op1.numberLength = Math.max(op1.numberLength, op2.numberLength) + 1
    op1.cutOffLeadingZeroes()
    op1.unCache()
  }

  /** Performs {@code op1 += op2}.
   *
   *  {@code op1} must have enough place to store the result
   *  (i.e. {@code op1.bitLength() >= op2.bitLength()}).
   *  Both should be positive (i.e. {@code op1 >= op2}).
   *
   *  @param op1 the input minuend, and the output result.
   *  @param op2 the addend
   */
  def inplaceAdd(op1: BigInteger, op2: BigInteger): Unit = {
    add(op1.digits, op1.digits, op1.numberLength, op2.digits, op2.numberLength)
    op1.numberLength = Math.min(Math.max(op1.numberLength, op2.numberLength) + 1, op1.digits.length)
    op1.cutOffLeadingZeroes()
    op1.unCache()
  }

  /** Performs: {@code op1 += addend}.
   *
   *  The number must to have place to hold a possible carry.
   */
  def inplaceAdd(op1: BigInteger, addend: Int): Unit = {
    val carry = inplaceAdd(op1.digits, op1.numberLength, addend)
    if (carry == 1) {
      op1.digits(op1.numberLength) = 1
      op1.numberLength += 1
    }
    op1.unCache()
  }

  /** Adds an integer value to the array of integers remembering carry.
   *
   *  @return a possible generated carry (0 or 1)
   */
  def inplaceAdd(a: Array[Int], aSize: Int, addend: Int): Int = {
    var carry: Int = addend // unsigned
    var i = 0
    while (carry != 0 && i < aSize) {
      val sum = (carry & UINT_MAX) + (a(i) & UINT_MAX)
      a(i) = sum.toInt
      carry = (sum >> 32).toInt
      i += 1
    }
    carry
  }

  /** Performs {@code op1 -= op2}.
   *
   *  {@code op1} must have enough place to store the result
   *  (i.e. {@code op1.bitLength() >= op2.bitLength()}).
   *  Both should be positive (what implies that {@code op1 >= op2}).
   *
   *  @param op1 the input minuend, and the output result.
   *  @param op2 the subtrahend
   */
  def inplaceSubtract(op1: BigInteger, op2: BigInteger): Unit = {
    subtract(op1.digits, op1.digits, op1.numberLength, op2.digits, op2.numberLength)
    op1.cutOffLeadingZeroes()
    op1.unCache()
  }

  /** Subtracts two {@link BigInteger}.
   *
   *  @see BigInteger#subtract(BigInteger) .
   *  @param op1
   *  @param op2
   *  @return
   */
  def subtract(op1: BigInteger, op2: BigInteger): BigInteger = {
    // scalastyle:off return
    val op1Sign = op1.sign
    val op2Sign = op2.sign
    val op1Len = op1.numberLength
    val op2Len = op2.numberLength

    if (op2Sign == 0) {
      op1
    } else if (op1Sign == 0) {
      op2.negate()
    } else if (op1Len + op2Len == 2) {
      var a = (op1.digits(0) & UINT_MAX)
      var b = (op2.digits(0) & UINT_MAX)
      if (op1Sign < 0) {
        a = -a
      }
      if (op2Sign < 0) {
        b = -b
      }
      BigInteger.valueOf(a - b)
    } else {
      val cmp = {
        if (op1Len != op2Len) {
          if (op1Len > op2Len) 1
          else -1
        } else {
          Elementary.compareArrays(op1.digits, op2.digits, op1Len)
        }
      }
      if (op1Sign == op2Sign && cmp == BigInteger.EQUALS)
        return BigInteger.ZERO

      val (resSign, resDigits) = {
        if (cmp == BigInteger.LESS) {
          val res =
            if (op1Sign == op2Sign) subtract(op2.digits, op2Len, op1.digits, op1Len)
            else add(op2.digits, op2Len, op1.digits, op1Len)
          (-op2Sign, res)
        } else if (op1Sign == op2Sign) {
          (op1Sign, subtract(op1.digits, op1Len, op2.digits, op2Len))
        } else {
          (op1Sign, add(op1.digits, op1Len, op2.digits, op2Len))
        }
      }
      val res = new BigInteger(resSign, resDigits.length, resDigits)
      res.cutOffLeadingZeroes()
      res
    }
    // scalastyle:on return
  }

  /**  Adds the value represented by {@code b} to the value represented by {@code a}.
   *
   *  It is assumed the magnitude of a is not less than the magnitude of b.
   *
   *  @return {@code a + b}
   */
  private def add(a: Array[Int], aSize: Int, b: Array[Int], bSize: Int): Array[Int] = {
    val res = new Array[Int](aSize + 1)
    add(res, a, aSize, b, bSize)
    res
  }

  /** Performs {@code res = a + b}. */
  private def add(res: Array[Int], a: Array[Int], aSize: Int,
      b: Array[Int], bSize: Int): Unit = {
    var i: Int = 1
    val firstSum: Long = (a(0) & UINT_MAX) + (b(0) & UINT_MAX)
    res(0) = firstSum.toInt
    var carry = (firstSum >> 32).toInt
    if (aSize >= bSize) {
      while (i < bSize) {
        val sum = (a(i) & UINT_MAX) + (b(i) & UINT_MAX) + (carry & UINT_MAX)
        res(i) = sum.toInt
        carry = (sum >> 32).toInt
        i += 1
      }
      while (i < aSize) {
        val sum = (a(i) & UINT_MAX) + (carry & UINT_MAX)
        res(i) = sum.toInt
        carry = (sum >> 32).toInt
        i += 1
      }
    } else {
      while (i < aSize) {
        val sum = (a(i) & UINT_MAX) + (b(i) & UINT_MAX) + (carry & UINT_MAX)
        res(i) = sum.toInt
        carry = (sum >> 32).toInt
        i += 1
      }
      while (i < bSize) {
        val sum = (b(i) & UINT_MAX) + (carry & UINT_MAX)
        res(i) = sum.toInt
        carry = (sum >> 32).toInt
        i += 1
      }
    }
    if (carry != 0)
      res(i) = carry
  }

  /** Performs {@code res = b - a}. */
  private def inverseSubtract(res: Array[Int], a: Array[Int], aSize: Int,
      b: Array[Int], bSize: Int): Unit = {
    var i: Int = 0
    var borrow: Int = 0 // signed
    if (aSize < bSize) {
      while (i < aSize) {
        val sub = (b(i) & UINT_MAX) - (a(i) & UINT_MAX) + borrow.toLong
        res(i) = sub.toInt
        borrow = (sub >> 32).toInt // -1 or 0
        i += 1
      }
      while (i < bSize) {
        val sub = (b(i) & UINT_MAX) + borrow.toLong
        res(i) = sub.toInt
        borrow = (sub >> 32).toInt // -1 or 0
        i += 1
      }
    } else {
      while (i < bSize) {
        val sub = (b(i) & UINT_MAX) - (a(i) & UINT_MAX) + borrow.toLong
        res(i) = sub.toInt
        borrow = (sub >> 32).toInt // -1 or 0
        i += 1
      }
      while (i < aSize) {
        val sub = borrow.toLong - (a(i) & UINT_MAX)
        res(i) = sub.toInt
        borrow = (sub >> 32).toInt // -1 or 0
        i += 1
      }
    }
  }

  /** Subtracts the value represented by {@code b} from the value represented by {@code a}.
   *
   *  It is assumed the magnitude of a is not less than the magnitude of b.
   *
   *  @return {@code a - b}
   */
  private def subtract(a: Array[Int], aSize: Int, b: Array[Int], bSize: Int): Array[Int] = {
    val res = new Array[Int](aSize)
    subtract(res, a, aSize, b, bSize)
    res
  }

  /** Performs {@code res = a - b}.
   *
   *  It is assumed the magnitude of a is not less than the magnitude of b.
   */
  private def subtract(res: Array[Int], a: Array[Int], aSize: Int,
      b: Array[Int], bSize: Int): Unit = {
    var i: Int = 0
    var borrow: Int = 0 // signed
    while (i < bSize) {
      val sub = (a(i) & UINT_MAX) - (b(i) & UINT_MAX) + borrow.toLong
      res(i) = sub.toInt
      borrow = (sub >> 32).toInt
      i += 1
    }
    while (i < aSize) {
      val sub = (a(i) & UINT_MAX) + borrow.toLong
      res(i) = sub.toInt
      borrow = (sub >> 32).toInt
      i += 1
    }
  }

  /** Compares two arrays.
   *
   *  Compares two arrays, representing unsigned integer in little-endian order.
   *  Returns +1,0,-1 if a is - respective - greater, equal or lesser then b
   */
  private def unsignedArraysCompare(a: Array[Int], b: Array[Int], aSize: Int,
      bSize: Int): Int = {
    if (aSize > bSize) {
      1
    } else if (aSize < bSize) {
      -1
    } else {
      var i: Int = 0
      i = aSize - 1
      while (i >= 0 && a(i) == b(i)) {
        i -= 1
      }
      if (i < 0) BigInteger.EQUALS
      else if ((a(i) & UINT_MAX) < (b(i) & UINT_MAX)) BigInteger.LESS
      else BigInteger.GREATER
    }
  }
}
