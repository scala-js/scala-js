/*
 * Ported by Alistair Johnson from
 * https://android.googlesource.com/platform/libcore/+/master/luni/src/main/java/java/math/Multiplication.java
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

/** Object that provides all multiplication of {@link BigInteger} methods. */
private[math] object Multiplication {

  /** An array of powers of ten.
   *
   *  An array with powers of ten that fit in the type
   *  {@code int}.({@code 10^0,10^1,...,10^9})
   */
  private val TenPows = newArrayOfPows(10, 10)

  /** An array of powers of five.
   *
   *  An array with powers of five that fit in the type
   *  {@code int}.({@code 5^0,5^1,...,5^13})
   */
  private val FivePows = newArrayOfPows(14, 5)

  /** An array of {@code BigInteger} of powers of ten.
   *
   *  An array with the first powers of ten in {@code BigInteger} version.
   *  ({@code 10^0,10^1,...,10^31})
   */
  private[math] val BigTenPows = new Array[BigInteger](32)

  /** An array of {@code BigInteger} of powers of five.
   *
   *  An array with the first powers of five in {@code BigInteger} version.
   *  ({@code 5^0,5^1,...,5^31})
   */
  private[math] val BigFivePows = new Array[BigInteger](32)

  private final val whenUseKaratsuba = 63

  initialiseArrays()

  /** Multiplies an array of integers by an integer value.
   *
   *  @param a the array of integers
   *  @param aSize the number of elements of intArray to be multiplied
   *  @param factor the multiplier
   *  @return the top digit of production
   */
  def multiplyByInt(a: Array[Int], aSize: Int, factor: Int): Int =
    multiplyByInt(a, a, aSize, factor)

  /** Multiplies a number by a positive integer.
   *
   *  @param bi an arbitrary {@code BigInteger}
   *  @param factor a positive {@code int} number
   *  @return {@code val * factor}
   */
  def multiplyByPosInt(bi: BigInteger, factor: Int): BigInteger = {
    val resSign: Int = bi.sign
    val aNumberLength = bi.numberLength
    val aDigits = bi.digits

    if (resSign == 0) {
      BigInteger.ZERO
    } else if (aNumberLength == 1) {
      val res: Long = unsignedMultAddAdd(aDigits(0), factor, 0, 0)
      val resLo = res.toInt
      val resHi = (res >>> 32).toInt
      if (resHi == 0) new BigInteger(resSign, resLo)
      else new BigInteger(resSign, 2, Array(resLo, resHi))
    } else {
      val resLength = aNumberLength + 1
      val resDigits = new Array[Int](resLength)
      resDigits(aNumberLength) = multiplyByInt(resDigits, aDigits, aNumberLength, factor)
      val result = new BigInteger(resSign, resLength, resDigits)
      result.cutOffLeadingZeroes()
      result
    }
  }

  /** Multiplies a number by a power of ten.
   *
   *  This method is used in {@code BigDecimal} class.
   *
   *  @param bi the number to be multiplied
   *  @param exp a positive {@code long} exponent
   *  @return {@code val * 10<sup>exp</sup>}
   */
  def multiplyByTenPow(bi: BigInteger, exp: Long): BigInteger = {
    if (exp < TenPows.length) multiplyByPosInt(bi, TenPows(exp.toInt))
    else bi.multiply(powerOf10(exp))
  }

  /** Performs a<sup>2</sup>.
   *
   *  @param a The number to square.
   *  @param aLen The length of the number to square.
   */
  def square(a: Array[Int], aLen: Int, res: Array[Int]): Array[Int] = {
    var carry = 0

    for (i <- 0 until aLen) {
      carry = 0
      for (j <- i + 1 until aLen) {
        val t = unsignedMultAddAdd(a(i), a(j), res(i + j), carry)
        res(i + j) = t.toInt
        carry = (t >>> 32).toInt
      }
      res(i + aLen) = carry
    }
    BitLevel.shiftLeftOneBit(res, res, aLen << 1)
    carry = 0
    var i = 0
    var index = 0
    while (i < aLen) {
      val t = unsignedMultAddAdd(a(i), a(i), res(index), carry)
      res(index) = t.toInt
      index += 1
      val t2 = (t >>> 32) + (res(index) & 0xFFFFFFFFL)
      res(index) = t2.toInt
      carry = (t2 >>> 32).toInt
      i += 1
      index += 1
    }
    res
  }

  /** Computes the value unsigned ((uint)a*(uint)b + (uint)c + (uint)d).
   *
   *  @param a parameter 1
   *  @param b parameter 2
   *  @param c parameter 3
   *  @param d parameter 4
   *  @return value of expression
   */
  @inline def unsignedMultAddAdd(a: Int, b: Int, c: Int, d: Int): Long =
    (a & 0xFFFFFFFFL) * (b & 0xFFFFFFFFL) + (c & 0xFFFFFFFFL) + (d & 0xFFFFFFFFL)

  /** Performs the multiplication with the Karatsuba's algorithm.
   *
   *  <b>Karatsuba's algorithm:</b> <tt>
   *              u = u<sub>1</sub> * B + u<sub>0</sub><br>
   *              v = v<sub>1</sub> * B + v<sub>0</sub><br>
   *
   *   u*v = (u<sub>1</sub> * v<sub>1</sub>) * B<sub>2</sub> +
   *     ((u<sub>1</sub> - u<sub>0</sub>) * (v<sub>0</sub> - v<sub>1</sub>) +
   *       u<sub>1</sub> * v<sub>1</sub> + u<sub>0</sub> * v<sub>0</sub>) * B +
   *     u<sub>0</sub> * v<sub>0</sub><br>
   *  </tt>
   *
   *  @param op1 first factor of the product
   *  @param op2 second factor of the product
   *  @return {@code op1 * op2}
   *  @see #multiply(BigInteger, BigInteger)
   */
  def karatsuba(val1: BigInteger, val2: BigInteger): BigInteger = {
    val (op1, op2) =
      if (val2.numberLength > val1.numberLength) (val2, val1)
      else (val1, val2)

    if (op2.numberLength < whenUseKaratsuba) {
      multiplyPAP(op1, op2)
    } else {
      /*
       * Karatsuba: u = u1*B + u0 v = v1*B + v0 u*v = (u1*v1)*B^2 +
       * ((u1-u0)*(v0-v1) + u1*v1 + u0*v0)*B + u0*v0
       */
      val ndiv2 = (op1.numberLength & 0xFFFFFFFE) << 4
      val upperOp1 = op1.shiftRight(ndiv2)
      val upperOp2 = op2.shiftRight(ndiv2)
      val lowerOp1 = op1.subtract(upperOp1.shiftLeft(ndiv2))
      val lowerOp2 = op2.subtract(upperOp2.shiftLeft(ndiv2))

      var upper = karatsuba(upperOp1, upperOp2)
      val lower = karatsuba(lowerOp1, lowerOp2)
      var middle = karatsuba(upperOp1.subtract(lowerOp1), lowerOp2.subtract(upperOp2))
      middle = middle.add(upper).add(lower)
      middle = middle.shiftLeft(ndiv2)
      upper = upper.shiftLeft(ndiv2 << 1)
      upper.add(middle).add(lower)
    }
  }

  def multArraysPAP(aDigits: Array[Int], aLen: Int, bDigits: Array[Int],
      bLen: Int, resDigits: Array[Int]): Unit = {
    if (!(aLen == 0 || bLen == 0)) {
      if (aLen == 1)
        resDigits(bLen) = multiplyByInt(resDigits, bDigits, bLen, aDigits(0))
      else if (bLen == 1)
        resDigits(aLen) = multiplyByInt(resDigits, aDigits, aLen, bDigits(0))
      else
        multPAP(aDigits, bDigits, resDigits, aLen, bLen)
    }
  }

  def multiply(x: BigInteger, y: BigInteger): BigInteger = karatsuba(x, y)

  /** Multiplies two BigIntegers.
   *
   *  Implements traditional scholar algorithmdescribed by Knuth.
   *
   *  <br>
   *  <tt>
   *          <table border="0">
   *  <tbody>
   *
   *
   *  <tr>
   *  <td align="center">A=</td>
   *  <td>a<sub>3</sub></td>
   *  <td>a<sub>2</sub></td>
   *  <td>a<sub>1</sub></td>
   *  <td>a<sub>0</sub></td>
   *  <td></td>
   *  <td></td>
   *  </tr>
   *
   *  <tr>
   *  <td align="center">B=</td>
   *  <td></td>
   *  <td>b<sub>2</sub></td>
   *  <td>b<sub>1</sub></td>
   *  <td>b<sub>1</sub></td>
   *  <td></td>
   *  <td></td>
   *  </tr>
   *
   *  <tr>
   *  <td></td>
   *  <td></td>
   *  <td></td>
   *  <td>b<sub>0</sub>*a<sub>3</sub></td>
   *  <td>b<sub>0</sub>*a<sub>2</sub></td>
   *  <td>b<sub>0</sub>*a<sub>1</sub></td>
   *  <td>b<sub>0</sub>*a<sub>0</sub></td>
   *  </tr>
   *
   *  <tr>
   *  <td></td>
   *  <td></td>
   *  <td>b<sub>1</sub>*a<sub>3</sub></td>
   *  <td>b<sub>1</sub>*a<sub>2</sub></td>
   *  <td>b<sub>1</sub>*a1</td>
   *  <td>b<sub>1</sub>*a0</td>
   *  </tr>
   *
   *  <tr>
   *  <td>+</td>
   *  <td>b<sub>2</sub>*a<sub>3</sub></td>
   *  <td>b<sub>2</sub>*a<sub>2</sub></td>
   *  <td>b<sub>2</sub>*a<sub>1</sub></td>
   *  <td>b<sub>2</sub>*a<sub>0</sub></td>
   *  </tr>
   *
   *  <tr>
   *  <td></td>
   *  <td>______</td>
   *  <td>______</td>
   *  <td>______</td>
   *  <td>______</td>
   *  <td>______</td>
   *  <td>______</td>
   *  </tr>
   *
   *  <tr>
   *
   *  <td align="center">A*B=R=</td>
   *  <td align="center">r<sub>5</sub></td>
   *  <td align="center">r<sub>4</sub></td>
   *  <td align="center">r<sub>3</sub></td>
   *  <td align="center">r<sub>2</sub></td>
   *  <td align="center">r<sub>1</sub></td>
   *  <td align="center">r<sub>0</sub></td>
   *  <td></td>
   *  </tr>
   *
   *  </tbody>
   *  </table>
   *
   * </tt>
   *
   *  @param op1 first factor of the multiplication {@code op1 >= 0}
   *  @param op2 second factor of the multiplication {@code op2 >= 0}
   *  @return a {@code BigInteger} of value {@code op1 * op2}
   */
  def multiplyPAP(a: BigInteger, b: BigInteger): BigInteger = {
    val aLen = a.numberLength
    val bLen = b.numberLength
    val resLength = aLen + bLen
    val resSign =
      if (a.sign != b.sign) -1
      else 1

    if (resLength == 2) {
      val v = unsignedMultAddAdd(a.digits(0), b.digits(0), 0, 0)
      val valueLo = v.toInt
      val valueHi = (v >>> 32).toInt
      if (valueHi == 0) new BigInteger(resSign, valueLo)
      else new BigInteger(resSign, 2, Array(valueLo, valueHi))
    } else {
      val aDigits = a.digits
      val bDigits = b.digits
      val resDigits = new Array[Int](resLength)
      multArraysPAP(aDigits, aLen, bDigits, bLen, resDigits)
      val result = new BigInteger(resSign, resLength, resDigits)
      result.cutOffLeadingZeroes()
      result
    }
  }

  @noinline
  def pow(base: BigInteger, exponent: Int): BigInteger = {
    @inline
    @tailrec
    def loop(exp: Int, res: BigInteger, acc: BigInteger): BigInteger = {
      if (exp > 1) {
        val res2 =
          if ((exp & 1) != 0) res.multiply(acc)
          else res
        val acc2 = {
          if (acc.numberLength == 1) {
            acc.multiply(acc)
          } else {
            val a = new Array[Int](acc.numberLength << 1)
            val sq = square(acc.digits, acc.numberLength, a)
            new BigInteger(1, sq)
          }
        }
        loop(exp >> 1, res2, acc2)
      } else {
        res.multiply(acc)
      }
    }

    loop(exponent, BigInteger.ONE, base)
  }

  /** Calculates a power of ten, which exponent could be out of 32-bit range.
   *
   *  Note that internally this method will be used in the worst case with
   *  an exponent equals to: {@code Integer.MAX_VALUE - Integer.MIN_VALUE}.
   *  @param exp the exponent of power of ten, it must be positive.
   *  @return a {@code BigInteger} with value {@code 10<sup>exp</sup>}.
   */
  def powerOf10(exp: Long): BigInteger = {
    // "SMALL POWERS"
    if (exp < BigTenPows.length) {
      BigTenPows(exp.toInt)
    } else if (exp <= 50) {
      BigInteger.TEN.pow(exp.toInt)
    } else if (exp <= Int.MaxValue) { // "LARGE POWERS"
      BigFivePows(1).pow(exp.toInt).shiftLeft(exp.toInt)
    } else { //"HUGE POWERS"
      val powerOfFive = BigFivePows(1).pow(Integer.MAX_VALUE)
      var res: BigInteger = powerOfFive
      var longExp = exp - Int.MaxValue
      val intExp = (exp % Int.MaxValue).toInt
      while (longExp > Int.MaxValue) {
        res = res.multiply(powerOfFive)
        longExp -= Int.MaxValue
      }
      res = res.multiply(BigFivePows(1).pow(intExp))
      res = res.shiftLeft(Int.MaxValue)
      longExp = exp - Int.MaxValue
      while (longExp > Int.MaxValue) {
        res = res.shiftLeft(Int.MaxValue)
        longExp -= Int.MaxValue
      }
      res.shiftLeft(intExp)
    }
  }

  /** Multiplies a number by a power of five.
   *
   *  This method is used in {@code BigDecimal} class.
   *  @param val the number to be multiplied
   *  @param exp a positive {@code int} exponent
   *  @return {@code val * 5<sup>exp</sup>}
   */
  def multiplyByFivePow(bi: BigInteger, exp: Int): BigInteger = {
    if (exp < FivePows.length) multiplyByPosInt(bi, FivePows(exp))
    else if (exp < BigFivePows.length) bi.multiply(BigFivePows(exp))
    else bi.multiply(BigFivePows(1).pow(exp))
  }

  private def initialiseArrays(): Unit = {
    var fivePow = 1L
    for (i <- 0 until 32) {
      if (i <= 18) {
        BigFivePows(i) = BigInteger.valueOf(fivePow)
        BigTenPows(i) = BigInteger.valueOf(fivePow << i)
        fivePow *= 5
      } else {
        BigFivePows(i) = BigFivePows(i - 1).multiply(BigFivePows(1))
        BigTenPows(i) = BigTenPows(i - 1).multiply(BigInteger.TEN)
      }
    }
  }

  private def multiplyByInt(res: Array[Int], a: Array[Int], aSize: Int,
      factor: Int): Int = {
    var carry = 0
    for (i <- 0 until aSize) {
      val t = unsignedMultAddAdd(a(i), factor, carry, 0)
      res(i) = t.toInt
      carry = (t >>> 32).toInt
    }
    carry
  }

  private def multPAP(a: Array[Int], b: Array[Int], t: Array[Int],
      aLen: Int, bLen: Int): Unit = {
    if (a == b && aLen == bLen) {
      square(a, aLen, t)
    } else {
      for (i <- 0 until aLen) {
        var carry = 0
        val aI = a(i)
        for (j <- 0 until bLen) {
          val added = unsignedMultAddAdd(aI, b(j), t(i + j), carry)
          t(i + j) = added.toInt
          carry = (added >>> 32).toInt
        }
        t(i + bLen) = carry
      }
    }
  }

  private def newArrayOfPows(len: Int, pow: Int) =
    new Array[Int](len - 1).scanLeft[Int, Array[Int]](1)((z, _) => z * pow)
}
