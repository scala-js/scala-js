/*
 * Ported by Alistair Johnson from
 * https://github.com/gwtproject/gwt/blob/master/user/super/com/google/gwt/emul/java/math/Primality.java
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

import java.util.Arrays
import java.util.Random

/** Provides primality probabilistic methods. */
private[math] object Primality {
  private val Bits = Array(
      0, 0, 1854, 1233, 927, 747, 627, 543, 480, 431, 393, 361, 335, 314, 295,
      279, 265, 253, 242, 232, 223, 216, 181, 169, 158, 150, 145, 140, 136,
      132, 127, 123, 119, 114, 110, 105, 101, 96, 92, 87, 83, 78, 73, 69, 64,
      59, 54, 49, 44, 38, 32, 26, 1)

  /** All prime numbers with bit length lesser than 10 bits. */
  private val Primes = Array[Int](2, 3, 5, 7, 11, 13, 17, 19, 23, 29,
      31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101,
      103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167,
      173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239,
      241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313,
      317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397,
      401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467,
      479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569,
      571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643,
      647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733,
      739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823,
      827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911,
      919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997, 1009,
      1013, 1019, 1021)

  /** Encodes how many i-bit primes there are in the table for {@code i=2,...,10}.
   *
   *  For example {@code offsetPrimes[6]} says that from index
   *  {@code 11} exists {@code 7} consecutive {@code 6}-bit prime numbers in the
   *  array.
   */
  private val OffsetPrimes = Array(
      null, null, (0, 2), (2, 2), (4, 2), (6, 5), (11, 7),
      (18, 13), (31, 23), (54, 43), (97, 75))

  /** All {@code BigInteger} prime numbers with bit length lesser than 8 bits. */
  private val BiPrimes =
    Array.tabulate[BigInteger](Primes.length)(i => BigInteger.valueOf(Primes(i)))

  /** A random number is generated until a probable prime number is found.
   *
   *  @see BigInteger#BigInteger(int,int,Random)
   *  @see BigInteger#probablePrime(int,Random)
   *  @see #isProbablePrime(BigInteger, int)
   */
  def consBigInteger(bitLength: Int, certainty: Int, rnd: Random): BigInteger = {
    // PRE: bitLength >= 2
    // For small numbers get a random prime from the prime table
    if (bitLength <= 10) {
      val rp = OffsetPrimes(bitLength)
      BiPrimes(rp._1 + rnd.nextInt(rp._2))
    } else {
      val shiftCount = (-bitLength) & 31
      val count = (bitLength + 31) >> 5
      val n = new BigInteger(1, count, new Array[Int](count))

      val last = count - 1
      do {
        // To fill the array with random integers
        for (i <- 0 until n.numberLength) {
          n.digits(i) = rnd.nextInt()
        }
        // To fix to the correct bitLength
        n.digits(last) = (n.digits(last) | 0x80000000) >>> shiftCount
        // To create an odd number
        n.digits(0) |= 1
      } while (!isProbablePrime(n, certainty))
      n
    }
  }

  /** Returns true if this is a prime, within the provided certainty.
   *
   *  @see BigInteger#isProbablePrime(int)
   *  @see #millerRabin(BigInteger, int)
   *  @ar.org.fitc.ref Optimizations: "A. Menezes - Handbook of applied
   *                   Cryptography, Chapter 4".
   */
  def isProbablePrime(n: BigInteger, certainty: Int): Boolean = {
    // scalastyle:off return
    // PRE: n >= 0
    if (certainty <= 0 || (n.numberLength == 1 && n.digits(0) == 2)) {
      true
    } else if (!n.testBit(0)) {
      // To discard all even numbers
      false
    } else if (n.numberLength == 1 && (n.digits(0) & 0XFFFFFC00) == 0) {
      // To check if 'n' exists in the table (it fit in 10 bits)
      Arrays.binarySearch(Primes, n.digits(0)) >= 0
    } else {
      // To check if 'n' is divisible by some prime of the table
      for (i <- 1 until Primes.length) {
        if (Division.remainderArrayByInt(n.digits, n.numberLength, Primes(i)) == 0)
          return false
      }

      // To set the number of iterations necessary for Miller-Rabin test
      var i: Int = 0
      val bitLength = n.bitLength()
      i = 2
      while (bitLength < Bits(i)) {
        i += 1
      }
      val newCertainty = Math.min(i, 1 + ((certainty - 1) >> 1))
      millerRabin(n, newCertainty)
    }
    // scalastyle:on return
  }

  /** Returns the next, probable prime number.
   *
   *  It uses the sieve of Eratosthenes to discard several composite numbers in
   *  some appropriate range (at the moment {@code [this, this + 1024]}). After
   *  this process it applies the Miller-Rabin test to the numbers that were not
   *  discarded in the sieve.
   *
   *  @see BigInteger#nextProbablePrime()
   *  @see #millerRabin(BigInteger, int)
   */
  def nextProbablePrime(n: BigInteger): BigInteger = {
    // scalastyle:off return
    // PRE: n >= 0
    val gapSize = 1024 // for searching of the next probable prime number
    val modules = new Array[Int](Primes.length)
    val isDivisible = new Array[Boolean](gapSize)

    // If n < "last prime of table" searches next prime in the table
    val digitsLessPrime = (n.digits(0) < Primes(Primes.length - 1))
    if ((n.numberLength == 1) && (n.digits(0) >= 0) && digitsLessPrime) {
      var i = 0
      while (n.digits(0) >= Primes(i)) {
        i += 1
      }
      return BiPrimes(i)
    }

    /*
     * Creates a "N" enough big to hold the next probable prime Note that: N <
     * "next prime" < 2*N
     */
    val a = new Array[Int](n.numberLength + 1)
    val startPoint: BigInteger = new BigInteger(1, n.numberLength, a)
    System.arraycopy(n.digits, 0, startPoint.digits, 0, n.numberLength)

    // To fix N to the "next odd number"
    if (n.testBit(0)) Elementary.inplaceAdd(startPoint, 2)
    else startPoint.digits(0) |= 1

    // To set the improved certainty of Miller-Rabin
    var certainty = 2
    for (j <- startPoint.bitLength() until Bits(certainty)) {
      certainty += 1
    }

    // To calculate modules: N mod p1, N mod p2, ... for first primes.
    for (i <- 0 until Primes.length) {
      modules(i) = Division.remainder(startPoint, Primes(i)) - gapSize
    }

    val probPrime: BigInteger = startPoint.copy()
    while (true) {
      // At this point, all numbers in the gap are initialized as probably primes
      Arrays.fill(isDivisible, false)
      // To discard multiples of first primes
      for (i <-0 until Primes.length) {
        modules(i) = (modules(i) + gapSize) % Primes(i)
        var j =
          if (modules(i) == 0) 0
          else (Primes(i) - modules(i))
        while (j < gapSize) {
          isDivisible(j) = true
          j += Primes(i)
        }
      }
      // To execute Miller-Rabin for non-divisible numbers by all first primes
      for (j <- 0 until gapSize) {
        if (!isDivisible(j)) {
          Elementary.inplaceAdd(probPrime, j)
          if (millerRabin(probPrime, certainty)) {
            return probPrime
          }
        }
      }
      Elementary.inplaceAdd(startPoint, gapSize)
    }
    throw new AssertionError("Primality.nextProbablePrime: Should not get here")
    // scalastyle:on return
  }

  /** The Miller-Rabin primality test.
   *
   *  @param n the input number to be tested.
   *  @param t the number of trials.
   *  @return {@code false} if the number is definitely compose, otherwise
   *          {@code true} with probability {@code 1 - 4<sup>(-t)</sup>}.
   *  @ar.org.fitc.ref "D. Knuth, The Art of Computer Programming Vo.2, Section
   *                   4.5.4., Algorithm P"
   */
  private def millerRabin(n: BigInteger, t: Int): Boolean = {
    // scalastyle:off return
    // PRE: n >= 0, t >= 0
    var x: BigInteger = null
    var y: BigInteger = null
    val nMinus1 = n.subtract(BigInteger.ONE)
    val bitLength = nMinus1.bitLength()
    val k = nMinus1.getLowestSetBit
    val q = nMinus1.shiftRight(k)
    val rnd = new Random()
    for (i <- 0 until t) {
      // To generate a witness 'x', first it use the primes of table
      if (i < Primes.length) {
        x = BiPrimes(i)
      } else {
        /*
         * It generates random witness only if it's necesssary. Note that all
         * methods would call Miller-Rabin with t <= 50 so this part is only to
         * do more robust the algorithm
         */
        do {
          x = new BigInteger(bitLength, rnd)
        } while ((x.compareTo(n) >= BigInteger.EQUALS) || x.sign == 0 || x.isOne)
      }

      y = x.modPow(q, n)
      if (!(y.isOne || y == nMinus1)) {
        for (j <- 1 until k) {
          if (y != nMinus1) {
            y = y.multiply(y).mod(n)
            if (y.isOne)
              return false
          }
        }
        if (y != nMinus1)
          return false
      }
    }
    true
    // scalastyle:on return
  }
}
