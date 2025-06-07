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

package org.scalajs.linker.runtime

import scala.scalajs.js

/** Polyfills for manipulating the bits of floating point numbers without DataView.
 *
 *  These polyfills are only used when targeting ECMAScript 5.1.
 *
 *  Originally inspired by
 *  https://github.com/inexorabletash/polyfill/blob/3447582628b6e3ea81959c4d5987aa332c22d1ca/typedarray.js#L150-L264
 *
 *  Note that if typed arrays are not supported, it is almost certain that
 *  fround is not supported natively, so Float operations are extremely slow.
 *
 *  We therefore do all computations in Doubles here.
 */
object FloatingPointBitsPolyfills {
  private val floatPowsOf2: js.Array[Double] =
    makePowsOf2(len = 1 << 8, java.lang.Float.MIN_NORMAL.toDouble)

  private val doublePowsOf2: js.Array[Double] =
    makePowsOf2(len = 1 << 11, java.lang.Double.MIN_NORMAL)

  private def makePowsOf2(len: Int, minNormal: Double): js.Array[Double] = {
    val r = new js.Array[Double](len)
    r(0) = 0.0
    var i = 1
    var next = minNormal
    while (i != len - 1) {
      r(i) = next
      i += 1
      next *= 2
    }
    r(len - 1) = Double.PositiveInfinity
    r
  }

  @inline // inline into the static forwarder, which will be the entry point
  def floatFromBits(bits: Int): Double = {
    val ebits = 8
    val fbits = 23
    val sign = (bits >> 31) | 1 // -1 or 1
    val e = (bits >> fbits) & ((1 << ebits) - 1)
    val f = bits & ((1 << fbits) - 1)
    decodeIEEE754(ebits, fbits, floatPowsOf2, Float.MinPositiveValue, sign, e, f)
  }

  @inline // inline into the static forwarder, which will be the entry point
  def floatToBits(floatValue: Float): Int = {
    // Some constants
    val ebits = 8
    val fbits = 23

    // Force computations to be on Doubles
    val value = floatValue.toDouble

    // Determine sign bit and compute the absolute value av
    val sign = if (value < 0.0 || (value == 0.0 && 1.0 / value < 0.0)) -1 else 1
    val s = sign & Int.MinValue
    val av = sign * value

    // Compute e and f
    val powsOf2 = this.floatPowsOf2 // local cache
    val e = encodeIEEE754Exponent(ebits, powsOf2, av)
    val f = encodeIEEE754MantissaBits(ebits, fbits, powsOf2, Float.MinPositiveValue.toDouble, av, e)

    // Encode
    s | (e << fbits) | rawToInt(f)
  }

  @inline // inline into the static forwarder, which will be the entry point
  def doubleFromBits(bits: Long): Double = {
    val ebits = 11
    val fbits = 52
    val hifbits = fbits - 32
    val hi = (bits >>> 32).toInt
    val lo = (bits & 0xffffffffL).toDouble
    val sign = (hi >> 31) | 1 // -1 or 1
    val e = (hi >> hifbits) & ((1 << ebits) - 1)
    val f = (hi & ((1 << hifbits) - 1)).toDouble * 0x100000000L.toDouble + lo
    decodeIEEE754(ebits, fbits, doublePowsOf2, Double.MinPositiveValue, sign, e, f)
  }

  @inline // inline into the static forwarder, which will be the entry point
  def doubleToBits(value: Double): Long = {
    // Some constants
    val ebits = 11
    val fbits = 52
    val hifbits = fbits - 32

    // Determine sign bit and compute the absolute value av
    val sign = if (value < 0.0 || (value == 0.0 && 1.0 / value < 0.0)) -1 else 1
    val s = sign & Int.MinValue
    val av = sign * value

    // Compute e and f
    val powsOf2 = this.doublePowsOf2 // local cache
    val e = encodeIEEE754Exponent(ebits, powsOf2, av)
    val f = encodeIEEE754MantissaBits(ebits, fbits, powsOf2, Double.MinPositiveValue, av, e)

    // Encode
    val hi = s | (e << hifbits) | rawToInt(f / 0x100000000L.toDouble)
    val lo = rawToInt(f)
    (hi.toLong << 32) | (lo.toLong & 0xffffffffL)
  }

  @inline
  private def decodeIEEE754(ebits: Int, fbits: Int,
      powsOf2: js.Array[Double], minPositiveValue: Double,
      sign: Int, e: Int, f: Double): Double = {

    // Some constants
    val specialExponent = (1 << ebits) - 1
    val twoPowFbits = (1L << fbits).toDouble

    if (e == specialExponent) {
      // Special
      if (f == 0.0)
        sign * Double.PositiveInfinity
      else
        Double.NaN
    } else if (e > 0) {
      // Normalized
      sign * powsOf2(e) * (1 + f / twoPowFbits)
    } else {
      // Subnormal
      sign * f * minPositiveValue
    }
  }

  @inline
  private def encodeIEEE754Exponent(ebits: Int,
      powsOf2: js.Array[Double], av: Double): Int = {

    /* Binary search of `av` inside `powsOf2`.
     * There are exactly `ebits` iterations of this loop (11 for Double, 8 for Float).
     */
    var eMin = 0
    var eMax = 1 << ebits
    while (eMin + 1 < eMax) {
      val e = (eMin + eMax) >> 1
      if (av < powsOf2(e)) // false when av is NaN
        eMax = e
      else
        eMin = e
    }
    eMin
  }

  @inline
  private def encodeIEEE754MantissaBits(ebits: Int, fbits: Int,
      powsOf2: js.Array[Double], minPositiveValue: Double,
      av: Double, e: Int): Double = {

    // Some constants
    val specialExponent = (1 << ebits) - 1
    val twoPowFbits = (1L << fbits).toDouble

    if (e == specialExponent) {
      if (av != av)
        (1L << (fbits - 1)).toDouble // NaN
      else
        0.0 // Infinity
    } else {
      if (e == 0)
        av / minPositiveValue // Subnormal
      else
        ((av / powsOf2(e)) - 1.0) * twoPowFbits // Normal
    }
  }

  @inline private def rawToInt(x: Double): Int =
    (x.asInstanceOf[js.Dynamic] | 0.asInstanceOf[js.Dynamic]).asInstanceOf[Int]

}
