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

package java.lang

import scala.scalajs.js
import scala.scalajs.js.Dynamic.global
import scala.scalajs.js.typedarray
import scala.scalajs.LinkingInfo.ESVersion

/** Manipulating the bits of floating point numbers. */
private[lang] object FloatingPointBits {

  import scala.scalajs.runtime.linkingInfo

  private[this] val _areTypedArraysSupported = {
    // Here we use the `esVersion` test to dce the 4 subsequent tests
    linkingInfo.esVersion >= ESVersion.ES2015 || {
      js.typeOf(global.ArrayBuffer) != "undefined" &&
      js.typeOf(global.Int32Array) != "undefined" &&
      js.typeOf(global.Float32Array) != "undefined" &&
      js.typeOf(global.Float64Array) != "undefined"
    }
  }

  @inline
  private def areTypedArraysSupported: scala.Boolean = {
    /* We have a forwarder to the internal `val _areTypedArraysSupported` to
     * be able to inline it. This achieves the following:
     * * If we emit ES2015+, dce `|| _areTypedArraysSupported` and replace
     *   `areTypedArraysSupported` by `true` in the calling code, allowing
     *   polyfills in the calling code to be dce'ed in turn.
     * * If we emit ES5, replace `areTypedArraysSupported` by
     *   `_areTypedArraysSupported` so we do not calculate it multiple times.
     */
    linkingInfo.esVersion >= ESVersion.ES2015 || _areTypedArraysSupported
  }

  private val arrayBuffer =
    if (areTypedArraysSupported) new typedarray.ArrayBuffer(8)
    else null

  private val int32Array =
    if (areTypedArraysSupported) new typedarray.Int32Array(arrayBuffer, 0, 2)
    else null

  private val float32Array =
    if (areTypedArraysSupported) new typedarray.Float32Array(arrayBuffer, 0, 2)
    else null

  private val float64Array =
    if (areTypedArraysSupported) new typedarray.Float64Array(arrayBuffer, 0, 1)
    else null

  private val areTypedArraysBigEndian = {
    if (areTypedArraysSupported) {
      int32Array(0) = 0x01020304
      (new typedarray.Int8Array(arrayBuffer, 0, 8))(0) == 0x01
    } else {
      true // as good a value as any
    }
  }

  private val highOffset = if (areTypedArraysBigEndian) 0 else 1
  private val lowOffset  = if (areTypedArraysBigEndian) 1 else 0

  /** Hash code of a number (excluding Longs).
   *
   *  Because of the common encoding for integer and floating point values,
   *  the hashCode of Floats and Doubles must align with that of Ints for the
   *  common values.
   *
   *  For other values, we use the hashCode specified by the JavaDoc for
   *  *Doubles*, even for values which are valid Float values. Because of the
   *  previous point, we cannot align completely with the Java specification,
   *  so there is no point trying to be a bit more aligned here. Always using
   *  the Double version should typically be faster on VMs without fround
   *  support because we avoid several fround operations.
   */
  def numberHashCode(value: scala.Double): Int = {
    val iv = rawToInt(value)
    if (iv == value && 1.0/value != scala.Double.NegativeInfinity) {
      iv
    } else {
      /* Basically an inlined version of `Long.hashCode(doubleToLongBits(value))`,
       * so that we never allocate a RuntimeLong instance (or anything, for
       * that matter).
       *
       * In addition, in the happy path where typed arrays are supported, since
       * we xor together the two Ints, it doesn't matter which one comes first
       * or second, and hence we can use constants 0 and 1 instead of having an
       * indirection through `highOffset` and `lowOffset`.
       */
      if (areTypedArraysSupported) {
        float64Array(0) = value
        int32Array(0) ^ int32Array(1)
      } else {
        doubleHashCodePolyfill(value)
      }
    }
  }

  @noinline
  private def doubleHashCodePolyfill(value: scala.Double): Int =
    Long.hashCode(doubleToLongBitsPolyfillInline(value))

  def intBitsToFloat(bits: Int): scala.Float = {
    if (areTypedArraysSupported) {
      int32Array(0) = bits
      float32Array(0)
    } else {
      intBitsToFloatPolyfill(bits).toFloat
    }
  }

  def floatToIntBits(value: scala.Float): Int = {
    if (areTypedArraysSupported) {
      float32Array(0) = value
      int32Array(0)
    } else {
      floatToIntBitsPolyfill(value.toDouble)
    }
  }

  def longBitsToDouble(bits: scala.Long): scala.Double = {
    if (areTypedArraysSupported) {
      int32Array(highOffset) = (bits >>> 32).toInt
      int32Array(lowOffset) = bits.toInt
      float64Array(0)
    } else {
      longBitsToDoublePolyfill(bits)
    }
  }

  def doubleToLongBits(value: scala.Double): scala.Long = {
    if (areTypedArraysSupported) {
      float64Array(0) = value
      ((int32Array(highOffset).toLong << 32) |
          (int32Array(lowOffset).toLong & 0xffffffffL))
    } else {
      doubleToLongBitsPolyfill(value)
    }
  }

  /* --- Polyfills for floating point bit manipulations ---
   *
   * Originally inspired by
   * https://github.com/inexorabletash/polyfill/blob/3447582628b6e3ea81959c4d5987aa332c22d1ca/typedarray.js#L150-L264
   *
   * Note that if typed arrays are not supported, it is almost certain that
   * fround is not supported natively, so Float operations are extremely slow.
   *
   * We therefore do all computations in Doubles here, which is also more
   * predictable, since the results do not depend on strict floats semantics.
   */

  private def intBitsToFloatPolyfill(bits: Int): scala.Double = {
    val ebits = 8
    val fbits = 23
    val s = bits < 0
    val e = (bits >> fbits) & ((1 << ebits) - 1)
    val f = bits & ((1 << fbits) - 1)
    decodeIEEE754(ebits, fbits, scala.Float.MinPositiveValue, s, e, f)
  }

  private def floatToIntBitsPolyfill(value: scala.Double): Int = {
    val ebits = 8
    val fbits = 23
    val sef = encodeIEEE754(ebits, fbits, Float.MIN_NORMAL, scala.Float.MinPositiveValue, value)
    (if (sef.s) 0x80000000 else 0) | (sef.e << fbits) | rawToInt(sef.f)
  }

  private def longBitsToDoublePolyfill(bits: scala.Long): scala.Double = {
    val ebits = 11
    val fbits = 52
    val hifbits = fbits-32
    val hi = (bits >>> 32).toInt
    val lo = Utils.toUint(bits.toInt)
    val s = hi < 0
    val e = (hi >> hifbits) & ((1 << ebits) - 1)
    val f = (hi & ((1 << hifbits) - 1)).toDouble * 0x100000000L.toDouble + lo
    decodeIEEE754(ebits, fbits, scala.Double.MinPositiveValue, s, e, f)
  }

  @noinline
  private def doubleToLongBitsPolyfill(value: scala.Double): scala.Long =
    doubleToLongBitsPolyfillInline(value)

  @inline
  private def doubleToLongBitsPolyfillInline(value: scala.Double): scala.Long = {
    val ebits = 11
    val fbits = 52
    val hifbits = fbits-32
    val sef = encodeIEEE754(ebits, fbits, Double.MIN_NORMAL, scala.Double.MinPositiveValue, value)
    val hif = rawToInt(sef.f / 0x100000000L.toDouble)
    val hi = (if (sef.s) 0x80000000 else 0) | (sef.e << hifbits) | hif
    val lo = rawToInt(sef.f)
    (hi.toLong << 32) | (lo.toLong & 0xffffffffL)
  }

  @inline private def decodeIEEE754(ebits: Int, fbits: Int,
      minPositiveValue: scala.Double, s: scala.Boolean, e: Int,
      f: scala.Double): scala.Double = {

    import Math.pow

    // Some constants
    val bias = (1 << (ebits - 1)) - 1
    val specialExponent = (1 << ebits) - 1
    val twoPowFbits = (1L << fbits).toDouble

    val absResult = if (e == specialExponent) {
      // Special
      if (f == 0.0)
        scala.Double.PositiveInfinity
      else
        scala.Double.NaN
    } else if (e > 0) {
      // Normalized
      pow(2, e - bias) * (1 + f / twoPowFbits)
    } else {
      // Subnormal
      f * minPositiveValue
    }

    if (s) -absResult else absResult
  }

  @inline private def encodeIEEE754(ebits: Int, fbits: Int,
      minNormal: scala.Double, minPositiveValue: scala.Double,
      v: scala.Double): EncodeIEEE754Result = {

    import js.Math.{floor, log, pow}

    // Some constants
    val bias = (1 << (ebits - 1)) - 1
    val specialExponent = (1 << ebits) - 1
    val twoPowFbits = (1L << fbits).toDouble
    val highestOneBitOfFbits = (1L << (fbits - 1)).toDouble
    val LN2 = 0.6931471805599453

    if (Double.isNaN(v)) {
      // http://dev.w3.org/2006/webapi/WebIDL/#es-type-mapping
      new EncodeIEEE754Result(false, specialExponent, highestOneBitOfFbits)
    } else if (Double.isInfinite(v)) {
      new EncodeIEEE754Result(v < 0, specialExponent, 0.0)
    } else if (v == 0.0) {
      new EncodeIEEE754Result(1 / v == scala.Double.NegativeInfinity, 0, 0.0)
    } else {
      val s = v < 0
      val av = if (s) -v else v

      if (av >= minNormal) {
        // Normalized

        var e = rawToInt(floor(log(av) / LN2))
        if (e > 1023)
          e = 1023
        var significand = av / pow(2, e)

        /* #2911 then #4433: When av is very close to a power of 2 (e.g.,
         * 9007199254740991.0 == 2^53 - 1), `log(av) / LN2` will already round
         * *up* to an `e` which is 1 too high, or *down* to an `e` which is 1
         * too low. The `floor()` afterwards comes too late to fix that.
         * We now adjust `e` and `significand` to make sure that `significand`
         * is in the range [1.0, 2.0)
         */
        if (significand < 1.0) {
          e -= 1
          significand *= 2
        } else if (significand >= 2.0) {
          e += 1
          significand /= 2
        }

        // Compute the stored bits of the mantissa (without the implicit leading '1')
        var f = roundToEven((significand - 1.0) * twoPowFbits)
        if (f == twoPowFbits) { // can happen because of the round-to-even
          e += 1
          f = 0
        }

        // Introduce the bias into `e`
        e += bias

        if (e > 2 * bias) {
          // Overflow
          e = specialExponent
          f = 0
        }

        new EncodeIEEE754Result(s, e, f)
      } else {
        // Subnormal
        new EncodeIEEE754Result(s, 0, roundToEven(av / minPositiveValue))
      }
    }
  }

  @inline private def rawToInt(x: scala.Double): Int =
    (x.asInstanceOf[js.Dynamic] | 0.asInstanceOf[js.Dynamic]).asInstanceOf[Int]

  @inline private def roundToEven(n: scala.Double): scala.Double =
    (n * scala.Double.MinPositiveValue) / scala.Double.MinPositiveValue

  // Cannot use tuples in the javalanglib
  @inline
  private final class EncodeIEEE754Result(val s: scala.Boolean, val e: Int,
      val f: scala.Double)

}
