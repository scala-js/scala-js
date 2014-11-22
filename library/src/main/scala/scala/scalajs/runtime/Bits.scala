/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.runtime

import scala.scalajs.js
import js.Dynamic.global
import js.typedarray

/** Low-level stuff. */
object Bits {

  val areTypedArraysSupported = (
      !(!global.ArrayBuffer) && !(!global.Int32Array) &&
      !(!global.Float32Array) && !(!global.Float64Array))

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

  val areTypedArraysBigEndian = {
    if (areTypedArraysSupported) {
      int32Array(0) = 0x01020304
      (new typedarray.Int8Array(arrayBuffer, 0, 8))(0) == 0x01
    } else {
      true // as good a value as any
    }
  }

  private val highOffset = if (areTypedArraysBigEndian) 0 else 1
  private val lowOffset  = if (areTypedArraysBigEndian) 1 else 0

  def intBitsToFloat(bits: Int): Float = {
    if (areTypedArraysSupported) {
      int32Array(0) = bits
      float32Array(0)
    } else {
      intBitsToFloatPolyfill(bits).toFloat
    }
  }

  def floatToIntBits(value: Float): Int = {
    if (areTypedArraysSupported) {
      float32Array(0) = value
      int32Array(0)
    } else {
      floatToIntBitsPolyfill(value.toDouble)
    }
  }

  def longBitsToDouble(bits: Long): Double = {
    if (areTypedArraysSupported) {
      int32Array(highOffset) = (bits >>> 32).toInt
      int32Array(lowOffset) = bits.toInt
      float64Array(0)
    } else {
      longBitsToDoublePolyfill(bits)
    }
  }

  def doubleToLongBits(value: Double): Long = {
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
   * https://github.com/inexorabletash/polyfill/blob/a682f42c1092280bb01907c245979fb07219513d/typedarray.js#L150-L255
   *
   * Note that if typed arrays are not supported, it is almost certain that
   * fround is not supported natively, so Float operations are extremely slow.
   *
   * We therefore do all computations in Doubles here, which is also more
   * predictable, since the results do not depend on strict floats semantics.
   */

  private def intBitsToFloatPolyfill(bits: Int): Double = {
    val ebits = 8
    val fbits = 23
    val s = bits < 0
    val e = (bits >> fbits) & ((1 << ebits) - 1)
    val f = bits & ((1 << fbits) - 1)
    decodeIEEE754(ebits, fbits, s, e, f)
  }

  private def floatToIntBitsPolyfill(value: Double): Int = {
    val ebits = 8
    val fbits = 23
    val (s, e, f) = encodeIEEE754(ebits, fbits, value)
    (if (s) 0x80000000 else 0) | (e << fbits) | f.toInt
  }

  private def longBitsToDoublePolyfill(bits: Long): Double = {
    val ebits = 11
    val fbits = 52
    val hifbits = fbits-32
    val hi = (bits >>> 32).toInt
    val lo = ((bits.toInt: js.prim.Number) >>> 0).toDouble
    val s = hi < 0
    val e = (hi >> hifbits) & ((1 << ebits) - 1)
    val f = (hi & ((1 << hifbits) - 1)).toDouble * 0x100000000L.toDouble + lo
    decodeIEEE754(ebits, fbits, s, e, f)
  }

  private def doubleToLongBitsPolyfill(value: Double): Long = {
    val ebits = 11
    val fbits = 52
    val hifbits = fbits-32
    val (s, e, f) = encodeIEEE754(ebits, fbits, value)
    val hif = (f / 0x100000000L.toDouble).toInt
    val hi = (if (s) 0x80000000 else 0) | (e << hifbits) | hif
    val lo = f.toInt
    (hi.toLong << 32) | (lo.toLong & 0xffffffffL)
  }

  @inline private def decodeIEEE754(ebits: Int, fbits: Int,
      s: Boolean, e: Int, f: Double): Double = {

    import Math.pow

    val bias = (1 << (ebits-1)) - 1 // constant

    if (e == (1 << ebits) - 1) {
      // Special
      if (f != 0.0) Double.NaN
      else if (s) Double.NegativeInfinity
      else Double.PositiveInfinity
    } else if (e > 0) {
      // Normalized
      val x = pow(2, e-bias) * (1 + f / pow(2, fbits))
      if (s) -x else x
    } else if (f != 0.0) {
      // Subnormal
      val x = pow(2, -(bias-1)) * (f / pow(2, fbits))
      if (s) -x else x
    } else {
      // Zero
      if (s) -0.0 else 0.0
    }
  }

  @inline private def encodeIEEE754(ebits: Int, fbits: Int,
      v: Double): (Boolean, Int, Double) = {

    import Math._

    val bias = (1 << (ebits-1)) - 1 // constant

    if (v.isNaN) {
      // http://dev.w3.org/2006/webapi/WebIDL/#es-type-mapping
      (false, (1 << ebits) - 1, pow(2, fbits-1))
    } else if (v.isInfinite) {
      (v < 0, (1 << ebits) - 1, 0.0)
    } else if (v == 0.0) {
      (1 / v == Double.NegativeInfinity, 0, 0.0)
    } else {
      val LN2 = 0.6931471805599453

      val s = v < 0
      val av = if (s) -v else v

      if (av >= pow(2, 1-bias)) {
        val twoPowFbits = pow(2, fbits)

        var e = min(floor(log(av) / LN2).toInt, 1023)
        var f = roundToEven(av / pow(2, e) * twoPowFbits)
        if (f / twoPowFbits >= 2) {
          e = e + 1
          f = 1
        }
        if (e > bias) {
          // Overflow
          e = (1 << ebits) - 1
          f = 0
        } else {
          // Normalized
          e = e + bias
          f = f - twoPowFbits
        }
        (s, e, f)
      } else {
        // Subnormal
        (s, 0, roundToEven(av / pow(2, 1-bias-fbits)))
      }
    }
  }

  @inline private[runtime] def roundToEven(n: Double): Double = {
    val w = Math.floor(n)
    val f = n - w
    if (f < 0.5) w
    else if (f > 0.5) w + 1
    else if (w % 2 != 0) w + 1
    else w
  }

}
