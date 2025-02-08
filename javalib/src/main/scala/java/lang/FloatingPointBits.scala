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

/** Manipulating the bits of floating point numbers. */
private[lang] object FloatingPointBits {
  private val arrayBuffer = new typedarray.ArrayBuffer(8)
  private val int32Array = new typedarray.Int32Array(arrayBuffer, 0, 2)
  private val float32Array = new typedarray.Float32Array(arrayBuffer, 0, 2)
  private val float64Array = new typedarray.Float64Array(arrayBuffer, 0, 1)

  private val areTypedArraysBigEndian = {
    int32Array(0) = 0x01020304
    (new typedarray.Int8Array(arrayBuffer, 0, 8))(0) == 0x01
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
       * In addition, since we xor together the two Ints, it doesn't matter
       * which one comes first or second, and hence we can use constants
       * 0 and 1 instead of having an indirection through `highOffset` and
       * `lowOffset`.
       */
      float64Array(0) = value
      int32Array(0) ^ int32Array(1)
    }
  }

  def intBitsToFloat(bits: Int): scala.Float = {
    int32Array(0) = bits
    float32Array(0)
  }

  def floatToIntBits(value: scala.Float): Int = {
    float32Array(0) = value
    int32Array(0)
  }

  def longBitsToDouble(bits: scala.Long): scala.Double = {
    int32Array(highOffset) = (bits >>> 32).toInt
    int32Array(lowOffset) = bits.toInt
    float64Array(0)
  }

  def doubleToLongBits(value: scala.Double): scala.Long = {
    float64Array(0) = value
    ((int32Array(highOffset).toLong << 32) |
        (int32Array(lowOffset).toLong & 0xffffffffL))
  }

  @inline private def rawToInt(x: scala.Double): Int = {
    import scala.scalajs.js.DynamicImplicits.number2dynamic
    (x | 0).asInstanceOf[Int]
  }

}
