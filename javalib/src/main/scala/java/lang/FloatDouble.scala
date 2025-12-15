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

import java.util.function.Function

/** Common algorithms between `Float` and `Double`. */
private[lang] object FloatDouble {

  @inline
  def toHexString[I, F](x: F, mantissaToHexString: Function[I, String])(
      implicit ops: IntFloatBits[I, F]): String = {
    import ops._

    val bits = floatToBits(x)
    val m = mantissaBitsOf(bits)
    val e = exponentOf(bits) // biased

    val posResult = if (isSpecialBitPattern(bits)) {
      if (e == 0)
        "0x0.0p0"
      else if (m === zero)
        "Infinity"
      else
        "NaN"
    } else {
      val mantissaStr = mantissaToHexString(m)
      var mantissaLen = mantissaStr.length
      while (mantissaLen > 1 && mantissaStr.charAt(mantissaLen - 1) == '0')
        mantissaLen -= 1
      val trimmed = mantissaStr.substring(0, mantissaLen)

      if (e != 0)
        "0x1." + trimmed + "p" + (e - bias) // Normal
      else
        "0x0." + trimmed + ("p" + (1 - bias)) // Subnormal
    }

    if (bits < zero) "-" + posResult else posResult
  }

}
