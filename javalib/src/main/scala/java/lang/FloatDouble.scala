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
  def toHexString[I, F](x: F)(implicit ops: IntFloatBits[I, F]): String = {
    import ops._

    val bits = floatToBits(x)
    val e = exponentOf(bits) // biased

    var result = if (bits < zero) "-" else ""

    if (isSpecialBitPattern(bits)) {
      if (e == 0)
        result + "0x0.0p0"
      else if (mantissaBitsOf(bits) === zero)
        result + "Infinity"
      else
        "NaN"
    } else {
      val unbiasedE = if (e != 0) {
        // Normal
        result += "0x1."
        e - bias
      } else {
        // Subnormal
        result += "0x0."
        1 - bias
      }

      val m = newIntBox(bits << (bitSize - mbits)) // align the mantissa bits to the left
      while ({
        val digit = toInt32Wrap(m() >>> (bitSize - 4))
        result += ((if (digit < 10) '0'.toInt else 'a'.toInt - 10) + digit).toChar
        m() <<= 4
        m() !== zero
      }) ()

      (result + "p") + unbiasedE
    }
  }

}
