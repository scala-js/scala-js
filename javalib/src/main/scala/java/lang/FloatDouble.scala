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

import java.math.BigInteger
import java.util.function.{BiFunction, Function}

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

  @inline
  def parseStringWasm[I, F](str: String, bellerophonFun: BiFunction[BigInteger, Int, F],
      hexMaxPrecisionChars: Int)(
      implicit ops: IntFloatBits[I, F]): F = {
    import ops._

    // requireNonNull avoids reaching AssertionError for the null case
    val (kindAndNegative, integralPartStr, fractionalPartStr, exponentStr) =
      java.util.Objects.requireNonNull(parseStringFormat(str))

    val absResult = (kindAndNegative & 3) match {
      case KindNaN =>
        fnan
      case KindInfinity =>
        finf
      case KindDecimal =>
        val f = new BigInteger(integralPartStr + fractionalPartStr)
        val e = Integer.parseInt(exponentStr) - fractionalPartStr.length()
        bellerophonFun(f, e)
      case KindHexadecimal =>
        val d = Double.parseHexDoubleImpl(integralPartStr, fractionalPartStr,
            exponentStr, hexMaxPrecisionChars)
        fromDoubleRound(d)
    }

    if ((kindAndNegative & FlagNegative) == 0)
      absResult
    else
      -absResult
  }

  private final val KindNaN = 0
  private final val KindInfinity = 1
  private final val KindDecimal = 2
  private final val KindHexadecimal = 3

  private final val FlagNegative = 4

  /** Parses the format of a floating point string.
   *
   *  If invalid, throws the appropriate exception.
   *
   *  If valid, returns a tuple
   *  `(kindAndNegative, integralPartStr, fractionalPartStr, exponentStr)`.
   *  where `kindAndNegative = kind | maybeFlagNegative`.
   */
  @noinline
  def parseStringFormat(str: String): (Int, String, String, String) = {
    def fail(): Nothing =
      throw new NumberFormatException(s"""For input string: "$str"""")

    // scalastyle:off return

    var endIndex = str.length()
    var i = 0

    // [\\x00-\\x20]* at the start and end
    while (i != endIndex && str.charAt(i) <= 0x20)
      i += 1
    while (endIndex != i && str.charAt(endIndex - 1) <= 0x20)
      endIndex -= 1

    // ([+-]?)
    if (i == endIndex)
      fail()
    var maybeFlagNegative = 0
    str.charAt(i) match {
      case '+' =>
        i += 1
      case '-' =>
        maybeFlagNegative = FlagNegative
        i += 1
      case _ =>
        ()
    }

    // determine kind; return early for NaN and Infinity
    var isHexadecimal = false
    if (i == endIndex)
      fail()
    str.charAt(i) match {
      case 'N' =>
        if (i + 3 != endIndex || str.charAt(i + 1) != 'a' || str.charAt(i + 2) != 'N')
          fail()
        return (KindNaN, null, null, null)

      case 'I' =>
        if (i + 8 != endIndex || !str.startsWith("nfinity", i + 1))
          fail()
        return (maybeFlagNegative | KindInfinity, null, null, null)

      case '0' =>
        if (i + 1 != endIndex && (str.charAt(i + 1) | 0x20) == 'x') {
          isHexadecimal = true
          i += 2
        }

      case _ =>
        ()
    }

    @inline
    def isDecDigit(c: Int): scala.Boolean = c >= '0' && c <= '9'

    @inline
    def isRadixDigit(c: Int): scala.Boolean = {
      isDecDigit(c) || {
        isHexadecimal && {
          val c2 = c | 0x20
          c2 >= 'a' && c2 <= 'f'
        }
      }
    }

    // integral part: [RD]*
    val startIntegral = i
    while (i != endIndex && isRadixDigit(str.charAt(i)))
      i += 1
    val integralPartStr = str.substring(startIntegral, i)

    // fractional part (\.[RD]*)?
    val fractionalPartStr = if (i != endIndex && str.charAt(i) == '.') {
      i += 1
      val startFractional = i
      while (i != endIndex && isRadixDigit(str.charAt(i)))
        i += 1
      str.substring(startFractional, i)
    } else {
      ""
    }

    // integral and fractional parts cannot both be empty
    if (integralPartStr.isEmpty() && fractionalPartStr.isEmpty())
      fail()

    // exponent part [eE|pP](+|-)?\d+), optional for decimal
    @inline def expChar = if (isHexadecimal) 'p' else 'e'
    val exponentPartStr = if (i + 1 < endIndex && (str.charAt(i) | 0x20) == expChar) {
      i += 1
      val startExponent = i
      val signChar = str.charAt(startExponent)
      if (signChar == '+' || signChar == '-')
        i += 1
      if (i == endIndex)
        fail()
      while (i != endIndex && isDecDigit(str.charAt(i)))
        i += 1
      str.substring(startExponent, i)
    } else {
      if (isHexadecimal)
        fail()
      "0"
    }

    // check that there are no stray chars
    if (i != endIndex) {
      /* There may be exactly one of [fFdD]
       * The bit patterns of the 4 letters are:
       * - 'f' -> 1100110
       * - 'F' -> 1000110
       * - 'd' -> 1100100
       * - 'D' -> 1000100
       * which are all 1.001.0 and cover the 4 possibilities for the two dots.
       * We can therefore test it all with just one comparion.
       * Double-check:
       *   for (i <- 0 until 65536 if (i | 0x22) == 'f') println(i.toChar)
       * prints exactly those 4 characters.
       */
      if (i + 1 != endIndex || (str.charAt(i) | 0x22) != 'f')
        fail()
    }

    val kindAndNegative =
      maybeFlagNegative | (if (isHexadecimal) KindHexadecimal else KindDecimal)
    (kindAndNegative, integralPartStr, fractionalPartStr, exponentPartStr)

    // scalastyle:on return
  }

}
