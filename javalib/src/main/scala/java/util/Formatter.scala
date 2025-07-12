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

package java.util

import scala.annotation.switch
import scala.scalajs.js

import java.lang.{Double => JDouble}
import java.lang.Utils._
import java.io._
import java.math.{BigDecimal, BigInteger}

final class Formatter private (private[this] var dest: Appendable,
    formatterLocaleInfo: Formatter.LocaleInfo)
    extends Closeable with Flushable {

  import Formatter._
  import Flags._

  /** If `dest == null`, the real content is in `stringOutput`.
   *
   *  A real `StringBuilder` may be created lazily if `out()` is called, which
   *  will then capture the current content of `stringOutput`.
   *
   *  This allows to bypass the allocation of the `StringBuilder`, the call
   *  through `dest.append()` and more importantly the `try..catch`es in the
   *  common case where the `Formatter` is created without a specific
   *  destination.
   */
  private[this] var stringOutput: String = ""

  private[this] var closed: Boolean = false
  private[this] var lastIOException: IOException = null

  def this() = this(null: Appendable, Formatter.RootLocaleInfo)

  def this(a: Appendable) = this(a, Formatter.RootLocaleInfo)

  def this(l: Locale) = this(null: Appendable, new Formatter.LocaleLocaleInfo(l))

  def this(a: Appendable, l: Locale) = this(a, new Formatter.LocaleLocaleInfo(l))

  @inline
  private def trapIOExceptions(body: Runnable): Unit = {
    try {
      body.run()
    } catch {
      case th: IOException =>
        lastIOException = th
    }
  }

  private def sendToDest(s: String): Unit = {
    if (dest eq null)
      stringOutput += s
    else
      sendToDestSlowPath(js.Array(s))
  }

  private def sendToDest(s1: String, s2: String): Unit = {
    if (dest eq null)
      stringOutput += s1 + s2
    else
      sendToDestSlowPath(js.Array(s1, s2))
  }

  private def sendToDest(s1: String, s2: String, s3: String): Unit = {
    if (dest eq null)
      stringOutput += s1 + s2 + s3
    else
      sendToDestSlowPath(js.Array(s1, s2, s3))
  }

  @noinline
  private def sendToDestSlowPath(ss: js.Array[String]): Unit = {
    trapIOExceptions { () =>
      forArrayElems(ss)(dest.append(_))
    }
  }

  def close(): Unit = {
    if (!closed && (dest ne null)) {
      dest match {
        case cl: Closeable =>
          trapIOExceptions { () =>
            cl.close()
          }
        case _ =>
      }
    }
    closed = true
  }

  def flush(): Unit = {
    checkNotClosed()
    if (dest ne null) {
      dest match {
        case fl: Flushable =>
          trapIOExceptions { () =>
            fl.flush()
          }
        case _ =>
      }
    }
  }

  def format(format: String, args: Array[AnyRef]): Formatter =
    this.format(formatterLocaleInfo, format, args)

  def format(l: Locale, format: String, args: Array[AnyRef]): Formatter =
    this.format(new LocaleLocaleInfo(l), format, args)

  private def format(localeInfo: LocaleInfo, format: String,
      args: Array[AnyRef]): Formatter = {
    // scalastyle:off return

    checkNotClosed()

    var lastImplicitArgIndex: Int = 0
    var lastArgIndex: Int = 0 // required for < flag

    val fmtLength = format.length
    var fmtIndex: Int = 0

    while (fmtIndex != fmtLength) {
      // Process a portion without '%'
      val nextPercentIndex = format.indexOf("%", fmtIndex)
      if (nextPercentIndex < 0) {
        // No more '%'
        sendToDest(format.substring(fmtIndex))
        return this
      }
      sendToDest(format.substring(fmtIndex, nextPercentIndex))

      // Parse the format specifier

      val formatSpecifierIndex = nextPercentIndex + 1
      val re = FormatSpecifier
      re.lastIndex = formatSpecifierIndex
      val execResult = re.exec(format)

      if (execResult == null || execResult.index != formatSpecifierIndex) {
        /* Could not parse a valid format specifier. The reported unknown
         * conversion is the character directly following the '%', or '%'
         * itself if this is a trailing '%'. This mimics the behavior of the
         * JVM.
         */
        val conversion =
          if (formatSpecifierIndex == fmtLength) '%'
          else format.charAt(formatSpecifierIndex)
        throwUnknownFormatConversionException(conversion)
      }

      fmtIndex = re.lastIndex // position at the end of the match

      // For error reporting
      def fullFormatSpecifier: String = "%" + execResult(0)

      /* Extract values from the match result
       *
       * 1. DuplicateFormatFlagsException (in parseFlags)
       */

      val conversion = format.charAt(fmtIndex - 1)
      val flags = parseFlags(execResult(2).asInstanceOf[String], conversion)
      val width = parsePositiveInt(execResult(3))
      val precision = parsePositiveInt(execResult(4))

      if (width == -2)
        throwIllegalFormatWidthException(Int.MinValue) // Int.MinValue mimics the JVM
      if (precision == -2)
        throwIllegalFormatPrecisionException(Int.MinValue) // Int.MinValue mimics the JVM

      /* At this point, we need to branch off for 'n', because it has a
       * completely different error reporting spec. In particular, it must
       * throw an IllegalFormatFlagsException if any flag is specified,
       * although the regular mechanism would throw a
       * FormatFlagsConversionMismatchException.
       *
       * It is also the only conversion that throws
       * IllegalFormatWidthException, so we use this forced special path to
       * also take care of that one.
       *
       * We also treat '%' specially. Although its spec suggests that its
       * validation could be done in the generic way, experimentation suggests
       * that it behaves differently. Anyway, once 'n' has its special path,
       * '%' becomes the only one that does not take an argument, and so it
       * would need a special path later. So we handle it here and get it out
       * of the way. This further allows the generic path to only deal with
       * ASCII letters, which is convenient.
       */

      if (conversion == 'n') {
        if (precision != -1)
          throwIllegalFormatPrecisionException(precision)
        if (width != -1)
          throwIllegalFormatWidthException(width)
        if (flags.bits != 0)
          throwIllegalFormatFlagsException(flags)

        sendToDest("\n")
      } else if (conversion == '%') {
        if (precision != -1)
          throwIllegalFormatPrecisionException(precision)
        checkIllegalFormatFlags(flags)
        if (flags.leftAlign && width == -1)
          throwMissingFormatWidthException(fullFormatSpecifier)
        checkFlagsConversionMismatch('%', flags, ~LeftAlign)

        padAndSendToDestNoZeroPad(flags, width, "%")
      } else {
        // 2. UnknownFormatConversionException

        // Because of the RegExp that we use, we know that `conversion` is an ASCII letter
        val conversionLower =
          if (flags.upperCase) (conversion + ('a' - 'A')).toChar
          else conversion
        val illegalFlags = ConversionsIllegalFlags(conversionLower - 'a')
        if (illegalFlags == -1 || (flags.bits & UpperCase & illegalFlags) != 0)
          throwUnknownFormatConversionException(conversion)

        // 3. MissingFormatWidthException

        if (flags.hasAnyOf(LeftAlign | ZeroPad) && width == -1)
          throwMissingFormatWidthException(fullFormatSpecifier)

        // 4. IllegalFormatFlagsException

        checkIllegalFormatFlags(flags)

        // 5. IllegalFormatPrecisionException

        if (precision != -1 && (illegalFlags & Precision) != 0)
          throwIllegalFormatPrecisionException(precision)

        // 6. FormatFlagsConversionMismatchException

        checkFlagsConversionMismatch(conversionLower, flags, illegalFlags)

        /* Finally, get the argument and format it.
         *
         * 7. MissingFormatArgumentException | IllegalFormatConversionException | IllegalFormatCodePointException
         *
         * The first one is handled here, while we extract the argument.
         * The other two are handled in formatArg().
         */

        val argIndex = if (flags.useLastIndex) {
          // Explicitly use the last index
          lastArgIndex
        } else {
          val i = parsePositiveInt(execResult(1))
          if (i == -1) {
            // No explicit index
            lastImplicitArgIndex += 1
            lastImplicitArgIndex
          } else if (i <= 0) {
            // Out of range
            throwIllegalFormatArgumentIndexException(i)
            lastArgIndex
          } else {
            // Could be parsed, this is the index
            i
          }
        }

        if (argIndex <= 0 || argIndex > args.length)
          throwMissingFormatArgumentException(fullFormatSpecifier)

        lastArgIndex = argIndex
        val arg = args(argIndex - 1)

        /* Format the arg. We handle `null` in a generic way, except for 'b'
         * and 's'. 'b' because it actually gives specific semantics to it.
         * 's' because it needs to reject the '#' flag for `null`, and '#' is
         * accepted for Formattable instances.
         */

        if (arg == null && conversionLower != 'b' && conversionLower != 's')
          formatNonNumericString(RootLocaleInfo, flags, width, precision, "null")
        else
          formatArg(localeInfo, arg, conversionLower, flags, width, precision)
      }
    }

    this

    // scalastyle:on return
  }

  /* Should in theory be a method of `object Flags`. See the comment on that
   * object about why we keep it here.
   */
  private def parseFlags(flags: String, conversion: Char): Flags = {
    var bits = if (conversion >= 'A' && conversion <= 'Z') UpperCase else 0

    val len = flags.length
    var i = 0
    while (i != len) {
      val f = flags.charAt(i)
      val bit = (f: @switch) match {
        case '-' => LeftAlign
        case '#' => AltFormat
        case '+' => PositivePlus
        case ' ' => PositiveSpace
        case '0' => ZeroPad
        case ',' => UseGroupingSeps
        case '(' => NegativeParen
        case '<' => UseLastIndex
      }

      if ((bits & bit) != 0)
        throwDuplicateFormatFlagsException(f)

      bits |= bit
      i += 1
    }

    new Flags(bits)
  }

  /** Parses an optional integer argument.
   *
   *  Returns -1 if it was not specified, and -2 if it was out of the
   *  Int range.
   */
  private def parsePositiveInt(capture: js.UndefOr[String]): Int = {
    undefOrFold(capture) { () =>
      -1
    } { s =>
      val x = js.Dynamic.global.parseInt(s, 10).asInstanceOf[Double]
      if (x <= Int.MaxValue)
        x.toInt
      else
        -2
    }
  }

  private def formatArg(localeInfo: LocaleInfo, arg: Any, conversionLower: Char,
      flags: Flags, width: Int, precision: Int): Unit = {

    @inline def illegalFormatConversion(): Nothing =
      throwIllegalFormatConversionException(conversionLower, arg)

    (conversionLower: @switch) match {
      case 'b' =>
        val str =
          if ((arg.asInstanceOf[AnyRef] eq false.asInstanceOf[AnyRef]) || arg == null) "false"
          else "true"
        formatNonNumericString(RootLocaleInfo, flags, width, precision, str)

      case 'h' =>
        val str = Integer.toHexString(arg.hashCode)
        formatNonNumericString(RootLocaleInfo, flags, width, precision, str)

      case 's' =>
        arg match {
          case formattable: Formattable =>
            val formattableFlags = {
              (if (flags.leftAlign) FormattableFlags.LEFT_JUSTIFY else 0) |
              (if (flags.altFormat) FormattableFlags.ALTERNATE else 0) |
              (if (flags.upperCase) FormattableFlags.UPPERCASE else 0)
            }
            formattable.formatTo(this, formattableFlags, width, precision)

          case _ =>
            /* The Formattable case accepts AltFormat, therefore it is not
             * present in the generic `ConversionsIllegalFlags` table. However,
             * it is illegal for any other value, so we must check it now.
             */
            checkFlagsConversionMismatch(conversionLower, flags, AltFormat)

            val str = String.valueOf(arg)
            formatNonNumericString(localeInfo, flags, width, precision, str)
        }

      case 'c' =>
        val str = arg match {
          case arg: Char =>
            arg.toString()
          case arg: Int =>
            if (!Character.isValidCodePoint(arg))
              throwIllegalFormatCodePointException(arg)
            if (arg < Character.MIN_SUPPLEMENTARY_CODE_POINT) {
              js.Dynamic.global.String.fromCharCode(arg).asInstanceOf[String]
            } else {
              js.Dynamic.global.String
                .fromCharCode(0xd800 | ((arg >> 10) - (0x10000 >> 10)), 0xdc00 | (arg & 0x3ff))
                .asInstanceOf[String]
            }
          case _ =>
            illegalFormatConversion()
        }
        formatNonNumericString(localeInfo, flags, width, -1, str)

      case 'd' =>
        val str = arg match {
          case arg: Int        => arg.toString()
          case arg: Long       => arg.toString()
          case arg: BigInteger => arg.toString()
          case _               => illegalFormatConversion()
        }
        formatNumericString(localeInfo, flags, width, str)

      case 'o' | 'x' =>
        // Octal/hex formatting is not localized

        val isOctal = conversionLower == 'o'
        val prefix = {
          if (!flags.altFormat) ""
          else if (isOctal) "0"
          else if (flags.upperCase) "0X"
          else "0x"
        }

        arg match {
          case arg: BigInteger =>
            val radix = if (isOctal) 8 else 16
            formatNumericString(RootLocaleInfo, flags, width,
                arg.toString(radix), prefix)

          case _ =>
            val str = arg match {
              case arg: Int =>
                if (isOctal) java.lang.Integer.toOctalString(arg)
                else java.lang.Integer.toHexString(arg)
              case arg: Long =>
                if (isOctal) java.lang.Long.toOctalString(arg)
                else java.lang.Long.toHexString(arg)
              case _ =>
                illegalFormatConversion()
            }

            /* The Int and Long conversions have extra illegal flags, which are
             * not in the `ConversionsIllegalFlags` table because they are
             * legal for BigIntegers. We must check them now.
             */
            checkFlagsConversionMismatch(conversionLower, flags,
                PositivePlus | PositiveSpace | NegativeParen)

            padAndSendToDest(RootLocaleInfo, flags, width, prefix,
                applyNumberUpperCase(flags, str))
        }

      case 'e' | 'f' | 'g' =>
        def formatDecimal(x: Decimal): Unit = {
          /* The alternative format # of 'e', 'f' and 'g' is to force a
           * decimal separator.
           */
          val forceDecimalSep = flags.altFormat
          val actualPrecision =
            if (precision >= 0) precision
            else 6

          val notation = conversionLower match {
            case 'e' => computerizedScientificNotation(x, digitsAfterDot = actualPrecision, forceDecimalSep)
            case 'f' => decimalNotation(x, scale = actualPrecision, forceDecimalSep)
            case _   => generalScientificNotation(x, precision = actualPrecision, forceDecimalSep)
          }
          formatNumericString(localeInfo, flags, width, notation)
        }

        arg match {
          case arg: Double =>
            if (JDouble.isNaN(arg) || JDouble.isInfinite(arg))
              formatNaNOrInfinite(flags, width, arg)
            else
              formatDecimal(numberToDecimal(arg))
          case arg: BigDecimal =>
            formatDecimal(bigDecimalToDecimal(arg))
          case _ =>
            illegalFormatConversion()
        }

      case 'a' =>
        // Floating point hex formatting is not localized
        arg match {
          case arg: Double =>
            formatHexFloatingPoint(flags, width, precision, arg)
          case _ =>
            illegalFormatConversion()
        }

      case _ =>
        throw new AssertionError(
            "Unknown conversion '" + conversionLower + "' was not rejected earlier")
    }
  }

  @inline private def checkIllegalFormatFlags(flags: Flags): Unit = {
    if (flags.hasAllOf(LeftAlign | ZeroPad) || flags.hasAllOf(PositivePlus | PositiveSpace))
      throwIllegalFormatFlagsException(flags)
  }

  @inline private def checkFlagsConversionMismatch(conversionLower: Char,
      flags: Flags, illegalFlags: Int): Unit = {

    if (flags.hasAnyOf(illegalFlags))
      throwFormatFlagsConversionMismatchException(conversionLower, flags, illegalFlags)
  }

  /* Should in theory be a method of `Flags`. See the comment on that class
   * about why we keep it here.
   */
  private def flagsToString(flags: Flags): String = {
    (if (flags.leftAlign) "-" else "") +
    (if (flags.altFormat) "#" else "") +
    (if (flags.positivePlus) "+" else "") +
    (if (flags.positiveSpace) " " else "") +
    (if (flags.zeroPad) "0" else "") +
    (if (flags.useGroupingSeps) "," else "") +
    (if (flags.negativeParen) "(" else "") +
    (if (flags.useLastIndex) "<" else "")
  }

  private def computerizedScientificNotation(x: Decimal, digitsAfterDot: Int,
      forceDecimalSep: Boolean): String = {

    val rounded = x.round(precision = 1 + digitsAfterDot)

    val signStr = if (rounded.negative) "-" else ""

    val intStr = rounded.unscaledValue
    val dotPos = 1
    val fractionalDigitCount = intStr.length() - dotPos
    val missingZeros = digitsAfterDot - fractionalDigitCount

    val significandStr = {
      val integerPart = intStr.substring(0, dotPos)
      val fractionalPart = intStr.substring(dotPos) + strOfZeros(missingZeros)
      if (fractionalPart == "" && !forceDecimalSep)
        integerPart
      else
        integerPart + "." + fractionalPart
    }

    val exponent = fractionalDigitCount - rounded.scale
    val exponentSign = if (exponent < 0) "-" else "+"
    val exponentAbsStr0 = Math.abs(exponent).toString()
    val exponentAbsStr =
      if (exponentAbsStr0.length() == 1) "0" + exponentAbsStr0
      else exponentAbsStr0

    signStr + significandStr + "e" + exponentSign + exponentAbsStr
  }

  private def decimalNotation(x: Decimal, scale: Int,
      forceDecimalSep: Boolean): String = {

    val rounded = x.setScale(scale)

    val signStr = if (rounded.negative) "-" else ""

    val intStr = rounded.unscaledValue
    val intStrLen = intStr.length()

    val minDigits = 1 + scale // 1 before the '.' plus `scale` after it
    val expandedIntStr =
      if (intStrLen >= minDigits) intStr
      else strOfZeros(minDigits - intStrLen) + intStr
    val dotPos = expandedIntStr.length() - scale

    val integerPart = signStr + expandedIntStr.substring(0, dotPos)
    if (scale == 0 && !forceDecimalSep)
      integerPart
    else
      integerPart + "." + expandedIntStr.substring(dotPos)
  }

  private def generalScientificNotation(x: Decimal, precision: Int,
      forceDecimalSep: Boolean): String = {

    val p =
      if (precision == 0) 1
      else precision

    /* The JavaDoc says:
     *
     * > After rounding for the precision, the formatting of the resulting
     * > magnitude m depends on its value.
     *
     * so we first round to `p` significant digits before deciding which
     * notation to use, based on the order of magnitude of the result. The
     * order of magnitude is an integer `n` such that
     *
     *   10^n <= abs(x) < 10^(n+1)
     *
     * (except if x is a zero value, in which case it is 0).
     *
     * We also pass `rounded` to the dedicated notation function. Both
     * functions perform rounding of their own, but the rounding methods will
     * detect that no further rounding is necessary, so it is worth it.
     */
    val rounded = x.round(p)
    val orderOfMagnitude = (rounded.precision - 1) - rounded.scale
    if (orderOfMagnitude >= -4 && orderOfMagnitude < p)
      decimalNotation(rounded, scale = Math.max(0, p - orderOfMagnitude - 1), forceDecimalSep)
    else
      computerizedScientificNotation(rounded, digitsAfterDot = p - 1, forceDecimalSep)
  }

  /** Format an argument for the 'a' conversion.
   *
   *  This conversion requires quite some code, compared to the others, and is
   *  therefore extracted into separate functions.
   *
   *  There is some logic that is duplicated from
   *  `java.lang.Double.toHexString()`. It cannot be factored out because:
   *
   *  - the javalanglib and javalib do not see each other's custom method
   *    (could be solved if we merged them),
   *  - this method deals with subnormals in a very weird way when the
   *    precision is set and is <= 12, and
   *  - the handling of padding is fairly specific to `Formatter`, and would
   *    not lend itself well to be factored with the more straightforward code
   *    in `Double.toHexString()`.
   */
  private def formatHexFloatingPoint(flags: Flags, width: Int, precision: Int,
      arg: Double): Unit = {

    if (JDouble.isNaN(arg) || JDouble.isInfinite(arg)) {
      formatNaNOrInfinite(flags, width, arg)
    } else {
      // Extract the raw bits from the argument

      val ebits = 11 // exponent size
      val mbits = 52 // mantissa size
      val mbitsMask = ((1L << mbits) - 1L)
      val bias = (1 << (ebits - 1)) - 1

      val bits = JDouble.doubleToRawLongBits(arg)
      val negative = bits < 0
      val explicitMBits = bits & mbitsMask
      val biasedExponent = (bits >>> mbits).toInt & ((1 << ebits) - 1)

      // Compute the actual precision

      val actualPrecision = {
        if (precision == 0) 1 // apparently, this is how it behaves on the JVM
        else if (precision > 12) -1 // important for subnormals
        else precision
      }

      // Sign string

      val signStr = {
        if (negative) "-"
        else if (flags.positivePlus) "+"
        else if (flags.positiveSpace) " "
        else ""
      }

      /* Extract the implicit bit, the mantissa, and the exponent.
       * Also apply the artificial normalization of subnormals when the
       * actualPrecision is in the interval [1, 12].
       */

      val (implicitBitStr, mantissa, exponent) = if (biasedExponent == 0) {
        if (explicitMBits == 0L) {
          // Zero
          ("0", 0L, 0)
        } else {
          // Subnormal
          if (actualPrecision == -1) {
            ("0", explicitMBits, -1022)
          } else {
            // Artificial normalization, required by the 'a' conversion spec
            val leadingZeros = java.lang.Long.numberOfLeadingZeros(explicitMBits)
            val shift = (leadingZeros + 1) - (64 - mbits)
            val normalizedMantissa = (explicitMBits << shift) & mbitsMask
            val normalizedExponent = -1022 - shift
            ("1", normalizedMantissa, normalizedExponent)
          }
        }
      } else {
        // Normalized
        ("1", explicitMBits, biasedExponent - bias)
      }

      // Apply the rounding mandated by the precision

      val roundedMantissa = if (actualPrecision == -1) {
        mantissa
      } else {
        val roundingUnit = 1L << (mbits - (actualPrecision * 4)) // 4 bits per hex character
        val droppedPartMask = roundingUnit - 1
        val halfRoundingUnit = roundingUnit >> 1

        val truncated = mantissa & ~droppedPartMask
        val droppedPart = mantissa & droppedPartMask

        /* The JavaDoc is not clear about what flavor of rounding should be
         * used. We use round-half-to-even to mimic the behavior of the JVM.
         */
        if (droppedPart < halfRoundingUnit)
          truncated
        else if (droppedPart > halfRoundingUnit)
          truncated + roundingUnit
        else if ((truncated & roundingUnit) == 0L) // truncated is even
          truncated
        else
          truncated + roundingUnit
      }

      // Mantissa string

      val mantissaStr = {
        val baseStr = java.lang.Long.toHexString(roundedMantissa)
        val padded = "0000000000000".substring(baseStr.length()) + baseStr // 13 zeros

        assert(padded.length() == 13 && (13 * 4 == mbits),
            "padded mantissa does not have the right number of bits")

        // ~ padded.dropRightWhile(_ == '0') but keep at least minLength chars
        val minLength = Math.max(1, actualPrecision)
        var len = padded.length
        while (len > minLength && padded.charAt(len - 1) == '0')
          len -= 1
        padded.substring(0, len)
      }

      // Exponent string

      val exponentStr = Integer.toString(exponent)

      // Assemble, pad and send to dest

      val prefix = signStr + (if (flags.upperCase) "0X" else "0x")
      val rest = implicitBitStr + "." + mantissaStr + "p" + exponentStr

      padAndSendToDest(RootLocaleInfo, flags, width, prefix,
          applyNumberUpperCase(flags, rest))
    }
  }

  private def formatNonNumericString(localeInfo: LocaleInfo, flags: Flags,
      width: Int, precision: Int, str: String): Unit = {

    val truncatedStr =
      if (precision < 0 || precision >= str.length()) str
      else str.substring(0, precision)
    padAndSendToDestNoZeroPad(flags, width,
        applyUpperCase(localeInfo, flags, truncatedStr))
  }

  private def formatNaNOrInfinite(flags: Flags, width: Int, x: Double): Unit = {
    // NaN and Infinite formatting are not localized

    val str = if (JDouble.isNaN(x)) {
      "NaN"
    } else if (x > 0.0) {
      if (flags.positivePlus) "+Infinity"
      else if (flags.positiveSpace) " Infinity"
      else "Infinity"
    } else {
      if (flags.negativeParen) "(Infinity)"
      else "-Infinity"
    }

    padAndSendToDestNoZeroPad(flags, width, applyNumberUpperCase(flags, str))
  }

  private def formatNumericString(localeInfo: LocaleInfo, flags: Flags,
      width: Int, str: String, basePrefix: String = ""): Unit = {
    /* Flags for which a numeric string needs to be decomposed and transformed,
     * not just padded and/or uppercased. We can write fast-paths in this
     * method if none of them are present.
     */
    val TransformativeFlags =
      PositivePlus | PositiveSpace | UseGroupingSeps | NegativeParen | AltFormat

    if (str.length >= width && !flags.hasAnyOf(TransformativeFlags)) {
      // Super-fast-path
      sendToDest(localeInfo.localizeNumber(applyNumberUpperCase(flags, str)))
    } else if (!flags.hasAnyOf(TransformativeFlags | ZeroPad)) {
      // Fast-path that does not need to inspect the string
      padAndSendToDestNoZeroPad(flags, width, applyNumberUpperCase(flags, str))
    } else {
      // Extract prefix and rest, based on flags and the presence of a sign
      val (numberPrefix, rest0) = if (str.charAt(0) != '-') {
        if (flags.positivePlus)
          ("+", str)
        else if (flags.positiveSpace)
          (" ", str)
        else
          ("", str)
      } else {
        if (flags.negativeParen)
          ("(", str.substring(1) + ")")
        else
          ("-", str.substring(1))
      }

      val prefix = numberPrefix + basePrefix

      // Insert grouping separators, if required
      val rest =
        if (flags.useGroupingSeps) insertGroupingCommas(localeInfo, rest0)
        else rest0

      // Apply uppercase, localization, pad and send
      padAndSendToDest(localeInfo, flags, width, prefix,
          localeInfo.localizeNumber(applyNumberUpperCase(flags, rest)))
    }
  }

  /** Inserts grouping commas at the right positions for the locale.
   *
   *  We already insert the ',' character, regardless of the locale. That is
   *  fixed later by `localeInfo.localizeNumber`. The only locale-sensitive
   *  behavior in this method is the grouping size.
   *
   *  The reason is that we do not want to insert a character that would
   *  collide with another meaning (such as '.') at this point.
   */
  private def insertGroupingCommas(localeInfo: LocaleInfo, s: String): String = {
    val groupingSize = localeInfo.groupingSize

    val len = s.length
    var index = 0
    while (index != len && { val c = s.charAt(index); c >= '0' && c <= '9' }) {
      index += 1
    }

    index -= groupingSize

    if (index <= 0) {
      s
    } else {
      var result = s.substring(index)
      while (index > groupingSize) {
        val next = index - groupingSize
        result = s.substring(next, index) + "," + result
        index = next
      }
      s.substring(0, index) + "," + result
    }
  }

  private def applyNumberUpperCase(flags: Flags, str: String): String =
    if (flags.upperCase) str.toUpperCase() // uppercasing is not localized for numbers
    else str

  private def applyUpperCase(localeInfo: LocaleInfo, flags: Flags, str: String): String =
    if (flags.upperCase) localeInfo.toUpperCase(str)
    else str

  /** This method ignores `flags.zeroPad` and `flags.upperCase`. */
  private def padAndSendToDestNoZeroPad(flags: Flags, width: Int,
      str: String): Unit = {

    val len = str.length

    if (len >= width)
      sendToDest(str)
    else if (flags.leftAlign)
      sendToDest(str, strRepeat(" ", width - len))
    else
      sendToDest(strRepeat(" ", width - len), str)
  }

  /** This method ignores `flags.upperCase`. */
  private def padAndSendToDest(localeInfo: LocaleInfo, flags: Flags,
      width: Int, prefix: String, str: String): Unit = {

    val len = prefix.length + str.length

    if (len >= width)
      sendToDest(prefix, str)
    else if (flags.zeroPad)
      sendToDest(prefix, strRepeat(localeInfo.zeroDigitString, width - len), str)
    else if (flags.leftAlign)
      sendToDest(prefix, str, strRepeat(" ", width - len))
    else
      sendToDest(strRepeat(" ", width - len), prefix, str)
  }

  private def strRepeat(s: String, times: Int): String = {
    var result: String = ""
    var i = 0
    while (i != times) {
      result += s
      i += 1
    }
    result
  }

  def ioException(): IOException = lastIOException

  def locale(): Locale = {
    checkNotClosed()
    formatterLocaleInfo.locale
  }

  def out(): Appendable = {
    checkNotClosed()
    if (dest eq null) {
      dest = new java.lang.StringBuilder(stringOutput)
      stringOutput == ""
    }
    dest
  }

  override def toString(): String = {
    checkNotClosed()
    if (dest eq null)
      stringOutput
    else
      dest.toString()
  }

  @inline private def checkNotClosed(): Unit = {
    if (closed)
      throw new FormatterClosedException()
  }

  /* Helpers to throw exceptions with all the right arguments.
   *
   * Some are direct forwarders, like `IllegalFormatPrecisionException`; they
   * are here for consistency.
   */

  private def throwDuplicateFormatFlagsException(flag: Char): Nothing =
    throw new DuplicateFormatFlagsException(flag.toString())

  private def throwUnknownFormatConversionException(conversion: Char): Nothing =
    throw new UnknownFormatConversionException(conversion.toString())

  private def throwIllegalFormatPrecisionException(precision: Int): Nothing =
    throw new IllegalFormatPrecisionException(precision)

  private def throwIllegalFormatWidthException(width: Int): Nothing =
    throw new IllegalFormatWidthException(width)

  private def throwIllegalFormatArgumentIndexException(index: Int): Nothing = {
    val msg =
      if (index == 0) "Illegal format argument index = 0"
      else "Format argument index: (not representable as int)"
    throw new IllegalFormatArgumentIndexException(msg)
  }

  private def throwIllegalFormatFlagsException(flags: Flags): Nothing =
    throw new IllegalFormatFlagsException(flagsToString(flags))

  private def throwMissingFormatWidthException(fullFormatSpecifier: String): Nothing =
    throw new MissingFormatWidthException(fullFormatSpecifier)

  private def throwFormatFlagsConversionMismatchException(conversionLower: Char,
      flags: Flags, illegalFlags: Int): Nothing = {
    throw new FormatFlagsConversionMismatchException(
        flagsToString(new Flags(flags.bits & illegalFlags)), conversionLower)
  }

  private def throwMissingFormatArgumentException(fullFormatSpecifier: String): Nothing =
    throw new MissingFormatArgumentException(fullFormatSpecifier)

  private def throwIllegalFormatConversionException(conversionLower: Char, arg: Any): Nothing =
    throw new IllegalFormatConversionException(conversionLower, arg.getClass)

  private def throwIllegalFormatCodePointException(arg: Int): Nothing =
    throw new IllegalFormatCodePointException(arg)

}

object Formatter {

  private val FormatSpecifier = new js.RegExp(
      """(?:(\d+)\$)?([-#+ 0,\(<]*)(\d+)?(?:\.(\d+))?[%A-Za-z]""", "g")

  private def strOfZeros(count: Int): String = {
    val twentyZeros = "00000000000000000000"
    if (count <= 20) {
      twentyZeros.substring(0, count)
    } else {
      var result = ""
      var remaining = count
      while (remaining > 20) {
        result += twentyZeros
        remaining -= 20
      }
      result + twentyZeros.substring(0, remaining)
    }
  }

  @inline
  private def assert(condition: Boolean, msg: String): Unit = {
    if (!condition)
      throw new AssertionError(msg)
  }

  /* This class is never used in a place where it would box, so it will
   * completely disappear at link-time. Make sure to keep it that way.
   *
   * Also note that methods in this class are moved to the companion object, so
   * also take into account the comment on `object Flags`. In particular, do
   * not add non-inlineable methods in this class.
   */
  private final class Flags(val bits: Int) extends AnyVal {
    import Flags._

    @inline def leftAlign: Boolean = (bits & LeftAlign) != 0
    @inline def altFormat: Boolean = (bits & AltFormat) != 0
    @inline def positivePlus: Boolean = (bits & PositivePlus) != 0
    @inline def positiveSpace: Boolean = (bits & PositiveSpace) != 0
    @inline def zeroPad: Boolean = (bits & ZeroPad) != 0
    @inline def useGroupingSeps: Boolean = (bits & UseGroupingSeps) != 0
    @inline def negativeParen: Boolean = (bits & NegativeParen) != 0
    @inline def useLastIndex: Boolean = (bits & UseLastIndex) != 0
    @inline def upperCase: Boolean = (bits & UpperCase) != 0

    @inline def hasAnyOf(testBits: Int): Boolean = (bits & testBits) != 0

    @inline def hasAllOf(testBits: Int): Boolean = (bits & testBits) == testBits
  }

  /* This object only contains `final val`s and (synthetic) `@inline`
   * methods. Therefore, it will completely disappear at link-time. Make sure
   * to keep it that way. In particular, do not add non-inlineable methods.
   */
  private object Flags {
    final val LeftAlign = 0x001
    final val AltFormat = 0x002
    final val PositivePlus = 0x004
    final val PositiveSpace = 0x008
    final val ZeroPad = 0x010
    final val UseGroupingSeps = 0x020
    final val NegativeParen = 0x040
    final val UseLastIndex = 0x080
    final val UpperCase = 0x100
    final val Precision = 0x200 // used in ConversionsIllegalFlags
  }

  private val ConversionsIllegalFlags: Array[Int] = {
    import Flags._

    val NumericOnlyFlags =
      PositivePlus | PositiveSpace | ZeroPad | UseGroupingSeps | NegativeParen

    // 'n' and '%' are not here because they have special paths in `format`

    Array(
        UseGroupingSeps | NegativeParen,          // a
        NumericOnlyFlags | AltFormat,             // b
        NumericOnlyFlags | AltFormat | Precision, // c
        AltFormat | UpperCase | Precision,        // d
        UseGroupingSeps,                          // e
        UpperCase,                                // f
        AltFormat,                                // g
        NumericOnlyFlags | AltFormat,             // h
        -1, -1, -1, -1, -1, -1,                   // i -> n
        UseGroupingSeps | UpperCase | Precision,  // o
        -1, -1, -1,                               // p -> r
        NumericOnlyFlags,                         // s
        -1, -1, -1, -1,                           // t -> w
        UseGroupingSeps | Precision,              // x
        -1, -1                                    // y -> z
    )
  }

  /** Converts a `Double` into a `Decimal` that has as few digits as possible
   *  while still uniquely identifying `x`.
   *
   *  We do this by converting the absolute value of the number into a string
   *  using its built-in `toString()` conversion. By ECMAScript's spec, this
   *  yields a decimal representation with as few significant digits as
   *  possible, although it can be in fixed notation or in computerized
   *  scientific notation.
   *
   *  We then parse that string to recover the integer part, the factional part
   *  and the exponent; the latter two being optional.
   *
   *  From the parts, we construct a `Decimal`, making sure to create one that
   *  does not have leading 0's (as it is forbidden by `Decimal`'s invariants).
   */
  private def numberToDecimal(x: Double): Decimal = {
    if (x == 0.0) {
      val negative = 1.0 / x < 0.0
      Decimal.zero(negative)
    } else {
      val negative = x < 0.0
      val s = JDouble.toString(if (negative) -x else x)

      val ePos = s.indexOf('e')
      val e =
        if (ePos < 0) 0
        else js.Dynamic.global.parseInt(s.substring(ePos + 1)).asInstanceOf[Int]
      val significandEnd = if (ePos < 0) s.length() else ePos

      val dotPos = s.indexOf('.')
      if (dotPos < 0) {
        // No '.'; there cannot be leading 0's (x == 0.0 was handled before)
        val unscaledValue = s.substring(0, significandEnd)
        val scale = -e
        new Decimal(negative, unscaledValue, scale)
      } else {
        // There is a '.'; there can be leading 0's, which we must remove
        val digits = s.substring(0, dotPos) + s.substring(dotPos + 1, significandEnd)
        val digitsLen = digits.length()
        var i = 0
        while (i < digitsLen && digits.charAt(i) == '0')
          i += 1
        val unscaledValue = digits.substring(i)
        val scale = -e + (significandEnd - (dotPos + 1))
        new Decimal(negative, unscaledValue, scale)
      }
    }
  }

  /** Converts a `BigDecimal` into a `Decimal`.
   *
   *  Zero values are considered positive for the conversion.
   *
   *  All other values keep their sign, unscaled value and scale.
   */
  private def bigDecimalToDecimal(x: BigDecimal): Decimal = {
    val unscaledValueWithSign = x.unscaledValue().toString()

    if (unscaledValueWithSign == "0") {
      Decimal.zero(negative = false)
    } else {
      val negative = unscaledValueWithSign.charAt(0) == '-'
      val unscaledValue =
        if (negative) unscaledValueWithSign.substring(1)
        else unscaledValueWithSign
      val scale = x.scale()
      new Decimal(negative, unscaledValue, scale)
    }
  }

  /** A decimal representation of a number.
   *
   *  An instance of this class represents the number whose absolute value is
   *  `unscaledValue × 10^(-scale)`, and that is negative iff `negative` is
   *  true.
   *
   *  The `unscaledValue` is stored as a String of decimal digits, i.e.,
   *  characters in the range ['0', '9'], expressed in base 10. Leading 0's are
   *  *not* valid.
   *
   *  As an exception, a zero value is represented by an `unscaledValue` of
   *  `"0"`. The scale of zero value is always 0.
   *
   *  `Decimal` is similar to `BigDecimal`, with some differences:
   *
   *  - `Decimal` distinguishes +0 from -0.
   *  - The unscaled value of `Decimal` is stored in base 10.
   *
   *  The methods it exposes have the same meaning as for BigDecimal, with the
   *  only rounding mode being HALF_UP.
   */
  private final class Decimal(val negative: Boolean, val unscaledValue: String,
      val scale: Int) {

    def isZero: Boolean = unscaledValue == "0"

    /** The number of digits in the unscaled value.
     *
     *  The precision of a zero value is 1.
     */
    def precision: Int = unscaledValue.length()

    /** Rounds the number so that it has at most the given precision, i.e., at
     *  most the given number of digits in its `unscaledValue`.
     *
     *  The given `precision` must be greater than 0.
     */
    def round(precision: Int): Decimal = {
      assert(precision > 0, "Decimal.round() called with non-positive precision")

      roundAtPos(roundingPos = precision)
    }

    /** Returns a new Decimal instance with the same value, possibly rounded,
     *  with the given scale.
     *
     *  If this is a zero value, the same value is returned (a zero value must
     *  always have a 0 scale). Rounding may also cause the result to be a zero
     *  value, in which case its scale must be 0 as well. Otherwise, the result
     *  is non-zero and is guaranteed to have exactly the given new scale.
     */
    def setScale(newScale: Int): Decimal = {
      val roundingPos = unscaledValue.length() + newScale - scale
      val rounded = roundAtPos(roundingPos)
      assert(rounded.isZero || rounded.scale <= newScale,
          "roundAtPos returned a non-zero value with a scale too large")

      if (rounded.isZero || rounded.scale == newScale)
        rounded
      else
        new Decimal(negative, rounded.unscaledValue + strOfZeros(newScale - rounded.scale), newScale)
    }

    /** Rounds the number at the given position in its `unscaledValue`.
     *
     *  The `roundingPos` may be any integer value.
     *
     *  - If it is < 0, the result is always a zero value.
     *  - If it is >= `unscaledValue.lenght()`, the result is always the same
     *    value.
     *  - Otherwise, the `unscaledValue` will be truncated at `roundingPos`,
     *    and rounded up iff `unscaledValue.charAt(roundingPos) >= '5'`.
     *
     *  The value of `negative` is always preserved.
     *
     *  Unless the result is a zero value, the following guarantees apply:
     *
     *  - its scale is guaranteed to be at most
     *    `scale - (unscaledValue.length() - roundingPos)`.
     *  - its precision is guaranteed to be at most
     *    `max(1, roundingPos)`.
     */
    private def roundAtPos(roundingPos: Int): Decimal = {
      val digits = this.unscaledValue // local copy
      val digitsLen = digits.length()

      if (roundingPos < 0) {
        Decimal.zero(negative)
      } else if (roundingPos >= digitsLen) {
        this // no rounding necessary
      } else {
        @inline def scaleAtPos(pos: Int): Int = scale - (digitsLen - pos)

        if (digits.charAt(roundingPos) < '5') {
          // Truncate at roundingPos
          if (roundingPos == 0)
            Decimal.zero(negative)
          else
            new Decimal(negative, digits.substring(0, roundingPos), scaleAtPos(roundingPos))
        } else {
          // Truncate and increment at roundingPos

          // Find the position of the last non-9 digit in the truncated digits (can be -1)
          var lastNonNinePos = roundingPos - 1
          while (lastNonNinePos >= 0 && digits.charAt(lastNonNinePos) == '9')
            lastNonNinePos -= 1

          val newUnscaledValue =
            if (lastNonNinePos < 0) "1"
            else digits.substring(0, lastNonNinePos) + (digits.charAt(lastNonNinePos) + 1).toChar

          val newScale = scaleAtPos(lastNonNinePos + 1)

          new Decimal(negative, newUnscaledValue, newScale)
        }
      }
    }

    // for debugging only
    override def toString(): String =
      s"Decimal($negative, $unscaledValue, $scale)"
  }

  private object Decimal {
    @inline def zero(negative: Boolean): Decimal =
      new Decimal(negative, "0", 0)
  }

  /** A proxy for a `java.util.Locale` or for the root locale that provides
   *  the info required by `Formatter`.
   *
   *  The purpose of this abstraction is to allow `java.util.Formatter` to link
   *  when `java.util.Locale` and `java.text.*` are not on the classpath, as
   *  long as only methods that do not take an explicit `Locale` are used.
   *
   *  While the `LocaleLocaleInfo` subclass actually delegates to a `Locale`
   *  (and hence cannot link without `Locale`), the object `RootLocaleInfo`
   *  hard-codes the required information about the Root locale.
   *
   *  We use object-oriented method calls so that the reachability analysis
   *  never reaches the `Locale`-dependent code if `LocaleLocaleInfo` is never
   *  instantiated, which is the case as long the methods and constructors
   *  taking an explicit `Locale` are not called.
   *
   *  When `LocaleLocaleInfo` can be dead-code-eliminated, the optimizer can
   *  even inline and constant-fold all the methods of `RootLocaleInfo`,
   *  resulting in top efficiency.
   */
  private sealed abstract class LocaleInfo {
    def locale: Locale
    def groupingSize: Int
    def zeroDigitString: String
    def localizeNumber(str: String): String
    def toUpperCase(str: String): String
  }

  private object RootLocaleInfo extends LocaleInfo {
    def locale: Locale = Locale.ROOT
    def groupingSize: Int = 3
    def zeroDigitString: String = "0"
    def localizeNumber(str: String): String = str
    def toUpperCase(str: String): String = str.toUpperCase()
  }

  private final class LocaleLocaleInfo(val locale: Locale) extends LocaleInfo {
    import java.text._

    private def actualLocale: Locale =
      if (locale == null) Locale.ROOT
      else locale

    private lazy val decimalFormatSymbols: DecimalFormatSymbols =
      DecimalFormatSymbols.getInstance(actualLocale)

    lazy val groupingSize: Int = {
      NumberFormat.getNumberInstance(actualLocale) match {
        case decimalFormat: DecimalFormat => decimalFormat.getGroupingSize()
        case _                            => 3
      }
    }

    def zeroDigitString: String = decimalFormatSymbols.getZeroDigit().toString()

    def localizeNumber(str: String): String = {
      val formatSymbols = decimalFormatSymbols
      val digitOffset = formatSymbols.getZeroDigit() - '0'
      var result = ""
      val len = str.length()
      var i = 0
      while (i != len) {
        result += (str.charAt(i) match {
          case c if c >= '0' && c <= '9' => (c + digitOffset).toChar
          case '.'                       => formatSymbols.getDecimalSeparator()
          case ','                       => formatSymbols.getGroupingSeparator()
          case c                         => c
        })
        i += 1
      }
      result
    }

    def toUpperCase(str: String): String = str.toUpperCase(actualLocale)
  }
}
