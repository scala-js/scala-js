package java.util

import scala.annotation.switch
import scala.scalajs.js

import java.io._

final class Formatter(private[this] var dest: Appendable)
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

  def this() = this(null: Appendable)

  @inline
  private def trapIOExceptions(body: => Unit): Unit = {
    try {
      body
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
    trapIOExceptions {
      ss.foreach(dest.append(_))
    }
  }

  def close(): Unit = {
    if (!closed && (dest ne null)) {
      dest match {
        case cl: Closeable =>
          trapIOExceptions {
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
          trapIOExceptions {
            fl.flush()
          }
        case _ =>
      }
    }
  }

  def format(format: String, args: Array[AnyRef]): Formatter = {
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

      // Process one '%'

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
          if (formatSpecifierIndex == fmtLength) "%"
          else format.substring(formatSpecifierIndex, formatSpecifierIndex + 1)
        throw new UnknownFormatConversionException(conversion)
      }

      fmtIndex = re.lastIndex // position at the end of the match

      val conversion = format.charAt(fmtIndex - 1)
      val flags = parseFlags(execResult(2).asInstanceOf[String], conversion)
      val width = parsePositiveIntSilent(execResult(3), default = -1)
      val precision = parsePositiveIntSilent(execResult(4), default = -1)

      val arg = if (conversion == '%' || conversion == 'n') {
        /* No argument. Make sure not to bump `lastImplicitArgIndex` nor to
         * affect `lastArgIndex`.
         */
        null
      } else {
        if (flags.leftAlign && width < 0)
          throw new MissingFormatWidthException("%" + execResult(0))

        val argIndex = if (flags.useLastIndex) {
          // Explicitly use the last index
          lastArgIndex
        } else {
          val i = parsePositiveIntSilent(execResult(1), default = 0)
          if (i == 0) {
            // Either there is no explicit index, or the explicit index is 0
            lastImplicitArgIndex += 1
            lastImplicitArgIndex
          } else if (i < 0) {
            // Cannot be parsed, same as useLastIndex
            lastArgIndex
          } else {
            // Could be parsed, this is the index
            i
          }
        }

        if (argIndex <= 0 || argIndex > args.length) {
          val conversionStr = conversion.toString
          if ("bBhHsHcCdoxXeEgGfn%".indexOf(conversionStr) < 0)
            throw new UnknownFormatConversionException(conversionStr)
          else
            throw new MissingFormatArgumentException("%" + execResult(0))
        }

        lastArgIndex = argIndex
        args(argIndex - 1)
      }

      formatArg(arg, conversion, flags, width, precision)
    }

    this

    // scalastyle:on return
  }

  /* Should in theory be a method of `object Flags`. See the comment on that
   * object about why we keep it here.
   */
  private def parseFlags(flags: String, conversion: Char): Flags = {
    var bits = if (conversion <= 'Z') UpperCase else 0

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
        throw new DuplicateFormatFlagsException(f.toString)

      bits |= bit
      i += 1
    }

    new Flags(bits)
  }

  private def parsePositiveIntSilent(capture: js.UndefOr[String],
      default: Int): Int = {
    capture.fold {
      default
    } { s =>
      val x = js.Dynamic.global.parseInt(s, 10).asInstanceOf[Double]
      if (x <= Int.MaxValue)
        x.toInt
      else
        -1 // Silently ignore and return -1
    }
  }

  private def formatArg(arg: Any, conversion: Char, flags: Flags, width: Int,
      precision: Int): Unit = {

    @inline def rejectPrecision(): Unit = {
      if (precision >= 0)
        throw new IllegalFormatPrecisionException(precision)
    }

    def formatNullOrThrowIllegalFormatConversion(): Unit = {
      if (arg == null)
        formatNonNumericString(flags, width, precision, "null")
      else
        throw new IllegalFormatConversionException(conversion, arg.getClass)
    }

    @inline def precisionWithDefault =
      if (precision >= 0) precision
      else 6

    @inline def efgCommon(notation: (Double, Int, Boolean) => String): Unit = {
      arg match {
        case arg: Double =>
          if (arg.isNaN || arg.isInfinite) {
            formatNaNOrInfinite(flags, width, arg)
          } else {
            /* The alternative format # of 'e', 'f' and 'g' is to force a
             * decimal separator.
             */
            val forceDecimalSep = flags.altFormat
            formatNumericString(flags, width,
                notation(arg, precisionWithDefault, forceDecimalSep))
          }
        case _ =>
          formatNullOrThrowIllegalFormatConversion()
      }
    }

    (conversion: @switch) match {
      case 'b' | 'B' =>
        validateFlags(flags, conversion,
            invalidFlags = NumericOnlyFlags | AltFormat)
        val str =
          if ((arg.asInstanceOf[AnyRef] eq false.asInstanceOf[AnyRef]) || arg == null) "false"
          else "true"
        formatNonNumericString(flags, width, precision, str)

      case 'h' | 'H' =>
        validateFlags(flags, conversion,
            invalidFlags = NumericOnlyFlags | AltFormat)
        val str =
          if (arg == null) "null"
          else Integer.toHexString(arg.hashCode)
        formatNonNumericString(flags, width, precision, str)

      case 's' | 'S' =>
        arg match {
          case formattable: Formattable =>
            validateFlags(flags, conversion, invalidFlags = NumericOnlyFlags)
            val formattableFlags = {
              (if (flags.leftAlign) FormattableFlags.LEFT_JUSTIFY else 0) |
              (if (flags.altFormat) FormattableFlags.ALTERNATE else 0) |
              (if (flags.upperCase) FormattableFlags.UPPERCASE else 0)
            }
            formattable.formatTo(this, formattableFlags, width, precision)

          case _ =>
            validateFlags(flags, conversion,
                invalidFlags = NumericOnlyFlags | AltFormat)
            val str = String.valueOf(arg)
            formatNonNumericString(flags, width, precision, str)
        }

      case 'c' | 'C' =>
        validateFlags(flags, conversion,
            invalidFlags = NumericOnlyFlags | AltFormat)
        rejectPrecision()
        arg match {
          case arg: Char =>
            formatNonNumericString(flags, width, -1, arg.toString)
          case arg: Int =>
            if (!Character.isValidCodePoint(arg))
              throw new IllegalFormatCodePointException(arg)
            val str = if (arg < Character.MIN_SUPPLEMENTARY_CODE_POINT) {
              js.Dynamic.global.String.fromCharCode(arg)
            } else {
              js.Dynamic.global.String.fromCharCode(
                  0xd800 | ((arg >> 10) - (0x10000 >> 10)),
                  0xdc00 | (arg & 0x3ff))
            }
            formatNonNumericString(flags, width, -1, str.asInstanceOf[String])
          case _ =>
            formatNullOrThrowIllegalFormatConversion()
        }

      case 'd' =>
        validateFlags(flags, conversion, invalidFlags = AltFormat)
        rejectPrecision()
        arg match {
          case arg: Int =>
            formatNumericString(flags, width, arg.toString())
          case arg: Long =>
            formatNumericString(flags, width, arg.toString())
          case _ =>
            formatNullOrThrowIllegalFormatConversion()
        }

      case 'o' =>
        validateFlags(flags, conversion,
            invalidFlags = InvalidFlagsForOctalAndHex)
        rejectPrecision()
        val prefix =
          if (flags.altFormat) "0"
          else ""
        arg match {
          case arg: Int =>
            padAndSendToDest(flags, width, prefix,
                java.lang.Integer.toOctalString(arg))
          case arg: Long =>
            padAndSendToDest(flags, width, prefix,
                java.lang.Long.toOctalString(arg))
          case _ =>
            formatNullOrThrowIllegalFormatConversion()
        }

      case 'x' | 'X' =>
        validateFlags(flags, conversion,
            invalidFlags = InvalidFlagsForOctalAndHex)
        rejectPrecision()
        val prefix = {
          if (!flags.altFormat) ""
          else if (flags.upperCase) "0X"
          else "0x"
        }
        arg match {
          case arg: Int =>
            padAndSendToDest(flags, width, prefix,
                applyUpperCase(flags, java.lang.Integer.toHexString(arg)))
          case arg: Long =>
            padAndSendToDest(flags, width, prefix,
                applyUpperCase(flags, java.lang.Long.toHexString(arg)))
          case _ =>
            formatNullOrThrowIllegalFormatConversion()
        }

      case 'e' | 'E' =>
        validateFlags(flags, conversion, invalidFlags = UseGroupingSeps)
        efgCommon(computerizedScientificNotation _)

      case 'g' | 'G' =>
        validateFlags(flags, conversion, invalidFlags = AltFormat)
        efgCommon(generalScientificNotation _)

      case 'f' =>
        validateFlags(flags, conversion, invalidFlags = 0)
        efgCommon(decimalNotation _)

      case '%' =>
        validateFlagsForPercentAndNewline(flags, conversion,
            invalidFlags = AllWrittenFlags & ~LeftAlign)
        rejectPrecision()
        if (flags.leftAlign && width < 0)
          throw new MissingFormatWidthException("%-%")
        padAndSendToDestNoZeroPad(flags, width, "%")

      case 'n' =>
        validateFlagsForPercentAndNewline(flags, conversion,
            invalidFlags = AllWrittenFlags)
        rejectPrecision()
        if (width >= 0)
          throw new IllegalFormatWidthException(width)
        sendToDest("\n")

      case _ =>
        throw new UnknownFormatConversionException(conversion.toString)
    }
  }

  @inline private def validateFlags(flags: Flags, conversion: Char,
      invalidFlags: Int): Unit = {

    @noinline def flagsConversionMismatch(): Nothing = {
      throw new FormatFlagsConversionMismatchException(
          flagsToString(new Flags(flags.bits & invalidFlags)), conversion)
    }

    if ((flags.bits & invalidFlags) != 0)
      flagsConversionMismatch()

    @noinline def illegalFlags(): Nothing =
      throw new IllegalFormatFlagsException(flagsToString(flags))

    /* The test `(invalidFlags & BadCombo) == 0` is redundant, but is
     * constant-folded away at called site, and if false it allows to dce the
     * test after the `&&`. If both tests are eliminated, the entire `if`
     * disappears.
     */
    val BadCombo1 = LeftAlign | ZeroPad
    val BadCombo2 = PositivePlus | PositiveSpace
    if (((invalidFlags & BadCombo1) == 0 && (flags.bits & BadCombo1) == BadCombo1) ||
        ((invalidFlags & BadCombo2) == 0 && (flags.bits & BadCombo2) == BadCombo2)) {
      illegalFlags()
    }
  }

  @inline private def validateFlagsForPercentAndNewline(flags: Flags,
      conversion: Char, invalidFlags: Int): Unit = {

    @noinline def illegalFlags(): Nothing =
      throw new IllegalFormatFlagsException(flagsToString(flags))

    if ((flags.bits & invalidFlags) != 0)
      illegalFlags()
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

  private def computerizedScientificNotation(x: Double, precision: Int,
      forceDecimalSep: Boolean): String = {
    import js.JSNumberOps._

    // First use JavaScript's native toExponential conversion
    val s1 = x.toExponential(precision)

    // -0.0 should have a leading '-'
    val s2 =
      if (x == 0.0 && 1 / x < 0) "-" + s1
      else s1

    // Then make sure the exponent has at least 2 digits for the JDK spec
    val len = s2.length
    val s3 =
      if ('e' != s2.charAt(len - 3)) s2
      else s2.substring(0, len - 1) + "0" + s2.substring(len - 1)

    // Finally, force the decimal separator, if requested
    if (!forceDecimalSep || s3.indexOf(".") >= 0) {
      s3
    } else {
      val pos = s3.indexOf("e")
      s3.substring(0, pos) + "." + s3.substring(pos)
    }
  }

  private def generalScientificNotation(x: Double, precision: Int,
      forceDecimalSep: Boolean): String = {
    val m = Math.abs(x)

    val p =
      if (precision == 0) 1
      else precision

    // between 1e-4 and 10e(p): display as fixed
    if (m >= 1e-4 && m < Math.pow(10, p)) {
      /* First approximation of the smallest power of 10 that is >= m.
       * Due to rounding errors in the event of an imprecise `log10`
       * function, sig0 could actually be the smallest power of 10
       * that is > m.
       */
      val sig0 = Math.ceil(Math.log10(m)).toInt
      /* Increment sig0 so that it is always the first power of 10
       * that is > m.
       */
      val sig = if (Math.pow(10, sig0) <= m) sig0 + 1 else sig0
      decimalNotation(x, Math.max(p - sig, 0), forceDecimalSep)
    } else {
      computerizedScientificNotation(x, p - 1, forceDecimalSep)
    }
  }

  private def decimalNotation(x: Double, precision: Int,
      forceDecimalSep: Boolean): String = {

    import js.JSNumberOps._

    // First use JavaScript's native toFixed conversion
    val s1 = x.toFixed(precision)

    // -0.0 should have a leading '-'
    val s2 =
      if (x == 0.0 && 1 / x < 0) "-" + s1
      else s1

    // Finally, force the decimal separator, if requested
    if (forceDecimalSep && s2.indexOf(".") < 0)
      s2 + "."
    else
      s2
  }

  private def formatNonNumericString(flags: Flags, width: Int, precision: Int,
      str: String): Unit = {

    val truncatedStr =
      if (precision < 0) str
      else str.substring(0, precision)
    padAndSendToDestNoZeroPad(flags, width, applyUpperCase(flags, truncatedStr))
  }

  private def formatNaNOrInfinite(flags: Flags, width: Int, x: Double): Unit = {
    val str = if (x.isNaN) {
      "NaN"
    } else if (x > 0.0) {
      if (flags.positivePlus) "+Infinity"
      else if (flags.positiveSpace) " Infinity"
      else "Infinity"
    } else {
      if (flags.negativeParen) "(Infinity)"
      else "-Infinity"
    }

    padAndSendToDestNoZeroPad(flags, width, applyUpperCase(flags, str))
  }

  private def formatNumericString(flags: Flags, width: Int, str: String): Unit = {
    /* Flags for which a numeric string needs to be decomposed and transformed,
     * not just padded and/or uppercased. We can write fast-paths in this
     * method if none of them are present.
     */
    val TransformativeFlags =
      PositivePlus | PositiveSpace | UseGroupingSeps | NegativeParen

    if (str.length >= width && !flags.hasAnyOf(TransformativeFlags)) {
      // Super-fast-path
      sendToDest(applyUpperCase(flags, str))
    } else if (!flags.hasAnyOf(TransformativeFlags | ZeroPad)) {
      // Fast-path that does not need to inspect the string
      formatNonNumericString(flags, width, -1, str)
    } else {
      // Extract prefix and rest, based on flags and the presence of a sign
      val (prefix, rest0) = if (str.charAt(0) != '-') {
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

      // Insert grouping separators, if required
      val rest =
        if (flags.useGroupingSeps) insertGroupingSeps(rest0)
        else rest0

      // Apply uppercase, pad and send
      padAndSendToDest(flags, width, prefix, applyUpperCase(flags, rest))
    }
  }

  private def insertGroupingSeps(s: String): String = {
    val len = s.length
    var index = 0
    while (index != len && { val c = s.charAt(index); c >= '0' && c <= '9' }) {
      index += 1
    }

    index -= 3

    if (index <= 0) {
      s
    } else {
      var result = s.substring(index)
      while (index > 3) {
        val next = index - 3
        result = s.substring(next, index) + "," + result
        index = next
      }
      s.substring(0, index) + "," + result
    }
  }

  private def applyUpperCase(flags: Flags, str: String): String =
    if (flags.upperCase) str.toUpperCase
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
  private def padAndSendToDest(flags: Flags, width: Int, prefix: String,
      str: String): Unit = {

    val len = prefix.length + str.length

    if (len >= width)
      sendToDest(prefix, str)
    else if (flags.zeroPad)
      sendToDest(prefix, strRepeat("0", width - len), str)
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
    null
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

}

object Formatter {

  private val FormatSpecifier = new js.RegExp(
      """(?:(\d+)\$)?([-#+ 0,\(<]*)(\d+)?(?:\.(\d+))?[%A-Za-z]""", "g")

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

    final val InvalidFlagsForOctalAndHex =
      PositivePlus | PositiveSpace | UseGroupingSeps | NegativeParen

    final val NumericOnlyFlags =
      PositivePlus | PositiveSpace | ZeroPad | UseGroupingSeps | NegativeParen

    final val AllWrittenFlags =
      LeftAlign | AltFormat | NumericOnlyFlags | UseLastIndex
  }
}
