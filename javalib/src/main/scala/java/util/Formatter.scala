package java.util

import scala.annotation.switch
import scala.scalajs.js

import java.io._
import java.lang._

final class Formatter(private val dest: Appendable) extends Closeable with Flushable {
  import Formatter._

  var closed = false

  def this() = this(new StringBuilder())

  def close(): Unit = {
    if (!closed) {
      dest match {
        case cl: Closeable => cl.close()
        case _ =>
      }
    }
    closed = true
  }

  def flush(): Unit = ifNotClosed {
    dest match {
      case fl: Flushable => fl.flush()
      case _ =>
    }
  }

  // Begin implem of format()

  def format(format_in: String, args: Array[AnyRef]): Formatter = ifNotClosed {
    import js.JSNumberOps._

    var fmt: String = format_in
    var lastImplicitIndex: Int = 0
    var lastIndex: Int = 0 // required for < flag

    while (!fmt.isEmpty) {
      fmt match {
        case RegularChunk(matchResult) =>
          fmt = fmt.substring(matchResult(0).get.length)
          dest.append(matchResult(0).get)

        case DoublePercent(_) =>
          fmt = fmt.substring(2)
          dest.append('%')

        case EOLChunk(_) =>
          fmt = fmt.substring(2)
          dest.append('\n')

        case FormattedChunk(matchResult) =>
          fmt = fmt.substring(matchResult(0).get.length)

          val flags = matchResult(2).get
          def hasFlag(flag: String) = flags.indexOf(flag) >= 0

          val indexStr = matchResult(1).getOrElse("")
          val index = if (!indexStr.isEmpty) {
            Integer.parseInt(indexStr)
          } else if (hasFlag("<")) {
            lastIndex
          } else {
            lastImplicitIndex += 1
            lastImplicitIndex
          }
          lastIndex = index
          if (index <= 0 || index > args.length)
            throw new MissingFormatArgumentException(matchResult(5).get)
          val arg = args(index-1)

          val widthStr = matchResult(3).getOrElse("")
          val hasWidth = !widthStr.isEmpty
          val width = {
            if (hasWidth) Integer.parseInt(widthStr)
            else if (hasFlag("-")) throw new MissingFormatWidthException(format_in)
            else 0
          }

          val precisionStr = matchResult(4).getOrElse("")
          val hasPrecision = !precisionStr.isEmpty
          val precision =
            if (hasPrecision) Integer.parseInt(precisionStr)
            else 0

          val conversion = matchResult(5).get.charAt(0)

          // Avoid using conversion.isUpper not to depend on the Unicode database
          @inline def isConversionUpperCase: scala.Boolean =
            conversion <= 'Z'

          def intArg: Int = (arg: Any) match {
            case arg: Int  => arg
            case arg: Char => arg.toInt
          }
          def numberArg: scala.Double = (arg: Any) match {
            case arg: Number => arg.doubleValue()
            case arg: Char   => arg.toDouble
          }

          def padCaptureSign(argStr: String, prefix: String) = {
            val firstChar = argStr.charAt(0)
            if (firstChar == '+' || firstChar == '-')
              pad(argStr.substring(1), firstChar+prefix)
            else
              pad(argStr, prefix)
          }

          def strRepeat(s: String, times: Int) = {
            var result: String = ""
            var i = times
            while (i > 0) {
              result += s
              i -= 1
            }
            result
          }

          def with_+(s: String, preventZero: scala.Boolean = false) = {
            if (s.charAt(0) != '-') {
              if (hasFlag("+"))
                pad(s, "+", preventZero)
              else if (hasFlag(" "))
                pad(s, " ", preventZero)
              else
                pad(s, "", preventZero)
            } else {
              if (hasFlag("("))
                pad(s.substring(1) + ")", "(", preventZero)
              else
                pad(s.substring(1), "-", preventZero)
            }
          }

          def pad(argStr: String, prefix: String = "",
                  preventZero: Boolean = false) = {
            val prePadLen = argStr.length + prefix.length

            val padStr = {
              if (width <= prePadLen) {
                prefix + argStr
              } else {
                val padRight = hasFlag("-")
                val padZero = hasFlag("0") && !preventZero
                val padLength = width - prePadLen
                val padChar: String = if (padZero) "0" else " "
                val padding = strRepeat(padChar, padLength)

                if (padZero && padRight)
                  throw new java.util.IllegalFormatFlagsException(flags)
                else if (padRight) prefix + argStr  + padding
                else if (padZero)  prefix + padding + argStr
                else padding + prefix + argStr
              }
            }

            val casedStr =
              if (isConversionUpperCase) padStr.toUpperCase()
              else padStr
            dest.append(casedStr)
          }

          (conversion: @switch) match {
            case 'b' | 'B' => pad { arg match {
              case null => "false"
              case b: Boolean => String.valueOf(b)
              case _ => "true"
            } }
            case 'h' | 'H' => pad {
              if (arg eq null) "null"
              else Integer.toHexString(arg.hashCode)
            }
            case 's' | 'S' => arg match {
              case formattable: Formattable =>
                val flags = {
                  (if (hasFlag("-")) FormattableFlags.LEFT_JUSTIFY else 0) |
                  (if (hasFlag("#")) FormattableFlags.ALTERNATE else 0) |
                  (if (isConversionUpperCase) FormattableFlags.UPPERCASE else 0)
                }
                formattable.formatTo(this, flags,
                    if (hasWidth) width else -1,
                    if (hasPrecision) precision else -1)
                None // no further processing
              case _ =>
                if (!hasFlag("#"))
                  pad(String.valueOf(arg))
                else
                  throw new FormatFlagsConversionMismatchException("#", 's')
            }
            case 'c' | 'C' =>
              pad(intArg.toChar.toString)
            case 'd' =>
              with_+(numberArg.toString())
            case 'o' =>
              val str = (arg: Any) match {
                case arg: scala.Int  => Integer.toOctalString(arg)
                case arg: scala.Long => Long.toOctalString(arg)
              }
              padCaptureSign(str, if (hasFlag("#")) "0" else "")
            case 'x' | 'X' =>
              val str = (arg: Any) match {
                case arg: scala.Int  => Integer.toHexString(arg)
                case arg: scala.Long => Long.toHexString(arg)
              }
              padCaptureSign(str, if (hasFlag("#")) "0x" else "")
            case 'e' | 'E' =>
              sciNotation(if (hasPrecision) precision else 6)
            case 'g' | 'G' =>
              val m = Math.abs(numberArg)
              // precision handling according to JavaDoc
              // precision here means number of significant digits
              // not digits after decimal point
              val p =
                if (!hasPrecision) 6
                else if (precision == 0) 1
                else precision
              // between 1e-4 and 10e(p): display as fixed
              if (m >= 1e-4 && m < Math.pow(10, p)) {
                val sig = Math.ceil(Math.log10(m)).toInt
                with_+(numberArg.toFixed(Math.max(p - sig, 0)))
              } else sciNotation(p - 1)
            case 'f' =>
              with_+({
                // JavaDoc: 6 is default precision
                numberArg.toFixed(if (hasPrecision) precision else 6)
              }, numberArg.isNaN || numberArg.isInfinite)
          }

          def sciNotation(precision: Int) = {
            val exp = numberArg.toExponential(precision)
            with_+({
              // check if we need additional 0 padding in exponent
              // JavaDoc: at least 2 digits
              if ('e' == exp.charAt(exp.length - 3)) {
                exp.substring(0, exp.length - 1) + "0" +
                  exp.charAt(exp.length - 1)
              } else exp
            }, numberArg.isNaN || numberArg.isInfinite)
          }
      }
    }

    this
  }

  def ioException(): IOException = null
  def locale(): Locale = ifNotClosed { null }
  def out(): Appendable = ifNotClosed { dest }

  override def toString(): String = out().toString()

  @inline private def ifNotClosed[T](body: => T): T =
    if (closed) throwClosedException()
    else body

  private def throwClosedException(): Nothing =
    throw new FormatterClosedException()

}

object Formatter {

  private class RegExpExtractor(val regexp: js.RegExp) {
    def unapply(str: String): Option[js.RegExp.ExecResult] = {
      Option(regexp.exec(str))
    }
  }

  private val RegularChunk = new RegExpExtractor(new js.RegExp("""^[^\x25]+"""))
  private val DoublePercent = new RegExpExtractor(new js.RegExp("""^\x25{2}"""))
  private val EOLChunk = new RegExpExtractor(new js.RegExp("""^\x25n"""))
  private val FormattedChunk = new RegExpExtractor(new js.RegExp(
      """^\x25(?:([1-9]\d*)\$)?([-#+ 0,\(<]*)(\d*)(?:\.(\d+))?([A-Za-z])"""))

}
