package java.util

import scala.annotation.switch
import scala.scalajs.js

import java.io._
import java.lang._

final class Formatter(private val dest: Appendable) extends Closeable with Flushable {

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

  private class RegExpExtractor(val regexp: js.RegExp) {
    def unapply(str: js.String): Option[js.RegExp.ExecResult] = {
      Option(regexp.exec(str))
    }
  }

  private val RegularChunk = new RegExpExtractor(new js.RegExp("""^[^\x25]+"""))
  private val DoublePercent = new RegExpExtractor(new js.RegExp("""^\x25{2}"""))
  private val EOLChunk = new RegExpExtractor(new js.RegExp("""^\x25n"""))
  private val FormattedChunk = new RegExpExtractor(new js.RegExp(
      """^\x25(?:([1-9]\d*)\$)?([-#+ 0,\(<]*)(\d*)(?:\.(\d+))?([A-Za-z])"""))

  def format(format_in: String, args: Array[AnyRef]): Formatter = ifNotClosed {
    var fmt: js.String = format_in
    var lastImplicitIndex: js.Number = 0
    var lastIndex: js.Number = 0 // required for < flag

    while (!(!fmt)) {
      fmt match {
        case RegularChunk(matchResult) =>
          fmt = fmt.substring(matchResult(0).length)
          dest.append(matchResult(0))

        case DoublePercent(_) =>
          fmt = fmt.substring(2)
          dest.append('%')

        case EOLChunk(_) =>
          fmt = fmt.substring(2)
          dest.append('\n')

        case FormattedChunk(matchResult) =>
          fmt = fmt.substring(matchResult(0).length)

          val flags = matchResult(2)
          def hasFlag(flag: js.String) = flags.indexOf(flag) >= 0

          val indexStr = matchResult(1)
          val index = if (!(!indexStr)) {
            js.parseInt(indexStr)
          } else if (hasFlag("<")) {
            lastIndex
          } else {
            lastImplicitIndex += 1
            lastImplicitIndex
          }
          lastIndex = index
          val arg = args(index-1)

          val widthStr = matchResult(3)
          val hasWidth = !(!widthStr)
          val width =
            if (hasWidth) js.parseInt(widthStr)
            else (0: js.Number)

          val precisionStr = matchResult(4)
          val hasPrecision = !(!precisionStr)
          val precision =
            if (hasPrecision) js.parseInt(precisionStr)
            else (0: js.Number)

          val conversion = (matchResult(5): String).charAt(0)

          def numberArg: js.Number = arg match {
            case arg: Number    => arg.doubleValue()
            case arg: Character => arg.charValue().toInt
            case arg: js.Number => arg
          }

          // threshold is supposed to be MaxValue+1
          def pseudoUnsignedArgStr(x: js.Number, threshold: js.Number,
              base: js.Number): js.String = {
            if (x >= 0) {
              if (x >= threshold) ("+": js.String) + x.toString(base)
              else                x.toString(base)
            } else {
              if (x < -threshold) x.toString(base) // includes "-"
              else                ((threshold << 1) + x).toString(base)
            }
          }

          def padCaptureSign(argStr: js.String, prefix: js.String) = {
            val firstChar = (argStr: String).charAt(0)
            if (firstChar == '+' || firstChar == '-')
              pad(argStr.substring(1), firstChar+prefix)
            else
              pad(argStr, prefix)
          }

          def strRepeat(s: js.String, times: js.Number) = {
            var result: js.String = ""
            var i = times
            while (i > 0) {
              result += s
              i -= 1
            }
            result
          }

          def with_+(s: js.String, preventZero: scala.Boolean = false) = {
            if ((s:String).charAt(0) != '-') {
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
                pad(s.substring(1),"-", preventZero)
            }
          }

          def pad(argStr: js.String, prefix: js.String = "",
                  preventZero: Boolean = false) = {
            val prePadLen = argStr.length + prefix.length

            val padStr = {
              if (width <= prePadLen) {
                prefix + argStr
              } else {
                val padRight = hasFlag("-")
                val padZero = hasFlag("0") && !preventZero
                val padLength = width - prePadLen
                val padChar: js.String = if (padZero) "0" else " "
                val padding = strRepeat(padChar, padLength)

                if (padZero && padRight)
                  throw new java.util.IllegalFormatFlagsException(flags)
                else if (padRight) prefix + argStr  + padding
                else if (padZero)  prefix + padding + argStr
                else padding + prefix + argStr
              }
            }

            val casedStr =
              if (conversion.isUpper) padStr.toUpperCase()
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
              case null if !hasFlag("#") => pad("null")
              case formattable: Formattable =>
                val flags = (
                  (if (hasFlag("-"))       FormattableFlags.LEFT_JUSTIFY else 0) |
                  (if (hasFlag("#"))       FormattableFlags.ALTERNATE    else 0) |
                  (if (conversion.isUpper) FormattableFlags.UPPERCASE    else 0)
                )

                formattable.formatTo(this, flags,
                                     if (hasWidth)     width.toInt     else -1,
                                     if (hasPrecision) precision.toInt else -1)
                None // no further processing
              case t: AnyRef if !hasFlag("#") => pad(t.toString)
              case _ =>
                throw new FormatFlagsConversionMismatchException("#", 's')
            }
            case 'c' | 'C' =>
              pad(js.String.fromCharCode(numberArg))
            case 'd' =>
              with_+(numberArg.toString())
            case 'o' =>
              val str: js.String = arg match {
                case arg: Byte =>
                  pseudoUnsignedArgStr(arg.byteValue,
                      threshold = scala.Byte.MaxValue+1, base = 8)
                case arg: Short=>
                  pseudoUnsignedArgStr(arg.shortValue,
                      threshold = scala.Short.MaxValue+1, base = 8)
                case arg: Integer   => Integer.toOctalString(arg)
                case arg: Long      => Long.toOctalString(arg)
                case arg: js.Number => arg.toString(8)
              }
              padCaptureSign(str, if (hasFlag("#")) "0" else "")
            case 'x' | 'X' =>
              val str: js.String = arg match {
                case arg: Byte =>
                  pseudoUnsignedArgStr(arg.byteValue,
                      threshold = scala.Byte.MaxValue+1, base = 16)
                case arg: Short =>
                  pseudoUnsignedArgStr(arg.shortValue,
                      threshold = scala.Short.MaxValue+1, base = 16)
                case arg: Integer   => Integer.toHexString(arg)
                case arg: Long      => Long.toHexString(arg)
                case arg: js.Number => arg.toString(16)
              }
              padCaptureSign(str, if (hasFlag("#")) "0x" else "")
            case 'e' | 'E' =>
              sciNotation(if (hasPrecision) precision else (6: js.Number))
            case 'g' | 'G' =>
              val m = js.Math.abs(numberArg)
              // precision handling according to JavaDoc
              // precision here means number of significant digits
              // not digits after decimal point
              val p =
                if (!hasPrecision)
                  6: js.Number
                else if (precision == (0: js.Number))
                  1: js.Number
                else precision
              // between 1e-4 and 10e(p): display as fixed
              if (m >= 1e-4 && m < js.Math.pow(10,p)) {
                val sig = js.Math.ceil(js.Math.log(m) / js.Math.LN10)
                with_+(numberArg.toFixed(js.Math.max(p - sig,0)))
              } else sciNotation(p - 1)
            case 'f' =>
              with_+ ( {
                if (hasPrecision)
                  numberArg.toFixed(precision)
                else
                  // JavaDoc: 6 is default precision
                  numberArg.toFixed(6)
              }, !js.isFinite(numberArg))
          }

          def sciNotation(precision: js.Number) = {
            val exp = numberArg.toExponential(precision)
            with_+( {
              // check if we need additional 0 padding in exponent
              // JavaDoc: at least 2 digits
              if ("e" == exp.charAt(exp.length - 3))
                exp.substring(0,exp.length - 1) + "0" +
                  exp.charAt(exp.length - 1)
              else exp
            } , !js.isFinite(numberArg))
          }
      }
    }

    this
  }

  def ioException(): IOException = null
  def locale() = ifNotClosed { null }
  def out(): Appendable = ifNotClosed { dest }
  override def toString(): String = out().toString()

  private def ifNotClosed[T](body: =>T): T =
    if (closed) throw new FormatterClosedException
    else body

}
