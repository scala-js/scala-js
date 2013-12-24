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
  private val FormattedChunk = new RegExpExtractor(new js.RegExp(
      """\x25(?:([1-9]\d*)\$)?([-#+ 0,\(]*)(\d*)(?:\.(\d+))?([A-Za-z])"""))

  def format(format_in: String, args: Array[AnyRef]): Formatter = ifNotClosed {
    var fmt: js.String = format_in
    var lastImplicitIndex: js.Number = 0

    while (!(!fmt)) {
      fmt match {
        case RegularChunk(matchResult) =>
          fmt = fmt.substring(matchResult(0).length)
          dest.append(matchResult(0))

        case DoublePercent(_) =>
          fmt = fmt.substring(2)
          dest.append('%')

        case FormattedChunk(matchResult) =>
          fmt = fmt.substring(matchResult(0).length)

          val indexStr = matchResult(1)
          val index = if (!(!indexStr)) {
            js.parseInt(indexStr)
          } else {
            lastImplicitIndex += 1
            lastImplicitIndex
          }
          val arg = args(index-1)

          val flags = matchResult(2)
          def hasFlag(flag: js.String) = flags.indexOf(flag) >= 0

          val widthStr = matchResult(3)
          val width =
            if (!(!widthStr)) js.parseInt(widthStr)
            else (0: js.Number)

          val precisionStr = matchResult(4)
          val hasPrecision = !(!precisionStr)
          val precision =
            if (hasPrecision) js.parseInt(precisionStr)
            else (0: js.Number)

          val conversion = (matchResult(5): String).charAt(0)

          def numberArg: js.Number = arg match {
            case arg: Number => arg.doubleValue()
            case arg: Character => arg.charValue().toInt
            case _ => arg.asInstanceOf[js.Number] // assume js.Number
          }

          def unsignedArg: js.Number = arg match {
            case arg: Byte    if arg < 0 => js.Math.pow(2, Byte.SIZE)    + arg.doubleValue()
            case arg: Short   if arg < 0 => js.Math.pow(2, Short.SIZE)   + arg.doubleValue()
            case arg: Integer if arg < 0 => js.Math.pow(2, Integer.SIZE) + arg.doubleValue()
            // FIXME (once long is integrated)
            case arg: Long    if arg < 0 => js.Math.pow(2, Long.SIZE)    + arg.doubleValue()
            case arg => numberArg // ignore negative case
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

          def strs(s1: js.String, s2: js.String = "") = (s1, s2)
 
          def with_+(s: js.String) = {
            if ((s:String).charAt(0) != '-') {
              if (hasFlag("+")) strs("+", s)
              else if (hasFlag(" ")) strs(" ", s)
              else strs(s)
            } else {
              if (hasFlag("(")) strs("(", s.substring(1) + ")")
              else strs(s.substring(1),"-")
            }
          }

          val (argStr, prefix) = (conversion: @switch) match {
            case 'b' | 'B' => strs {
              if (arg eq null) "false"
              else arg.asInstanceOf[Boolean].toString()
            }
            case 'h' | 'H' => strs {
              if (arg eq null) "null"
              else Integer.toHexString(arg.hashCode)
            }
            case 's' | 'S' => strs {
              val s: js.String = if (arg eq null) "null" else arg.toString()
              if (hasPrecision) s.substring(0, precision)
              else s
            }
            case 'c' | 'C' =>
              strs(js.String.fromCharCode(numberArg))
            case 'd' =>
              with_+(numberArg.toString())
            case 'o' =>
              strs(unsignedArg.toString(8), if (hasFlag("#")) "0" else "")
            case 'x' | 'X' =>
              strs(unsignedArg.toString(16), if (hasFlag("#")) "0x" else "")
            case 'e' | 'E' =>
              with_+(
                  if (hasPrecision) numberArg.toExponential(precision)
                  else numberArg.toExponential())
            case 'f' | 'g' | 'G' =>
              with_+(
                  if (hasPrecision) numberArg.toFixed(precision)
                  else numberArg.toFixed())
            case 'n' =>
              strs("\n")
          }

          val prePadLen = argStr.length + prefix.length

          val paddedStr = {
            if (width <= prePadLen) {
              prefix + argStr
            } else {
              val padZero = hasFlag("0")
              val padRight = hasFlag("-")
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
            if (conversion.isUpper) paddedStr.toUpperCase()
            else paddedStr
            
          dest.append(casedStr)
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
