package java.lang

import scala.annotation.switch

object String {
  def valueOf(value: scala.Boolean) = new java.lang.Boolean(value).toString()
  def valueOf(value: scala.Char) = new java.lang.Character(value).toString()
  def valueOf(value: scala.Byte) = new java.lang.Byte(value).toString()
  def valueOf(value: scala.Short) = new java.lang.Short(value).toString()
  def valueOf(value: scala.Int) = new java.lang.Integer(value).toString()
  def valueOf(value: scala.Long) = new java.lang.Long(value).toString()
  def valueOf(value: scala.Float) = new java.lang.Float(value).toString()
  def valueOf(value: scala.Double) = new java.lang.Double(value).toString()
  def valueOf(value: java.lang.Object) = value.toString()

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

  def format(format: String, args: Array[AnyRef]): String = {
    var fmt: js.String = format
    var lastImplicitIndex: js.Number = 0
    var result: js.String = ""

    while (!(!fmt)) {
      fmt match {
        case RegularChunk(matchResult) =>
          fmt = fmt.substring(matchResult(0).length)
          result += matchResult(0)

        case DoublePercent(_) =>
          fmt = fmt.substring(2)
          result += "%"

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

          def strRepeat(s: js.String, times: js.Number) = {
            var result: js.String = ""
            var i = times
            while (i > 0) {
              result += s
              i -= 1
            }
            result
          }

          def with_+(s: js.String): js.String = {
            if ((s:String).charAt(0) != '-') {
              if (hasFlag("+")) "+" + s
              else if (hasFlag(" ")) " " + s
              else s
            } else {
              if (hasFlag("(")) ("(":js.String) + s.substring(1) + ")"
              else s
            }
          }

          val argStr: js.String = (conversion: @switch) match {
            case 'b' | 'B' =>
              if (arg eq null) "false"
              else arg.asInstanceOf[Boolean].toString()
            case 'h' | 'H' =>
              if (arg eq null) "null"
              else Integer.toHexString(arg.hashCode)
            case 's' | 'S' =>
              val s: js.String = if (arg eq null) "null" else arg.toString()
              if (hasPrecision) s.substring(0, precision)
              else s
            case 'c' | 'C' =>
              js.String.fromCharCode(numberArg)
            case 'd' =>
              with_+(numberArg.toString())
            case 'o' =>
              val prefix: js.String = if (hasFlag("#")) "0" else ""
              with_+(prefix + numberArg.toString(8))
            case 'x' | 'X' =>
              val prefix: js.String = if (hasFlag("#")) "0x" else ""
              with_+(prefix + numberArg.toString(16))
            case 'e' | 'E' =>
              with_+(
                  if (hasPrecision) numberArg.toExponential(precision)
                  else numberArg.toExponential())
            case 'f' | 'g' | 'G' =>
              with_+(
                  if (hasPrecision) numberArg.toFixed(precision)
                  else numberArg.toFixed())
            case 'n' =>
              "\n"
          }

          val casedStr =
            if (conversion >= 'A' && conversion <= 'Z') argStr.toUpperCase()
            else argStr

          val paddedStr = {
            if (width <= casedStr.length) {
              casedStr
            } else {
              val padLength = width - casedStr.length
              val padChar: js.String = if (hasFlag("0")) "0" else " "
              val padding = strRepeat(padChar, padLength)

              if (hasFlag("-")) casedStr + padding
              else padding + casedStr
            }
          }

          result += paddedStr
      }
    }

    result
  }
}
