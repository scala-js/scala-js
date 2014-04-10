package scala.scalajs.sbtplugin

object JSUtils {
  def listToJS(xs: List[String]): String =
    xs.map(toJSstr _).mkString("[",",","]")

  /** (almost) stolen from scala.scalajs.compiler.JSPrinters */
  def toJSstr(str: String): String = {
    /* Note that Java and JavaScript happen to use the same encoding for
     * Unicode, namely UTF-16, which means that 1 char from Java always equals
     * 1 char in JavaScript. */
    val builder = new StringBuilder()
    builder.append('"')
    str foreach {
      case '\\' => builder.append("\\\\")
      case '"' => builder.append("\\\"")
      case '\u0007' => builder.append("\\a")
      case '\u0008' => builder.append("\\b")
      case '\u0009' => builder.append("\\t")
      case '\u000A' => builder.append("\\n")
      case '\u000B' => builder.append("\\v")
      case '\u000C' => builder.append("\\f")
      case '\u000D' => builder.append("\\r")
      case c =>
        if (c >= 32 && c <= 126) builder.append(c.toChar) // ASCII printable characters
        else builder.append(f"\\u$c%04x")
    }
    builder.append('"')
    builder.result()
  }
}
