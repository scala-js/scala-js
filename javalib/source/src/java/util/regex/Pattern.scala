package java.util.regex

import scala.annotation.switch

final class Pattern private (pattern0: String, flags0: Int) {
  import Pattern._

  def pattern(): String = pattern0
  def flags(): Int = flags0

  private[regex] val jspattern = {
    if ((flags0 & LITERAL) != 0) quote(pattern0)
    else pattern0
  }

  private[regex] val jsflags = {
    var f = "g"
    if ((flags & CASE_INSENSITIVE) != 0)
      f += "i"
    if ((flags & MULTILINE) != 0)
      f += "m"
    f
  }

  override def toString(): String = pattern0

  def matcher(input: CharSequence): Matcher =
    new Matcher(this, input)

  def split(input: CharSequence): Array[String] =
    split(input, 0)

  def split(input: CharSequence, limit: Int): Array[String] = {
    val hasLimit = limit > 0
    val lim = if (hasLimit) limit else Int.MaxValue

    val result = new js.Array[String](0)
    val inputStr = input.toString
    val matcher = this.matcher(inputStr)
    var prevEnd = 0

    while ((result.length < lim-1) && matcher.find()) {
      result.push(inputStr.substring(prevEnd, matcher.start))
      prevEnd = matcher.end
    }
    result.push(inputStr.substring(prevEnd))

    var len = result.length.toInt
    if (limit == 0) {
      while (len > 0 && result(len-1).isEmpty)
        len -= 1
    }

    val actualResult = new Array[String](len)
    var i = 0
    while (i < len) {
      actualResult(i) = result(i)
      i += 1
    }
    actualResult
  }
}

object Pattern {
  final val UNIX_LINES = 0x01
  final val CASE_INSENSITIVE = 0x02
  final val COMMENTS = 0x04
  final val MULTILINE = 0x08
  final val LITERAL = 0x10
  final val DOTALL = 0x20
  final val UNICODE_CASE = 0x40
  final val CANON_EQ = 0x80
  final val UNICODE_CHARACTER_CLASS = 0x100

  def compile(regex: String, flags: Int): Pattern =
    new Pattern(regex, flags)

  def compile(regex: String): Pattern =
    new Pattern(regex, 0)

  def matches(regex: String, input: CharSequence): Boolean =
    compile(regex).matcher(input).matches()

  def quote(s: String): String = {
    var result = ""
    var i = 0
    while (i < s.length) {
      val c = s.charAt(i)
      result += ((c: @switch) match {
        case '\\' | '.' | '(' | ')' | '[' | ']' | '{' | '}' | '|'
          | '?' | '*' | '+' | '^' | '$' => "\\"+c
        case _ => c
      })
      i += 1
    }
    result
  }
}
