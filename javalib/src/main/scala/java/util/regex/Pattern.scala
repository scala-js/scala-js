package java.util.regex

import scala.annotation.switch

import scala.scalajs.js

final class Pattern private (jsRegExp: js.RegExp, _pattern: String, _flags: Int)
    extends Serializable {

  import Pattern._

  def pattern(): String = _pattern
  def flags(): Int = _flags

  override def toString(): String = pattern

  private[regex] def newJSRegExp(): js.RegExp = {
    val r = new js.RegExp(jsRegExp)
    if (r ne jsRegExp) {
      r
    } else {
      /* Workaround for the PhantomJS 1.x bug
       * https://github.com/ariya/phantomjs/issues/11494
       * which causes new js.RegExp(jsRegExp) to return the same object,
       * rather than a new one.
       * We therefore reconstruct the pattern and flags used to create
       * jsRegExp and create a new one from there.
       */
      val jsFlags = {
        (if (jsRegExp.global) "g" else "") +
        (if (jsRegExp.ignoreCase) "i" else "") +
        (if (jsRegExp.multiline) "m" else "")
      }
      new js.RegExp(jsRegExp.source, jsFlags)
    }
  }

  def matcher(input: CharSequence): Matcher =
    new Matcher(this, input, 0, input.length)

  def split(input: CharSequence): Array[String] =
    split(input, 0)

  def split(input: CharSequence, limit: Int): Array[String] = {
    val lim = if (limit > 0) limit else Int.MaxValue

    val result = js.Array[String]()
    val inputStr = input.toString
    val matcher = this.matcher(inputStr)
    var prevEnd = 0

    // Actually split original string
    while ((result.length < lim-1) && matcher.find()) {
      result.push(inputStr.substring(prevEnd, matcher.start))
      prevEnd = matcher.end
    }
    result.push(inputStr.substring(prevEnd))

    // Remove a leading empty element iff the first match was zero-length
    // and there is no other place the regex matches
    if (prevEnd == 0 && result.length == 2 && (lim > 2 || !matcher.find())) {
      Array(inputStr)
    } else {
      var len = result.length
      if (limit == 0) {
        while (len > 1 && result(len-1).isEmpty)
          len -= 1
      }

      val actualResult = new Array[String](len)
      result.copyToArray(actualResult)
      actualResult
    }
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

  def compile(regex: String, flags: Int): Pattern = {
    val (jsPattern, flags1) = {
      if ((flags & LITERAL) != 0) {
        (quote(regex), flags)
      } else {
        trySplitHack(regex, flags) orElse
        tryFlagHack(regex, flags) getOrElse
        (regex, flags)
      }
    }

    val jsFlags = {
      "g" +
      (if ((flags1 & CASE_INSENSITIVE) != 0) "i" else "") +
      (if ((flags1 & MULTILINE) != 0) "m" else "")
    }

    val jsRegExp = new js.RegExp(jsPattern, jsFlags)

    new Pattern(jsRegExp, regex, flags1)
  }

  def compile(regex: String): Pattern =
    compile(regex, 0)

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

  /** This is a hack to support StringLike.split().
   *  It replaces occurrences of \Q<char>\E by quoted(<char>)
   */
  @inline
  private def trySplitHack(pat: String, flags: Int) = {
    val m = splitHackPat.exec(pat)
    if (m != null)
      Some((quote(m(1).get), flags))
    else
      None
  }

  @inline
  private def tryFlagHack(pat: String, flags0: Int) = {
    val m = flagHackPat.exec(pat)
    if (m != null) {
      val newPat = pat.substring(m(0).get.length) // cut off the flag specifiers
      val flags1 = m(1).fold(flags0) { chars =>
        chars.foldLeft(flags0) { (f, c) => f | charToFlag(c) }
      }
      val flags2 = m(2).fold(flags1) { chars =>
        chars.foldLeft(flags1) { (f, c) => f & ~charToFlag(c) }
      }
      Some((newPat, flags2))
    } else
      None
  }

  private def charToFlag(c: Char) = (c: @switch) match {
    case 'i' => CASE_INSENSITIVE
    case 'd' => UNIX_LINES
    case 'm' => MULTILINE
    case 's' => DOTALL
    case 'u' => UNICODE_CASE
    case 'x' => COMMENTS
    case 'U' => UNICODE_CHARACTER_CLASS
    case _   => sys.error("bad in-pattern flag")
  }

  /** matches \Q<char>\E to support StringLike.split */
  private val splitHackPat = new js.RegExp("^\\\\Q(.|\\n|\\r)\\\\E$")

  /** regex to match flag specifiers in regex. E.g. (?u), (?-i), (?U-i) */
  private val flagHackPat =
    new js.RegExp("^\\(\\?([idmsuxU]*)(?:-([idmsuxU]*))?\\)")
}
