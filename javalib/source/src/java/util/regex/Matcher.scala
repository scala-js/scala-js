package java.util.regex

import scala.language.implicitConversions
import scala.annotation.switch
import scala.scalajs.js

final class Matcher private[regex] (
    private var pattern0: Pattern, private var input0: CharSequence) {

  import Matcher._

  def pattern(): Pattern = pattern0
  def input(): CharSequence = input0

  // Configuration (updated manually)
  private var regexp = new js.RegExp(pattern0.jspattern, pattern0.jsflags)
  private var inputstr = input0.toString

  // Match result (updated by successful matches)
  private var lastMatch: js.RegExp.ExecResult = null

  // Append state (updated by replacement methods)
  private var appendPos: Int = 0

  // Lookup methods

  def matches(): Boolean = {
    lastMatch = regexp.exec(inputstr)
    if ((lastMatch ne null) && (start != 0 || end != inputstr.length))
      lastMatch = null
    lastMatch ne null
  }

  def lookingAt(): Boolean = {
    lastMatch = regexp.exec(inputstr)
    if ((lastMatch ne null) && (start != 0))
      lastMatch = null
    lastMatch ne null
  }

  def find(): Boolean = {
    lastMatch = regexp.exec(inputstr)
    lastMatch ne null
  }

  def find(start: Int): Boolean = {
    reset()
    regexp.lastIndex = start
    find()
  }

  // Replace methods

  def appendReplacement(sb: StringBuffer, replacement: String): StringBuffer = {
    sb.append(inputstr.substring(appendPos, start))

    def isDigit(c: Char) = c >= '0' && c <= '9'

    val len = replacement.length
    var i = 0
    while (i < len) {
      replacement.charAt(i) match {
        case '$' =>
          i += 1
          var j = i
          while (i < len && isDigit(replacement.charAt(i)))
            i += 1
          val group = Integer.parseInt(replacement.substring(j, i))
          sb.append(this.group(group))

        case '\\' =>
          i += 1
          if (i < len)
            sb.append(replacement.charAt(i))
          i += 1

        case c =>
          sb.append(c)
          i += 1
      }
    }

    appendPos = end
    sb
  }

  def appendTail(sb: StringBuffer): StringBuffer = {
    sb.append(inputstr.substring(appendPos))
    appendPos = inputstr.length
    sb
  }

  def replaceFirst(replacement: String): String = {
    reset()

    if (find()) {
      val sb = new StringBuffer
      appendReplacement(sb, replacement)
      appendTail(sb)
      sb.toString
    } else {
      inputstr
    }
  }

  def replaceAll(replacement: String): String = {
    reset()

    val sb = new StringBuffer
    while (find()) {
      appendReplacement(sb, replacement)
    }
    appendTail(sb)

    sb.toString
  }

  // Reset methods

  def reset(): Matcher = {
    regexp.lastIndex = 0
    lastMatch = null
    appendPos = 0
    this
  }

  def reset(input: CharSequence): Matcher = {
    input0 = input
    inputstr = input.toString
    reset()
  }

  def usePattern(pattern: Pattern): Matcher = {
    val prevLastIndex = regexp.lastIndex

    pattern0 = pattern
    regexp = new js.RegExp(pattern.jspattern, pattern.jsflags)
    regexp.lastIndex = prevLastIndex
    this
  }

  // Query state methods - implementation of MatchResult

  def groupCount(): Int = lastMatch.length-1

  def start(): Int = lastMatch.index
  def end(): Int = start() + group().length
  def group(): String = lastMatch(0)

  def start(group: Int): Int = {
    if (group == 0) start()
    else {
      // not provided by JS RegExp, so we make up something that at least
      // will have some sound behavior from scala.util.matching.Regex
      inputstr.indexOf(lastMatch(group), lastMatch.index)
    }
  }

  def end(group: Int): Int = start(group) + this.group(group).length
  def group(group: Int): String = lastMatch(group)

  // Seal the state

  def toMatchResult(): MatchResult = new SealedResult(inputstr, lastMatch)

  // Other query state methods

  def hitEnd(): Boolean = end() == inputstr.length

  def requireEnd(): Boolean = ??? // I don't understand the spec

  // Stub methods for region management

  def regionStart(): Int = 0
  def regionEnd(): Int = inputstr.length
  def region(start: Int, end: Int): Matcher = ???

  def hasTransparentBounds(): Boolean = false
  def useTransparentBounds(b: Boolean): Matcher = ???

  def hasAnchoringBounds(): Boolean = true
  def useAnchoringBounds(b: Boolean): Matcher = ???
}

object Matcher {
  // js.Numbers used here are always Ints
  private implicit def jsNumberToInt(i: js.Number): Int = (i: Double).toInt

  def quoteReplacement(s: String): String = {
    var result = ""
    var i = 0
    while (i < s.length) {
      val c = s.charAt(i)
      result += ((c: @switch) match {
        case '\\' | '$' => "\\"+c
        case _ => c
      })
      i += 1
    }
    result
  }

  private final class SealedResult(inputstr: String,
      lastMatch: js.RegExp.ExecResult) extends MatchResult {

    def groupCount(): Int = lastMatch.length-1

    def start(): Int = lastMatch.index
    def end(): Int = start() + group().length
    def group(): String = lastMatch(0)

    def start(group: Int): Int = {
      if (group == 0) start()
      else {
        // not provided by JS RegExp, so we make up something that at least
        // will have some sound behavior from scala.util.matching.Regex
        inputstr.indexOf(lastMatch(group), lastMatch.index)
      }
    }

    def end(group: Int): Int = start(group) + this.group(group).length
    def group(group: Int): String = lastMatch(group)
  }
}
