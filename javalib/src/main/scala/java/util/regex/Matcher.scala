package java.util.regex

import scala.language.implicitConversions

import scala.annotation.switch

import scala.scalajs.js

final class Matcher private[regex] (
    private var pattern0: Pattern, private var input0: CharSequence,
    private var regionStart0: Int, private var regionEnd0: Int)
    extends AnyRef with MatchResult {

  import Matcher._

  def pattern(): Pattern = pattern0

  // Configuration (updated manually)
  private var regexp = pattern0.newJSRegExp()
  private var inputstr = input0.subSequence(regionStart0, regionEnd0).toString

  // Match result (updated by successful matches)
  private var lastMatch: js.RegExp.ExecResult = null
  private var lastMatchIsValid = false
  private var canStillFind = true

  // Append state (updated by replacement methods)
  private var appendPos: Int = 0

  // Lookup methods

  def matches(): Boolean = {
    reset()
    find()
    // TODO this check is wrong with non-greedy patterns
    // Further, it might be wrong to just use ^$ delimiters for two reasons:
    // - They might already be there
    // - They might not behave as expected when newline characters are present
    if ((lastMatch ne null) && (start != 0 || end != inputstr.length))
      reset()
    lastMatch ne null
  }

  def lookingAt(): Boolean = {
    reset()
    find()
    if ((lastMatch ne null) && (start != 0))
      reset()
    lastMatch ne null
  }

  def find(): Boolean = if (canStillFind) {
    lastMatchIsValid = true
    lastMatch = regexp.exec(inputstr)
    if (lastMatch ne null) {
      if (lastMatch(0).get.isEmpty)
        regexp.lastIndex += 1
    } else {
      canStillFind = false
    }
    lastMatch ne null
  } else false

  def find(start: Int): Boolean = {
    reset()
    regexp.lastIndex = start
    find()
  }

  // Replace methods

  def appendReplacement(sb: StringBuffer, replacement: String): Matcher = {
    sb.append(inputstr.substring(appendPos, start))

    @inline def isDigit(c: Char) = c >= '0' && c <= '9'

    val len = replacement.length
    var i = 0
    while (i < len) {
      replacement.charAt(i) match {
        case '$' =>
          i += 1
          val j = i
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
    this
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
    lastMatchIsValid = false
    canStillFind = true
    appendPos = 0
    this
  }

  def reset(input: CharSequence): Matcher = {
    regionStart0 = 0
    regionEnd0 = input.length()
    input0 = input
    inputstr = input0.toString
    reset()
  }

  def usePattern(pattern: Pattern): Matcher = {
    val prevLastIndex = regexp.lastIndex
    pattern0 = pattern
    regexp = pattern.newJSRegExp()
    regexp.lastIndex = prevLastIndex
    lastMatch = null
    this
  }

  // Query state methods - implementation of MatchResult

  private def ensureLastMatch: js.RegExp.ExecResult = {
    if (lastMatch == null)
      throw new IllegalStateException("No match available")
    lastMatch
  }

  def groupCount(): Int = ensureLastMatch.length-1

  def start(): Int = ensureLastMatch.index
  def end(): Int = start() + group().length
  def group(): String = ensureLastMatch(0).get

  def start(group: Int): Int = {
    if (group == 0) start()
    else {
      val last = ensureLastMatch
      // not provided by JS RegExp, so we make up something that at least
      // will have some sound behavior from scala.util.matching.Regex
      last(group).fold(-1) {
        groupStr => inputstr.indexOf(groupStr, last.index)
      }
    }
  }

  def end(group: Int): Int = {
    val s = start(group)
    if (s == -1) -1
    else s + this.group(group).length
  }

  def group(group: Int): String = ensureLastMatch(group).orNull

  def group(name: String): String = {
    ensureLastMatch
    throw new IllegalArgumentException
  }

  // Seal the state

  def toMatchResult(): MatchResult = new SealedResult(inputstr, lastMatch)

  // Other query state methods

  def hitEnd(): Boolean =
    lastMatchIsValid && (lastMatch == null || end() == inputstr.length)

  //def requireEnd(): Boolean // I don't understand the spec

  // Stub methods for region management

  def regionStart(): Int = regionStart0
  def regionEnd(): Int = regionEnd0
  def region(start: Int, end: Int): Matcher =
    new Matcher(pattern0, input0, start, end)

  def hasTransparentBounds(): Boolean = false
  //def useTransparentBounds(b: Boolean): Matcher

  def hasAnchoringBounds(): Boolean = true
  //def useAnchoringBounds(b: Boolean): Matcher
}

object Matcher {
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

    def groupCount(): Int = ensureLastMatch.length-1

    def start(): Int = ensureLastMatch.index
    def end(): Int = start() + group().length
    def group(): String = ensureLastMatch(0).get

    def start(group: Int): Int = {
      if (group == 0) start()
      else {
        val last = ensureLastMatch

        // not provided by JS RegExp, so we make up something that at least
        // will have some sound behavior from scala.util.matching.Regex
        last(group).fold(-1) {
          groupStr => inputstr.indexOf(groupStr, last.index)
        }
      }
    }

    def end(group: Int): Int = {
      val s = start(group)
      if (s == -1) -1
      else s + this.group(group).length
    }

    def group(group: Int): String = ensureLastMatch(group).orNull

    private def ensureLastMatch: js.RegExp.ExecResult = {
      if (lastMatch == null)
        throw new IllegalStateException("No match available")
      lastMatch
    }
  }
}
