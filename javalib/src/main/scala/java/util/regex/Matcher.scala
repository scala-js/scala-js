/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

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
  private var lastMatchIsForMatches = false
  private var canStillFind = true

  // Append state (updated by replacement methods)
  private var appendPos: Int = 0

  // Lookup methods

  def matches(): Boolean = {
    resetMatch()

    lastMatch = pattern().execMatches(inputstr)
    lastMatchIsForMatches = true
    lastMatch ne null
  }

  def lookingAt(): Boolean = {
    resetMatch()
    find()
    if ((lastMatch ne null) && (ensureLastMatch.index != 0))
      resetMatch()
    lastMatch ne null
  }

  def find(): Boolean = if (canStillFind) {
    lastMatch = pattern().execFind(regexp, inputstr)
    if (lastMatch ne null) {
      if (lastMatch(0).get.isEmpty)
        regexp.lastIndex += 1
    } else {
      canStillFind = false
    }
    lastMatchIsForMatches = false
    startOfGroupCache = null
    lastMatch ne null
  } else false

  def find(start: Int): Boolean = {
    reset()
    regexp.lastIndex = start
    find()
  }

  // Replace methods

  def appendReplacement(sb: StringBuffer, replacement: String): Matcher = {
    sb.append(inputstr.substring(appendPos, start()))

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
          val replaced = this.group(group)
          if (replaced != null)
            sb.append(replaced)

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

    appendPos = end()
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

  private def resetMatch(): Matcher = {
    regexp.lastIndex = 0
    lastMatch = null
    canStillFind = true
    appendPos = 0
    startOfGroupCache = null
    this
  }

  def reset(): Matcher =
    region(0, input0.length())

  def reset(input: CharSequence): Matcher = {
    input0 = input
    reset()
  }

  def usePattern(pattern: Pattern): Matcher = {
    val prevLastIndex = regexp.lastIndex
    pattern0 = pattern
    regexp = pattern.newJSRegExp()
    regexp.lastIndex = prevLastIndex
    lastMatch = null
    startOfGroupCache = null
    this
  }

  // Query state methods - implementation of MatchResult

  private def ensureLastMatch: js.RegExp.ExecResult = {
    if (lastMatch == null)
      throw new IllegalStateException("No match available")
    lastMatch
  }

  def groupCount(): Int = pattern().groupCount

  def start(): Int = ensureLastMatch.index + regionStart()
  def end(): Int = start() + group().length
  def group(): String = ensureLastMatch(0).get

  private def startInternal(compiledGroup: Int): Int = {
    val s = startOfGroup(compiledGroup)
    if (s == -1) -1
    else s + regionStart()
  }

  def start(group: Int): Int = {
    if (group == 0) start()
    else startInternal(pattern().numberedGroup(group))
  }

  def start(name: String): Int =
    startInternal(pattern().namedGroup(name))

  private def endInternal(compiledGroup: Int): Int = {
    val s = startOfGroup(compiledGroup)
    if (s == -1) -1
    else s + ensureLastMatch(compiledGroup).get.length + regionStart()
  }

  def end(group: Int): Int =
    if (group == 0) end()
    else endInternal(pattern().numberedGroup(group))

  def end(name: String): Int =
    endInternal(pattern().namedGroup(name))

  def group(group: Int): String =
    ensureLastMatch(pattern().numberedGroup(group)).orNull

  def group(name: String): String =
    ensureLastMatch(pattern().namedGroup(name)).orNull

  // Seal the state

  def toMatchResult(): MatchResult = {
    new SealedResult(inputstr, lastMatch, lastMatchIsForMatches, pattern(),
        regionStart(), startOfGroupCache)
  }

  // Other query state methods

  // Cannot be implemented (see #3454)
  //def hitEnd(): Boolean

  // Similar difficulties as with hitEnd()
  //def requireEnd(): Boolean

  // Stub methods for region management

  def regionStart(): Int = regionStart0
  def regionEnd(): Int = regionEnd0

  def region(start: Int, end: Int): Matcher = {
    regionStart0 = start
    regionEnd0 = end
    inputstr = input0.subSequence(regionStart0, regionEnd0).toString
    resetMatch()
  }

  def hasTransparentBounds(): Boolean = false
  //def useTransparentBounds(b: Boolean): Matcher

  def hasAnchoringBounds(): Boolean = true
  //def useAnchoringBounds(b: Boolean): Matcher

  // Lazily computed by `startOfGroup`, reset every time `lastMatch` changes
  private var startOfGroupCache: js.Array[Int] = _

  /** Returns a mapping from the group number to the respective start position. */
  private def startOfGroup: js.Array[Int] = {
    if (startOfGroupCache eq null)
      startOfGroupCache = pattern0.groupStartMapper(lastMatchIsForMatches, inputstr, ensureLastMatch.index)
    startOfGroupCache
  }
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
      lastMatch: js.RegExp.ExecResult, lastMatchIsForMatches: Boolean,
      pattern: Pattern, regionStart: Int,
      private var startOfGroupCache: js.Array[Int])
      extends MatchResult {

    def groupCount(): Int = pattern.groupCount

    def start(): Int = ensureLastMatch.index + regionStart
    def end(): Int = start() + group().length
    def group(): String = ensureLastMatch(0).get

    private def startOfGroup: js.Array[Int] = {
      if (startOfGroupCache eq null)
        startOfGroupCache = pattern.groupStartMapper(lastMatchIsForMatches, inputstr, ensureLastMatch.index)
      startOfGroupCache
    }

    /* Note that MatchResult does *not* define the named versions of `group`,
     * `start` and `end`, so we don't have them here either.
     */

    private def startInternal(compiledGroup: Int): Int = {
      val s = startOfGroup(compiledGroup)
      if (s == -1) -1
      else s + regionStart
    }

    def start(group: Int): Int = {
      if (group == 0) start()
      else startInternal(pattern.numberedGroup(group))
    }

    private def endInternal(compiledGroup: Int): Int = {
      val s = startOfGroup(compiledGroup)
      if (s == -1) -1
      else s + ensureLastMatch(compiledGroup).get.length + regionStart
    }

    def end(group: Int): Int =
      if (group == 0) end()
      else endInternal(pattern.numberedGroup(group))

    def group(group: Int): String =
      ensureLastMatch(pattern.numberedGroup(group)).orNull

    private def ensureLastMatch: js.RegExp.ExecResult = {
      if (lastMatch == null)
        throw new IllegalStateException("No match available")
      lastMatch
    }
  }
}
