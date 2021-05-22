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

import scala.annotation.tailrec

import scala.scalajs.js

import PatternCompiler.Support._

final class Pattern private[regex] (
  _pattern: String,
  _flags: Int,
  jsPattern: String,
  jsFlags: String,
  sticky: Boolean,
  private[regex] val groupCount: Int,
  groupNumberMap: js.Array[Int],
  namedGroups: js.Dictionary[Int]
) extends Serializable {

  import Pattern._

  @inline private def jsFlagsForFind: String =
    jsFlags + (if (sticky && supportsSticky) "gy" else "g")

  /** The RegExp that is used for `Matcher.find()`.
   *
   *  It receives the 'g' flag so that `lastIndex` is taken into acount.
   *
   *  It also receives the 'y' flag if this pattern is sticky and it is
   *  supported. If it is not supported, its behavior is polyfilled in
   *  `execFind()`.
   *
   *  Since that RegExp is only used locally within `execFind()`, we can
   *  always reuse the same instance.
   */
  private[this] val jsRegExpForFind =
    new js.RegExp(jsPattern, jsFlagsForFind)

  /** Another version of the RegExp that is used by `Matcher.matches()`.
   *
   *  It forces `^` and `$` at the beginning and end of the pattern so that
   *  only entire inputs are matched. In addition, it does not have the 'g'
   *  flag, so that it can be repeatedly used without managing `lastIndex`.
   *
   *  Since that RegExp is only used locally within `execMatches()`, we can
   *  always reuse the same instance.
   */
  private[this] val jsRegExpForMatches: js.RegExp =
    new js.RegExp(wrapJSPatternForMatches(jsPattern), jsFlags)

  private[regex] lazy val groupStartMapper: GroupStartMapper =
    GroupStartMapper(jsPattern, jsFlags)

  private[regex] def execMatches(input: String): js.RegExp.ExecResult =
    jsRegExpForMatches.exec(input)

  @inline // to stack-allocate the tuple
  private[regex] def execFind(input: String, start: Int): (js.RegExp.ExecResult, Int) = {
    val mtch = execFindInternal(input, start)
    val end = jsRegExpForFind.lastIndex
    (mtch, end)
  }

  private def execFindInternal(input: String, start: Int): js.RegExp.ExecResult = {
    val regexp = jsRegExpForFind

    if (!supportsSticky && sticky) {
      regexp.lastIndex = start
      val mtch = regexp.exec(input)
      if (mtch == null || mtch.index > start)
        null
      else
        mtch
    } else if (supportsUnicode) {
      regexp.lastIndex = start
      regexp.exec(input)
    } else {
      /* When the native RegExp does not support the 'u' flag (introduced in
       * ECMAScript 2015), it can find a match starting in the middle of a
       * surrogate pair. This can happen if the pattern can match a substring
       * starting with a lone low surrogate. However, that is not valid,
       * because surrogate pairs must always stick together.
       *
       * In all the other situations, the `PatternCompiler` makes sure that
       * surrogate pairs are always matched together or not at all, but it
       * cannot avoid this specific situation because there is no look-behind
       * support in that case either. So we take care of it now by skipping
       * matches that start in the middle of a surrogate pair.
       */
      @tailrec
      def loop(start: Int): js.RegExp.ExecResult = {
        regexp.lastIndex = start
        val mtch = regexp.exec(input)
        if (mtch == null) {
          null
        } else {
          val index = mtch.index
          if (index > start && index < input.length() &&
              Character.isLowSurrogate(input.charAt(index)) &&
              Character.isHighSurrogate(input.charAt(index - 1))) {
            loop(index + 1)
          } else {
            mtch
          }
        }
      }
      loop(start)
    }
  }

  private[regex] def numberedGroup(group: Int): Int = {
    if (group < 0 || group > groupCount)
      throw new IndexOutOfBoundsException(group.toString())
    groupNumberMap(group)
  }

  private[regex] def namedGroup(name: String): Int = {
    groupNumberMap(namedGroups.getOrElse(name, {
      throw new IllegalArgumentException(s"No group with name <$name>")
    }))
  }

  // Public API ---------------------------------------------------------------

  def pattern(): String = _pattern
  def flags(): Int = _flags

  override def toString(): String = pattern()

  def matcher(input: CharSequence): Matcher =
    new Matcher(this, input)

  def split(input: CharSequence): Array[String] =
    split(input, 0)

  def split(input: CharSequence, limit: Int): Array[String] = {
    val inputStr = input.toString

    // If the input string is empty, always return Array("") - #987, #2592
    if (inputStr == "") {
      Array("")
    } else {
      // Actually split original string
      val lim = if (limit > 0) limit else Int.MaxValue
      val matcher = this.matcher(inputStr)
      val builder = Array.newBuilder[String]
      var prevEnd = 0
      var size = 0
      while ((size < lim-1) && matcher.find()) {
        if (matcher.end() == 0) {
          /* If there is a zero-width match at the beginning of the string,
           * ignore it, i.e., omit the resulting empty string at the beginning
           * of the array.
           */
        } else {
          builder += inputStr.substring(prevEnd, matcher.start())
          size += 1
        }
        prevEnd = matcher.end()
      }
      builder += inputStr.substring(prevEnd)
      val result = builder.result()

      // With `limit == 0`, remove trailing empty strings.
      if (limit != 0) {
        result
      } else {
        var actualLength = result.length
        while (actualLength != 0 && result(actualLength - 1) == "")
          actualLength -= 1

        if (actualLength == result.length) {
          result
        } else {
          val actualResult = new Array[String](actualLength)
          System.arraycopy(result, 0, actualResult, 0, actualLength)
          actualResult
        }
      }
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

  def compile(regex: String, flags: Int): Pattern =
    PatternCompiler.compile(regex, flags)

  def compile(regex: String): Pattern =
    compile(regex, 0)

  def matches(regex: String, input: CharSequence): Boolean =
    compile(regex).matcher(input).matches()

  def quote(s: String): String = {
    var result = "\\Q"
    var start = 0
    var end = s.indexOf("\\E", start)
    while (end >= 0) {
      result += s.substring(start, end) + "\\E\\\\E\\Q"
      start = end + 2
      end = s.indexOf("\\E", start)
    }
    result + s.substring(start) + "\\E"
  }

  @inline
  private[regex] def wrapJSPatternForMatches(jsPattern: String): String =
    "^(?:" + jsPattern + ")$" // the group is needed if there is a top-level | in jsPattern
}
