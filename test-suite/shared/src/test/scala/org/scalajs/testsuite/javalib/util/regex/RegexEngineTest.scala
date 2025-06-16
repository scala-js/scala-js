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

package org.scalajs.testsuite.javalib.util.regex

import java.util.regex._
import java.util.regex.Pattern.compile

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.Platform._
import org.scalajs.testsuite.utils.AssertThrows.assertThrows

class RegexEngineTest  {

  // Scala-friendly names for flags
  private final val UnixLines = Pattern.UNIX_LINES
  private final val CaseInsensitive = Pattern.CASE_INSENSITIVE
  private final val Comments = Pattern.COMMENTS
  private final val Multiline = Pattern.MULTILINE
  private final val Literal = Pattern.LITERAL
  private final val DotAll = Pattern.DOTALL
  private final val UnicodeCase = Pattern.UNICODE_CASE
  private final val CanonEq = Pattern.CANON_EQ
  private final val UnicodeCharacterClass = Pattern.UNICODE_CHARACTER_CLASS

  // Assertion utils

  private val flagStrings = List(
    UnixLines -> "d",
    CaseInsensitive -> "i",
    Comments -> "x",
    Multiline -> "m",
    Literal -> "L",
    DotAll -> "s",
    UnicodeCase -> "u",
    CanonEq -> "C",
    UnicodeCharacterClass -> "U"
  )

  private def flagsToString(flags: Int): String =
    flagStrings.filter(p => (flags & p._1) != 0).map(_._2).mkString

  private def debugEscape(pattern: String): String = {
    pattern.flatMap {
      case '\t'         => "`t"
      case '\n'         => "`n"
      case '\r'         => "`r"
      case c if c < ' ' => "`x%02X".format(c.toInt)
      case c            => c.toString()
    }
  }

  private def patternToString(pattern: Pattern): String =
    s"/${debugEscape(pattern.pattern())}/${flagsToString(pattern.flags())}"

  @noinline
  private def assertMatches(pattern: Pattern, input: String): Matcher = {
    val matcher = pattern.matcher(input)
    if (!matcher.matches())
      fail(s"expected ${patternToString(pattern)} to match '${debugEscape(input)}'")
    matcher
  }

  @noinline
  private def assertMatches(pattern: String, flags: Int, input: String): Matcher =
    assertMatches(Pattern.compile(pattern, flags), input)

  @noinline
  private def assertMatches(pattern: String, input: String): Matcher =
    assertMatches(Pattern.compile(pattern), input)

  @noinline
  private def assertMatchesAndGroupsEquals(pattern: Pattern, input: String,
      expectedGroups: String*): Matcher = {
    val m = assertMatches(pattern, input)
    assertGroupsEquals(m, expectedGroups: _*)
  }

  @noinline
  private def assertGroupsEquals(m: Matcher, expectedGroups: String*): Matcher = {
    val groupCount = expectedGroups.size
    assertEquals(groupCount, m.groupCount())
    if (groupCount == 1) {
      // nicer error messages for a single group
      assertEquals(expectedGroups.head, m.group(1))
    } else {
      val actualGroups = (1 to groupCount).map(m.group(_)).toArray[AnyRef]
      assertArrayEquals(expectedGroups.toArray[AnyRef], actualGroups)
    }
    m
  }

  @noinline
  private def assertFind(pattern: Pattern, input: String, pos: Int): Matcher = {
    val matcher = pattern.matcher(input)
    if (!matcher.find()) {
      fail(
          s"expected ${patternToString(pattern)} to be found in " +
        s"'${debugEscape(input)}' at $pos, but was not found")
    } else if (matcher.start() != pos) {
      fail(
          s"expected ${patternToString(pattern)} to be found in " +
          s"'${debugEscape(input)}' at $pos, but was found at ${matcher.start()}")
    }
    matcher
  }

  @noinline
  private def assertFind(pattern: String, flags: Int, input: String, pos: Int): Matcher =
    assertFind(compile(pattern, flags), input, pos)

  @noinline
  private def assertFind(pattern: String, input: String, pos: Int): Matcher =
    assertFind(compile(pattern), input, pos)

  @noinline
  private def assertFind(pattern: Pattern, input: String, start: Int, end: Int): Matcher = {
    val matcher = pattern.matcher(input)
    if (!matcher.find()) {
      fail(
          s"expected ${patternToString(pattern)} to be found in " +
          s"'${debugEscape(input)}' at $start-$end, but was not found")
    } else if (matcher.start() != start || matcher.end() != end) {
      fail(
          s"expected ${patternToString(pattern)} to be found in " +
          s"'${debugEscape(input)}' at $start-$end, but was found at " +
          s"${matcher.start()}-${matcher.end()}")
    }
    matcher
  }

  @noinline
  private def assertFind(pattern: String, flags: Int, input: String, start: Int, end: Int): Matcher =
    assertFind(compile(pattern, flags), input, start, end)

  @noinline
  private def assertFind(pattern: String, input: String, start: Int, end: Int): Matcher =
    assertFind(compile(pattern), input, start, end)

  @noinline
  private def assertFindAll(pattern: Pattern, input: String, startPoses: Int*): Matcher = {
    val matcher = pattern.matcher(input)
    for (pos <- startPoses) {
      if (!matcher.find()) {
        fail(
            s"expected ${patternToString(pattern)} to be found in " +
            s"'${debugEscape(input)}' at $pos, but was not found")
      } else if (matcher.start() != pos) {
        fail(
            s"expected ${patternToString(pattern)} to be found in " +
            s"'${debugEscape(input)}' at $pos, but was found at ${matcher.start()}")
      }
    }
    if (matcher.find()) {
      fail(
          s"expected ${patternToString(pattern)} *not* to be found anymore in " +
          s"'${debugEscape(input)}', but was found at ${matcher.start()}")
    }
    matcher
  }

  @noinline
  private def assertFindAndGroupsEquals(pattern: Pattern, input: String,
      pos: Int, expectedGroups: String*): Matcher = {
    val m = assertFind(pattern, input, pos)
    assertGroupsEquals(m, expectedGroups: _*)
  }

  @noinline
  private def assertNotMatches(pattern: Pattern, input: String): Unit = {
    if (pattern.matcher(input).matches())
      fail(s"expected ${patternToString(pattern)} *not* to match '${debugEscape(input)}'")
  }

  @noinline
  private def assertNotMatches(pattern: String, flags: Int, input: String): Unit =
    assertNotMatches(compile(pattern, flags), input)

  @noinline
  private def assertNotMatches(pattern: String, input: String): Unit =
    assertNotMatches(compile(pattern), input)

  @noinline
  private def assertNotFind(pattern: Pattern, input: String): Unit = {
    val matcher = pattern.matcher(input)
    if (matcher.find()) {
      fail(
          s"expected ${patternToString(pattern)} *not* to be found in " +
          s"'${debugEscape(input)}', but was found at ${matcher.start()}")
    }
  }

  @noinline
  private def assertSyntaxError(pattern: String, flags: Int, desc: String, index: Int): Unit = {
    val th = assertThrows(classOf[PatternSyntaxException], compile(pattern, flags))
    if (!executingInJVM) {
      assertEquals(desc, th.getDescription())
      assertEquals(index, th.getIndex())
    }
  }

  @noinline
  private def assertSyntaxError(pattern: String, desc: String, index: Int): Unit =
    assertSyntaxError(pattern, 0, desc, index)

  @noinline
  private def assertSyntaxErrorInJS(pattern: String, flags: Int, desc: String, index: Int): Unit = {
    if (executingInJVM) {
      compile(pattern, flags)
    } else {
      val th = assertThrows(classOf[PatternSyntaxException], compile(pattern, flags))
      assertEquals(desc, th.getDescription())
      assertEquals(index, th.getIndex())
    }
  }

  @noinline
  private def assertSyntaxErrorInJS(pattern: String, desc: String, index: Int): Unit =
    assertSyntaxErrorInJS(pattern, 0, desc, index)

  // Common code points and chars that we use

  private final val GClef = "\uD834\uDD1E" // 𝄞 U+1D11E MUSICAL SYMBOL G CLEF
  private final val GClefHigh = "\uD834"
  private final val GClefLow = "\uDD1E"
  private final val EscapedGClefX = "\\x{1D11E}"
  private final val EscapedGClefU = "\\uD834\\uDD1E"
  private final val EscapedGClefHigh = "\\uD834"
  private final val EscapedGClefLow = "\\uDD1E"

  private final val Domino = "\uD83C\uDC3D" // 🀽 U+1F03D DOMINO TILE HORIZONTAL-01-05
  private final val DominoHigh = "\uD83C"
  private final val DominoLow = "\uDC3D"
  private final val EscapedDominoX = "\\x{1F03D}"
  private final val EscapedDominoU = "\\uD83C\\uDC3D"
  private final val EscapedDominoHigh = "\\uD83C"
  private final val EscapedDominoLow = "\\uDC3D"

  // Tests

  @Test def singleCodePoint(): Unit = {
    val a = compile("a")
    assertMatches(a, "a")
    assertNotMatches(a, "A")
    assertNotMatches(a, "b")
    assertNotMatches(a, "")
    assertNotMatches(a, "aa")
    assertFind(a, "aa", 0)
    assertFind(a, "bcadef", 2)
    assertFind(a, GClef + "a", 2) // Surrogate pair counts as 2 for indices
    assertNotFind(a, "bdcef")

    val highSurrogate = compile(GClefHigh)
    assertMatches(highSurrogate, GClefHigh)
    assertNotMatches(highSurrogate, GClef)
    assertNotFind(highSurrogate, GClef) // a surrogate pair in the input cannot be decomposed
    assertFind(highSurrogate, "a" + GClefHigh + "b", 1) // but can be found if not paired
    assertFind(highSurrogate, GClef + GClefHigh, 2)
    assertFind(highSurrogate, GClefHigh + DominoHigh, 0)

    val lowSurrogate = compile(GClefLow)
    assertMatches(lowSurrogate, GClefLow)
    assertNotMatches(lowSurrogate, GClef)
    assertNotFind(lowSurrogate, GClef) // a surrogate pair in the input cannot be decomposed
    assertFind(lowSurrogate, "a" + GClefLow + "b", 1) // but can be found if not paired
    assertFind(lowSurrogate, GClef + GClefLow, 2)
    assertFind(lowSurrogate, DominoLow + GClefLow, 1)

    val gClef = compile(GClef)
    assertMatches(gClef, GClef)
    assertNotMatches(gClef, "a")
    assertNotMatches(gClef, Domino)
    assertFind(gClef, "a" + GClef + "b", 1)
    assertNotFind(gClef, Domino)
    assertFind(gClef, DominoHigh + GClef + DominoLow, 1)

    // Characters for which we might think that they would need escaping, but they don't
    val weirdCharsString = "\u0000\t\r\n\f\u001f /-0]}\u0085\u2028\u2029#"
    assertMatches(weirdCharsString, weirdCharsString)
  }

  @Test def singleEscapedCodePoint(): Unit = {
    // Useless escape of ASCII char
    val quote = compile("\\'")
    assertMatches(quote, "'")
    assertNotMatches(quote, "a")

    // Escapes for syntax characters
    assertMatches("\\.", ".")
    assertNotMatches("\\.", "a")
    assertMatches("\\\\", "\\")
    assertMatches("\\\\t", "\\t")
    assertMatches("\\[a]", "[a]")
    assertMatches("a\\?", "a?")
    assertMatches("a\\*", "a*")
    assertMatches("a\\+", "a+")
    assertMatches("a\\{3}", "a{3}")
    assertMatches("\\(5\\)", "(5)")

    // Escapes for characters that are syntax characters only when using Comments (the escapes work regardless)
    assertMatches("\\ \\\t\\\n\\\u000B\\\f\\\r", " \t\n\u000B\f\r")
    assertMatches("\\ \\\t\\\n\\\u000B\\\f\\\r", Comments, " \t\n\u000B\f\r")
    assertMatches("\\#abc", "#abc")
    assertMatches("\\#abc", Comments, "#abc")

    // Letter escapes for special chars
    assertMatches("\\t", "\t")
    assertMatches("\\n", "\n")
    assertMatches("\\r", "\r")
    assertMatches("\\f", "\f")
    assertMatches("\\a", "\u0007")
    assertMatches("\\e", "\u001B")

    // Control escapes (meant to be used in the range '@' to '_' and with '?')
    assertMatches("\\c@", "\u0000")
    assertMatches("\\cA", "\u0001")
    assertNotMatches("\\cA", "A")
    assertMatches("\\cR", "\u0012")
    assertMatches("\\c_", "\u001F")
    assertMatches("\\c?", 0x007f.toChar.toString())

    /* More control escapes that are not really meant to be used.
     * In general, '\cx' means `x ^ 0x40`, as explained at
     * https://stackoverflow.com/questions/35208570/java-regular-expression-cx-control-characters
     */
    assertMatches("\\cb", 0x0022.toChar.toString())
    assertMatches("\\c" + GClefHigh, "" + (0xd834 ^ 0x40).toChar)
    assertMatches("\\c" + GClef, GClefHigh + (0xdd1e ^ 0x40).toChar)

    // Illegal trailing 'c' escape
    assertSyntaxError("\\c", "Illegal control escape sequence", 2)

    // Latin-1 'x' escapes
    assertMatches("\\x41", "A")
    assertMatches("\\xe9", "é")
    assertMatches("\\xE9", "é")

    assertSyntaxError("\\x", "Illegal hexadecimal escape sequence", 1)
    assertSyntaxError("a\\xZ0", "Illegal hexadecimal escape sequence", 2)
    assertSyntaxError("a\\x0z", "Illegal hexadecimal escape sequence", 2)
    assertSyntaxError("a\\x0/", "Illegal hexadecimal escape sequence", 2)

    // Surrogates with 'u' and 'x' escapes
    assertMatches(EscapedGClefHigh, GClefHigh)
    assertMatches(EscapedGClefLow, GClefLow)
    assertMatches(EscapedGClefU, GClef)
    assertMatches(EscapedGClefX, GClef)

    // A surrogate "pair" where one is literal and the other is escaped does *not* create a pair
    assertNotMatches(EscapedGClefHigh + GClefLow, GClef)
    assertNotMatches(GClefHigh + EscapedGClefLow, GClef)
    assertNotMatches(GClefHigh + "\\" + GClefLow, GClef)

    // A surrogate "pair" with 'x' escapes does *not* create a pair
    assertNotMatches("\\x{D834}\\x{DD1E}", GClef)

    // A bare escape in front of a surrogate pair considers it as a whole
    assertMatches("\\" + GClef, GClef)

    // Octal escapes
    assertMatches("\\0040", " ")
    assertMatches("\\004a", "\u0004a")
    assertMatches("\\008", "\u00008")
    assertMatches("\\0101", "A")
    assertMatches("\\00101", "\u00081") // at most 3 significant chars are considered
    assertMatches("\\0377", "\u00ff")
    assertMatches("\\0400", " 0") // if the first char is 4-7, only 2 chars are considered

    assertSyntaxError("\\0", "Illegal octal escape sequence", 1)
    assertSyntaxError("\\0a", "Illegal octal escape sequence", 1)
    assertSyntaxError("abc\\08", "Illegal octal escape sequence", 4)

    // Unknown ASCII letter escapes are reserved
    assertSyntaxError("\\g", "Illegal/unsupported escape sequence", 1)
    assertSyntaxError("\\Y", "Illegal/unsupported escape sequence", 1)
  }

  @Test def simpleAlternatives(): Unit = {
    val fooOrBar = compile("foo|bar")
    assertMatches(fooOrBar, "foo")
    assertMatches(fooOrBar, "bar")
    assertNotMatches(fooOrBar, "foobar")
    assertNotMatches(fooOrBar, "foox")
    assertNotMatches(fooOrBar, "xfoo")
    assertNotMatches(fooOrBar, "barx")
    assertNotMatches(fooOrBar, "xbar")
    assertFind(fooOrBar, "hello foo bar", 6)
    assertNotFind(fooOrBar, "hello")

    val aOrAB = compile("a|ab")
    assertMatches(aOrAB, "a")
    assertMatches(aOrAB, "ab")
    assertFind(aOrAB, "ab", 0, 1) // The left hand side is preferred if both would work

    val abOrA = compile("ab|a")
    assertMatches(abOrA, "a")
    assertMatches(abOrA, "ab")
    assertFind(abOrA, "ab", 0, 2) // The left hand side is preferred if both would work, even if it is longer
  }

  @Test def greedyQuantifiers(): Unit = {
    val starGreedy = compile("ba*")
    assertMatches(starGreedy, "b")
    assertMatches(starGreedy, "ba")
    assertMatches(starGreedy, "baaaaa")
    assertFind(starGreedy, "cbaaassefaa", 1, 5)
    assertFind(starGreedy, "cbsssefaaaaa", 1, 2)
    assertNotFind(starGreedy, "qsessqsssddff")

    val plusGreedy = compile("ba+")
    assertMatches(plusGreedy, "ba")
    assertMatches(plusGreedy, "baaaa")
    assertNotFind(plusGreedy, "b")
    assertFind(plusGreedy, "cbaaassefaa", 1, 5)
    assertFind(plusGreedy, "cbsssefbaaa", 7, 11)
    assertNotFind(plusGreedy, "qsebsqsbsddfb")

    val questionGreedy = compile("ba?")
    assertMatches(questionGreedy, "b")
    assertMatches(questionGreedy, "ba")
    assertNotMatches(questionGreedy, "baa")
    assertFind(questionGreedy, "cbaaassefaa", 1, 3)
    assertFind(questionGreedy, "cbssefbaaa", 1, 2)
    assertNotFind(questionGreedy, "qsessqsssddff")

    val repeatedSupplementaryCodePoint = compile("a\uD834\uDD1E*")
    assertFind(repeatedSupplementaryCodePoint, "bca\uD834\uDD1E\uD834\uDD1Edef", 2, 7)
    assertFind(repeatedSupplementaryCodePoint, "bca\uD834\uDD1E\uDD1Edef", 2, 5)

    // After quotes, a quantifier applies to the last code point (sic!)
    val repeatedQuote = compile("a\\Qbc\\d\\E*")
    assertFind(repeatedQuote, "aaabc\\dbc\\dbc", 2, 7)
    assertFind(repeatedQuote, "aaabc\\ddc", 2, 8)
    assertFind(repeatedQuote, "aaabc\\bc", 2, 6)

    val repeatedQuoteEndingWithSupplementaryCodePoint = compile("a\\Qbc\\\uD834\uDD1E\\E*")
    assertFind(repeatedQuoteEndingWithSupplementaryCodePoint, "aaabc\\\uD834\uDD1E\uD834\uDD1Ebc", 2, 10)
    assertFind(repeatedQuoteEndingWithSupplementaryCodePoint, "aaabc\\\uD834\uDD1Ebc\\\uD834\uDD1Ebc", 2, 8)
    assertFind(repeatedQuoteEndingWithSupplementaryCodePoint, "aaabc\\\uD834\uDD1E\uDD1Ebc", 2, 8)
  }

  @Test def lazyQuantifiers(): Unit = {
    val starLazy = compile("a[bc]*?b")
    assertMatches(starLazy, "ab")
    assertMatches(starLazy, "abbbb")
    assertMatches(starLazy, "abccbb")
    assertFind(starLazy, "abbb", 0, 2)
    assertFind(starLazy, "accbbbccb", 0, 4)
    assertNotFind(starLazy, "accc")

    val starLazyAtEnd = compile("ba*?")
    assertMatches(starLazyAtEnd, "b")
    assertMatches(starLazyAtEnd, "ba")
    assertMatches(starLazyAtEnd, "baaaaa")
    assertFind(starLazyAtEnd, "cbaaassefaa", 1, 2)
    assertFind(starLazyAtEnd, "cbsssefaaaaa", 1, 2)
    assertNotFind(starLazyAtEnd, "qsessqsssddff")

    val plusLazy = compile("a[bc]+?b")
    assertMatches(plusLazy, "abb")
    assertMatches(plusLazy, "acb")
    assertMatches(plusLazy, "abbbcccbb")
    assertFind(plusLazy, "abbb", 0, 3)
    assertFind(plusLazy, "accbbbccb", 0, 4)
    assertNotFind(plusLazy, "accc")
    assertNotFind(plusLazy, "ab")

    val plusLazyAtEnd = compile("ba+?")
    assertMatches(plusLazyAtEnd, "ba")
    assertMatches(plusLazyAtEnd, "baaaa")
    assertNotFind(plusLazyAtEnd, "b")
    assertFind(plusLazyAtEnd, "cbaaassefaa", 1, 3)
    assertFind(plusLazyAtEnd, "cbsssefbaaa", 7, 9)
    assertNotFind(plusLazyAtEnd, "qsebsqsbsddfb")

    val questionLazy = compile("a[bc]??b")
    assertMatches(questionLazy, "ab")
    assertMatches(questionLazy, "abb")
    assertMatches(questionLazy, "acb")
    assertFind(questionLazy, "abbb", 0, 2)
    assertFind(questionLazy, "acbbbccb", 0, 3)
    assertNotFind(questionLazy, "accbb")

    val questionLazyAtEnd = compile("ba??")
    assertMatches(questionLazyAtEnd, "b")
    assertMatches(questionLazyAtEnd, "ba")
    assertNotMatches(questionLazyAtEnd, "baa")
    assertFind(questionLazyAtEnd, "cbaaassefaa", 1, 2)
    assertFind(questionLazyAtEnd, "cbssefbaaa", 1, 2)
    assertNotFind(questionLazyAtEnd, "qsessqsssddff")
  }

  @Test def possessiveQuantifiers(): Unit = {
    val starPossessive = compile("ab*+[bd]")
    assertFind(starPossessive, " a  abbbb  abbbdba ", 11, 16)
    assertFind(starPossessive, " a  abbbb  adba ", 11, 13)
    assertNotFind(starPossessive, " a  abbbb dab ")

    val plusPossessive = compile("ab++[bd]")
    assertFind(plusPossessive, " a  abbbb  abbbdba ", 11, 16)
    assertFind(plusPossessive, " a  abbbb  adba  abdd ", 17, 20)
    assertNotFind(plusPossessive, " a  ad  abbbb dab adba ")

    val questionPossessive = compile("ab?+[bd]")
    assertFind(questionPossessive, " a  ab   abb abb ", 9, 12)
    assertFind(questionPossessive, " ad abb", 1, 3)
    assertFind(questionPossessive, " abd abb ", 1, 4)
    assertNotFind(questionPossessive, " a  ab ")

    // Don't break numbered groups before, within, and after
    val possessiveAndNumberedGroups = compile("(a)((bc)\\1\\3|(c))?+(c) \\1 \\2 \\3 \\5")
    assertFindAndGroupsEquals(possessiveAndNumberedGroups, "abcabcc a bcabc bc c", 0, "a", "bcabc", "bc", null, "c")

    // Don't break named groups before, within, and after
    val possessiveAndNamedGroups =
      compile("(?<A>a)(?<P>(?<B>bc)\\k<A>\\k<B>|(?<C>c))?+(?<D>c) \\k<A> \\k<P> \\k<B> \\k<D>")
    val m =
      assertFindAndGroupsEquals(possessiveAndNamedGroups, "abcabcc a bcabc bc c", 0, "a", "bcabc", "bc", null, "c")
    assertEquals("a", m.group("A"))
    assertEquals("bcabc", m.group("P"))
    assertEquals("bc", m.group("B"))
    assertEquals(null, m.group("C"))
    assertEquals("c", m.group("D"))
  }

  @Test def quantifiedAssertions_Issue4784(): Unit = {
    /* In ES RegExp's, Assertions cannot be quantified. We test here that our
     * compiler emits them in a way that satisfies the ES regex syntax.
     */

    val questionStart = compile("^?a")
    assertFind(questionStart, "ab", 0, 1)
    assertFind(questionStart, "ba", 1, 2)
    assertNotFind(questionStart, "bc")

    val plusStart = compile("^+a")
    assertFind(plusStart, "ab", 0, 1)
    assertNotFind(plusStart, "ba")
    assertNotFind(plusStart, "bc")

    val questionEnd = compile("a$?")
    assertFind(questionEnd, "ab", 0, 1)
    assertFind(questionEnd, "ba", 1, 2)
    assertNotFind(questionEnd, "bc")

    val plusEnd = compile("a$+")
    assertNotFind(plusEnd, "ab")
    assertFind(plusEnd, "ba", 1, 2)
    assertNotFind(plusEnd, "bc")

    val questionWordBoundary = compile("a\\b?")
    assertFind(questionWordBoundary, "ab", 0, 1)
    assertFind(questionWordBoundary, "ba", 1, 2)
    assertNotFind(questionWordBoundary, "bc")

    val plusWordBoundary = compile("a\\b+")
    assertNotFind(plusWordBoundary, "ab")
    assertFind(plusWordBoundary, "ba", 1, 2)
    assertNotFind(plusWordBoundary, "bc")

    val questionNotWordBoundary = compile("a\\B?")
    assertFind(questionNotWordBoundary, "ab", 0, 1)
    assertFind(questionNotWordBoundary, "ba", 1, 2)
    assertNotFind(questionNotWordBoundary, "bc")

    val plusNotWordBoundary = compile("a\\B+")
    assertFind(plusNotWordBoundary, "ab", 0, 1)
    assertNotFind(plusNotWordBoundary, "ba")
    assertNotFind(plusNotWordBoundary, "bc")

    val questionPosLookahead = compile("a(?=b)?")
    assertFind(questionPosLookahead, "ab", 0, 1)
    assertFind(questionPosLookahead, "ba", 1, 2)
    assertNotFind(questionPosLookahead, "bc")

    val plusPosLookahead = compile("a(?=b)+")
    assertFind(plusPosLookahead, "ab", 0, 1)
    assertNotFind(plusPosLookahead, "ba")
    assertNotFind(plusPosLookahead, "bc")

    val questionNegLookahead = compile("a(?!b)?")
    assertFind(questionNegLookahead, "ab", 0, 1)
    assertFind(questionNegLookahead, "ba", 1, 2)
    assertNotFind(questionNegLookahead, "bc")

    val plusNegLookahead = compile("a(?!b)+")
    assertNotFind(plusNegLookahead, "ab")
    assertFind(plusNegLookahead, "ba", 1, 2)
    assertNotFind(plusNegLookahead, "bc")
  }

  @Test def quantifiedLookBehindAssertions_Issue4784(): Unit = {
    assumeTrue("requires look-behinds", regexSupportsLookBehinds)

    val questionPosLookbehind = compile("(?<=b)?a")
    assertFind(questionPosLookbehind, "ab", 0, 1)
    assertFind(questionPosLookbehind, "ba", 1, 2)
    assertNotFind(questionPosLookbehind, "bc")

    val plusPosLookbehind = compile("(?<=b)+a")
    assertNotFind(plusPosLookbehind, "ab")
    assertFind(plusPosLookbehind, "ba", 1, 2)
    assertNotFind(plusPosLookbehind, "bc")

    val questionNegLookbehind = compile("(?<!b)?a")
    assertFind(questionNegLookbehind, "ab", 0, 1)
    assertFind(questionNegLookbehind, "ba", 1, 2)
    assertNotFind(questionNegLookbehind, "bc")

    val plusNegLookbehind = compile("(?<!b)+a")
    assertFind(plusNegLookbehind, "ab", 0, 1)
    assertNotFind(plusNegLookbehind, "ba")
    assertNotFind(plusNegLookbehind, "bc")
  }

  @Test def quotes(): Unit = {
    val str = "D($[^e" + DominoHigh + "\\R)]" + GClef + DominoLow
    val quoted = compile("a\\Q" + str + "\\Ec")
    assertMatches(quoted, "a" + str + "c")
    assertNotFind(quoted, "A" + str.toUpperCase() + "c")

    val caseInsensitive = compile("a\\Q" + str + "\\Ec", CaseInsensitive)
    assertMatches(caseInsensitive, "A" + str.toUpperCase() + "c")

    // #1677
    assertMatches("^\\Qmember\\E.*\\Q\\E$", "member0")
  }

  @Test def literal(): Unit = {
    val str = "aD($[^e" + DominoHigh + "\\R)]" + GClef + DominoLow + "c"
    val quoted = compile(str, Literal)
    assertMatches(quoted, str)
    assertNotFind(quoted, str.toUpperCase())

    val caseInsensitive = compile(str, Literal | CaseInsensitive)
    assertMatches(caseInsensitive, str.toUpperCase())
  }

  @Test def dot(): Unit = {
    val dot = compile(".")

    assertMatches(dot, "a")
    assertMatches(dot, "0")
    assertMatches(dot, "\u0000")
    assertMatches(dot, GClefHigh)
    assertMatches(dot, GClefLow)
    assertMatches(dot, GClef) // . matches an entire surrogate pair

    assertNotFind(dot, "\n")
    assertNotFind(dot, "\r")
    assertNotFind(dot, "\u0085") // Would be matched by a '.' in a JS RegExp
    assertNotFind(dot, "\u2028")
    assertNotFind(dot, "\u2029")

    assertNotFind(dot, "\r\n")

    val dotUnixLines = compile(".", UnixLines)

    assertMatches(dotUnixLines, "a")
    assertMatches(dotUnixLines, "0")
    assertMatches(dotUnixLines, "\u0000")
    assertMatches(dotUnixLines, GClefHigh)
    assertMatches(dotUnixLines, GClefLow)
    assertMatches(dotUnixLines, GClef) // . matches an entire surrogate pair

    assertNotFind(dotUnixLines, "\n")
    assertMatches(dotUnixLines, "\r")
    assertMatches(dotUnixLines, "\u0085")
    assertMatches(dotUnixLines, "\u2028")
    assertMatches(dotUnixLines, "\u2029")

    assertNotMatches(dotUnixLines, "\r\n")
    assertFind(dotUnixLines, "\r\n", 0, 1)

    val dotAll = compile(".", DotAll)

    assertMatches(dotAll, "a")
    assertMatches(dotAll, "0")
    assertMatches(dotAll, "\u0000")
    assertMatches(dotAll, GClefHigh)
    assertMatches(dotAll, GClefLow)
    assertMatches(dotAll, GClef) // . matches an entire surrogate pair

    assertMatches(dotAll, "\n")
    assertMatches(dotAll, "\r")
    assertMatches(dotAll, "\u0085")
    assertMatches(dotAll, "\u2028")
    assertMatches(dotAll, "\u2029")

    assertNotMatches(dotAll, "\r\n")
    assertFind(dotAll, "\r\n", 0, 1)

    val dotAllUnixLines = compile(".", DotAll | UnixLines)

    assertMatches(dotAllUnixLines, "a")
    assertMatches(dotAllUnixLines, "0")
    assertMatches(dotAllUnixLines, "\u0000")
    assertMatches(dotAllUnixLines, GClefHigh)
    assertMatches(dotAllUnixLines, GClefLow)
    assertMatches(dotAllUnixLines, GClef) // . matches an entire surrogate pair

    assertMatches(dotAllUnixLines, "\n")
    assertMatches(dotAllUnixLines, "\r")
    assertMatches(dotAllUnixLines, "\u0085")
    assertMatches(dotAllUnixLines, "\u2028")
    assertMatches(dotAllUnixLines, "\u2029")

    assertNotMatches(dotAllUnixLines, "\r\n")
    assertFind(dotAllUnixLines, "\r\n", 0, 1)

    // Test case for #1847, and for the (?s) leading flag
    val codeMatcher = Pattern.compile("(?s).*<code>(.*?)</code>.*")
    assertMatches(codeMatcher, "<p><code>\nList(1)\n</code></p>")
  }

  @Test def startAndEnd(): Unit = {
    val startA = compile("^a")
    assertMatches(startA, "a")
    assertFind(startA, "ab", 0)
    assertNotFind(startA, "ba")
    assertNotFind(startA, "b\na")

    val forceStartA = compile("\\Aa")
    assertMatches(forceStartA, "a")
    assertFind(forceStartA, "ab", 0)
    assertNotFind(forceStartA, "ba")
    assertNotFind(forceStartA, "b\na")

    val endA = compile("a$")
    assertMatches(endA, "a")
    assertFind(endA, "ba", 1)
    assertNotFind(endA, "ab")
    assertNotFind(endA, "a\nb")

    val forceEndA = compile("a\\z")
    assertMatches(forceEndA, "a")
    assertFind(forceEndA, "ba", 1)
    assertNotFind(forceEndA, "ab")
    assertNotFind(forceEndA, "a\nb")
  }

  @Test def startAndEndMultiline(): Unit = {
    assumeTrue("requires look-behinds", regexSupportsLookBehinds)

    val startA = compile("^a", Multiline)
    assertMatches(startA, "a")
    assertFind(startA, "ab", 0)
    assertNotFind(startA, "ba")
    assertFind(startA, "b\na", 2)
    assertFind(startA, "b\u0085a", 2)
    assertFind(startA, "b\u2028a", 2)
    assertFind(startA, "b\u2029a", 2)
    assertFind(startA, "b\ra", 2)
    assertFind(startA, "b\r\na", 3)

    val forceStartA = compile("\\Aa", Multiline)
    assertMatches(forceStartA, "a")
    assertFind(forceStartA, "ab", 0)
    assertNotFind(forceStartA, "ba")
    assertNotFind(forceStartA, "b\na")

    // Between \r and \n is not the start of a line
    val startNL = compile("^\n", Multiline)
    assertFind(startNL, "a\n\nb", 2)
    assertNotFind(startNL, "a\r\nb")

    val endA = compile("a$", Multiline)
    assertMatches(endA, "a")
    assertFind(endA, "ba", 1)
    assertNotFind(endA, "ab")
    assertFind(endA, "a\nb", 0)
    assertFind(endA, "a\u0085b", 0)
    assertFind(endA, "a\u2028b", 0)
    assertFind(endA, "a\u2029b", 0)
    assertFind(endA, "a\rb", 0)
    assertFind(endA, "a\r\nb", 0)

    val forceEndA = compile("a\\z", Multiline)
    assertMatches(forceEndA, "a")
    assertFind(forceEndA, "ba", 1)
    assertNotFind(forceEndA, "ab")
    assertNotFind(forceEndA, "a\nb")

    // Between \r and \n is not the end of a line
    val endCR = compile("\r$", Multiline)
    assertFind(endCR, "a\r\rb", 1)
    assertNotFind(endCR, "a\r\nb")
  }

  @Test def sticky(): Unit = {
    val stickyFoo = compile("\\G(?:foo|bar)")
    assertFindAll(stickyFoo, "foofoobarfoo foo bar", 0, 3, 6, 9)
    assertMatches(stickyFoo, "foo")
    assertMatches(stickyFoo, "bar")
  }

  @Test def comments(): Unit = {
    val lotsOfComments = compile(
        "  \ta  # a comment is interrupted by \r" +
        "b      # or \n" +
        "c      # or \u0085" +
        "  d    # or \u2028" +
        "      e# or \u2029" +
        " f     # but the latter 3 are not ignored! \n" +
        " g     # in addition, \n" +
        "\t \u000B \f \r \n h # are also ignored \r" +
        "i",
        Comments)

    assertMatches(lotsOfComments, "abc\u0085d\u2028e\u2029fghi")

    val commentsInCharClass = compile(
        "[\n" +
        "  A-Z  # an uppercase letter\n" +
        "  _ \t # or an underscore\n" +
        "  f  - # gosh, we can even have ranges\n" +
        "    j  # split by comments!\n" +
        "]",
        Comments)

    assertMatches(commentsInCharClass, "A")
    assertMatches(commentsInCharClass, "F")
    assertMatches(commentsInCharClass, "R")
    assertMatches(commentsInCharClass, "Z")
    assertMatches(commentsInCharClass, "f")
    assertMatches(commentsInCharClass, "g")
    assertMatches(commentsInCharClass, "h")
    assertMatches(commentsInCharClass, "i")
    assertMatches(commentsInCharClass, "j")
    assertMatches(commentsInCharClass, "_")
    assertNotFind(commentsInCharClass, " ")
    assertNotFind(commentsInCharClass, "\t")
    assertNotFind(commentsInCharClass, "\n")
    assertNotFind(commentsInCharClass, "#")
    assertNotFind(commentsInCharClass, "-")
    assertNotFind(commentsInCharClass, "!")
    assertNotFind(commentsInCharClass, "a")
    assertNotFind(commentsInCharClass, "e")
    assertNotFind(commentsInCharClass, "k")
    assertNotFind(commentsInCharClass, "l")
    assertNotFind(commentsInCharClass, "z")

    val fakeRangeWithComments = compile("[A-D G # comment\n -]", Comments)
    assertMatches(fakeRangeWithComments, "A")
    assertMatches(fakeRangeWithComments, "C")
    assertMatches(fakeRangeWithComments, "D")
    assertMatches(fakeRangeWithComments, "G")
    assertMatches(fakeRangeWithComments, "-")
    assertNotMatches(fakeRangeWithComments, "E")
    assertNotMatches(fakeRangeWithComments, "I")
    assertNotMatches(fakeRangeWithComments, "e")
    assertNotMatches(fakeRangeWithComments, "]")
    assertNotMatches(fakeRangeWithComments, " ")
    assertNotMatches(fakeRangeWithComments, "\n")
    assertNotMatches(fakeRangeWithComments, "#")

    /* If there is a comment between the '-' and the ']', the JVM does not
     * detect that it is a fake range, and reports a syntax error. Our
     * implementation correctly detects that case, because it was easier than
     * not detecting it.
     */
    if (executingInJVM) {
      assertSyntaxError("[A-D G - ]", Comments, "irrelevant", 0)
      assertSyntaxError("[A-D G -# comment\n]", Comments, "irrelevant", 0)
    } else {
      val fakeRangeWithCommentsOnRHS = compile("[A-D G - # comment\n ]", Comments)
      assertMatches(fakeRangeWithCommentsOnRHS, "A")
      assertMatches(fakeRangeWithCommentsOnRHS, "C")
      assertMatches(fakeRangeWithCommentsOnRHS, "D")
      assertMatches(fakeRangeWithCommentsOnRHS, "G")
      assertMatches(fakeRangeWithCommentsOnRHS, "-")
      assertNotMatches(fakeRangeWithCommentsOnRHS, "E")
      assertNotMatches(fakeRangeWithCommentsOnRHS, "I")
      assertNotMatches(fakeRangeWithCommentsOnRHS, "e")
      assertNotMatches(fakeRangeWithCommentsOnRHS, "]")
      assertNotMatches(fakeRangeWithCommentsOnRHS, " ")
      assertNotMatches(fakeRangeWithCommentsOnRHS, "\n")
      assertNotMatches(fakeRangeWithCommentsOnRHS, "#")
    }

    // We can still match against whitespace in the input
    assertMatches("\ta\\ b\t", Comments, "a b")
    assertMatches("\ta.b\t", Comments, "a b")
    assertMatches("\ta[\\ c]b\t", Comments, "a b")

    // We can still match against '#' in the input
    assertMatches("\ta\\#b\t", Comments, "a#b")
    assertMatches("\ta.b\t", Comments, "a#b")
    assertMatches("\ta[\\#c]b\t", Comments, "a#b")
  }

  @Test def predefinedCharacterClasses(): Unit = {
    val digit = compile("\\d")
    assertMatches(digit, "0")
    assertMatches(digit, "7")
    assertMatches(digit, "9")
    assertNotMatches(digit, "/")
    assertNotMatches(digit, ":")
    assertNotMatches(digit, "A")
    assertNotMatches(digit, "१") // U+0967 DEVANAGARI DIGIT ONE

    val notDigit = compile("\\D")
    assertNotMatches(notDigit, "0")
    assertNotMatches(notDigit, "7")
    assertNotMatches(notDigit, "9")
    assertMatches(notDigit, "/")
    assertMatches(notDigit, ":")
    assertMatches(notDigit, "A")
    assertMatches(notDigit, "१") // U+0967 DEVANAGARI DIGIT ONE

    val h = compile("\\h")
    assertMatches(h, " ")
    assertMatches(h, "\t")
    assertMatches(h, "\u00A0")
    assertMatches(h, "\u1680")
    assertMatches(h, "\u180E")
    assertMatches(h, "\u2000")
    assertMatches(h, "\u2008")
    assertMatches(h, "\u200A")
    assertMatches(h, "\u202F")
    assertMatches(h, "\u205F")
    assertMatches(h, "\u3000")
    assertNotMatches(h, "\n")
    assertNotMatches(h, "\r")
    assertNotMatches(h, "a")
    assertNotMatches(h, "\u0085")
    assertNotMatches(h, "\u1FFF")
    assertNotMatches(h, "\u200B")
    assertNotMatches(h, "\u2028")
    assertNotMatches(h, "\u2029")
    assertNotMatches(h, "\u3001")
    assertNotMatches(h, GClefHigh)
    assertNotMatches(h, GClefLow)
    assertNotMatches(h, GClef)

    val noth = compile("\\H")
    assertNotMatches(noth, " ")
    assertNotMatches(noth, "\t")
    assertNotMatches(noth, "\u00A0")
    assertNotMatches(noth, "\u1680")
    assertNotMatches(noth, "\u180E")
    assertNotMatches(noth, "\u2000")
    assertNotMatches(noth, "\u2008")
    assertNotMatches(noth, "\u200A")
    assertNotMatches(noth, "\u202F")
    assertNotMatches(noth, "\u205F")
    assertNotMatches(noth, "\u3000")
    assertMatches(noth, "\n")
    assertMatches(noth, "\r")
    assertMatches(noth, "a")
    assertMatches(noth, "\u0085")
    assertMatches(noth, "\u1FFF")
    assertMatches(noth, "\u200B")
    assertMatches(noth, "\u2028")
    assertMatches(noth, "\u2029")
    assertMatches(noth, "\u3001")
    assertMatches(noth, GClefHigh)
    assertMatches(noth, GClefLow)
    assertMatches(noth, GClef)

    val s = compile("\\s")
    assertMatches(s, "\t")
    assertMatches(s, "\n")
    assertMatches(s, "\u000B")
    assertMatches(s, "\u000C")
    assertMatches(s, "\r")
    assertMatches(s, " ")
    assertNotMatches(s, "\u0008")
    assertNotMatches(s, "\u000E")
    assertNotMatches(s, "\u00A0") // #3959 would be matched by JS' \s
    assertNotMatches(s, "\uFEFF") // would be matched by JS' \s
    assertNotMatches(s, "\u2008")
    assertNotMatches(s, "\u2028")
    assertNotMatches(s, GClefHigh)
    assertNotMatches(s, GClefLow)
    assertNotMatches(s, GClef)

    val nots = compile("\\S")
    assertNotMatches(nots, "\t")
    assertNotMatches(nots, "\n")
    assertNotMatches(nots, "\u000B")
    assertNotMatches(nots, "\u000C")
    assertNotMatches(nots, "\r")
    assertNotMatches(nots, " ")
    assertMatches(nots, "\u0008")
    assertMatches(nots, "\u000E")
    assertMatches(nots, "\u00A0") // #3959 would not be matched by JS' \S
    assertMatches(nots, "\uFEFF") // would not be matched by JS' \S
    assertMatches(nots, "\u2008")
    assertMatches(nots, "\u2028")
    assertMatches(nots, GClefHigh)
    assertMatches(nots, GClefLow)
    assertMatches(nots, GClef)

    val w = compile("\\w")
    assertMatches(w, "a")
    assertMatches(w, "X")
    assertMatches(w, "5")
    assertMatches(w, "_")
    assertNotMatches(w, "?")
    assertNotMatches(w, "é")
    assertNotFind(w, GClef)

    val notw = compile("\\W")
    assertNotMatches(notw, "a")
    assertNotMatches(notw, "X")
    assertNotMatches(notw, "5")
    assertNotMatches(notw, "_")
    assertMatches(notw, "?")
    assertMatches(notw, "é")
    assertMatches(notw, GClef)

    val lower = compile("\\p{Lower}")
    assertMatches(lower, "a")
    assertMatches(lower, "f")
    assertMatches(lower, "z")
    assertNotMatches(lower, "A")
    assertNotMatches(lower, "G")
    assertNotMatches(lower, "0")
    assertNotMatches(lower, "-")
    assertNotMatches(lower, "à")
    assertNotMatches(lower, "É")

    // https://bugs.openjdk.java.net/browse/JDK-8214245
    if (!executingInJVMOnLowerThanJDK(15)) {
      val lowerCI = compile("\\p{Lower}", CaseInsensitive)
      assertMatches(lowerCI, "a")
      assertMatches(lowerCI, "f")
      assertMatches(lowerCI, "z")
      assertMatches(lowerCI, "A")
      assertMatches(lowerCI, "G")
      assertNotMatches(lowerCI, "0")
      assertNotMatches(lowerCI, "-")
      assertNotMatches(lowerCI, "à")
      assertNotMatches(lowerCI, "É")
    }

    val punct = compile("\\p{Punct}")
    assertMatches(punct, ":")
    assertMatches(punct, "!")
    assertMatches(punct, "*")
    assertMatches(punct, "/")
    assertMatches(punct, "?")
    assertMatches(punct, "}")
    assertNotMatches(punct, "a")
    assertNotMatches(punct, "\n")
    assertNotMatches(punct, "5")
    assertNotFind(punct, GClef)
    assertNotFind(punct, "\u0E4F") // ๏ Thai Character Fongman
    assertNotFind(punct, "\uD804\uDC4D") // 𑁍 Brahmi Punctuation Lotus

    val notPunct = compile("\\P{Punct}")
    assertNotMatches(notPunct, ":")
    assertNotMatches(notPunct, "!")
    assertNotMatches(notPunct, "*")
    assertNotMatches(notPunct, "/")
    assertNotMatches(notPunct, "?")
    assertNotMatches(notPunct, "}")
    assertMatches(notPunct, "a")
    assertMatches(notPunct, "\n")
    assertMatches(notPunct, "5")
    assertMatches(notPunct, GClef)
    assertMatches(notPunct, "\u0E4F") // ๏ Thai Character Fongman
    assertMatches(notPunct, "\uD804\uDC4D") // 𑁍 Brahmi Punctuation Lotus
  }

  @Test def predefinedUnicodeCharacterClasses(): Unit = {
    assumeTrue("requires \\p{} support", regexSupportsUnicodeCharacterClasses)

    val digit = compile("\\d", UnicodeCharacterClass)
    assertMatches(digit, "0")
    assertMatches(digit, "7")
    assertMatches(digit, "9")
    assertMatches(digit, "१") // U+0967 DEVANAGARI DIGIT ONE
    assertNotMatches(digit, "/")
    assertNotMatches(digit, ":")
    assertNotMatches(digit, "A")
    assertNotMatches(digit, GClef)

    val notDigit = compile("\\D", UnicodeCharacterClass)
    assertNotMatches(notDigit, "0")
    assertNotMatches(notDigit, "7")
    assertNotMatches(notDigit, "9")
    assertNotMatches(notDigit, "१") // U+0967 DEVANAGARI DIGIT ONE
    assertMatches(notDigit, "/")
    assertMatches(notDigit, ":")
    assertMatches(notDigit, "A")
    assertMatches(notDigit, GClef)

    val h = compile("\\h", UnicodeCharacterClass)
    assertMatches(h, " ")
    assertMatches(h, "\t")
    assertMatches(h, "\u00A0")
    assertMatches(h, "\u1680")
    assertMatches(h, "\u180E")
    assertMatches(h, "\u2000")
    assertMatches(h, "\u2008")
    assertMatches(h, "\u200A")
    assertMatches(h, "\u202F")
    assertMatches(h, "\u205F")
    assertMatches(h, "\u3000")
    assertNotMatches(h, "\n")
    assertNotMatches(h, "\r")
    assertNotMatches(h, "a")
    assertNotMatches(h, "\u0085")
    assertNotMatches(h, "\u1FFF")
    assertNotMatches(h, "\u200B")
    assertNotMatches(h, "\u2028")
    assertNotMatches(h, "\u2029")
    assertNotMatches(h, "\u3001")
    assertNotMatches(h, GClefHigh)
    assertNotMatches(h, GClefLow)
    assertNotMatches(h, GClef)

    val noth = compile("\\H", UnicodeCharacterClass)
    assertNotMatches(noth, " ")
    assertNotMatches(noth, "\t")
    assertNotMatches(noth, "\u00A0")
    assertNotMatches(noth, "\u1680")
    assertNotMatches(noth, "\u180E")
    assertNotMatches(noth, "\u2000")
    assertNotMatches(noth, "\u2008")
    assertNotMatches(noth, "\u200A")
    assertNotMatches(noth, "\u202F")
    assertNotMatches(noth, "\u205F")
    assertNotMatches(noth, "\u3000")
    assertMatches(noth, "\n")
    assertMatches(noth, "\r")
    assertMatches(noth, "a")
    assertMatches(noth, "\u0085")
    assertMatches(noth, "\u1FFF")
    assertMatches(noth, "\u200B")
    assertMatches(noth, "\u2028")
    assertMatches(noth, "\u2029")
    assertMatches(noth, "\u3001")
    assertMatches(noth, GClefHigh)
    assertMatches(noth, GClefLow)
    assertMatches(noth, GClef)

    val s = compile("\\s", UnicodeCharacterClass)
    assertMatches(s, "\t")
    assertMatches(s, "\n")
    assertMatches(s, "\u000B")
    assertMatches(s, "\u000C")
    assertMatches(s, "\r")
    assertMatches(s, " ")
    assertMatches(s, "\u00A0")
    assertMatches(s, "\u2008")
    assertMatches(s, "\u2028")
    assertNotMatches(s, "\u0008")
    assertNotMatches(s, "\u000E")
    assertNotMatches(s, "\uFEFF") // would be matched by JS' \s
    assertNotMatches(s, GClefHigh)
    assertNotMatches(s, GClefLow)
    assertNotMatches(s, GClef)

    val nots = compile("\\S", UnicodeCharacterClass)
    assertNotMatches(nots, "\t")
    assertNotMatches(nots, "\n")
    assertNotMatches(nots, "\u000B")
    assertNotMatches(nots, "\u000C")
    assertNotMatches(nots, "\r")
    assertNotMatches(nots, " ")
    assertNotMatches(nots, "\u00A0")
    assertNotMatches(nots, "\u2008")
    assertNotMatches(nots, "\u2028")
    assertMatches(nots, "\u0008")
    assertMatches(nots, "\u000E")
    assertMatches(nots, "\uFEFF") // would not be matched by JS' \S
    assertMatches(nots, GClefHigh)
    assertMatches(nots, GClefLow)
    assertMatches(nots, GClef)

    val w = compile("\\w", UnicodeCharacterClass)
    assertMatches(w, "a")
    assertMatches(w, "X")
    assertMatches(w, "5")
    assertMatches(w, "_")
    assertMatches(w, "é")
    assertMatches(w, "é")
    assertMatches(w, "\u03B1") // α U+03B1 Greek Small Letter Alpha
    assertMatches(w, "१") // U+0967 DEVANAGARI DIGIT ONE
    assertMatches(w, "\uD835\uDC1D") // 𝐝 U+1D41D Mathematical Bold Small D
    assertNotMatches(w, "?")
    assertNotFind(w, GClef)

    val notw = compile("\\W", UnicodeCharacterClass)
    assertNotFind(notw, "a")
    assertNotFind(notw, "X")
    assertNotFind(notw, "5")
    assertNotFind(notw, "_")
    assertNotFind(notw, "é")
    assertNotFind(notw, "é")
    assertNotFind(notw, "\u03B1") // α U+03B1 Greek Small Letter Alpha
    assertNotFind(notw, "१") // U+0967 DEVANAGARI DIGIT ONE
    assertNotMatches(notw, "\uD835\uDC1D") // 𝐝 U+1D41D Mathematical Bold Small D
    if (!executingInJVM) // on JDK 8, the JVM finds the low surrogate at pos 1 (not reproducible on JDK 17 ea)
      assertNotFind(notw, "\uD835\uDC1D") // 𝐝 U+1D41D Mathematical Bold Small D
    assertMatches(notw, "?")
    assertMatches(notw, GClef)

    val lower = compile("\\p{Lower}", UnicodeCharacterClass)
    assertMatches(lower, "a")
    assertMatches(lower, "f")
    assertMatches(lower, "z")
    assertMatches(lower, "à")
    assertNotMatches(lower, "A")
    assertNotMatches(lower, "G")
    assertNotMatches(lower, "0")
    assertNotMatches(lower, "-")
    assertNotMatches(lower, "É")

    // https://bugs.openjdk.java.net/browse/JDK-8214245
    if (!executingInJVMOnLowerThanJDK(15)) {
      val lowerCI = compile("\\p{Lower}", CaseInsensitive | UnicodeCharacterClass)
      assertMatches(lowerCI, "a")
      assertMatches(lowerCI, "f")
      assertMatches(lowerCI, "z")
      assertMatches(lowerCI, "à")
      assertMatches(lowerCI, "A")
      assertMatches(lowerCI, "G")
      assertMatches(lowerCI, "É")
      assertNotMatches(lowerCI, "0")
      assertNotMatches(lowerCI, "-")
    }

    val punct = compile("\\p{Punct}", UnicodeCharacterClass)
    assertMatches(punct, ":")
    assertMatches(punct, "!")
    assertMatches(punct, "*")
    assertMatches(punct, "/")
    assertMatches(punct, "?")
    assertMatches(punct, "}")
    assertMatches(punct, "\u0E4F") // ๏ Thai Character Fongman
    assertMatches(punct, "\uD804\uDC4D") // 𑁍 Brahmi Punctuation Lotus
    assertNotMatches(punct, "a")
    assertNotMatches(punct, "\n")
    assertNotMatches(punct, "5")
    assertNotFind(punct, GClef)

    val notPunct = compile("\\P{Punct}", UnicodeCharacterClass)
    assertNotMatches(notPunct, ":")
    assertNotMatches(notPunct, "!")
    assertNotMatches(notPunct, "*")
    assertNotMatches(notPunct, "/")
    assertNotMatches(notPunct, "?")
    assertNotMatches(notPunct, "}")
    assertNotMatches(notPunct, "\u0E4F") // ๏ Thai Character Fongman
    assertNotMatches(notPunct, "\uD804\uDC4D") // 𑁍 Brahmi Punctuation Lotus
    assertMatches(notPunct, "a")
    assertMatches(notPunct, "\n")
    assertMatches(notPunct, "5")
    assertMatches(notPunct, GClef)
  }

  @Test def javaCharacterClasses(): Unit = {
    assumeTrue("requires \\p{} support", regexSupportsUnicodeCharacterClasses)

    val javaMirrored = compile("\\p{javaMirrored}")
    assertMatches(javaMirrored, "(")
    assertMatches(javaMirrored, "]")
    assertMatches(javaMirrored, "\u2993") // ⦓ LEFT ARC LESS-THAN BRACKET
    assertNotMatches(javaMirrored, "+")
    assertNotMatches(javaMirrored, "À")
    assertNotMatches(javaMirrored, "_")

    val javaUnicodeIdentifierStart = compile("\\p{javaUnicodeIdentifierStart}")
    assertMatches(javaUnicodeIdentifierStart, "A")
    assertMatches(javaUnicodeIdentifierStart, "É")
    assertMatches(javaUnicodeIdentifierStart, "オ")
    assertNotMatches(javaUnicodeIdentifierStart, "0")
    assertNotMatches(javaUnicodeIdentifierStart, "_")
    assertNotMatches(javaUnicodeIdentifierStart, "+")

    val javaUnicodeIdentifierPart = compile("\\p{javaUnicodeIdentifierPart}")
    assertMatches(javaUnicodeIdentifierPart, "A")
    assertMatches(javaUnicodeIdentifierPart, "É")
    assertMatches(javaUnicodeIdentifierPart, "オ")
    assertMatches(javaUnicodeIdentifierPart, "0")
    assertMatches(javaUnicodeIdentifierPart, "_")
    assertNotMatches(javaUnicodeIdentifierPart, "+")

    /* Other javaX character classes are exhaustively tested in
     * unicodeCharClassesAreConsistentWithTheirDefinitions().
     */
  }

  @Test def asciiCharClassesAreConsistentWithTheirDefinitions(): Unit = {
    @noinline
    def checkConsistency(name: String, definition: String): Unit = {
      val namePattern = Pattern.compile(s"\\p{$name}")
      val definitionPattern = Pattern.compile(definition)

      for (cp <- 0 to 0xff) {
        val cpString = cp.toChar.toString()
        val nameMatches = namePattern.matcher(cpString).matches()
        val definitionMatches = definitionPattern.matcher(cpString).matches()
        if (nameMatches != definitionMatches) {
          fail(
              s"for U+${Integer.toHexString(cp)}, " +
              s"\\p{$name} was $nameMatches but its definition was $definitionMatches")
        }
      }
    }

    checkConsistency("Lower", "[a-z]")
    checkConsistency("Upper", "[A-Z]")
    checkConsistency("ASCII", "[\\x00-\\x7F]")
    checkConsistency("Alpha", "[\\p{Lower}\\p{Upper}]")
    checkConsistency("Digit", "[0-9]")
    checkConsistency("Alnum", "[\\p{Alpha}\\p{Digit}]")
    checkConsistency("Punct", "[!\"#$%&'()*+,\\-./:;<=>?@\\[\\\\\\]^_`{|}~]")
    checkConsistency("Graph", "[\\p{Alnum}\\p{Punct}]")
    checkConsistency("Print", "[\\p{Graph}\\x20]")
    checkConsistency("Blank", "[ \\t]")
    checkConsistency("Cntrl", "[\\x00-\\x1F\\x7F]")
    checkConsistency("XDigit", "[0-9a-fA-F]")
    checkConsistency("Space", "[ \\t\\n\\x0B\\f\\r]")
  }

  /** Tests that the pre-computed desugarings of the Unicode character classes
   *  actually match the original definitions.
   *
   *  This is particularly important for `Print`, `Blank` and the
   *  `java.lang.Character` classes, which have complex definitions that we
   *  heavily simplified by hand to remove the intersections, even using inside
   *  knowledge of the Unicode data.
   *
   *  This test is expensive. It takes 15 seconds on my (sjrd) machine.
   *  However, since it only runs in ES 2018+ anyway, it is unlikely to slow
   *  down a typical workflow, and I believe it is worth it.
   */
  @Test def unicodeCharClassesAreConsistentWithTheirDefinitions(): Unit = {
    assumeTrue("requires \\p{} support", regexSupportsUnicodeCharacterClasses)

    @noinline
    def checkConsistency(name: String, definition: String): Unit = {
      val namePattern = Pattern.compile(s"\\p{$name}", UnicodeCharacterClass)
      val definitionPattern = Pattern.compile(definition, UnicodeCharacterClass)

      // Allocate this once and for all, which shaves off 20% running time for this test
      val buffer = new Array[Int](1)

      // Explicitly use a var and while loop so that even without optimizer, this test remains viable
      var cp = 0
      while (cp <= Character.MAX_CODE_POINT) {
        buffer(0) = cp
        val cpString = new String(buffer, 0, 1) // TODO Use Character.toString(Int) when we can (JDK11+)
        val nameMatches = namePattern.matcher(cpString).matches()
        val definitionMatches = definitionPattern.matcher(cpString).matches()
        if (nameMatches != definitionMatches) {
          fail(
              s"for U+${Integer.toHexString(cp)}, " +
              s"\\p{$name} was $nameMatches but its definition was $definitionMatches")
        }
        cp += 1
      }
    }

    checkConsistency("Lower", "\\p{IsLowercase}")
    checkConsistency("Upper", "\\p{IsUppercase}")
    checkConsistency("ASCII", "[\\u0000-\\u007f]")
    checkConsistency("Alpha", "\\p{IsAlphabetic}")
    checkConsistency("Digit", "\\p{IsDigit}")
    checkConsistency("Alnum", "[\\p{IsAlphabetic}\\p{IsDigit}]")
    checkConsistency("Punct", "\\p{IsPunctuation}")
    checkConsistency("Graph", "[^\\p{IsWhite_Space}\\p{gc=Cc}\\p{gc=Cs}\\p{gc=Cn}]")
    checkConsistency("Print", "[\\p{Graph}\\p{Blank}&&[^\\p{Cntrl}]]")
    checkConsistency("Blank", "[\\p{IsWhite_Space}&&[^\\p{gc=Zl}\\p{gc=Zp}\\x0a\\x0b\\x0c\\x0d\\x85]]")
    checkConsistency("Cntrl", "\\p{gc=Cc}")
    checkConsistency("XDigit", "[\\p{gc=Nd}\\p{IsHex_Digit}]")
    checkConsistency("Space", "\\p{IsWhite_Space}")

    checkConsistency("javaAlphabetic", "\\p{IsAlphabetic}")
    checkConsistency("javaDefined", "\\P{Cn}")
    checkConsistency("javaDigit", "\\p{Nd}")
    checkConsistency("javaIdentifierIgnorable", "[\u0000-\u0008\u000E-\u001B\u007F-\u009F\\p{Cf}]")
    checkConsistency("javaIdeographic", "\\p{IsIdeographic}")
    checkConsistency("javaISOControl", "[\u0000-\u001F\u007F-\u009F]")
    if (!executingInJVMOnLowerThanJDK(9)) {
      checkConsistency("javaJavaIdentifierPart",
          "[\\p{L}\\p{Sc}\\p{Pc}\\p{Nd}\\p{Nl}\\p{Mn}\\p{Mc}\u0000-\u0008\u000E-\u001B\u007F-\u009F\\p{Cf}]")
      checkConsistency("javaJavaIdentifierStart", "[\\p{L}\\p{Sc}\\p{Pc}\\p{Nl}]")
    }
    checkConsistency("javaLetterOrDigit", "[\\p{L}\\p{Nd}]")
    checkConsistency("javaLowerCase", "\\p{IsLowercase}")
    checkConsistency("javaSpaceChar", "\\p{Z}")
    checkConsistency("javaTitleCase", "\\p{Lt}")
    checkConsistency("javaUpperCase", "\\p{IsUppercase}")
    checkConsistency("javaWhitespace", "[\t-\r\u001C-\u001F\\p{Z}&&[^\u00A0\u2007\u202F]]")

    /* The last 3 are not testable using this approach, because there is no
     * support in `j.u.r.Pattern` for `Bidi_Mirrored`, `ID_Start` and
     * `ID_Continue`.
    checkConsistency("javaMirrored", "\\p{IsBidi_Mirrored}")
    checkConsistency("javaUnicodeIdentifierPart",
        "[\\p{IsID_Continue}\u2E2F\u0000-\u0008\u000E-\u001B\u007F-\u009F\\p{Cf}]")
    checkConsistency("javaUnicodeIdentifierStart", "[\\p{IsID_Start}\u2E2F]")
     */
  }

  @Test def unicodeProperties(): Unit = {
    assumeTrue("requires \\p{} support", regexSupportsUnicodeCharacterClasses)

    for (prop <- List("L", "gc=L", "IsL", "general_category=L")) { // #2028
      val letter = compile(s"\\p{$prop}")
      assertMatches(letter, "A")
      assertMatches(letter, "à")
      assertMatches(letter, "ç")
      assertMatches(letter, "か")
      assertMatches(letter, "\u03B1") // α U+03B1 Greek Small Letter Alpha
      assertMatches(letter, "\uD801\uDC93") // 𐒓 U+10493 Osmanya Letter Waw
      assertNotMatches(letter, ".")
      assertNotMatches(letter, "-")
      assertNotMatches(letter, "3")
      assertNotMatches(letter, "\u2030") // ‰ U+2030 Per Mille Sign
      assertNotMatches(letter, GClef)
      assertNotMatches(letter, GClefHigh)
      assertNotMatches(letter, GClefLow)
    }

    val hiragana = compile("\\p{IsHiragana}")
    assertMatches(hiragana, "ぁ")
    assertMatches(hiragana, "う")
    assertMatches(hiragana, "か")
    assertMatches(hiragana, "が")
    assertNotMatches(hiragana, "a")
    assertNotMatches(hiragana, "0")
    assertNotMatches(hiragana, "オ")
    assertNotMatches(hiragana, "今")

    val notHiragana = compile("\\P{sc=hiRAgana}")
    assertNotMatches(notHiragana, "ぁ")
    assertNotMatches(notHiragana, "う")
    assertNotMatches(notHiragana, "か")
    assertNotMatches(notHiragana, "が")
    assertMatches(notHiragana, "a")
    assertMatches(notHiragana, "0")
    assertMatches(notHiragana, "オ")
    assertMatches(notHiragana, "今")

    // Test the normalization of script names
    val olChiki = compile("\\p{script=ol_cHIki}")
    assertMatches(olChiki, "\u1C5A") // ᱚ U+1C5A OL CHIKI LETTER LA
    assertNotMatches(olChiki, "A")

    /* SignWriting is special because of its canonical name, which is not Sign_Writing.
     * It's from Unicode 8.0.0, so it requires JDK 9+.
     */
    if (!executingInJVMOnLowerThanJDK(9)) {
      val signWriting = compile("\\p{script=signwrItIng}")
      assertMatches(signWriting, "\uD836\uDC36") // U+1D836 SIGNWRITING HAND-FIST MIDDLE THUMB CUPPED INDEX UP
      assertNotMatches(signWriting, "A")
    }

    // Non existing script names are rejected
    assertSyntaxError("\\p{sc=FooBar}", "Unknown character script name {FooBar}", 12)
    assertSyntaxError("\\p{script=ba_bar}", "Unknown character script name {ba_bar}", 16)
    assertSyntaxError("\\p{IsFrobber}", "Unknown character script name {Frobber}", 12)
  }

  @Test def simpleCharacterClasses(): Unit = {
    val adf = compile("[adf]")
    assertMatches(adf, "a")
    assertMatches(adf, "d")
    assertMatches(adf, "f")
    assertNotMatches(adf, "b")
    assertNotMatches(adf, "A")
    assertNotMatches(adf, GClefHigh)
    assertNotMatches(adf, GClefLow)
    assertNotMatches(adf, GClef)

    val not_adf = compile("[^adf]")
    assertNotMatches(not_adf, "a")
    assertNotMatches(not_adf, "d")
    assertNotMatches(not_adf, "f")
    assertMatches(not_adf, "b")
    assertMatches(not_adf, "A")
    assertMatches(not_adf, GClefHigh)
    assertMatches(not_adf, GClefLow)
    assertMatches(not_adf, GClef)

    val letter = compile("[a-zA-Z]")
    assertMatches(letter, "a")
    assertMatches(letter, "z")
    assertMatches(letter, "A")
    assertMatches(letter, "Z")
    assertMatches(letter, "d")
    assertMatches(letter, "G")
    assertNotMatches(letter, "0")
    assertNotMatches(letter, "@")
    assertNotMatches(letter, "_")
    assertNotMatches(letter, GClefHigh)
    assertNotMatches(letter, GClefLow)
    assertNotMatches(letter, GClef)

    val notLetter = compile("[^a-zA-Z]")
    assertNotMatches(notLetter, "a")
    assertNotMatches(notLetter, "z")
    assertNotMatches(notLetter, "A")
    assertNotMatches(notLetter, "Z")
    assertNotMatches(notLetter, "d")
    assertNotMatches(notLetter, "G")
    assertMatches(notLetter, "0")
    assertMatches(notLetter, "@")
    assertMatches(notLetter, "_")
    assertMatches(notLetter, GClefHigh)
    assertMatches(notLetter, GClefLow)
    assertMatches(notLetter, GClef)

    // Supplementary code points count as a single unit
    val letterOrGClef = compile("[a-z" + GClef + "A-Z]")
    assertMatches(letterOrGClef, "a")
    assertMatches(letterOrGClef, "z")
    assertMatches(letterOrGClef, "A")
    assertMatches(letterOrGClef, "Z")
    assertMatches(letterOrGClef, "d")
    assertMatches(letterOrGClef, "G")
    assertMatches(letterOrGClef, GClef)
    assertNotMatches(letterOrGClef, "0")
    assertNotMatches(letterOrGClef, "@")
    assertNotMatches(letterOrGClef, "_")
    assertNotMatches(letterOrGClef, GClefHigh)
    assertNotMatches(letterOrGClef, GClefLow)

    // ^ Supplementary code points count as a single unit
    val notLetterNorGClef = compile("[^a-z" + GClef + "A-Z]")
    assertNotMatches(notLetterNorGClef, "a")
    assertNotMatches(notLetterNorGClef, "z")
    assertNotMatches(notLetterNorGClef, "A")
    assertNotMatches(notLetterNorGClef, "Z")
    assertNotMatches(notLetterNorGClef, "d")
    assertNotMatches(notLetterNorGClef, "G")
    assertNotMatches(notLetterNorGClef, GClef)
    assertMatches(notLetterNorGClef, "0")
    assertMatches(notLetterNorGClef, "@")
    assertMatches(notLetterNorGClef, "_")
    assertMatches(notLetterNorGClef, GClefHigh)
    assertMatches(notLetterNorGClef, GClefLow)

    // A surrogate "pair" where one is literal and the other is escaped does *not* create a pair
    val gClefHighOrLow1 = compile("[" + EscapedGClefHigh + GClefLow + "]")
    assertNotMatches(gClefHighOrLow1, GClef)
    assertMatches(gClefHighOrLow1, GClefHigh)
    assertMatches(gClefHighOrLow1, GClefLow)
    val gClefHighOrLow2 = compile("[" + GClefHigh + EscapedGClefLow + "]")
    assertNotMatches(gClefHighOrLow2, GClef)
    assertMatches(gClefHighOrLow2, GClefHigh)
    assertMatches(gClefHighOrLow2, GClefLow)
    val gClefHighOrSomeLows = compile("[" + EscapedGClefHigh + "\uDD1E-\uDD24" + "]")
    assertNotMatches(gClefHighOrSomeLows, GClef)
    assertMatches(gClefHighOrSomeLows, GClefHigh)
    assertMatches(gClefHighOrSomeLows, GClefLow)
    assertMatches(gClefHighOrSomeLows, "\uDD22")
    assertMatches(gClefHighOrSomeLows, "\uDD24")
    assertNotMatches(gClefHighOrSomeLows, "\uDD25")

    // A surrogate "pair" with 'x' escapes does *not* create a pair
    val gClefHighOrLow3 = compile("[\\x{D834}\\x{DD1E}]")
    assertNotMatches(gClefHighOrLow3, GClef)
    assertMatches(gClefHighOrLow3, GClefHigh)
    assertMatches(gClefHighOrLow3, GClefLow)

    // Range of supplementary code points with the same high surrogate
    val clefs = compile("[" + GClef + "-\\x{1D124}]")
    assertMatches(clefs, GClef)
    assertMatches(clefs, "\uD834\uDD24") // 𝄤 U+1D124 Musical Symbol F Clef Ottava Bassa
    assertMatches(clefs, "\uD834\uDD22") // 𝄢 Musical Symbol F Clef
    assertNotMatches(clefs, "a")
    assertNotMatches(clefs, GClefHigh)
    assertNotMatches(clefs, GClefHigh + "a")
    assertNotMatches(clefs, "\uD834\uDD10") // 𝄐 MUSICAL SYMBOL FERMATA
    assertNotMatches(clefs, GClefLow)
    assertNotMatches(clefs, "a" + GClefLow)

    // ^ Range of supplementary code points with the same high surrogate
    val notClefs = compile("[^" + GClef + "-\\x{1D124}]")
    assertNotMatches(notClefs, GClef)
    assertNotMatches(notClefs, "\uD834\uDD24") // 𝄤 U+1D124 Musical Symbol F Clef Ottava Bassa
    assertNotMatches(notClefs, "\uD834\uDD22") // 𝄢 Musical Symbol F Clef
    assertMatches(notClefs, "a")
    assertMatches(notClefs, GClefHigh)
    assertFind(notClefs, GClefHigh + "a", 0)
    assertMatches(notClefs, "\uD834\uDD10") // 𝄐 MUSICAL SYMBOL FERMATA
    assertMatches(notClefs, GClefLow)
    assertFind(notClefs, "a" + GClefLow, 0)

    // Range of supplementary code points spanning several high surrogates
    val supplementaryRange = compile("[" + GClef + "-\\x{1F914}]") // 🤔 U+1F914 THINKING FACE
    assertMatches(supplementaryRange, GClef)
    assertMatches(supplementaryRange, "\uD834\uDD22") // 𝄢 Musical Symbol F Clef
    assertMatches(supplementaryRange, "\uD83C\uDC3D") // 🀽 U+1F03D DOMINO TILE HORIZONTAL-01-05
    assertMatches(supplementaryRange, "\uD83E\uDD0F") // 🤏 U+1F90F PINCHING HAND
    assertMatches(supplementaryRange, "\uD83E\uDD14") // 🤔 U+1F914 THINKING FACE
    assertNotMatches(supplementaryRange, "a")
    assertNotMatches(supplementaryRange, GClefHigh)
    assertNotMatches(supplementaryRange, GClefHigh + "a")
    assertNotMatches(supplementaryRange, "\uD834\uDD10") // 𝄐 MUSICAL SYMBOL FERMATA
    assertNotMatches(supplementaryRange, "\uD83E\uDD26") // 🤦 U+1F926 FACE PALM
    assertNotMatches(supplementaryRange, GClefLow)
    assertNotMatches(supplementaryRange, "a" + GClefLow)

    // ^ Range of supplementary code points spanning several high surrogates
    val notSupplementaryRange = compile("[^" + GClef + "-\\x{1F914}]") // 🤔 U+1F914 THINKING FACE
    assertNotMatches(notSupplementaryRange, GClef)
    assertNotMatches(notSupplementaryRange, "\uD834\uDD22") // 𝄢 Musical Symbol F Clef
    assertNotMatches(notSupplementaryRange, "\uD83C\uDC3D") // 🀽 U+1F03D DOMINO TILE HORIZONTAL-01-05
    assertNotMatches(notSupplementaryRange, "\uD83E\uDD0F") // 🤏 U+1F90F PINCHING HAND
    assertNotMatches(notSupplementaryRange, "\uD83E\uDD14") // 🤔 U+1F914 THINKING FACE
    assertMatches(notSupplementaryRange, "a")
    assertMatches(notSupplementaryRange, GClefHigh)
    assertFind(notSupplementaryRange, GClefHigh + "a", 0)
    assertMatches(notSupplementaryRange, "\uD834\uDD10") // 𝄐 MUSICAL SYMBOL FERMATA
    assertMatches(notSupplementaryRange, "\uD83E\uDD26") // 🤦 U+1F926 FACE PALM
    assertMatches(notSupplementaryRange, GClefLow)
    assertFind(notSupplementaryRange, "a" + GClefLow, 0)

    // Pseudo-surrogate pairs do not collapse as a single code point
    val letterOrGClefComponents = compile("[a-z" + GClefHigh + EscapedGClefLow + "A-Z]")
    assertMatches(letterOrGClefComponents, "a")
    assertMatches(letterOrGClefComponents, "z")
    assertMatches(letterOrGClefComponents, "A")
    assertMatches(letterOrGClefComponents, "Z")
    assertMatches(letterOrGClefComponents, "d")
    assertMatches(letterOrGClefComponents, "G")
    assertNotMatches(letterOrGClefComponents, "0")
    assertNotMatches(letterOrGClefComponents, "@")
    assertNotMatches(letterOrGClefComponents, "_")
    assertNotMatches(letterOrGClefComponents, GClef)
    assertMatches(letterOrGClefComponents, GClefHigh)
    assertMatches(letterOrGClefComponents, GClefLow)

    // ^ Pseudo-surrogate pairs do not collapse as a single code point
    val notLetterNorGClefComponents = compile("[^a-z" + GClefHigh + EscapedGClefLow + "A-Z]")
    assertNotMatches(notLetterNorGClefComponents, "a")
    assertNotMatches(notLetterNorGClefComponents, "z")
    assertNotMatches(notLetterNorGClefComponents, "A")
    assertNotMatches(notLetterNorGClefComponents, "Z")
    assertNotMatches(notLetterNorGClefComponents, "d")
    assertNotMatches(notLetterNorGClefComponents, "G")
    assertMatches(notLetterNorGClefComponents, "0")
    assertMatches(notLetterNorGClefComponents, "@")
    assertMatches(notLetterNorGClefComponents, "_")
    assertMatches(notLetterNorGClefComponents, GClef)
    assertNotMatches(notLetterNorGClefComponents, GClefHigh)
    assertNotMatches(notLetterNorGClefComponents, GClefLow)
  }

  @Test def characterClassesWithPredefinedCharacterClasses(): Unit = {
    val digitOrF = compile("[\\dF]")
    assertMatches(digitOrF, "0")
    assertMatches(digitOrF, "7")
    assertMatches(digitOrF, "F")
    assertNotMatches(digitOrF, "E")
    assertNotMatches(digitOrF, "@")
    assertNotMatches(digitOrF, GClefHigh)
    assertNotMatches(digitOrF, GClefLow)
    assertNotMatches(digitOrF, GClef)

    val notDigitNorF = compile("[^\\dF]")
    assertNotMatches(notDigitNorF, "0")
    assertNotMatches(notDigitNorF, "7")
    assertNotMatches(notDigitNorF, "F")
    assertMatches(notDigitNorF, "E")
    assertMatches(notDigitNorF, "@")
    assertMatches(notDigitNorF, GClefHigh)
    assertMatches(notDigitNorF, GClefLow)
    assertMatches(notDigitNorF, GClef)

    val notDigitOrElse5 = compile("[\\D5]")
    assertNotMatches(notDigitOrElse5, "0")
    assertNotMatches(notDigitOrElse5, "7")
    assertMatches(notDigitOrElse5, "5")
    assertMatches(notDigitOrElse5, "F")
    assertMatches(notDigitOrElse5, "@")
    assertMatches(notDigitOrElse5, GClefHigh)
    assertMatches(notDigitOrElse5, GClefLow)
    assertMatches(notDigitOrElse5, GClef)

    val notNotDigitOrElse5 = compile("[^\\D5]")
    assertMatches(notNotDigitOrElse5, "0")
    assertMatches(notNotDigitOrElse5, "7")
    assertNotMatches(notNotDigitOrElse5, "5")
    assertNotMatches(notNotDigitOrElse5, "F")
    assertNotMatches(notNotDigitOrElse5, "@")
    assertNotMatches(notNotDigitOrElse5, GClefHigh)
    assertNotMatches(notNotDigitOrElse5, GClefLow)
    assertNotMatches(notNotDigitOrElse5, GClef)

    val whitespaceOrF = compile("[\\sF]")
    assertMatches(whitespaceOrF, "\t")
    assertMatches(whitespaceOrF, "\r")
    assertMatches(whitespaceOrF, "\n")
    assertMatches(whitespaceOrF, " ")
    assertMatches(whitespaceOrF, "F")
    assertNotMatches(whitespaceOrF, "E")
    assertNotMatches(whitespaceOrF, "\u0008")
    assertNotMatches(whitespaceOrF, "\u0011")
    assertNotMatches(whitespaceOrF, GClefHigh)
    assertNotMatches(whitespaceOrF, GClefLow)
    assertNotMatches(whitespaceOrF, GClef)

    val notWhitespaceNorF = compile("[^\\sF]")
    assertNotMatches(notWhitespaceNorF, "\t")
    assertNotMatches(notWhitespaceNorF, "\r")
    assertNotMatches(notWhitespaceNorF, "\n")
    assertNotMatches(notWhitespaceNorF, " ")
    assertNotMatches(notWhitespaceNorF, "F")
    assertMatches(notWhitespaceNorF, "E")
    assertMatches(notWhitespaceNorF, "\u0008")
    assertMatches(notWhitespaceNorF, "\u0011")
    assertMatches(notWhitespaceNorF, GClefHigh)
    assertMatches(notWhitespaceNorF, GClefLow)
    assertMatches(notWhitespaceNorF, GClef)

    val notWhitespaceOrElseNL = compile("[\\S\\n]")
    assertNotMatches(notWhitespaceOrElseNL, "\t")
    assertNotMatches(notWhitespaceOrElseNL, "\r")
    assertNotMatches(notWhitespaceOrElseNL, " ")
    assertMatches(notWhitespaceOrElseNL, "\n")
    assertMatches(notWhitespaceOrElseNL, "F")
    assertMatches(notWhitespaceOrElseNL, "\u0008")
    assertMatches(notWhitespaceOrElseNL, "\u0011")
    assertMatches(notWhitespaceOrElseNL, GClefHigh)
    assertMatches(notWhitespaceOrElseNL, GClefLow)
    assertMatches(notWhitespaceOrElseNL, GClef)

    val notNotWhitespaceOrElseNL = compile("[^\\S\\n]")
    assertMatches(notNotWhitespaceOrElseNL, "\t")
    assertMatches(notNotWhitespaceOrElseNL, "\r")
    assertMatches(notNotWhitespaceOrElseNL, " ")
    assertNotMatches(notNotWhitespaceOrElseNL, "\n")
    assertNotMatches(notNotWhitespaceOrElseNL, "F")
    assertNotMatches(notNotWhitespaceOrElseNL, "\u0008")
    assertNotMatches(notNotWhitespaceOrElseNL, "\u0011")
    assertNotMatches(notNotWhitespaceOrElseNL, GClefHigh)
    assertNotMatches(notNotWhitespaceOrElseNL, GClefLow)
    assertNotMatches(notNotWhitespaceOrElseNL, GClef)
  }

  @Test def complexCharacterClasses(): Unit = {
    val ad_or_mp = compile("[a-d[m-p]]")
    assertMatches(ad_or_mp, "a")
    assertMatches(ad_or_mp, "c")
    assertMatches(ad_or_mp, "d")
    assertMatches(ad_or_mp, "m")
    assertMatches(ad_or_mp, "n")
    assertMatches(ad_or_mp, "p")
    assertNotMatches(ad_or_mp, "e")
    assertNotMatches(ad_or_mp, "A")
    assertNotMatches(ad_or_mp, "N")

    val an_and_ks = compile("[a-n&&k-s]")
    assertMatches(an_and_ks, "k")
    assertMatches(an_and_ks, "m")
    assertMatches(an_and_ks, "n")
    assertNotMatches(an_and_ks, "0")
    assertNotMatches(an_and_ks, "e")
    assertNotMatches(an_and_ks, "j")
    assertNotMatches(an_and_ks, "o")
    assertNotMatches(an_and_ks, "z")
    assertNotMatches(an_and_ks, "A")
    assertNotMatches(an_and_ks, "N")

    val az_butNot_dfh = compile("[a-z&&[^dfh]]")
    assertMatches(az_butNot_dfh, "a")
    assertMatches(az_butNot_dfh, "c")
    assertMatches(az_butNot_dfh, "e")
    assertMatches(az_butNot_dfh, "i")
    assertMatches(az_butNot_dfh, "r")
    assertNotMatches(az_butNot_dfh, "d")
    assertNotMatches(az_butNot_dfh, "f")
    assertNotMatches(az_butNot_dfh, "h")
    assertNotMatches(az_butNot_dfh, "A")
    assertNotMatches(az_butNot_dfh, "0")
    assertNotMatches(az_butNot_dfh, "\n")

    val az_butNot_mp = compile("[a-z&&[^m-p]]")
    assertMatches(az_butNot_mp, "a")
    assertMatches(az_butNot_mp, "e")
    assertMatches(az_butNot_mp, "l")
    assertMatches(az_butNot_mp, "q")
    assertMatches(az_butNot_mp, "t")
    assertNotMatches(az_butNot_mp, "m")
    assertNotMatches(az_butNot_mp, "n")
    assertNotMatches(az_butNot_mp, "o")
    assertNotMatches(az_butNot_mp, "p")
    assertNotMatches(az_butNot_mp, "E")
    assertNotMatches(az_butNot_mp, "0")
    assertNotMatches(az_butNot_mp, "\n")

    val id = compile("([\\w&&[\\D]][\\w]*)")
    assertMatches(id, "foo")
    assertMatches(id, "foo56")
    assertMatches(id, "foo56bar")
    assertMatches(id, "_1")
    assertMatches(id, "foo_bar")
    assertMatches(id, "HELLO")
    assertNotMatches(id, "0")
    assertNotMatches(id, "0a")
    assertNotMatches(id, "01")
    assertNotMatches(id, "foo-bar")
    assertNotMatches(id, "foo bar")
    assertNotMatches(id, "!Foo")
    assertNotMatches(id, "  Foo")
    assertNotMatches(id, "Foo  ")

    val complexUnionsAndIntersections = compile("[d-l[o-t].-?&&f[k-q] -Z&&1-3\\D]")
    assertMatches(complexUnionsAndIntersections, ".")
    assertMatches(complexUnionsAndIntersections, "/")
    assertMatches(complexUnionsAndIntersections, "1")
    assertMatches(complexUnionsAndIntersections, "3")
    assertMatches(complexUnionsAndIntersections, "=")
    assertMatches(complexUnionsAndIntersections, "?")
    assertMatches(complexUnionsAndIntersections, "f")
    assertMatches(complexUnionsAndIntersections, "k")
    assertMatches(complexUnionsAndIntersections, "l")
    assertMatches(complexUnionsAndIntersections, "o")
    assertMatches(complexUnionsAndIntersections, "q")
    assertNotMatches(complexUnionsAndIntersections, "!")
    assertNotMatches(complexUnionsAndIntersections, "0")
    assertNotMatches(complexUnionsAndIntersections, "5")
    assertNotMatches(complexUnionsAndIntersections, "@")
    assertNotMatches(complexUnionsAndIntersections, "F")
    assertNotMatches(complexUnionsAndIntersections, "a")
    assertNotMatches(complexUnionsAndIntersections, "e")
    assertNotMatches(complexUnionsAndIntersections, "g")
    assertNotMatches(complexUnionsAndIntersections, "j")
    assertNotMatches(complexUnionsAndIntersections, "m")
    assertNotMatches(complexUnionsAndIntersections, "n")
    assertNotMatches(complexUnionsAndIntersections, "r")
    assertNotMatches(complexUnionsAndIntersections, "t")
    assertNotMatches(complexUnionsAndIntersections, "u")
    assertNotMatches(complexUnionsAndIntersections, "z")

    // https://bugs.openjdk.java.net/browse/JDK-8216391
    if (!executingInJVMOnLowerThanJDK(9)) {
      val not_ad_or_mp = compile("[^a-d[m-p]]")
      assertNotMatches(not_ad_or_mp, "a")
      assertNotMatches(not_ad_or_mp, "c")
      assertNotMatches(not_ad_or_mp, "d")
      assertNotMatches(not_ad_or_mp, "m")
      assertNotMatches(not_ad_or_mp, "n")
      assertNotMatches(not_ad_or_mp, "p")
      assertMatches(not_ad_or_mp, "e")
      assertMatches(not_ad_or_mp, "A")
      assertMatches(not_ad_or_mp, "N")

      val not_an_and_ks = compile("[^a-n&&k-s]")
      assertNotMatches(not_an_and_ks, "k")
      assertNotMatches(not_an_and_ks, "m")
      assertNotMatches(not_an_and_ks, "n")
      assertMatches(not_an_and_ks, "0")
      assertMatches(not_an_and_ks, "e")
      assertMatches(not_an_and_ks, "j")
      assertMatches(not_an_and_ks, "o")
      assertMatches(not_an_and_ks, "z")
      assertMatches(not_an_and_ks, "A")
      assertMatches(not_an_and_ks, "N")

      val notComplexUnionsAndIntersections = compile("[^d-l[o-t].-?&&f[k-q] -Z&&1-3\\D]")
      assertNotMatches(notComplexUnionsAndIntersections, ".")
      assertNotMatches(notComplexUnionsAndIntersections, "/")
      assertNotMatches(notComplexUnionsAndIntersections, "1")
      assertNotMatches(notComplexUnionsAndIntersections, "3")
      assertNotMatches(notComplexUnionsAndIntersections, "=")
      assertNotMatches(notComplexUnionsAndIntersections, "?")
      assertNotMatches(notComplexUnionsAndIntersections, "f")
      assertNotMatches(notComplexUnionsAndIntersections, "k")
      assertNotMatches(notComplexUnionsAndIntersections, "l")
      assertNotMatches(notComplexUnionsAndIntersections, "o")
      assertNotMatches(notComplexUnionsAndIntersections, "q")
      assertMatches(notComplexUnionsAndIntersections, "!")
      assertMatches(notComplexUnionsAndIntersections, "0")
      assertMatches(notComplexUnionsAndIntersections, "5")
      assertMatches(notComplexUnionsAndIntersections, "@")
      assertMatches(notComplexUnionsAndIntersections, "F")
      assertMatches(notComplexUnionsAndIntersections, "a")
      assertMatches(notComplexUnionsAndIntersections, "e")
      assertMatches(notComplexUnionsAndIntersections, "g")
      assertMatches(notComplexUnionsAndIntersections, "j")
      assertMatches(notComplexUnionsAndIntersections, "m")
      assertMatches(notComplexUnionsAndIntersections, "n")
      assertMatches(notComplexUnionsAndIntersections, "r")
      assertMatches(notComplexUnionsAndIntersections, "t")
      assertMatches(notComplexUnionsAndIntersections, "u")
      assertMatches(notComplexUnionsAndIntersections, "z")
    }
  }

  @Test def complexUnicodeCharacterClasses(): Unit = {
    assumeTrue("requires \\p{} support", regexSupportsUnicodeCharacterClasses)

    val letterNotUpperNorLower = compile("[\\p{L}&&[^\\p{Lu}\\p{Ll}]]")
    assertMatches(letterNotUpperNorLower, "か")
    assertMatches(letterNotUpperNorLower, "\u01F2") // U+01F2 ǲ Latin Capital Letter D with Small Letter Z
    assertMatches(letterNotUpperNorLower, "今")
    assertNotMatches(letterNotUpperNorLower, "e")
    assertNotMatches(letterNotUpperNorLower, "A")
    assertNotMatches(letterNotUpperNorLower, "N")
    assertNotMatches(letterNotUpperNorLower, "À")
    assertNotMatches(letterNotUpperNorLower, "0")
  }

  @Test def characterClassWithQuote(): Unit = {
    val cc1 = compile("[a\\Q[]\\(t-z" + GClef + "\\Ed]")
    assertMatches(cc1, "a")
    assertMatches(cc1, "[")
    assertMatches(cc1, "]")
    assertMatches(cc1, "\\")
    assertMatches(cc1, "(")
    assertMatches(cc1, "t")
    assertMatches(cc1, "-")
    assertMatches(cc1, "z")
    assertMatches(cc1, GClef)
    assertMatches(cc1, "d")
    assertNotMatches(cc1, "A")
    assertNotMatches(cc1, "b")
    assertNotMatches(cc1, "Q")
    assertNotMatches(cc1, "E")
    assertNotMatches(cc1, "T")
    assertNotMatches(cc1, "u") // between 't' and 'z'
    assertNotMatches(cc1, GClefHigh)
    assertNotMatches(cc1, GClefLow)

    val cc1CaseInsensitive = compile("[a\\Q[]\\(t-z" + GClef + "\\Ed]", CaseInsensitive)
    assertMatches(cc1CaseInsensitive, "a")
    assertMatches(cc1CaseInsensitive, "A")
    assertMatches(cc1CaseInsensitive, "[")
    assertMatches(cc1CaseInsensitive, "]")
    assertMatches(cc1CaseInsensitive, "\\")
    assertMatches(cc1CaseInsensitive, "(")
    assertMatches(cc1CaseInsensitive, "t")
    assertMatches(cc1CaseInsensitive, "T")
    assertMatches(cc1CaseInsensitive, "-")
    assertMatches(cc1CaseInsensitive, "z")
    assertMatches(cc1CaseInsensitive, GClef)
    assertMatches(cc1CaseInsensitive, "d")
    assertNotMatches(cc1CaseInsensitive, "b")
    assertNotMatches(cc1CaseInsensitive, "Q")
    assertNotMatches(cc1CaseInsensitive, "E")
    assertNotMatches(cc1CaseInsensitive, "u") // between 't' and 'z'
    assertNotMatches(cc1CaseInsensitive, GClefHigh)
    assertNotMatches(cc1CaseInsensitive, GClefLow)
  }

  @Test def positiveLookAheadDoesNotBacktrack(): Unit = {
    val m = assertFind("(?=(a+))a*b\\1", "baaabac", 3)
    assertEquals("aba", m.group())
    assertEquals("a", m.group(1))
  }

  @Test def asciiCaseInsensitive(): Unit = {
    val s = compile("s", CaseInsensitive)
    assertMatches(s, "s")
    assertMatches(s, "S")
    assertNotMatches(s, "\u017F") // ſ LATIN SMALL LETTER LONG S
    assertNotMatches(s, "t")
  }

  @Test def unicodeCaseInsensitive(): Unit = {
    assumeTrue("requires 'u' flag support", regexSupportsUnicodeCase)

    val s = compile("s", CaseInsensitive | UnicodeCase)
    assertMatches(s, "s")
    assertMatches(s, "S")
    assertMatches(s, "\u017F") // ſ LATIN SMALL LETTER LONG S; 017F folds to 's'
    assertNotMatches(s, "t")

    val ranges = compile("[g-l\uFB00\u0175-\u0182\u0540-\u0550\u1F68-\u1F8E\u1FAA-\u1FAF\u2126]",
        CaseInsensitive | UnicodeCase)
    // g-l
    assertMatches(ranges, "H")
    assertMatches(ranges, "\u212A") // K KELVIN SIGN, folds to 'k'
    // FB00
    assertMatches(ranges, "\uFB00") // ﬀ LATIN SMALL LIGATURE FF
    // 0175-0182 (contains 017F which folds to 's')
    if (!executingInJVM) {
      // https://bugs.openjdk.org/browse/JDK-8360459
      assertMatches(ranges, "s")
      assertMatches(ranges, "S")
    }
    assertMatches(ranges, "\u017F")
    assertMatches(ranges, "\u0180") // in range; does not participate in case folding
    // 0540-0550
    assertMatches(ranges, "\u0547") // in range
    assertMatches(ranges, "\u0577") // 0547 folds to 0577
    // 1F68-1F8E
    assertMatches(ranges, "\u1F65") // 1F6D folds to 1F65
    assertMatches(ranges, "\u1F6D") // in range
    assertMatches(ranges, "\u1F82") // 1F8A folds to 1F82, and 1F82 is also in range
    // 1FAA-1FAF
    assertMatches(ranges, "\u1FA4") // 1FAC folds to 1FA4 only in simple case folding
    // 2126
    assertMatches(ranges, "\u2126") // in the set
    assertMatches(ranges, "\u03C9") // 2126 folds to 03C9
    assertMatches(ranges, "\u03A9") // 03A9 also folds to 03C9
    // No matches
    assertNotMatches(ranges, "t")
    assertNotMatches(ranges, "ff") // ﬀ FB00 would only match with full case folding

    // Demonstrate that the JVM recognizes 017F as folding to 's' if the range is ASCII
    val rangeWithASCII_S = compile("[P-U]", CaseInsensitive | UnicodeCase)
    assertMatches(rangeWithASCII_S, "s")
    assertMatches(rangeWithASCII_S, "S")
    assertMatches(rangeWithASCII_S, "\u017F")

    // Demonstrate that the JVM recognizes 017F as folding to 's' if it is not a range
    val nonRangeWith_017F = compile("[\u017F\u0184]", CaseInsensitive | UnicodeCase)
    assertMatches(nonRangeWith_017F, "s")
    assertMatches(nonRangeWith_017F, "S")
    assertMatches(nonRangeWith_017F, "\u017F")
  }

  @Test def wordBoundary(): Unit = {
    val fooWordBoundary = compile("foo\\b")
    assertMatches(fooWordBoundary, "foo")
    assertFind(fooWordBoundary, "foo bar", 0)
    assertFind(fooWordBoundary, "foobar foo+bar", 7)
    assertNotFind(fooWordBoundary, "foobar")

    // https://bugs.openjdk.java.net/browse/JDK-8264160
    if (!executingInJVM)
      assertFind(fooWordBoundary, "fooÀbar", 0)

    val fooNonWordBoundary = compile("foo\\B")
    assertNotFind(fooNonWordBoundary, "foo")
    assertNotFind(fooNonWordBoundary, "foo bar")
    assertFind(fooNonWordBoundary, "foo+barfoobar", 7)
    assertFind(fooNonWordBoundary, "foobar", 0)

    // https://bugs.openjdk.java.net/browse/JDK-8264160
    if (!executingInJVM)
      assertNotFind(fooNonWordBoundary, "fooÀbar")
  }

  @Test def wordBoundaryUnicode(): Unit = {
    assumeTrue("requires look-behinds", regexSupportsLookBehinds)

    val fooWordBoundary = compile("な\\b", UnicodeCharacterClass)
    assertMatches(fooWordBoundary, "な")
    assertFind(fooWordBoundary, "ひらがな bar", 3)
    assertFind(fooWordBoundary, "なつ ひらがな+bar", 6)
    assertNotFind(fooWordBoundary, "なつ")

    val fooNonWordBoundary = compile("な\\B", UnicodeCharacterClass)
    assertNotFind(fooNonWordBoundary, "な")
    assertNotFind(fooNonWordBoundary, "ひらがな bar")
    assertFind(fooNonWordBoundary, "ひらがな+barひらがなbar", 11)
    assertFind(fooNonWordBoundary, "なつ", 0)
  }

  @Test def endOfInputPossiblyBeforeLineTerminator(): Unit = {
    val aZ = compile("a\\Z")
    assertMatches(aZ, "a")
    assertFind(aZ, "a\n", 0, 1)
    assertFind(aZ, "a\r\n", 0, 1)
    assertFind(aZ, "a\u2028", 0, 1)
    assertFind(aZ, "baa", 2, 3)
    assertFind(aZ, "baa\n", 2, 3)
    assertNotFind(aZ, "ab")
    assertNotFind(aZ, "a\nb")

    val aZUnixLines = compile("a\\Z", UnixLines)
    assertMatches(aZUnixLines, "a")
    assertFind(aZUnixLines, "a\n", 0, 1)
    assertNotFind(aZUnixLines, "a\r\n")
    assertNotFind(aZUnixLines, "a\u2028")
    assertFind(aZUnixLines, "baa", 2, 3)
    assertFind(aZUnixLines, "baa\n", 2, 3)
    assertNotFind(aZUnixLines, "ab")
    assertNotFind(aZUnixLines, "a\nb")

    val nlZ = compile("\\n\\Z")
    assertFind(nlZ, "\n", 0, 1)
    assertFind(nlZ, "\n\n", 0, 1)
  }

  @Test def unicodeLineBreak(): Unit = {
    val lineBreak = compile("\\R")
    assertMatches(lineBreak, "\n")
    assertMatches(lineBreak, "\u000B")
    assertMatches(lineBreak, "\u000C")
    assertMatches(lineBreak, "\r")
    assertMatches(lineBreak, "\u0085")
    assertMatches(lineBreak, "\u2028")
    assertMatches(lineBreak, "\u2029")
    assertMatches(lineBreak, "\r\n")
    assertNotMatches(lineBreak, "\t")
    assertNotMatches(lineBreak, " ")
    assertNotMatches(lineBreak, "a")

    assertFind(lineBreak, "ab\r\ncd", 2, 4)
    assertFind(lineBreak, "ab\n\ncd", 2, 3)

    // \R is not affected by UNIX_LINES

    val lineBreakUnixLines = compile("\\R", UnixLines)
    assertMatches(lineBreakUnixLines, "\n")
    assertMatches(lineBreakUnixLines, "\u000B")
    assertMatches(lineBreakUnixLines, "\u000C")
    assertMatches(lineBreakUnixLines, "\r")
    assertMatches(lineBreakUnixLines, "\u0085")
    assertMatches(lineBreakUnixLines, "\u2028")
    assertMatches(lineBreakUnixLines, "\u2029")
    assertMatches(lineBreakUnixLines, "\r\n")
    assertNotMatches(lineBreakUnixLines, "\t")
    assertNotMatches(lineBreakUnixLines, " ")
    assertNotMatches(lineBreakUnixLines, "a")

    assertFind(lineBreakUnixLines, "ab\r\ncd", 2, 4)
    assertFind(lineBreakUnixLines, "ab\n\ncd", 2, 3)
  }

  @Test def namedCaptureGroups(): Unit = {
    val named = compile(".*((?<pizza>Pizza).*?)+")
    val m = assertMatchesAndGroupsEquals(named, "PizzaWithPizza", "Pizza", "Pizza")
    assertEquals("Pizza", m.group("pizza"))

    val ref = compile("(?<pizza>Pizza)\\k<pizza>*?")
    assertMatches(ref, "Pizza")
    assertMatches(ref, "PizzaPizza")
    assertMatches(ref, "PizzaPizzaPizza")
    assertNotMatches(ref, "PizzaPizzicatoPizza")

    assertSyntaxError("(?<A>a?)\\k<B>?", "named capturing group <B> does not exit", 12)

    assertSyntaxError("(?<A>a?)(?<A>dupe)", "named capturing group <A> is already defined", 12)
  }

  @Test def recursiveCapturingGroups(): Unit = {
    val rec = compile("""(a?\1?)\1""")
    assertMatches(rec, "aa")
    assertMatches(rec, "")
    assertNotMatches(rec, "ab")
    assertNotMatches(rec, "a")
    assertNotMatches(rec, "aaa")

    // The JVM kind of supports "back references" to later groups, but we don't
    assertSyntaxErrorInJS("(a?\\2?)(b?\\1?)", "numbered capturing group <2> does not exist", 4)

    // The JVM tolerates "back references" to non-existing groups, but we don't
    assertSyntaxErrorInJS("(a?\\3?)(b?\\1?)", "numbered capturing group <3> does not exist", 4)

    val namedRec = compile("(?<A>a?\\k<A>?)\\k<A>")
    assertMatches(namedRec, "aa")
    assertMatches(namedRec, "")
    assertNotMatches(namedRec, "ab")
    assertNotMatches(namedRec, "a")

    assertSyntaxError("(?<A>a?\\k<B>?)(?<B>b?\\k<A>?)", "named capturing group <B> does not exit", 11)
  }

  @Test def backReferenceLimit(): Unit = {
    val backRef12 = compile("""(a?)(b?)(c?)(d?)(e?)(f?)(g?)(h?)(i?)(k?)(l?)(m?)\12""")
    assertMatches(backRef12, "acfg")
    assertMatches(backRef12, "acfgmm")
    assertNotMatches(backRef12, "acfgm")
    assertNotMatches(backRef12, "acfgma2")
    assertNotMatches(backRef12, "acfgma")

    val backRefLimited = compile("""(a?)\12""")
    assertMatches(backRefLimited, "2")
    assertMatches(backRefLimited, "aa2")
    assertNotMatches(backRefLimited, "a2")
    assertNotMatches(backRefLimited, "a")
    assertNotMatches(backRefLimited, "aa")
    assertNotMatches(backRefLimited, "aaa")
  }

  @Test def repeatedNestedCapture(): Unit = {
    val pattern = compile("(Foo(Bar)?)+")
    val matcher = pattern.matcher("FooBarFoo")
    assert(matcher.matches())
    assertEquals(matcher.group(), "FooBarFoo")
    assertEquals(matcher.groupCount(), 2)
    assertEquals(matcher.group(1), "Foo")

    // This is not good, but I (sjrd) don't think there's anything we can do about it
    if (executingInJVM)
      assertEquals(matcher.group(2), "Bar")
    else
      assertEquals(matcher.group(2), null)
  }

  @Test def atomicGroups(): Unit = {
    val abccNonAtomic = compile("a(bc|b)c")
    assertFind(abccNonAtomic, "abcc", 0, 4)
    assertFind(abccNonAtomic, "abc", 0, 3)

    val abccAtomic = compile("a(?>bc|b)c")
    assertFind(abccAtomic, "abcc", 0, 4)
    assertNotFind(abccAtomic, "abc")

    // Don't break numbered groups before, within, and after
    val atomicGroupsAndNumberedGroups = compile("(a)(?>(bc)|(c))(c) \\1 \\2 \\4")
    assertFindAndGroupsEquals(atomicGroupsAndNumberedGroups, "abcc a bc c", 0, "a", "bc", null, "c")

    // Don't break named groups before, within, and after
    val atomicGroupsAndNamedGroups = compile("(?<A>a)(?>(?<B>bc)|(?<C>c))(?<D>c) \\k<A> \\k<B> \\k<D>")
    val m = assertFindAndGroupsEquals(atomicGroupsAndNamedGroups, "abcc a bc c", 0, "a", "bc", null, "c")
    assertEquals("a", m.group("A"))
    assertEquals("bc", m.group("B"))
    assertEquals(null, m.group("C"))
    assertEquals("c", m.group("D"))
  }

  @Test def atomicGroupsAndPossessiveQuantifiersAvoidCatastrophicBacktracking(): Unit = {
    // See https://www.regular-expressions.info/catastrophic.html

    val input = "x" * 50

    /* Enable this if you want to confirm that the catastrophic version is
     * indeed catastrophic: it is going to loop "forever".
     */
    //val catastrophicPattern = compile("(x+x+)+y")
    //assertNotMatches(catastrophicPattern, input)

    val solutionWithPossessiveQuantifier = compile("(x+x+)++y")
    assertNotMatches(solutionWithPossessiveQuantifier, input)

    val solutionWithAtomicGroup = compile("(?>(x+x+)+)y")
    assertNotMatches(solutionWithAtomicGroup, input)
  }

  /** Tests for regexes that we found in the public Scala.js libraries at the
   *  time of switching from the JS `RegExp` behavior to the JVM `Pattern`
   *  behavior.
   *
   *  When the groups are fetched in the original code, we check the groups
   *  here. Otherwise, we don't, even if there are capturing groups in the
   *  regex.
   *
   *  These tests only really test that the regexes still work, but not that
   *  they work *in the same way* as before. In fact, they don't for some
   *  corner cases. By inspection, all the regexes below use features in 4
   *  categories:
   *
   *  - Features whose semantics are equivalent in `js.RegExp` and `Pattern`,
   *    notably ASCII characters, repeaters, classes of ASCII characters, the
   *    '\d' character class, the '^' and '$' boundary matchers (without
   *    multiline).
   *  - The '.', which *is* different: it matches '\x85' in `js.RegExp` but not
   *    in `Pattern`; this was judged acceptable as unlikely to cause a real
   *    difference in practice.
   *  - One regex uses the `CASE_INSENSITIVE` with a pattern that contains only
   *    ASCII letters: it now really only matches other ASCII letters; this was
   *    judged acceptable as probably the intended meaning anyway.
   *  - One regex uses '\s' and '\S', for which we obtained confirmation from
   *    the maintainer that the change in semantics was not an issue.
   */
  @Test def regexesFoundInLibraries(): Unit = {
    // scalastyle:off line.size.limit

    // https://github.com/Bathtor/api-framework/blob/d85d454b787393c539fcc4d8a09fe383041cfc2b/src/main/scala/com/lkroll/roll20/api/Attribute.scala#L144
    locally {
      val rowIdPattern = compile(raw"repeating_[a-zA-Z]+_([-a-zA-Z0-9]+)_.*")
      assertMatchesAndGroupsEquals(rowIdPattern,
          "repeating_testsec_-KkWmmPeaGP87vaZLpkt_testsec_testf",
          "-KkWmmPeaGP87vaZLpkt")
    }

    // https://github.com/gemini-hlsw/lucuma-ui/blob/57dbc3c9ccf2cc108a13c18a878a9e48099ac7f4/modules/ui/src/main/scala/lucuma/ui/optics/ChangeAuditor.scala#L363
    locally {
      // note: IMO the '+' is a mistake in the original regex, but we're testing it as is anyway
      val n = 3
      val stripZerosPastNPlaces = compile(s"(.*\\.\\d{0,$n}[1-9]*)+0*")
      assertMatchesAndGroupsEquals(stripZerosPastNPlaces, "foo.23034500000", "foo.230345")
      assertMatchesAndGroupsEquals(stripZerosPastNPlaces, "foo.034500000", "foo.0345")
      assertMatchesAndGroupsEquals(stripZerosPastNPlaces, "foo.34500000", "foo.345")
      assertMatchesAndGroupsEquals(stripZerosPastNPlaces, "foo.00000000", "foo.000")
      assertMatchesAndGroupsEquals(stripZerosPastNPlaces, "foo.000345bar.0100000", "foo.000345bar.010")
      assertNotMatches(stripZerosPastNPlaces, "foo123")
      assertNotMatches(stripZerosPastNPlaces, "foo.03450012000")
    }

    // https://github.com/gemini-hlsw/lucuma-ui/blob/57dbc3c9ccf2cc108a13c18a878a9e48099ac7f4/modules/ui/src/main/scala/lucuma/ui/optics/ChangeAuditor.scala#L375
    locally {
      val newPos = 3
      val stripZerosBeforeN = compile(s"0{0,$newPos}(\\d+)")
      assertMatchesAndGroupsEquals(stripZerosBeforeN, "000001230", "001230")
      assertMatchesAndGroupsEquals(stripZerosBeforeN, "001230001230", "1230001230")
      assertMatchesAndGroupsEquals(stripZerosBeforeN, "1230001230", "1230001230")
      assertNotMatches(stripZerosBeforeN, "00123a0001230")
    }

    // https://github.com/exoego/scala-js-nodejs/blob/4bfa4a96d646665b0e0c3e7c47eb1c206e31c01d/core/src/main/scala/io/scalajs/nodejs/internal/CompilerSwitches.scala#L6
    locally {
      val nodejsVersionPattern = compile("^nodeJs([0-9]{1,2})\\.([0-9]{1,2})\\.([0-9]{1,2})$")
      assertMatchesAndGroupsEquals(nodejsVersionPattern, "nodeJs14.15.1", "14", "15", "1")
      assertMatchesAndGroupsEquals(nodejsVersionPattern, "nodeJs1.0.2", "1", "0", "2")
      assertNotMatches(nodejsVersionPattern, "nodeJs123.4.5")
      assertNotMatches(nodejsVersionPattern, "node12.4.5")
    }

    // https://github.com/japgolly/scalajs-benchmark/blob/7c6061c221e0deb09f8dde9a762f5001bbe3c247/benchmark/src/main/scala/japgolly/scalajs/benchmark/gui/GuiUtil.scala#L25
    locally {
      val numberFmt = compile("""^-?(\d[,.]?)+(?:[,.]\d+)?$""")
      assertMatches(numberFmt, "123.456644,112")
      assertMatches(numberFmt, "-123.456644,112")
      assertMatches(numberFmt, "1")
      assertNotMatches(numberFmt, "123..456644,112")
      assertNotMatches(numberFmt, "123,,456644,112")
      assertNotMatches(numberFmt, "123.,456644,112")
      assertNotMatches(numberFmt, "123.45a644,112")
      assertNotMatches(numberFmt, ".1")
    }

    // https://github.com/japgolly/scalajs-benchmark/blob/7c6061c221e0deb09f8dde9a762f5001bbe3c247/benchmark/src/main/scala/japgolly/scalajs/benchmark/gui/IntEditor.scala#L46
    locally {
      val illegalChars = compile("[^0-9-]+")
      assertMatches(illegalChars, "abd_\n^foo")
      assertNotMatches(illegalChars, "ab5foo")
      assertNotMatches(illegalChars, "ab-foo")
      assertNotMatches(illegalChars, "")
    }

    // https://github.com/japgolly/scalajs-benchmark/blob/7c6061c221e0deb09f8dde9a762f5001bbe3c247/benchmark/src/main/scala/japgolly/scalajs/benchmark/gui/package.scala#L73
    locally {
      val r = compile("[ ,]")
      assertMatches(r, " ")
      assertMatches(r, ",")
      assertNotMatches(r, "_")
      assertNotMatches(r, "  ")
      assertNotMatches(r, " ,")
      assertNotMatches(r, "[")
      assertNotMatches(r, "]")
    }

    // https://github.com/japgolly/scalajs-react/blob/3639a2a7bafabac8a9ad048e9a942cba3ca34054/core/src/main/scala/japgolly/scalajs/react/ScalaJsReactConfig.scala#L47
    locally {
      val regex = compile("\\.?comp(?:onent)?$", CaseInsensitive)
      assertMatches(regex, ".comp")
      assertMatches(regex, ".component")
      assertMatches(regex, ".coMpOneNT")
      assertMatches(regex, ".COMP")
      assertMatches(regex, "comp")
      assertMatches(regex, ".coMPOnent")
      assertNotMatches(regex, ".compon")
      assertNotMatches(regex, ".cimponent")
      assertNotMatches(regex, "+comp")
    }

    // https://github.com/japgolly/scalajs-react/blob/4db165c363efa379d146f97401f6bcf97bc8a698/extra/src/main/scala/japgolly/scalajs/react/extra/router/Dsl.scala#L19
    locally {
      val regexEscape1 = compile("""([-()\[\]{}+?*.$\^|,:#<!\\])""")
      assertMatches(regexEscape1, "-")
      assertMatches(regexEscape1, "(")
      assertMatches(regexEscape1, ")")
      assertMatches(regexEscape1, "]")
      assertMatches(regexEscape1, "{")
      assertMatches(regexEscape1, "}")
      assertMatches(regexEscape1, "+")
      assertMatches(regexEscape1, "?")
      assertMatches(regexEscape1, "*")
      assertMatches(regexEscape1, ".")
      assertMatches(regexEscape1, "$")
      assertMatches(regexEscape1, "^")
      assertMatches(regexEscape1, "|")
      assertMatches(regexEscape1, ",")
      assertMatches(regexEscape1, ":")
      assertMatches(regexEscape1, "#")
      assertMatches(regexEscape1, "<")
      assertMatches(regexEscape1, "!")
      assertMatches(regexEscape1, "\\")
      assertNotMatches(regexEscape1, "a")
      assertNotMatches(regexEscape1, "((")
      assertNotMatches(regexEscape1, "(0")

      /* A bit about the intent of that regex: it is used to quote characters
       * *in a regex*. So we make sure here that all those characters can
       * indeed be quoted.
       */
      val quotedChars = compile("""a\-\(\)\[\]\{\}\+\?\*\.\$\^\|\,\:\#\<\!\\b""")
      assertMatches(quotedChars, """a-()[]{}+?*.$^|,:#<!\b""")
    }

    // https://github.com/japgolly/scalajs-react/blob/4db165c363efa379d146f97401f6bcf97bc8a698/extra/src/main/scala/japgolly/scalajs/react/extra/router/Dsl.scala#L464
    locally {
      val queryRegex = compile("""(\?[^#]*)?""")
      assertMatchesAndGroupsEquals(queryRegex, "?foo=bar", "?foo=bar")
      assertMatchesAndGroupsEquals(queryRegex, "?foo", "?foo")
      assertMatchesAndGroupsEquals(queryRegex, "?", "?")
      assertMatchesAndGroupsEquals(queryRegex, "", null)
      assertNotMatches(queryRegex, "?foo#bar")
      assertNotMatches(queryRegex, "?foo#")
      assertNotMatches(queryRegex, "foo")
      assertNotMatches(queryRegex, "foo?bar")
    }

    // https://github.com/japgolly/scalajs-react/blob/4db165c363efa379d146f97401f6bcf97bc8a698/extra/src/main/scala/japgolly/scalajs/react/extra/router/Dsl.scala#L317
    locally {
      val uuidRegex = compile("([A-Fa-f0-9]{8}(?:-[A-Fa-f0-9]{4}){3}-[A-Fa-f0-9]{12})")
      assertMatchesAndGroupsEquals(uuidRegex, "12345678-1234-abcd-1234-123456789012", "12345678-1234-abcd-1234-123456789012")
      assertNotMatches(uuidRegex, "12345678-1234-abcd-1234-12345678901")
      assertNotMatches(uuidRegex, "12345678-1234-1234-123456789012")
      assertNotMatches(uuidRegex, "12345678-1234-efgh-1234-123456789012")
    }

    // https://github.com/japgolly/scalajs-react/blob/4db165c363efa379d146f97401f6bcf97bc8a698/extra/src/main/scala/japgolly/scalajs/react/extra/router/Dsl.scala#L466
    locally {
      val kvRegex = compile("^([^=]+)(?:=(.*))?$")
      assertMatchesAndGroupsEquals(kvRegex, "foo", "foo", null)
      assertMatchesAndGroupsEquals(kvRegex, "foo=bar", "foo", "bar")
      assertMatchesAndGroupsEquals(kvRegex, "fo\no=bar", "fo\no", "bar")
      assertMatchesAndGroupsEquals(kvRegex, "foo=bar=babar", "foo", "bar=babar")
      assertMatchesAndGroupsEquals(kvRegex, "foo=", "foo", "")
      assertNotMatches(kvRegex, "foo=ba\nr")
      assertNotMatches(kvRegex, "=bar")
    }

    // https://github.com/japgolly/scalajs-react/blob/4db165c363efa379d146f97401f6bcf97bc8a698/extra/src/main/scala/japgolly/scalajs/react/extra/router/Dsl.scala#L471
    locally {
      val needingEncoding = compile("""[~!'()]|%[02]0""")
      assertMatches(needingEncoding, "~")
      assertMatches(needingEncoding, "!")
      assertMatches(needingEncoding, "'")
      assertMatches(needingEncoding, "(")
      assertMatches(needingEncoding, ")")
      assertMatches(needingEncoding, "%00")
      assertMatches(needingEncoding, "%20")
      assertNotMatches(needingEncoding, "]")
      assertNotMatches(needingEncoding, "|")
      assertNotMatches(needingEncoding, "%10")
      assertNotMatches(needingEncoding, "%0")
    }

    // https://github.com/japgolly/scalajs-react/blob/4db165c363efa379d146f97401f6bcf97bc8a698/extra/src/main/scala/japgolly/scalajs/react/extra/router/Dsl.scala#L533
    locally {
      val removeQuery = compile("^(.*?)\\?.*$")
      assertMatchesAndGroupsEquals(removeQuery, "foo?bar", "foo")
      assertMatchesAndGroupsEquals(removeQuery, "foo?bar?babar", "foo")
      assertMatchesAndGroupsEquals(removeQuery, "?bar", "")
      assertMatchesAndGroupsEquals(removeQuery, "foo?", "foo")
      assertNotMatches(removeQuery, "")
      assertNotMatches(removeQuery, "foo")
    }

    // https://github.com/japgolly/scalajs-react/blob/4db165c363efa379d146f97401f6bcf97bc8a698/extra/src/main/scala/japgolly/scalajs/react/extra/router/Dsl.scala#L539
    locally {
      val removeTrailingSlashes = compile("^(.*?)/+$")
      assertMatchesAndGroupsEquals(removeTrailingSlashes, "foo///", "foo")
      assertMatchesAndGroupsEquals(removeTrailingSlashes, "foo/", "foo")
      assertMatchesAndGroupsEquals(removeTrailingSlashes, "foo/bar/", "foo/bar")
      assertMatchesAndGroupsEquals(removeTrailingSlashes, "///", "")
      assertNotMatches(removeTrailingSlashes, "foo")
      assertNotMatches(removeTrailingSlashes, "foo/bar")
    }

    // https://github.com/japgolly/scalajs-react/blob/4db165c363efa379d146f97401f6bcf97bc8a698/extra/src/main/scala/japgolly/scalajs/react/extra/router/Dsl.scala#L545
    locally {
      val removeLeadingSlashes = compile("^/+(.*)$")
      assertMatchesAndGroupsEquals(removeLeadingSlashes, "///foo", "foo")
      assertMatchesAndGroupsEquals(removeLeadingSlashes, "/foo", "foo")
      assertMatchesAndGroupsEquals(removeLeadingSlashes, "/foo/bar", "foo/bar")
      assertMatchesAndGroupsEquals(removeLeadingSlashes, "///", "")
      assertNotMatches(removeLeadingSlashes, "foo")
      assertNotMatches(removeLeadingSlashes, "foo/bar")
    }

    // https://github.com/japgolly/scalajs-react/blob/4db165c363efa379d146f97401f6bcf97bc8a698/extra/src/main/scala/japgolly/scalajs/react/extra/router/Dsl.scala#L551
    locally {
      val trimSlashes = compile("^/*(.*?)/+$")
      assertMatchesAndGroupsEquals(trimSlashes, "//foo///", "foo")
      assertMatchesAndGroupsEquals(trimSlashes, "foo/", "foo")
      assertMatchesAndGroupsEquals(trimSlashes, "///foo/bar/", "foo/bar")
      assertMatchesAndGroupsEquals(trimSlashes, "///", "")
      assertNotMatches(trimSlashes, "foo")
      assertNotMatches(trimSlashes, "/foo/bar")
    }

    // https://github.com/japgolly/scalajs-react/blob/4db165c363efa379d146f97401f6bcf97bc8a698/gh-pages-macros/src/main/scala/ghpages/GhPagesMacros.scala#L33
    locally {
      val marker = "some?marker"
      val r = compile(s"""\n[ \t]*//[ \t]*${Pattern.quote(marker)}[ \t]*""")
      assertMatches(r, "\n// some?marker")
      assertMatches(r, "\n  \t  //some?marker")
      assertMatches(r, "\n  \t  //some?marker   \t")
      assertMatches(r, "\n  \t  //   \tsome?marker")
      assertNotMatches(r, "\n  \t  //   \tsommarker")
      assertNotMatches(r, "  \t  //   \tsome?marker")
      assertNotMatches(r, "\n  \r  //   \tsome?marker")
      assertNotMatches(r, "\n  \t  //   \nsome?marker")
    }

    // https://github.com/japgolly/scalajs-react/blob/4db165c363efa379d146f97401f6bcf97bc8a698/test/src/test/scala/japgolly/scalajs/react/test/TestUtil.scala#L59
    locally {
      val reactStuffToIgnore = compile("""\s+data-react\S*?\s*?=\s*?".*?"|<!--(?:.|[\r\n])*?-->""")
      assertMatches(reactStuffToIgnore, """ data-react-foo  =  "arg"""")
      assertMatches(reactStuffToIgnore, s"""<!--  foo bar ${"\n"} more -->""")
      assertFind(reactStuffToIgnore, """begin <!--  foo bar --> more -->""", 6, 23)
      assertNotMatches(reactStuffToIgnore, """ data-react-foo  =  arg""")
      assertNotMatches(reactStuffToIgnore, """<!--  foo bar""")
    }

    // https://github.com/getshaka-org/shaka-router/blob/4330f330deb9dd6bbd277d4b7ee1fd48ea821d81/src/main/scala/org/getshaka/shaka/router/Router.scala#L51
    locally {
      val hashRegex = compile(raw".*#(\S+)")
      assertFind(hashRegex, "foo  #foobar babar", 0, 12)
      assertNotFind(hashRegex, "foo# bar")
    }

    // https://github.com/ekrich/sjavatime/blob/f92561eb0506abd416a6259a7beb08aac610bfd2/sjavatime/shared/src/main/scala/java/time/Instant.scala#L390
    locally {
      val pattern = compile("""(^[-+]?)(\d*)-(\d*)-(\d*)T(\d*):(\d*):(\d*).?(\d*)Z""")
      assertMatchesAndGroupsEquals(pattern, "1970-01-01T00:00:00Z", "", "1970", "01", "01", "00", "00", "00", "")
      assertMatchesAndGroupsEquals(pattern, "-1000000000-01-01T00:00:00Z", "-", "1000000000", "01", "01", "00", "00", "00", "")
      assertMatchesAndGroupsEquals(pattern, "-999999999-01-01T00:00:00Z", "-", "999999999", "01", "01", "00", "00", "00", "")
      assertMatchesAndGroupsEquals(pattern, "1970-01-01T00:10:00.100Z", "", "1970", "01", "01", "00", "10", "00", "100")
      assertMatchesAndGroupsEquals(pattern, "+1000000000-12-31T23:59:59.999999999Z", "+", "1000000000", "12", "31", "23", "59", "59", "999999999")
      assertMatchesAndGroupsEquals(pattern, "+999999999-12-31T23:59:59.999999999Z", "+", "999999999", "12", "31", "23", "59", "59", "999999999")
      assertMatchesAndGroupsEquals(pattern, "1999-06-03T06:56:23.942Z", "", "1999", "06", "03", "06", "56", "23", "942")
      assertMatchesAndGroupsEquals(pattern, "-0687-08-07T23:38:33.088936253Z", "-", "0687", "08", "07", "23", "38", "33", "088936253")
      assertNotMatches(pattern, "-ABCD-08-07T23:38:33.088936253Z")
    }

    // scalastyle:on line.size.limit
  }

  @Test def testVariousOtherRegexes(): Unit = {
    // From the Metals codebase
    locally {
      val pattern = {
        val markdownLink = """- \[([^\[]+)\]\s*\([^\(]+\)"""
        val whitespacesWithSingleNewline = """[ \t]*\r?\n?[ \t]*"""
        val optionalDescription = """\(?([^\n)]*)\)?"""
        compile(s"${markdownLink}${whitespacesWithSingleNewline}${optionalDescription}")
      }

      val input = """
        |#### Scala
        |
        |- [scala/scala-seed.g8](https://github.com/scala/scala-seed.g8)
        |(Seed template for Scala)
      """.stripMargin

      assertFindAndGroupsEquals(pattern, input, 13, "scala/scala-seed.g8", "Seed template for Scala")
    }

    locally {
      val patVarUnused = compile("^pattern var .* in (value|method) .* is never used")
      assertFind(patVarUnused, "pattern var test in method loop is never used: use a wildcard `_`", 0)
    }

    locally {
      val pattern = compile("([^/]+)/(.*)")
      assertFindAndGroupsEquals(pattern, "s://123", 0, "s:", "/123")
      assertNotFind(pattern, "nothing")
      assertNotFind(pattern, "/nothing")
    }

    locally {
      val pattern = compile("location.*(London|Paris|Zurich|Strasbourg)")
      assertFind(pattern, "Course location is London or Paris!", 7)
    }

    locally {
      val pattern = compile("|([a-o]+)([^ ]+)[ ]([A-Z]+)| |")
      assertFind(pattern, "abcdefghijklmnopqrstuvwxyz ABCDEFG 0123456789", 0)
    }

    locally {
      val pattern = compile("""(\w+ (\d+))""")
      assertMatchesAndGroupsEquals(pattern, "Jan 1987", "Jan 1987", "1987")
    }

    locally {
      val pattern = compile("""(((rat )bat )cat )dog""")
      assertFindAndGroupsEquals(pattern, "**rat bat cat dog", 2, "rat bat cat ", "rat bat ", "rat ")
    }

    assertSyntaxError("""^(1[-\s.])?(\()?\d{3}(?(2)\))[-\s.]?\d{3}[-\s.]?\d{4}$""",
        "Embedded flag expression in the middle of a pattern is not supported", 21)

    locally {
      val usPhone = compile("""^(1[-\s.])?(\(\d{3}\)|\d{3})[-\s.]?\d{3}[-\s.]?\d{4}$""")
      assertMatches(usPhone, "123 555 6789")
      assertMatches(usPhone, "1-(123)-555-6789")
      assertMatches(usPhone, "(123).555.6789")
      assertNotMatches(usPhone, "123 55 6789")
      assertNotMatches(usPhone, "(123-555-6789")
    }
  }

  @Test def unsupportedFeatures(): Unit = {
    // Never supported

    assertSyntaxErrorInJS("foo", CanonEq, "CANON_EQ is not supported", 0)

    assertSyntaxErrorInJS("foo\\Gbar", "\\G in the middle of a pattern is not supported", 4)
    assertSyntaxErrorInJS("\\Gfoo|bar", "\\G is not supported when there is an alternative at the top level", 5)
    assertSyntaxErrorInJS("\\G+foo", "Dangling meta character '+'", 2)

    assertSyntaxErrorInJS("foo(?i)bar", "Embedded flag expression in the middle of a pattern is not supported", 3)
    assertSyntaxErrorInJS("foo(?i:bar)sef", "Embedded flag expression in the middle of a pattern is not supported", 3)
    assertSyntaxErrorInJS("(?i:bar)sef", "Embedded flag expression in the middle of a pattern is not supported", 0)

    assertSyntaxErrorInJS("\\G(?i)bar", "Embedded flag expression in the middle of a pattern is not supported", 2)

    if (regexSupportsUnicodeCharacterClasses) {
      assertSyntaxErrorInJS("\\p{InGreek}", "Blocks are not supported in \\p Unicode character families", 10)
      assertSyntaxErrorInJS("\\p{blk=Greek}", "Blocks are not supported in \\p Unicode character families", 12)
      assertSyntaxErrorInJS("\\p{block=Greek}", "Blocks are not supported in \\p Unicode character families", 14)
    }

    // JDK 9+ features
    if (!executingInJVMOnLowerThanJDK(9)) {
      assertSyntaxErrorInJS("\\N{DIGIT TWO}", "\\N is not supported", 1)
      assertSyntaxErrorInJS("foo\\b{g}.", "\\b{g} is not supported", 4)
      assertSyntaxErrorInJS("foo\\X", "\\X is not supported", 4)
    }

    // Not supported below ES2018

    if (!regexSupportsLookBehinds || !regexSupportsUnicodeCharacterClasses) {
      val reason = """
        |because it requires RegExp features of ECMAScript 2018.
        |If you only target environments with ES2018+, you can enable ES2018 features with
        |  scalaJSLinkerConfig ~= { _.withESFeatures(_.withESVersion(ESVersion.ES2018)) }
        |or an equivalent configuration depending on your build tool.
      """.stripMargin.trim()

      assertSyntaxErrorInJS(".(?<=b)c", s"Look-behind group is not supported $reason", 1)
      assertSyntaxErrorInJS(".(?<!b)c", s"Look-behind group is not supported $reason", 1)
      assertSyntaxErrorInJS("a", Multiline, s"MULTILINE is not supported $reason", 0)
      assertSyntaxErrorInJS("a", UnicodeCharacterClass, s"UNICODE_CHARACTER_CLASS is not supported $reason", 0)
      assertSyntaxErrorInJS(".\\p{L}", s"Unicode character family is not supported $reason", 5)

      if (regexSupportsUnicodeCase) {
        assertSyntaxErrorInJS("a\\b.", UnicodeCase, s"\\b with UNICODE_CASE is not supported $reason", 2)
        assertSyntaxErrorInJS("a\\B.", UnicodeCase, s"\\B with UNICODE_CASE is not supported $reason", 2)
      }
    }

    // Not supported below ES2015

    if (!regexSupportsUnicodeCase) {
      val reason = """
        |because it requires RegExp features of ECMAScript 2015.
        |If you only target environments with ES2015+, you can enable ES2015 features with
        |  scalaJSLinkerConfig ~= { _.withESFeatures(_.withESVersion(ESVersion.ES2015)) }
        |or an equivalent configuration depending on your build tool.
      """.stripMargin.trim()

      assertSyntaxErrorInJS("a", CaseInsensitive | UnicodeCase, s"UNICODE_CASE is not supported $reason", 0)
    }
  }
}
