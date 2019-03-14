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

import scala.language.implicitConversions

import java.util.regex._

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.Platform._
import org.scalajs.testsuite.utils.AssertThrows._

import scala.util.matching.Regex

class RegexMatcherTest  {

  @Test def find(): Unit = {
    val matcher = Pattern.compile("a").matcher("Scala.js")

    assertTrue(matcher.find())
    assertTrue(matcher.find())
    assertFalse(matcher.find())
    assertTrue(matcher.find(4))
    assertFalse(matcher.find())
  }

  @Test def start_end_group_and_toMatchResult(): Unit = {
    val matcher = Pattern
      .compile("\\s(([A-Za-z]{5}(hum)?).js)\\s")
      .matcher("Write Scala.js everyday!")
    checkGroups(matcher,
        (5, 15, " Scala.js "),
        (6, 14, "Scala.js"),
        (6, 11, "Scala"),
        (-1, -1, null)
    )
  }

  @Test def start_end_group_tricky_and_toMatchResult(): Unit = {
    val matcher = Pattern
      .compile("(Scala\\.js).*(Scala)$")
      .matcher("Scala.js is a Scalable javascript compiler based on Scala")
    checkGroups(matcher,
        (0, 57, "Scala.js is a Scalable javascript compiler based on Scala"),
        (0, 8, "Scala.js"),
        (52, 57, "Scala")
    )
  }

  @Test def start_end_group_matchnot_and_toMatchResult(): Unit = {
    val matcher = Pattern
      .compile("(?!Scala\\.js)(Scala)")
      .matcher("There is a difference between Scala.js and Scala, but both are Scalable")
    checkGroups(matcher,
        (43, 48, "Scala"),
        (43, 48, "Scala")
    )
    checkGroups(matcher,
        (63, 68, "Scala"),
        (63, 68, "Scala")
    )
  }

  @Test def start_end_group_multiple_and_toMatchResult(): Unit = {
    val matcher = Pattern
      .compile("(?=Scala\\.js is (nice|awesome))(Scala)\\.js")
      .matcher("Scala.js is nice, Scala.js is awesome")
    checkGroups(matcher,
        (0,  8,  "Scala.js"),
        (12, 16, "nice"),
        (0,  5,  "Scala")
    )
    checkGroups(matcher,
        (18, 26, "Scala.js"),
        (30, 37, "awesome"),
        (18, 23, "Scala")
    )
  }

  @Test def start_end_group_and_toMatchResult_with_inline_flags_issue3406(): Unit = {
    val matcher = Pattern
      .compile("(?i)(a).*(aa)")
      .matcher("bBaccAaD")
    checkGroups(matcher,
        (2, 7, "accAa"),
        (2, 3, "a"),
        (5, 7, "Aa")
    )

    // Test case from the bug report
    assertEquals(List("a", "A"),
        "(?i)(a)".r.findAllMatchIn("aA").map(_.matched).toList)
  }

  def parseExpect(regex: String, str: String, pos: (Int, Int)*): Unit = {
    val matcher = Pattern.compile(regex).matcher(str)
    assertEquals(pos.length - 1, matcher.groupCount)
    assertTrue(matcher.find())
    assertEquals(pos.length - 1, matcher.groupCount)
    var i = 0
    val tmp = pos.iterator
    while (tmp.hasNext) {
      assertEquals(tmp.next, (matcher.start(i), matcher.end(i)))
      i += 1
    }
  }

  @Test def parseRegex_test(): Unit = {
    parseExpect("aa", "aa", 0 -> 2)
    parseExpect("a(a)", "aa", 0 -> 2, 1 -> 2)
    parseExpect("ABC(A(B))(C)", "ABCABC", 0 -> 6, 3 -> 5, 4 -> 5, 5 -> 6)
    parseExpect("A(?:A)", "AA", 0 -> 2)
    parseExpect("A(?:(\\d))", "A1", 0 -> 2, 1 -> 2)
    parseExpect("A((?:A))", "AA", 0 -> 2, 1 -> 2)
    parseExpect("ab((ab))", "abab", 0 -> 4, 2 -> 4,  2 -> 4)
    parseExpect("hum(hum)?", "humhum", 0 -> 6, 3 -> 6)
    parseExpect("hum(hum)?", "hum", 0 -> 3, -1 -> -1)
    parseExpect("hum(?=hum)", "humhum", 0 -> 3)
    parseExpect("hum(?!hum)", "humhumhuf", 3 -> 6)
    parseExpect("hum(?=h(um))", "humhum", 0 -> 3, 4 -> 6)
    parseExpect("abab(ab){1,2}", "abababab", 0 -> 8, 6 -> 8)
    parseExpect("abab(ab){1,2}(abc){1,2}", "abababababcabc", 0 -> 14, 6 -> 8, 11 -> 14)
    parseExpect("ab(ab)?ab(?=aba)(ab)*", "abababababa", 0 -> 10, 2 -> 4, 8 -> 10)
    parseExpect("ab(?=aba)ab(aba)?(ab)*", "abababababa", 0 -> 7, 4 -> 7, -1 -> -1)
    parseExpect("ab(?=aba)ab(aba)??(ab)*","abababababa", 0 -> 10, -1 -> -1, 8 -> 10)
    parseExpect("abab(ab){1,2}?", "abababab", 0 -> 6, 4 -> 6)
    parseExpect("ab(?:ab)*", "abababab", 0 -> 8)
    if (!executingInJVM) {
      parseExpect("ab(?:a(c))*ac", "abacacac", 0 -> 8, 5 -> 6)
      parseExpect("ab(?:a(c))+ac", "abacacac", 0 -> 8, 5 -> 6)
    }
    parseExpect("ab(?:ac)*?ac", "abacacac", 0 -> 4)
    parseExpect("ab(?:ac)+?ac", "abacacac", 0 -> 6)
    parseExpect("ab((?=abab(ab))a(b))*a", "abababab", 0 -> 5, 2 -> 4, 6 -> 8, 3 -> 4)
  }

  @Test def parseRegex_backgroups_test(): Unit = {
    parseExpect("bc(.c).c(\\1)", "bczcxczc", 0 -> 8, 2 -> 4, 6 -> 8)
    parseExpect("(bc(.c).c)(\\2)", "bczcxczc", 0 -> 8, 0 -> 6, 2 -> 4, 6 -> 8)
  }

  @Test def parseRegex_disjunctions_test(): Unit = {
    parseExpect("a(b)|b(c)", "ab", 0 -> 2, 1 -> 2, -1 -> -1)
    parseExpect("a(b)|b(c)", "bc", 0 -> 2, -1 -> -1, 1 -> 2)
    if (!executingInJVM) {
      parseExpect("az(a(.)|b(.))+aw", "aza1b2b3aw",
          0 -> 10, 6 -> 8, -1 -> -1, 7 -> 8)
    } else {
      parseExpect("az(a(.)|b(.))+aw", "aza1b2b3aw",
          0 -> 10, 6 -> 8, 3 -> 4, 7 -> 8)
    }
  }

  def checkGroups(matcher: Matcher, startEndMatch: (Int, Int, String)*): Unit = {
    assertEquals(startEndMatch.size - 1, matcher.groupCount)

    assertTrue(matcher.find())

    assertEquals(startEndMatch(0)._1, matcher.start)
    assertEquals(startEndMatch(0)._2, matcher.end)
    assertEquals(startEndMatch(0)._3, matcher.group)
    assertEquals(startEndMatch.size - 1, matcher.groupCount)
    for (((start, end, mtch), i) <- startEndMatch.zipWithIndex) {
      assertEquals("" + i, start, matcher.start(i))
      assertEquals("" + i, end, matcher.end(i))
      assertEquals("" + i, mtch, matcher.group(i))
    }

    val matchResult = matcher.toMatchResult
    assertEquals(startEndMatch.size - 1, matchResult.groupCount)
    assertEquals(startEndMatch(0)._1, matchResult.start)
    assertEquals(startEndMatch(0)._2, matchResult.end)
    assertEquals(startEndMatch(0)._3, matchResult.group)
    for (((start, end, mtch), i) <- startEndMatch.zipWithIndex) {
      assertEquals(start, matchResult.start(i))
      assertEquals(end, matchResult.end(i))
      assertEquals(mtch, matchResult.group(i))
    }
  }

  @Test def matches(): Unit = {
    val matcher0 = Pattern.compile("S[a-z]+").matcher("Scala")
    val matcher1 = Pattern.compile("S[a-z]+").matcher("Scala.js")

    assertTrue(matcher0.matches())
    assertFalse(matcher1.matches())
  }

  @Test def several_matches_from_the_same_pattern_should_be_independent(): Unit = {
    val pattern = Pattern.compile("S[a-z]+")
    val matcher0 = pattern.matcher("Scalable")
    val matcher1 = pattern.matcher("Scalable")

    assertTrue(matcher0.find())
    assertFalse(matcher0.find())

    assertTrue(matcher1.find())
    assertFalse(matcher1.find())
  }

  @Test def reset(): Unit = {
    val matcher = Pattern.compile("S[a-z]+").matcher("Scalable")

    assertTrue(matcher.find())
    assertFalse(matcher.find())
    matcher.reset()
    assertTrue(matcher.find())
  }

  @Test def reset_string(): Unit = {
    val matcher = Pattern.compile("S[a-z]+").matcher("Scalable")

    assertTrue(matcher.matches())
    matcher.reset("Scala.js")
    assertFalse(matcher.matches())
  }

  @Test def usePattern(): Unit = {
    val patternNoDots = Pattern.compile("S[a-z]+")
    val patternWithDots = Pattern.compile("S[a-z.]+")

    val matcher0 = patternNoDots.matcher("Scala.js")
    assertFalse(matcher0.matches())
    matcher0.usePattern(patternWithDots)
    assertTrue(matcher0.matches())

    val matcher1 = patternWithDots.matcher("Scala.js")
    assertTrue(matcher1.matches())
    matcher1.usePattern(patternNoDots)
    assertFalse(matcher1.matches())

    val patternWithOneGroup = Pattern.compile("ab(cd)efg")
    val patternWithTwoGroups = Pattern.compile("ab(cd)(ef)g")
    val matcher2 = patternWithOneGroup.matcher("Scala.js")
    assertEquals(1, matcher2.groupCount())
    matcher2.usePattern(patternWithTwoGroups)
    assertEquals(2, matcher2.groupCount())
  }

  @Test def lookingAt(): Unit = {
    val matcher0 = Pattern.compile("S[a-z]+").matcher("Scala")
    val matcher1 = Pattern.compile("S[a-z]+").matcher("Scala.js")
    val matcher2 = Pattern.compile("[a-z]+").matcher("Scala.js")

    assertTrue(matcher0.lookingAt())
    assertTrue(matcher1.lookingAt())
    assertFalse(matcher2.lookingAt())

    val matcher3 = Pattern.compile("S[a-z]+").matcher("Scala.js")
    assertTrue(matcher3.find())
    assertTrue(matcher3.lookingAt())
  }

  @Test def region(): Unit = {
    val matcher0 = Pattern.compile("S[a-z]+").matcher("A Scalable Solution")

    val region0to3 = matcher0.region(0, 3)
    assertEquals(0, region0to3.regionStart)
    assertEquals(3, region0to3.regionEnd)
    assertFalse(region0to3.find())

    val region0to15 = matcher0.region(0, 15)
    assertEquals(0, region0to15.regionStart)
    assertEquals(15, region0to15.regionEnd)
    assertTrue(region0to15.find())
    assertEquals("Scalable", region0to15.group)

    val region2to7 = region0to15.region(2, 7)
    assertEquals(2, region2to7.regionStart)
    assertEquals(7, region2to7.regionEnd)
    assertTrue(region2to7.find())
    assertEquals("Scala", region2to7.group)

    val region5toEnd = matcher0.region(5, matcher0.regionEnd)
    assertEquals(5, region5toEnd.regionStart)
    if (!executingInJVM) {
      assertEquals(19, region5toEnd.regionEnd)
      assertTrue(region5toEnd.find())
      assertEquals("Solution", region5toEnd.group)
    }

    val matcher1 = Pattern.compile("0[xX][A-Fa-f0-9]{3}$").matcher("In CSS, 0xc4fe is not a color")

    val region5to13 = matcher1.region(5, 13)
    assertEquals(5, region5to13.regionStart)
    assertEquals(13, region5to13.regionEnd)
    assertTrue(region5to13.find())
    assertEquals("0xc4f", region5to13.group)

    val region5to20 = matcher1.region(5, 20)
    assertEquals(5, region5to20.regionStart)
    assertEquals(20, region5to20.regionEnd)
    assertFalse(region5to20.find())
  }

  @Test def appendReplacement_and_appendTail(): Unit = {
    // From the JavaDoc
    val matcher = Pattern.compile("cat").matcher("one cat two cats in the yard")
    val sb = new StringBuffer

    while (matcher.find()) {
      matcher.appendReplacement(sb, "dog")
    }
    matcher.appendTail(sb)

    assertEquals("one dog two dogs in the yard", sb.toString)
  }

  @Test def replaceAll(): Unit = {
    // From the JavaDoc
    val matcher = Pattern.compile("a*b").matcher("aabfooaabfooabfoob")
    assertEquals("-foo-foo-foo-", matcher.replaceAll("-"))
  }

  @Test def replaceFirst(): Unit = {
    // From the JavaDoc
    val matcher = Pattern.compile("dog").matcher("zzzdogzzzdogzzz")
    assertEquals("zzzcatzzzdogzzz", matcher.replaceFirst("cat"))
  }

  @Test def should_throw_exception_if_match_accessors_are_called_before_find(): Unit = {
    def checkInvalidAccess(block: => Unit): Unit = {
      val exception: Throwable = try {
        block
        throw new Error("No exception thrown")
      } catch {
        case e: Throwable => e
      }

      assertEquals("java.lang.IllegalStateException", exception.getClass.getName)
      if (!executingInJVM) // On JVM the message is "No match found"
        assertEquals("No match available", exception.getMessage)
    }

    val matcher = Pattern.compile("(Sc([a-z]*))").matcher("Scala.js")

    checkInvalidAccess { matcher.start }
    checkInvalidAccess { matcher.end }
    checkInvalidAccess { matcher.group }
    checkInvalidAccess { matcher.group(42) }

    val matchResult = matcher.toMatchResult

    checkInvalidAccess { matchResult.start }
    checkInvalidAccess { matchResult.end }
    checkInvalidAccess { matchResult.group }
    checkInvalidAccess { matchResult.group(42) }
  }

  @Test def should_correctly_handle_zero_length_matches(): Unit = {
    val pat = Pattern.compile("a*?")
    val mat = pat.matcher("aaaaa")
    for (i <- 0 to 5) {
      assertTrue(mat.find())
      assertEquals(i, mat.start)
      assertEquals(i, mat.end)
    }

    // Make sure we don't suddenly re-match
    for (i <- 0 to 5) {
      assertFalse(mat.find())
    }
  }

  @Test def should_support_in_pattern_flags_issue_997(): Unit = {
    val p0 = Pattern.compile("(?i)abc")

    assertNotEquals(0, p0.flags() & Pattern.CASE_INSENSITIVE)

    val m0 = p0.matcher("abcABC")

    assertTrue(m0.find())
    assertEquals("abc", m0.group())
    assertTrue(m0.find())
    assertEquals("ABC", m0.group())
    assertFalse(m0.find())

    val p1 = Pattern.compile("(?-i)abc", Pattern.CASE_INSENSITIVE)

    assertEquals(0, p1.flags() & Pattern.CASE_INSENSITIVE)

    val m1 = p1.matcher("abcABC")

    assertTrue(m1.find())
    assertEquals("abc", m1.group())
    assertFalse(m1.find())
  }

  @Test def should_link_and_fail_on_group_of_String_issue_2381(): Unit = {
    val r = new Regex("a(b*)c", "Bee")
    val ms = r findAllIn "stuff abbbc more abc and so on"
    if (!executingInJVM)
      assertThrows(classOf[Exception], ms group "Ape")
    assertTrue(ms.hasNext)
    assertEquals("abbbc", ms.next())
    assertEquals("bbb", ms group "Bee")
    if (!executingInJVM)
      assertThrows(classOf[Exception], ms group "Ape")
    assertTrue(ms.hasNext)
    assertEquals("abc", ms.next())
    assertEquals("b", ms group "Bee")
    if (!executingInJVM)
      assertThrows(classOf[Exception], ms group "Ape")
    assertFalse(ms.hasNext)
  }
}
