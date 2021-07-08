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

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.Platform._
import org.scalajs.testsuite.utils.AssertThrows.assertThrows

class RegexMatcherTest  {

  @Test def find(): Unit = {
    val matcher = Pattern.compile("a").matcher("Scala.js")

    assertTrue(matcher.find())
    assertTrue(matcher.find())
    assertFalse(matcher.find())

    // Find with argument should discard region.
    matcher.region(0, 2)

    assertTrue(matcher.find(4))
    assertFalse(matcher.find())
  }

  @Test def startEndGroupAndToMatchResult(): Unit = {
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

  @Test def startEndGroupTrickyAndToMatchResult(): Unit = {
    val matcher = Pattern
      .compile("(Scala\\.js).*(Scala)$")
      .matcher("Scala.js is a Scalable javascript compiler based on Scala")
    checkGroups(matcher,
        (0, 57, "Scala.js is a Scalable javascript compiler based on Scala"),
        (0, 8, "Scala.js"),
        (52, 57, "Scala")
    )
  }

  @Test def startEndGroupMatchnotAndToMatchResult(): Unit = {
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

  @Test def startEndGroupMultipleAndToMatchResult(): Unit = {
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

  @Test def startEndGroupWithUnicodeCharClassAndToMatchResult(): Unit = {
    assumeTrue("requires \\p support", regexSupportsUnicodeCharacterClasses)

    val matcher = Pattern
      .compile("\\p{L}+(\\p{Nd}+)\\P{L}+")
      .matcher("  .-(0+  fooオ5१3.4१今   ")
    checkGroups(matcher,
        (9, 19, "fooオ5१3.4१"),
        (13, 16, "5१3")
    )
  }

  @Test def startEndGroupAndToMatchResultWithInlineFlags_Issue3406(): Unit = {
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

  @Test def startEndGroupWithRegion_Issue4204(): Unit = {
    val matcher = Pattern
      .compile("([a-z]+) ([a-z]+)( for)?")
      .matcher("This is only a test for group positions and regions")
      .region(5, 28)
    checkGroups(matcher,
        (5, 12, "is only"),
        (5, 7, "is"),
        (8, 12, "only"),
        (-1, -1, null) // make sure we don't add regionStart() to -1
    )
    // Call it a second time to make sure that chained `find()` operations work as well
    checkGroups(matcher,
        (13, 23, "a test for"),
        (13, 14, "a"),
        (15, 19, "test"),
        (19, 23, " for")
    )
    assertFalse(matcher.find())
  }

  @Test def groupStartEndKnowsTheDifferenceBetweenFindAndMatches(): Unit = {
    val p = Pattern.compile("a(b+?)(b)")

    val matcherForFind = p.matcher("abbbb")
    assertTrue(matcherForFind.find())
    assertEquals("b", matcherForFind.group(1))
    assertEquals(1, matcherForFind.start(1))
    assertEquals(2, matcherForFind.end(1))
    assertEquals("b", matcherForFind.group(2))
    assertEquals(2, matcherForFind.start(2))
    assertEquals(3, matcherForFind.end(2))

    val matcherForMatches = p.matcher("abbbb")
    assertTrue(matcherForMatches.matches())
    assertEquals("bbb", matcherForMatches.group(1))
    assertEquals(1, matcherForMatches.start(1))
    assertEquals(4, matcherForMatches.end(1))
    assertEquals("b", matcherForMatches.group(2))
    assertEquals(4, matcherForMatches.start(2))
    assertEquals(5, matcherForMatches.end(2))
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

  @Test def parseRegexTest(): Unit = {
    parseExpect("aa", "aa", 0 -> 2)
    parseExpect("a(a)", "aa", 0 -> 2, 1 -> 2)
    parseExpect("ABC(A(B))(C)", "ABCABC", 0 -> 6, 3 -> 5, 4 -> 5, 5 -> 6)
    parseExpect("A(?:A)", "AA", 0 -> 2)
    parseExpect("A(?:(\\d))", "A1", 0 -> 2, 1 -> 2)
    parseExpect("A((?:A))", "AA", 0 -> 2, 1 -> 2)
    parseExpect("ab((ab))", "abab", 0 -> 4, 2 -> 4,  2 -> 4)
    parseExpect("hum(hum)?", "humhum", 0 -> 6, 3 -> 6)
    parseExpect("hum(hum)?", "hum", 0 -> 3, -1 -> -1)
    parseExpect("hum(?=hum)(.)", "humhuma", 0 -> 4, 3 -> 4)
    parseExpect("hum(?!hum)(.)", "humhumhufa", 3 -> 7, 6 -> 7)
    parseExpect("hum(?=h(um))(.)", "humhuma", 0 -> 4, 4 -> 6, 3 -> 4)
    if (regexSupportsLookBehinds) {
      parseExpect("(.)(?<=hum)(h)um", "ahumhum", 3 -> 7, 3 -> 4, 4 -> 5)
      parseExpect("(.)(?<!hum)(h)um", "ahumhum", 0 -> 4, 0 -> 1, 1 -> 2)
      parseExpect("(.)(?<=(hu)m)(h)um", "ahumhum", 3 -> 7, 3 -> 4, 1 -> 3, 4 -> 5)
    }
    parseExpect("abab(ab){1,2}", "abababab", 0 -> 8, 6 -> 8)
    parseExpect("abab(ab){1,2}(abc){1,2}", "abababababcabc", 0 -> 14, 6 -> 8, 11 -> 14)
    parseExpect("ab(ab)?ab(?=aba)(ab)*", "abababababa", 0 -> 10, 2 -> 4, 8 -> 10)
    parseExpect("ab(?=aba)ab(aba)?(ab)*", "abababababa", 0 -> 7, 4 -> 7, -1 -> -1)
    parseExpect("ab(?=aba)ab(aba)??(ab)*","abababababa", 0 -> 10, -1 -> -1, 8 -> 10)
    parseExpect("abab(ab){1,2}?", "abababab", 0 -> 6, 4 -> 6)
    parseExpect("ab(?:ab)*", "abababab", 0 -> 8)
    if (!executingInJVM) {
      parseExpect("ab(?:a(c))*ac", "abacacac", 0 -> 8, 5 -> 6) // JVM: 7 -> 8
      parseExpect("ab(?:a(c))+ac", "abacacac", 0 -> 8, 5 -> 6) // JVM: 7 -> 8
    }
    parseExpect("ab(?:ac)*?ac", "abacacac", 0 -> 4)
    parseExpect("ab(?:ac)+?ac", "abacacac", 0 -> 6)
    parseExpect("ab(?:(c){2})*d", "abccccd", 0 -> 7, 5 -> 6)
    parseExpect("ab((?=abab(ab))a(b))*a", "abababab", 0 -> 5, 2 -> 4, 6 -> 8, 3 -> 4)
    parseExpect("(?!(a))(b)", "b", 0 -> 1, -1 -> -1, 0 -> 1) // #3901
    parseExpect("(\\x{D834}\\x{DD1E}a|\\x{1D11E})(a)", "a\uD834\uDD1Eaaa", 1 -> 4, 1 -> 3, 3 -> 4)
    parseExpect("a\uD834\uDD1E*(.)", "bca\uD834\uDD1E\uD834\uDD1Edef", 2 -> 8, 7 -> 8)
  }

  @Test def parseRegexBackgroupsTest(): Unit = {
    parseExpect("bc(.c).c(\\1)", "bczcxczc", 0 -> 8, 2 -> 4, 6 -> 8)
    parseExpect("(bc(.c).c)(\\2)", "bczcxczc", 0 -> 8, 0 -> 6, 2 -> 4, 6 -> 8)
  }

  @Test def parseRegexDisjunctionsTest(): Unit = {
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

  @Test def matchersFromTheSamePatternAreIndependent(): Unit = {
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
    matcher.region(1, 3)
    assertFalse(matcher.find())
    matcher.reset() // test we reset region
    assertTrue(matcher.find())
  }

  @Test def resetString(): Unit = {
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

    assertSame(matcher0, matcher0.region(0, 3))
    assertFalse(matcher0.lookingAt())
    assertFalse(matcher0.matches())
    assertFalse(matcher0.find())
    assertEquals(0, matcher0.regionStart)
    assertEquals(3, matcher0.regionEnd)

    matcher0.region(0, 15)
    assertFalse(matcher0.lookingAt())
    assertFalse(matcher0.matches())
    assertTrue(matcher0.find())
    assertEquals("Scalable", matcher0.group)
    assertEquals(0, matcher0.regionStart)
    assertEquals(15, matcher0.regionEnd)

    matcher0.region(2, 7)
    assertEquals(2, matcher0.regionStart)
    assertEquals(7, matcher0.regionEnd)
    assertTrue(matcher0.lookingAt())
    assertTrue(matcher0.matches())
    matcher0.region(2, 7)
    assertTrue(matcher0.find())
    assertEquals("Scala", matcher0.group)
    assertEquals(2, matcher0.regionStart)
    assertEquals(7, matcher0.regionEnd)

    matcher0.region(2, 12)
    assertTrue(matcher0.lookingAt())
    assertFalse(matcher0.matches())
    matcher0.region(2, 12)
    assertTrue(matcher0.find())
    assertEquals("Scalable", matcher0.group)
    assertEquals(2, matcher0.regionStart)
    assertEquals(12, matcher0.regionEnd)
    assertFalse(matcher0.find())

    matcher0.region(5, 19)
    assertFalse(matcher0.lookingAt())
    assertFalse(matcher0.matches())
    assertTrue(matcher0.find())
    assertEquals("Solution", matcher0.group)
    assertEquals(5, matcher0.regionStart)
    assertEquals(19, matcher0.regionEnd)

    val matcher1 = Pattern.compile("0[xX][A-Fa-f0-9]{3}$").matcher("In CSS, 0xc4fe is not a color")

    matcher1.region(5, 13)
    assertFalse(matcher1.lookingAt())
    assertFalse(matcher1.matches())
    assertTrue(matcher1.find())
    assertEquals("0xc4f", matcher1.group)
    assertEquals(5, matcher1.regionStart)
    assertEquals(13, matcher1.regionEnd)

    matcher1.region(5, 20)
    assertFalse(matcher1.lookingAt())
    assertFalse(matcher1.matches())
    assertFalse(matcher1.find())
    assertEquals(5, matcher1.regionStart)
    assertEquals(20, matcher1.regionEnd)
  }

  @Test def appendReplacementAndAppendTail(): Unit = {
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
    val matcher = Pattern
      .compile("a*b")
      .matcher("aabfooaabfooabfoob")
      .region(0, 3) // should be discarded
    assertEquals("-foo-foo-foo-", matcher.replaceAll("-"))
  }

  @Test def replaceAllIn(): Unit = {
    val matcher = Pattern
      .compile("(?:(ab)|(a))c(d)|(e)")
      .matcher("abcd")
    assertEquals("1=ab,2=,3=d,4=", matcher.replaceAll("1=$1,2=$2,3=$3,4=$4"))
  }

  @Test def replaceFirst(): Unit = {
    // From the JavaDoc
    val matcher = Pattern
      .compile("dog")
      .matcher("zzzdogzzzdogzzz")
      .region(0, 3) // should be discarded
    assertEquals("zzzcatzzzdogzzz", matcher.replaceFirst("cat"))
  }

  @Test def usingMatchAccessorsBeforeFindThrows(): Unit = {
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

  @Test def zeroLengthMatches(): Unit = {
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

  @Test def inPatternFlags_Issue997(): Unit = {
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

  @Test def groupAndGroupName_Issue2381(): Unit = {
    val p = Pattern.compile("a(?<Ape>b*)c")
    val m = p.matcher("stuff abbbc more abc and so on")
    assertThrows(classOf[IllegalStateException], m.group("Ape"))
    assertTrue(m.find())
    assertEquals("abbbc", m.group())
    assertEquals("bbb", m.group("Ape"))
    assertThrows(classOf[IllegalArgumentException], m.group("Bee"))
    assertTrue(m.find())
    assertEquals("abc", m.group())
    assertEquals("b", m.group("Ape"))
    assertThrows(classOf[IllegalArgumentException], m.group("Bee"))
    assertFalse(m.find())
  }

  @Test def groupAndGroupNameThroughScalaRegex_Issue2381(): Unit = {
    import scala.util.matching.Regex

    val r = new Regex("a(b*)c", "Bee")
    val ms = r.findAllIn("stuff abbbc more abc and so on")
    assertTrue(ms.hasNext)
    assertEquals("abbbc", ms.next())
    assertEquals("bbb", ms.group("Bee"))
    assertTrue(ms.hasNext)
    assertEquals("abc", ms.next())
    assertEquals("b", ms.group("Bee"))
    assertFalse(ms.hasNext)
  }
}
