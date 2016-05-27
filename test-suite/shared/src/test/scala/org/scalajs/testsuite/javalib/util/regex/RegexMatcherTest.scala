/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
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
    val matcher = Pattern.compile("\\s(([A-Za-z]{5}(hum)?).js)\\s").matcher("Write Scala.js everyday!")

    def checkGroup0(start: Int, end: Int, group: String): Unit =
      checkGroup(start, 5, end, 15, group, " Scala.js ")

    def checkGroup1(start: Int, end: Int, group: String): Unit =
      checkGroup(start, 6, end, 14, group, "Scala.js")

    def checkGroup2(start: Int, end: Int, group: String): Unit =
      checkGroup(start, 6, end, 11, group, "Scala")

    def checkGroup3(start: Int, end: Int, group: String): Unit =
      checkGroup(start, -1, end, -1, group, null)

    def checkGroup(start: Int, startExpected: Int, end: Int, endExpected: Int,
                   group: String, groupExpected: String): Unit = {
      assertEquals(startExpected, start)
      assertEquals(endExpected, end)
      assertEquals(groupExpected, group)
    }

    assertTrue(matcher.find())
    assertEquals(3, matcher.groupCount)
    checkGroup0(matcher.start, matcher.end, matcher.group)
    checkGroup0(matcher.start(0), matcher.end(0), matcher.group(0))
    checkGroup1(matcher.start(1), matcher.end(1), matcher.group(1))
    checkGroup2(matcher.start(2), matcher.end(2), matcher.group(2))
    checkGroup3(matcher.start(3), matcher.end(3), matcher.group(3))

    val matchResult = matcher.toMatchResult
    assertEquals(3, matchResult.groupCount)
    checkGroup0(matchResult.start, matchResult.end, matchResult.group)
    checkGroup0(matchResult.start(0), matchResult.end(0), matchResult.group(0))
    checkGroup1(matchResult.start(1), matchResult.end(1), matchResult.group(1))
    checkGroup2(matchResult.start(2), matchResult.end(2), matchResult.group(2))
    checkGroup3(matchResult.start(3), matchResult.end(3), matchResult.group(3))
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

  @Test def hitEnd(): Unit = {
    val matcher0 = Pattern.compile("S[a-z]*").matcher("Scala.js")
    assertTrue(matcher0.find())
    assertFalse(matcher0.hitEnd)
    assertFalse(matcher0.find())
    assertTrue(matcher0.hitEnd)

    val matcher1 = Pattern.compile("[A-Za-z]+").matcher("Scala.js")
    assertTrue(matcher1.find())
    assertFalse(matcher1.hitEnd)
    assertEquals("Scala", matcher1.group)
    assertTrue(matcher1.find())
    assertTrue(matcher1.hitEnd)
    assertEquals("js", matcher1.group)
    assertTrue(matcher1.lookingAt())
    assertEquals("Scala", matcher1.group)
    assertFalse(matcher1.hitEnd)
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

    if (!executingInJVM) {
      assertThrows(classOf[Exception], new Regex("a(?<Bee>b*)c"))
      assertThrows(classOf[Exception], Pattern.compile("a(?<Bee>b*)c"))
    }
  }
}
