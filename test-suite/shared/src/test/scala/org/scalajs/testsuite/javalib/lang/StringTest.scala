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

package org.scalajs.testsuite.javalib.lang

import java.nio.charset.Charset

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows.{assertThrows, assertThrowsNPEIfCompliant}
import org.scalajs.testsuite.utils.Platform._

class StringTest {

  @Test def lengthTest(): Unit = {
    assertEquals(8, "Scala.js".length)
    assertEquals(0, "".length)
  }

  @Test def intern(): Unit = {
    val s = "Scala.js"
    assertEquals(s, s.intern)
  }

  @Test def equalsTest(): Unit = {
    assertTrue("Scala.js".equals("Scala.js"))
    assertFalse("Scala.js".equals("Java"))
  }

  @Test def equalsIgnoreCase(): Unit = {
    assertTrue("Scala.JS".equalsIgnoreCase("Scala.js"))
    assertTrue("åløb".equalsIgnoreCase("ÅLØb"))
    assertFalse("Scala.js".equalsIgnoreCase("Java"))
    assertFalse("Scala.js".equalsIgnoreCase(null))

    // Case folding that changes the string length are not supported,
    // therefore ligatures are not equal to their expansion.
    // U+FB00 LATIN SMALL LIGATURE FF
    assertFalse("Eﬀet".equalsIgnoreCase("effEt"))
    assertFalse("Eﬀet".equalsIgnoreCase("eFFEt"))

    // "ı" and 'i' are considered equal, as well as their uppercase variants
    assertTrue("ıiIİ ıiIİ ıiIİ ıiIİ".equalsIgnoreCase("ıııı iiii IIII İİİİ"))

    // null is a valid input
    assertFalse("foo".equalsIgnoreCase(null))
  }

  @Test def compareTo(): Unit = {
    assertEquals(3, "Scala.js".compareTo("Scala"))
    assertEquals(0, "Scala.js".compareTo("Scala.js"))
    assertEquals(-15, "Scala.js".compareTo("banana"))
  }

  @Test def compareToIgnoreCase(): Unit = {
    assertEquals(0, "Scala.JS".compareToIgnoreCase("Scala.js"))
    assertEquals(3, "Scala.JS".compareToIgnoreCase("scala"))
    assertEquals(0, "åløb".compareToIgnoreCase("ÅLØB"))
    assertEquals(-9, "Java".compareToIgnoreCase("Scala"))

    // Case folding that changes the string length are not supported,
    // therefore ligatures are not equal to their expansion.
    // U+FB00 LATIN SMALL LIGATURE FF
    assertEquals(64154, "Eﬀet".compareToIgnoreCase("effEt"))
    assertEquals(64154, "Eﬀet".compareToIgnoreCase("eFFEt"))

    // "ı" and 'i' are considered equal, as well as their uppercase variants
    assertEquals(0, "ıiIİ ıiIİ ıiIİ ıiIİ".compareToIgnoreCase("ıııı iiii IIII İİİİ"))
  }

  @Test def isEmpty(): Unit = {
    assertFalse("Scala.js".isEmpty)
    assertTrue("".isEmpty)
  }

  @Test def contains(): Unit = {
    assertTrue("Scala.js".contains("Scala"))
    assertTrue("Scala.js".contains("Scala.js"))
    assertTrue("ananas".contains("na"))
    assertFalse("Scala.js".contains("scala"))
  }

  @Test def startWith(): Unit = {
    assertTrue("Scala.js".startsWith("Scala"))
    assertTrue("Scala.js".startsWith("Scala.js"))
    assertFalse("Scala.js".startsWith("scala"))
    assertTrue("ananas".startsWith("an"))

    assertThrowsNPEIfCompliant("ananas".startsWith(null))
  }

  @Test def endsWith(): Unit = {
    assertTrue("Scala.js".endsWith("js"))
    assertTrue("Scala.js".endsWith("Scala.js"))
    assertFalse("Scala.js".endsWith("JS"))
    assertTrue("banana".endsWith("na"))

    assertThrowsNPEIfCompliant("banana".endsWith(null))
  }

  @Test def indexOfString(): Unit = {
    assertEquals(6, "Scala.js".indexOf("js"))
    assertEquals(0, "Scala.js".indexOf("Scala.js"))
    assertEquals(1, "ananas".indexOf("na"))
    assertEquals(-1, "Scala.js".indexOf("Java"))
  }

  @Test def indexOfInt(): Unit = {
    assertEquals(0, "abc\uD834\uDF06def\uD834\uDF06def".indexOf(0x61))
    assertEquals(3, "abc\uD834\uDF06def\uD834\uDF06def".indexOf(0x1d306))
    assertEquals(3, "abc\uD834\uDF06def\uD834\uDF06def".indexOf(0xd834))
    assertEquals(4, "abc\uD834\uDF06def\uD834\uDF06def".indexOf(0xdf06))
    assertEquals(5, "abc\uD834\uDF06def\uD834\uDF06def".indexOf(0x64))
  }

  @Test def lastIndexOfString(): Unit = {
    assertEquals(0, "Scala.js".lastIndexOf("Scala.js"))
    assertEquals(3, "ananas".lastIndexOf("na"))
    assertEquals(-1, "Scala.js".lastIndexOf("Java"))
    assertEquals(-1, "Negative index".lastIndexOf("N", -5))
  }

  @Test def lastIndexOfInt(): Unit = {
    assertEquals(0, "abc\uD834\uDF06def\uD834\uDF06def".lastIndexOf(0x61))
    assertEquals(8, "abc\uD834\uDF06def\uD834\uDF06def".lastIndexOf(0x1d306))
    assertEquals(8, "abc\uD834\uDF06def\uD834\uDF06def".lastIndexOf(0xd834))
    assertEquals(9, "abc\uD834\uDF06def\uD834\uDF06def".lastIndexOf(0xdf06))
    assertEquals(10, "abc\uD834\uDF06def\uD834\uDF06def".lastIndexOf(0x64))
    assertEquals(-1, "abc\uD834\uDF06def\uD834\uDF06def".lastIndexOf(0x64, -1))
  }

  @Test def toUpperCase(): Unit = {
    assertEquals("SCALA.JS", "Scala.js".toUpperCase())
  }

  @Test def toLowerCase(): Unit = {
    assertEquals("scala.js", "Scala.js".toLowerCase())
  }

  @Test def charAt(): Unit = {
    @noinline def testNoInline(expected: Char, s: String, i: Int): Unit =
      assertEquals(expected, s.charAt(i))

    @inline def test(expected: Char, s: String, i: Int): Unit = {
      testNoInline(expected, s, i)
      assertEquals(expected, s.charAt(i))
    }

    test('S', "Scala.js", 0)
    test('.', "Scala.js", 5)
    test('s', "Scala.js", 7)
    test('o', "foo", 1)
  }

  @Test def charAtIndexOutOfBounds(): Unit = {
    assumeTrue("Assuming compliant StringIndexOutOfBounds",
        hasCompliantStringIndexOutOfBounds)

    def test(s: String, i: Int): Unit = {
      val e = assertThrows(classOf[StringIndexOutOfBoundsException], s.charAt(i))
      assertTrue(e.getMessage(), e.getMessage().contains(i.toString()))
    }

    test("foo", -1)
    test("foo", -10000)
    test("foo", Int.MinValue)
    test("foo", 3)
    test("foo", 10000)
    test("foo", Int.MaxValue)

    test("", -1)
    test("", 0)
    test("", 1)

    // Test non-constant-folding
    assertThrows(classOf[StringIndexOutOfBoundsException], "foo".charAt(4))
  }

  @Test def codePointAt(): Unit = {
    // String that starts with a BMP symbol
    assertEquals(0x61, "abc\uD834\uDF06def".codePointAt(0))
    assertEquals(0x1d306, "abc\uD834\uDF06def".codePointAt(3))
    assertEquals(0xdf06, "abc\uD834\uDF06def".codePointAt(4))
    assertEquals(0x64, "abc\uD834\uDF06def".codePointAt(5))

    // String that starts with an astral symbol
    assertEquals(0x1d306, "\uD834\uDF06def".codePointAt(0))
    assertEquals(0xdf06, "\uD834\uDF06def".codePointAt(1))

    // Lone high surrogates
    assertEquals(0xd834, "\uD834abc".codePointAt(0))

    // Lone low surrogates
    assertEquals(0xdf06, "\uDF06abc".codePointAt(0))
    assertEquals(0xd834, "abc\uD834".codePointAt(3))
  }

  @Test def codePointAtIndexOutOfBounds(): Unit = {
    assumeTrue("Assuming compliant StringIndexOutOfBounds",
        hasCompliantStringIndexOutOfBounds)

    assertThrows(classOf[StringIndexOutOfBoundsException],
        "abc\ud834\udf06def".codePointAt(-1))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        "abc\ud834\udf06def".codePointAt(8))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        "abc\ud834\udf06def".codePointAt(15))
  }

  @Test def codePointBefore(): Unit = {
    assertEquals(0x61, "abc\ud834\udf06def".codePointBefore(1))
    assertEquals(0x1d306, "abc\ud834\udf06def".codePointBefore(5))
    assertEquals(0xd834, "abc\ud834\udf06def".codePointBefore(4))
    assertEquals(0x64, "abc\ud834\udf06def".codePointBefore(6))
    assertEquals('f'.toInt, "abc\ud834\udf06def".codePointBefore(8))
    assertEquals(0x1d306, "\ud834\udf06def".codePointBefore(2))
    assertEquals(0xd834, "\ud834\udf06def".codePointBefore(1))
    assertEquals(0xd834, "\ud834abc".codePointBefore(1))
    assertEquals(0xdf06, "\udf06abc".codePointBefore(1))
  }

  @Test def codePointBeforeIndexOutOfBounds(): Unit = {
    assumeTrue("Assuming compliant StringIndexOutOfBounds",
        hasCompliantStringIndexOutOfBounds)

    assertThrows(classOf[StringIndexOutOfBoundsException],
        "abc\ud834\udf06def".codePointBefore(-5))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        "abc\ud834\udf06def".codePointBefore(0))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        "abc\ud834\udf06def".codePointBefore(9))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        "abc\ud834\udf06def".codePointBefore(15))
  }

  @Test def codePointCount(): Unit = {
    val s = "abc\uD834\uDF06de\uD834\uDF06fgh\uD834ij\uDF06\uD834kl\uDF06"
    assertEquals(18, s.codePointCount(0, s.length))
    assertEquals(1, s.codePointCount(3, 5))
    assertEquals(1, s.codePointCount(2, 3))
    assertEquals(2, s.codePointCount(2, 4))
    assertEquals(2, s.codePointCount(2, 5))
    assertEquals(3, s.codePointCount(2, 6))
    assertEquals(5, s.codePointCount(12, 17))
    assertEquals(2, s.codePointCount(8, 10))
    assertEquals(2, s.codePointCount(7, 10))
    assertEquals(0, s.codePointCount(7, 7))
    assertEquals(1, s.codePointCount(s.length - 1, s.length))
    assertEquals(0, s.codePointCount(s.length - 1, s.length - 1))
    assertEquals(0, s.codePointCount(s.length, s.length))

    assertThrows(classOf[IndexOutOfBoundsException], s.codePointCount(-3, 0))
    assertThrows(classOf[IndexOutOfBoundsException], s.codePointCount(-3, 4))
    assertThrows(classOf[IndexOutOfBoundsException], s.codePointCount(6, 2))
    assertThrows(classOf[IndexOutOfBoundsException], s.codePointCount(10, 30))
    assertThrows(classOf[IndexOutOfBoundsException], s.codePointCount(10, 0))
  }

  @Test def offsetByCodePoints(): Unit = {
    val s = "abc\uD834\uDF06de\uD834\uDF06fgh\uD834ij\uDF06\uD834kl\uDF06"

    assertEquals(s.length, s.offsetByCodePoints(0, 18))
    assertEquals(5, s.offsetByCodePoints(3, 1))
    assertEquals(3, s.offsetByCodePoints(2, 1))
    assertEquals(5, s.offsetByCodePoints(2, 2))
    assertEquals(6, s.offsetByCodePoints(2, 3))
    assertEquals(17, s.offsetByCodePoints(12, 5))
    assertEquals(10, s.offsetByCodePoints(8, 2))
    assertEquals(10, s.offsetByCodePoints(7, 2))
    assertEquals(7, s.offsetByCodePoints(7, 0))
    assertEquals(s.length, s.offsetByCodePoints(s.length - 1, 1))
    assertEquals(s.length - 1, s.offsetByCodePoints(s.length - 1, 0))
    assertEquals(s.length, s.offsetByCodePoints(s.length, 0))

    assertThrows(classOf[IndexOutOfBoundsException], s.offsetByCodePoints(-3, 4))
    assertThrows(classOf[IndexOutOfBoundsException], s.offsetByCodePoints(6, 18))
    assertThrows(classOf[IndexOutOfBoundsException], s.offsetByCodePoints(30, 2))
  }

  @Test def offsetByCodePointsBackwards(): Unit = {
    val s = "abc\uD834\uDF06de\uD834\uDF06fgh\uD834ij\uDF06\uD834kl\uDF06"

    assertEquals(0, s.offsetByCodePoints(s.length, -18))
    assertEquals(3, s.offsetByCodePoints(5, -1))
    assertEquals(2, s.offsetByCodePoints(3, -1))
    assertEquals(2, s.offsetByCodePoints(4, -2))
    assertEquals(2, s.offsetByCodePoints(5, -2))
    assertEquals(2, s.offsetByCodePoints(6, -3))
    assertEquals(12, s.offsetByCodePoints(17, -5))
    assertEquals(7, s.offsetByCodePoints(10, -2))
    assertEquals(7, s.offsetByCodePoints(7, -0))
    assertEquals(s.length - 1, s.offsetByCodePoints(s.length, -1))
    assertEquals(s.length - 1, s.offsetByCodePoints(s.length - 1, -0))
    assertEquals(s.length, s.offsetByCodePoints(s.length, -0))

    assertThrows(classOf[IndexOutOfBoundsException], s.offsetByCodePoints(-3, -4))
    assertThrows(classOf[IndexOutOfBoundsException], s.offsetByCodePoints(6, -18))
    assertThrows(classOf[IndexOutOfBoundsException], s.offsetByCodePoints(30, -2))
  }

  @Test def substringBegin(): Unit = {
    assertEquals("", "".substring(0))
    assertEquals("", "foo".substring(3))
    assertEquals("", "hello".substring(5))
    assertEquals("lo", "hello".substring(3))
    assertEquals("baz", "foo bar baz".substring(8))
    assertEquals("foo bar baz", "foo bar baz".substring(0))
  }

  @Test def substringBeginIndexOutOfBounds(): Unit = {
    assumeTrue("Assuming compliant StringIndexOutOfBounds",
        hasCompliantStringIndexOutOfBounds)

    assertThrows(classOf[StringIndexOutOfBoundsException],
        "foo".substring(-1))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        "foo".substring(4))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        "foo".substring(15))

    assertThrows(classOf[StringIndexOutOfBoundsException],
        "".substring(-1))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        "".substring(1))
  }

  @Test def substringBeginEnd(): Unit = {
    assertEquals("", "".substring(0, 0))
    assertEquals("", "foo".substring(3, 3))
    assertEquals("", "hello".substring(3, 3))
    assertEquals("lo", "hello".substring(3, 5))
    assertEquals("bar", "foo bar baz".substring(4, 7))
    assertEquals("foo bar baz", "foo bar baz".substring(0, 11))
    assertEquals("foo bar", "foo bar baz".substring(0, 7))
  }

  @Test def substringBeginEndIndexOutOfBounds(): Unit = {
    assumeTrue("Assuming compliant StringIndexOutOfBounds",
        hasCompliantStringIndexOutOfBounds)

    assertThrows(classOf[StringIndexOutOfBoundsException],
        "foo".substring(-1, 1))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        "foo".substring(4, 4))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        "foo".substring(1, 4))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        "foo".substring(-1, 4))

    assertThrows(classOf[StringIndexOutOfBoundsException],
        "foo".substring(2, 1))

    assertThrows(classOf[StringIndexOutOfBoundsException],
        "".substring(-1, -1))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        "".substring(1, 1))
  }

  @Test def subSequence(): Unit = {
    assertEquals("Scala", "Scala.js".subSequence(0, 5))
    assertEquals("js", "Scala.js".subSequence(6, 8))
    assertEquals("la", "Scala.js".subSequence(3, 5))
    assertEquals("", "Scala.js".subSequence(3, 3))
  }

  @Test def subSequenceIndexOutOfBounds(): Unit = {
    assumeTrue("Assuming compliant StringIndexOutOfBounds",
        hasCompliantStringIndexOutOfBounds)

    assertThrows(classOf[StringIndexOutOfBoundsException],
        "foo".subSequence(-1, 1))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        "foo".subSequence(4, 4))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        "foo".subSequence(1, 4))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        "foo".subSequence(-1, 4))

    assertThrows(classOf[StringIndexOutOfBoundsException],
        "foo".subSequence(2, 1))

    assertThrows(classOf[StringIndexOutOfBoundsException],
        "".subSequence(-1, -1))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        "".subSequence(1, 1))
  }

  @Test def replace(): Unit = {
    assertEquals("Scala", "Scala.js".replace(".js", ""))
    assertEquals("Scala.js", "Scala.js".replace("JS", ""))
    assertEquals("bb", "aa".replace('a', 'b')) // #25
  }

  @Test def matches(): Unit = {
    assertTrue("Scala.js".matches(".*js"))
    assertFalse("Scala.js".matches(".*JS"))
  }

  @Test def split(): Unit = {
    assertArrayEquals(Array[AnyRef]("Sc", "l", ".js"), erased("Scala.js".split("a")))
    assertArrayEquals(Array[AnyRef]("a", "s", "d", "f"), erased("asdf".split("")))
    assertArrayEquals(Array[AnyRef]("a", "s", "d", "f", ""), erased("asdf".split("", -1)))
  }

  @Test def splitWithCharAsArgument(): Unit = {
    assertArrayEquals(Array[AnyRef]("Scala", "js"), erased("Scala.js".split('.')))
    for (i <- 0 to 32) {
      val c = i.toChar
      assertArrayEquals(Array[AnyRef]("blah", "blah", "blah", "blah"),
          erased(s"blah${c}blah${c}blah${c}blah".split(c)))
    }
  }

  @Test def startsWithPrefixToffset_Issue1603(): Unit = {
    assertTrue("Scala.js".startsWith("ala", 2))
    assertTrue("Scala.js".startsWith("Scal", 0))

    assertTrue("Scala.js".startsWith("", 3))
    assertTrue("Scala.js".startsWith("", 0))
    assertTrue("Scala.js".startsWith("", 8))

    assertFalse("Scala.js".startsWith("ala", 0))
    assertFalse("Scala.js".startsWith("Scal", 2))

    assertFalse("Scala.js".startsWith("Sc", -1))
    assertFalse("Scala.js".startsWith(".js", 10))
    assertFalse("Scala.js".startsWith("", -1))
    assertFalse("Scala.js".startsWith("", 9))

    assertThrowsNPEIfCompliant("Scala.js".startsWith(null, 2))
  }

  @Test def toCharArray(): Unit = {
    assertEquals('.', "Scala.js".toCharArray()(5))
  }

  @Test def hashCodeTest(): Unit = {
    assertEquals(-1395193631, "a`jkxzcbfaslkjfbkj,289oinkasdf".hashCode())
    assertEquals(44878, "-34".hashCode())
    assertEquals(0, "".hashCode())
  }

  @Test def getChars(): Unit = {
    val trg = new Array[Char](10)
    "asdf_foo".getChars(2, 6, trg, 3)
    val exp = Array(0, 0, 0, 'd', 'f', '_', 'f', 0, 0, 0)

    for ((i, e) <- trg zip exp) {
      assertEquals(e, i.toInt)
    }
  }

  @Test def concat(): Unit = {
    assertEquals("asdffdsa", "asdf".concat("fdsa"))
  }

  @Test def constructors(): Unit = {
    val charArray =
      Array('a', 'b', 'c', 'd', '\uD834', '\uDF06', 'e', 'f', 'g', 'h', 'i')
    val codePointArray =
      Array(65, 0x1d306, 67, 68, 0xd834, 69, 72, 0xdf06)

    assertEquals("", new String())
    assertEquals("abcd\uD834\uDF06efghi", new String(charArray))
    assertEquals("d\uD834\uDF06ef", new String(charArray, 3, 5))
    assertEquals("\uD834\uDF06CD\uD834E", new String(codePointArray, 1, 5))
    assertEquals("foo", new String("foo"))
    assertEquals("buffer-foo", new String(new StringBuffer("buffer-foo")))
    assertEquals(new String(new java.lang.StringBuilder("builder-foo")), "builder-foo")
  }

  @Test def format(): Unit = {
    assertEquals("5", String.format("%d", new Integer(5)))
    assertEquals("00005", String.format("%05d", new Integer(5)))
    assertEquals("0x005", String.format("%0#5x", new Integer(5)))
    assertEquals("  0x5", String.format("%#5x", new Integer(5)))
    assertEquals("  0X5", String.format("%#5X", new Integer(5)))
    assertEquals("  -10", String.format("%5d", new Integer(-10)))
    assertEquals("-0010", String.format("%05d", new Integer(-10)))
    assertEquals("fffffffd", String.format("%x", new Integer(-3)))
    if (!executingInJVM)
      assertEquals("fffffffc", String.format("%x", new java.lang.Byte(-4.toByte)))
  }

  @Test def getBytes(): Unit = {

    assertArrayEquals("hello-world".getBytes(Charset.forName("UTF-8")),
        Array[Byte](104, 101, 108, 108, 111, 45, 119, 111, 114, 108, 100))
    assertArrayEquals("ᚠᛇᚻ᛫ᛒᛦᚦ᛫ᚠᚱᚩᚠᚢᚱ᛫ᚠᛁᚱᚪ᛫ᚷᛖᚻᚹᛦᛚᚳᚢᛗ".getBytes(Charset.forName("UTF-16")),
        Array[Byte](-2, -1, 22, -96, 22, -57, 22, -69, 22, -21, 22, -46, 22, -26,
            22, -90, 22, -21, 22, -96, 22, -79, 22, -87, 22, -96, 22, -94, 22,
            -79, 22, -21, 22, -96, 22, -63, 22, -79, 22, -86, 22, -21, 22,
            -73, 22, -42, 22, -69, 22, -71, 22, -26, 22, -38, 22, -77, 22,
            -94, 22, -41))
  }

  @Test def regionMatches(): Unit = {
    /* Ported from
     * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/lang/StringTest.java
     */
    val test = "abcdef"

    assertTrue(test.regionMatches(1, "bcd", 0, 3))
    assertTrue(test.regionMatches(1, "bcdx", 0, 3))
    assertFalse(test.regionMatches(1, "bcdx", 0, 4))
    assertFalse(test.regionMatches(1, "bcdx", 1, 3))
    assertTrue(test.regionMatches(true, 0, "XAbCd", 1, 4))
    assertTrue(test.regionMatches(true, 1, "BcD", 0, 3))
    assertTrue(test.regionMatches(true, 1, "bCdx", 0, 3))
    assertFalse(test.regionMatches(true, 1, "bCdx", 0, 4))
    assertFalse(test.regionMatches(true, 1, "bCdx", 1, 3))
    assertTrue(test.regionMatches(true, 0, "xaBcd", 1, 4))

    val testU = test.toUpperCase()
    assertTrue(testU.regionMatches(true, 0, "XAbCd", 1, 4))
    assertTrue(testU.regionMatches(true, 1, "BcD", 0, 3))
    assertTrue(testU.regionMatches(true, 1, "bCdx", 0, 3))
    assertFalse(testU.regionMatches(true, 1, "bCdx", 0, 4))
    assertFalse(testU.regionMatches(true, 1, "bCdx", 1, 3))
    assertTrue(testU.regionMatches(true, 0, "xaBcd", 1, 4))

    // scalastyle:off line.size.limit
    /* If len is negative, you must return true in some cases. See
     * http://docs.oracle.com/javase/8/docs/api/java/lang/String.html#regionMatches-boolean-int-java.lang.String-int-int-
     */
    // scalastyle:on line.size.limit

    // four cases that are false, irrelevant of sign of len nor the value of the other string
    assertFalse(test.regionMatches(-1, test, 0, -4))
    assertFalse(test.regionMatches(0, test, -1, -4))
    assertFalse(test.regionMatches(100, test, 0, -4))
    assertFalse(test.regionMatches(0, test, 100, -4))

    // offset + len > length
    assertFalse(test.regionMatches(3, "defg", 0, 4)) // on receiver string
    assertFalse(test.regionMatches(3, "abcde", 3, 3)) // on other string
    assertFalse(test.regionMatches(Int.MaxValue, "ab", 0, 1)) // #4878 overflow, large toffset
    assertFalse(test.regionMatches(0, "ab", Int.MaxValue, 1)) // #4878 overflow, large ooffset
    assertFalse(test.regionMatches(1, "ab", 1, Int.MaxValue)) // #4878 overflow, large len
    assertFalse(test.regionMatches(true, 3, "defg", 0, 4)) // on receiver string
    assertFalse(test.regionMatches(true, 3, "abcde", 3, 3)) // on other string
    assertFalse(test.regionMatches(true, Int.MaxValue, "ab", 0, 1)) // #4878 overflow, large toffset
    assertFalse(test.regionMatches(true, 0, "ab", Int.MaxValue, 1)) // #4878 overflow, large ooffset
    assertFalse(test.regionMatches(true, 1, "ab", 1, Int.MaxValue)) // #4878 overflow, large len

    // the strange cases that are true
    assertTrue(test.regionMatches(0, test, 0, -4))
    assertTrue(test.regionMatches(1, "bcdx", 0, -4))
    assertTrue(test.regionMatches(1, "bcdx", 1, -3))
    assertTrue(test.regionMatches(true, 1, "bCdx", 0, -4))
    assertTrue(test.regionMatches(true, 1, "bCdx", 1, -3))
    assertTrue(testU.regionMatches(true, 1, "bCdx", 0, -4))
    assertTrue(testU.regionMatches(true, 1, "bCdx", 1, -3))
  }

  @Test def trim(): Unit = {
    // Char values <= ' ' are trimmed
    for (c <- '\u0000' to '\u0020') {
      // on the left
      assertEquals(c.toInt.toString, s"foo${c}bar", s"${c} foo${c}bar".trim())
      // on the right
      assertEquals(c.toInt.toString, s"foo${c}bar", s"foo${c}bar\n${c}".trim())
      // on both sides
      assertEquals(c.toInt.toString, s"foo${c}bar", s"${c} foo${c}bar\n${c}".trim())
    }

    // Char values > ' ' are not trimmed, even Unicode Whitespace characters
    for (c <- '\u0021' to Char.MaxValue)
      assertEquals(c.toInt.toString, s"${c}foo${c}bar${c}", s"${c}foo${c}bar${c}")

    // Potential corner cases
    assertEquals("", "".trim())
    assertEquals("", " \n\r".trim())
    assertEquals("foo bar", "foo bar".trim())
  }

  @Test def createFromLargeCharArray_Issue2553(): Unit = {
    val largeCharArray =
      (1 to 100000).toArray.flatMap(_ => Array('a', 'b', 'c', 'd', 'e', 'f'))
    val str = new String(largeCharArray)

    assertEquals(600000, str.length)

    for (i <- 0 until str.length)
      assertEquals(('a' + i % 6).toChar, str.charAt(i))
  }

  @Test def createFromLargeCodePointArray_Issue2553(): Unit = {
    val largeCodePointArray =
      (1 to 100000).toArray.flatMap(_ => Array[Int]('a', 'b', 'c', 'd', 'e', 'f'))
    val str = new String(largeCodePointArray, 0, largeCodePointArray.length)

    assertEquals(600000, str.length)

    for (i <- 0 until str.length)
      assertEquals(('a' + i % 6).toChar, str.charAt(i))
  }

  @Test def stringCaseInsensitiveOrdering(): Unit = {
    def compare(s1: String, s2: String): Int =
      String.CASE_INSENSITIVE_ORDER.compare(s1, s2)

    assertEquals(0, compare("a", "a"))
    assertEquals(0, compare("a", "A"))
    assertEquals(0, compare("A", "a"))
    assertEquals(0, compare("Scala.JS", "Scala.js"))
    assertTrue(compare("Scala.JS", "scala") > 0)
    assertEquals(0, compare("åløb", "ÅLØB"))
    assertTrue(compare("Java", "Scala") < 0)
  }

  @inline private def erased(array: Array[String]): Array[AnyRef] = {
    array.asInstanceOf[Array[AnyRef]]
  }
}
