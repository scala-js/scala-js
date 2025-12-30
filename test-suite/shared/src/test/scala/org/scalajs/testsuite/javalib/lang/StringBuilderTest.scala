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

import java.lang.StringBuilder

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform.executingInJVM

import WrappedStringCharSequence.charSequence

/* !!! This test class is basically copy-pasted in StringBufferTest.
 * Make sure to always update them in sync.
 */
class StringBuilderTest {

  private def newBuilder: StringBuilder =
    new StringBuilder

  private def initBuilder(str: String): StringBuilder =
    new StringBuilder(str)

  @Test def init(): Unit = {
    assertEquals("", new StringBuilder().toString())
  }

  @Test def initInt(): Unit = {
    assertEquals("", new StringBuilder(5).toString())
  }

  @Test def initString(): Unit = {
    assertEquals("hello", new StringBuilder("hello").toString())

    if (executingInJVM) {
      assertThrows(classOf[NullPointerException],
          new StringBuilder(null: String))
    }
  }

  @Test def initCharSequence(): Unit = {
    assertEquals("hello", new StringBuilder(charSequence("hello")).toString())

    if (executingInJVM) {
      assertThrows(classOf[NullPointerException],
          new StringBuilder(null: CharSequence))
    }
  }

  @Test def appendAnyRef(): Unit = {
    def resultFor(x: AnyRef): String = newBuilder.append(x).toString()

    assertEquals("null", resultFor(null))
    assertEquals("None", resultFor(None))
    assertEquals("hello", resultFor("hello"))
    assertEquals("foobar", resultFor(charSequence("foobar")))
  }

  @Test def appendString(): Unit = {
    def resultFor(x: String): String = newBuilder.append(x).toString()

    assertEquals("null", resultFor(null))
    assertEquals("hello", resultFor("hello"))
  }

  @Test def appendStringBuffer(): Unit = {
    def resultFor(x: StringBuffer): String = newBuilder.append(x).toString()

    assertEquals("null", resultFor(null))
    assertEquals("", resultFor(new StringBuffer()))
    assertEquals("hello", resultFor(new StringBuffer("hello")))
  }

  @Test def appendCharSequence(): Unit = {
    def resultFor(x: CharSequence): String = newBuilder.append(x).toString()

    assertEquals("null", resultFor(null))
    assertEquals("hello", resultFor("hello"))
    assertEquals("", resultFor(charSequence("")))
    assertEquals("foobar", resultFor(charSequence("foobar")))
  }

  @Test def appendCharSequenceStartEnd(): Unit = {
    def resultFor(x: CharSequence, start: Int, end: Int): String =
      newBuilder.append(x, start, end).toString()

    assertEquals("ul", resultFor(null, 1, 3))
    assertEquals("null", resultFor(null, 0, 4))
    assertEquals("ello", resultFor("hello", 1, 5))
    assertEquals("ob", resultFor(charSequence("foobar"), 2, 4))

    assertThrows(classOf[IndexOutOfBoundsException],
        resultFor(charSequence("he"), 1, 3))
    assertThrows(classOf[IndexOutOfBoundsException],
        resultFor(charSequence("he"), -1, 2))
    assertThrows(classOf[IndexOutOfBoundsException],
        resultFor(charSequence("he"), 2, 1))
  }

  @Test def appendCharArray(): Unit = {
    def resultFor(x: Array[Char]): String = newBuilder.append(x).toString()

    assertEquals("hello", resultFor(Array('h', 'e', 'l', 'l', 'o')))

    if (executingInJVM)
      assertThrows(classOf[NullPointerException], resultFor(null))
  }

  @Test def appendCharArrayOffsetLen(): Unit = {
    def resultFor(x: Array[Char], offset: Int, len: Int): String =
      newBuilder.append(x, offset, len).toString()

    val arr = Array('h', 'e', 'l', 'l', 'o')
    assertEquals("hello", resultFor(arr, 0, 5))
    assertEquals("ell", resultFor(arr, 1, 3))

    if (executingInJVM)
      assertThrows(classOf[NullPointerException], resultFor(null, 0, 0))

    assertThrows(classOf[IndexOutOfBoundsException], resultFor(arr, -1, 2))
    assertThrows(classOf[IndexOutOfBoundsException], resultFor(arr, 3, 3))
    assertThrows(classOf[IndexOutOfBoundsException], resultFor(arr, 3, -2))
  }

  @Test def appendPrimitive(): Unit = {
    assertEquals("true", newBuilder.append(true).toString)
    assertEquals("a", newBuilder.append('a').toString)
    assertEquals("100000", newBuilder.append(100000).toString)
    assertEquals("12345678910", newBuilder.append(12345678910L).toString)
    assertEquals("2.5", newBuilder.append(2.5f).toString)
    assertEquals("3.5", newBuilder.append(3.5).toString)

    // There is no overload for Byte nor Short; these call the Int version
    assertEquals("4", newBuilder.append(4.toByte).toString)
    assertEquals("304", newBuilder.append(304.toShort).toString)
  }

  @Test def appendCodePoint(): Unit = {
    def resultFor(codePoint: Int): String =
      newBuilder.appendCodePoint(codePoint).toString()

    assertEquals("a", resultFor(0x61))
    assertEquals("\ud800\udc00", resultFor(0x10000))
    assertEquals("\ud800\udc01", resultFor(0x10001))
    assertEquals("\ud801\udc01", resultFor(0x10401))
    assertEquals("\udbff\udfff", resultFor(0x10ffff))

    assertThrows(classOf[IllegalArgumentException], resultFor(0x111111))
    assertThrows(classOf[IllegalArgumentException], resultFor(-1))
  }

  @Test def delete(): Unit = {
    def resultFor(input: String, start: Int, end: Int): String =
      initBuilder(input).delete(start, end).toString()

    assertEquals("heo", resultFor("hello", 2, 4))
    assertEquals("foo\ud800r", resultFor("foo\ud800\udc00bar", 4, 7))
    assertEquals("hello", resultFor("hello", 0, 0))
    assertEquals("hello", resultFor("hello", 5, 5))
    assertEquals("hel", resultFor("hello", 3, 8))

    assertThrows(classOf[StringIndexOutOfBoundsException],
        resultFor("hello", -1, 2))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        resultFor("hello", 3, 2))
  }

  @Test def deleteCharAt(): Unit = {
    def resultFor(input: String, index: Int): String =
      initBuilder(input).deleteCharAt(index).toString()

    assertEquals("023", resultFor("0123", 1))
    assertEquals("123", resultFor("0123", 0))
    assertEquals("012", resultFor("0123", 3))

    assertThrows(classOf[StringIndexOutOfBoundsException],
        resultFor("0123", -1))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        resultFor("0123", 4))
  }

  @Test def replace(): Unit = {
    def resultFor(input: String, start: Int, end: Int, str: String): String =
      initBuilder(input).replace(start, end, str).toString()

    assertEquals("0bc3", resultFor("0123", 1, 3, "bc"))
    assertEquals("abcd", resultFor("0123", 0, 4, "abcd"))
    assertEquals("abcd", resultFor("0123", 0, 10, "abcd"))
    assertEquals("012defg", resultFor("0123", 3, 10, "defg"))
    assertEquals("xxxx123", resultFor("0123", 0, 1, "xxxx"))
    assertEquals("0xxxx123", resultFor("0123", 1, 1, "xxxx"))
    assertEquals("0123x", resultFor("0123", 4, 5, "x"))

    assertThrows(classOf[StringIndexOutOfBoundsException],
        resultFor("0123", -1, 3, "x"))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        resultFor("0123", 4, 3, "x"))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        resultFor("0123", 5, 8, "x"))

    if (executingInJVM)
      assertThrows(classOf[NullPointerException], resultFor("0123", 1, 3, null))
  }

  @Test def insertCharArrayOffsetLen(): Unit = {
    def resultFor(input: String, index: Int, str: Array[Char], offset: Int,
        len: Int): String = {
      initBuilder(input).insert(index, str, offset, len).toString()
    }

    val arr = Array('a', 'b', 'c', 'd', 'e')

    assertEquals("0bc12", resultFor("012", 1, arr, 1, 2))
    assertEquals("abcdef", resultFor("abef", 2, arr, 2, 2))

    assertThrows(classOf[StringIndexOutOfBoundsException],
        resultFor("1234", -1, arr, 1, 2))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        resultFor("1234", 6, arr, 1, 2))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        resultFor("1234", 1, arr, -1, 2))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        resultFor("1234", 1, arr, 1, -2))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        resultFor("1234", 1, arr, 4, 3))

    if (executingInJVM) {
      assertThrows(classOf[NullPointerException],
          resultFor("1234", 1, null, 0, 0))
    }
  }

  @Test def insertAnyRef(): Unit = {
    def resultFor(input: String, index: Int, x: AnyRef): String =
      initBuilder(input).insert(index, x).toString()

    assertEquals("01null234", resultFor("01234", 2, null))
    assertEquals("01None234", resultFor("01234", 2, None))
    assertEquals("01hello234", resultFor("01234", 2, "hello"))
    assertEquals("01foobar234", resultFor("01234", 2, charSequence("foobar")))

    assertThrows(classOf[StringIndexOutOfBoundsException],
        resultFor("1234", -1, "foo"))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        resultFor("1234", 6, "foo"))
  }

  @Test def insertString(): Unit = {
    def resultFor(input: String, index: Int, x: String): String =
      initBuilder(input).insert(index, x).toString()

    assertEquals("01null234", resultFor("01234", 2, null))
    assertEquals("01hello234", resultFor("01234", 2, "hello"))

    assertThrows(classOf[StringIndexOutOfBoundsException],
        resultFor("1234", -1, "foo"))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        resultFor("1234", 6, "foo"))
  }

  @Test def insertCharArray(): Unit = {
    def resultFor(input: String, index: Int, str: Array[Char]): String =
      initBuilder(input).insert(index, str).toString()

    val arr = Array('a', 'b', 'c', 'd', 'e')

    assertEquals("0abcde12", resultFor("012", 1, arr))
    assertEquals("ababcdeef", resultFor("abef", 2, arr))

    assertThrows(classOf[StringIndexOutOfBoundsException],
        resultFor("1234", -1, arr))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        resultFor("1234", 6, arr))

    if (executingInJVM)
      assertThrows(classOf[NullPointerException], resultFor("1234", 1, null))
  }

  @Test def insertCharSequence(): Unit = {
    def resultFor(input: String, index: Int, x: CharSequence): String =
      initBuilder(input).insert(index, x).toString()

    assertEquals("01null234", resultFor("01234", 2, null))
    assertEquals("01hello234", resultFor("01234", 2, "hello"))
    assertEquals("01foobar234", resultFor("01234", 2, charSequence("foobar")))

    assertThrows(classOf[IndexOutOfBoundsException],
        resultFor("1234", -1, "foo"))
    assertThrows(classOf[IndexOutOfBoundsException],
        resultFor("1234", 6, "foo"))
  }

  @Test def insertCharSequenceStartEnd(): Unit = {
    def resultFor(input: String, index: Int, x: CharSequence, start: Int,
        end: Int): String = {
      initBuilder(input).insert(index, x, start, end).toString()
    }

    assertEquals("01ul234", resultFor("01234", 2, null, 1, 3))
    assertEquals("01ello234", resultFor("01234", 2, "hello", 1, 5))
    assertEquals("01ba234", resultFor("01234", 2, charSequence("foobar"), 3, 5))

    assertThrows(classOf[IndexOutOfBoundsException],
        resultFor("1234", -1, charSequence("foobar"), 1, 3))
    assertThrows(classOf[IndexOutOfBoundsException],
        resultFor("1234", 6, charSequence("foobar"), 1, 3))
    assertThrows(classOf[IndexOutOfBoundsException],
        resultFor("1234", 1, charSequence("foobar"), -1, 3))
    assertThrows(classOf[IndexOutOfBoundsException],
        resultFor("1234", 1, charSequence("foobar"), 2, -1))
    assertThrows(classOf[IndexOutOfBoundsException],
        resultFor("1234", 1, charSequence("foobar"), 3, 1))
    assertThrows(classOf[IndexOutOfBoundsException],
        resultFor("1234", 1, charSequence("foobar"), 7, 8))
    assertThrows(classOf[IndexOutOfBoundsException],
        resultFor("1234", 1, charSequence("foobar"), 2, 8))
  }

  @Test def insertPrimitive(): Unit = {
    assertEquals("atruebcd", initBuilder("abcd").insert(1, true).toString)
    assertEquals("axbcd", initBuilder("abcd").insert(1, 'x').toString)
    assertEquals("a100000bcd", initBuilder("abcd").insert(1, 100000).toString)
    assertEquals("a12345678910bcd",
        initBuilder("abcd").insert(1, 12345678910L).toString)
    assertEquals("a2.5bcd", initBuilder("abcd").insert(1, 2.5f).toString)
    assertEquals("a3.5bcd", initBuilder("abcd").insert(1, 3.5).toString)

    // There is no overload for Byte nor Short; these call the Int version
    assertEquals("a4bcd", initBuilder("abcd").insert(1, 4.toByte).toString)
    assertEquals("a304bcd", initBuilder("abcd").insert(1, 304.toShort).toString)

    assertThrows(classOf[StringIndexOutOfBoundsException],
        initBuilder("abcd").insert(5, 56))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        initBuilder("abcd").insert(-1, 56))
  }

  @Test def indexOfString(): Unit = {
    def resultFor(input: String, str: String): Int =
      initBuilder(input).indexOf(str)

    assertEquals(2, resultFor("ababcdeabcf", "abc"))
    assertEquals(-1, resultFor("ababcdeabcf", "acb"))
  }

  @Test def indexOfStringInt(): Unit = {
    def resultFor(input: String, str: String, fromIndex: Int): Int =
      initBuilder(input).indexOf(str, fromIndex)

    assertEquals(7, resultFor("ababcdeabcf", "abc", 4))
    assertEquals(2, resultFor("ababcdeabcf", "abc", 2))
    assertEquals(2, resultFor("ababcdeabcf", "abc", -5))
    assertEquals(-1, resultFor("ababcdeabcf", "abc", 10))
    assertEquals(-1, resultFor("ababcdeabcf", "abc", 20))
    assertEquals(-1, resultFor("ababcdeabcf", "acb", 2))
  }

  @Test def lastIndexOfString(): Unit = {
    def resultFor(input: String, str: String): Int =
      initBuilder(input).lastIndexOf(str)

    assertEquals(7, resultFor("ababcdeabcf", "abc"))
    assertEquals(-1, resultFor("ababcdeabcf", "acb"))
  }

  @Test def lastIndexOfStringInt(): Unit = {
    def resultFor(input: String, str: String, fromIndex: Int): Int =
      initBuilder(input).lastIndexOf(str, fromIndex)

    assertEquals(2, resultFor("ababcdeabcf", "abc", 2))
    assertEquals(2, resultFor("ababcdeabcf", "abc", 6))
    assertEquals(7, resultFor("ababcdeabcf", "abc", 8))
    assertEquals(7, resultFor("ababcdeabcf", "abc", 20))
    assertEquals(-1, resultFor("ababcdeabcf", "abc", 1))
    assertEquals(-1, resultFor("ababcdeabcf", "abc", -5))
    assertEquals(-1, resultFor("ababcdeabcf", "acb", 10))
  }

  @Test def reverse(): Unit = {
    def resultFor(input: String): String =
      initBuilder(input).reverse().toString()

    assertEquals("987654321", resultFor("123456789"))
    assertEquals("dc\ud801\udc02ba", resultFor("ab\ud801\udc02cd"))
    assertEquals("dc\ud802\ud801ba", resultFor("ab\ud801\ud802cd"))
    assertEquals("dc\udc02\udc01ba", resultFor("ab\udc01\udc02cd"))
    assertEquals("\ud801ba", resultFor("ab\ud801"))
    assertEquals("dc\udc02", resultFor("\udc02cd"))
  }

  @Test def length(): Unit = {
    assertEquals(5, initBuilder("hello").length())
    assertEquals(6, initBuilder("ab\ud801\udc02cd").length())
  }

  @Test def capacity(): Unit = {
    assertTrue(initBuilder("hello").capacity() >= 5)
    assertTrue(initBuilder("ab\ud801\udc02cd").capacity() >= 6)
  }

  @Test def ensureCapacity(): Unit = {
    // Just make sure it links
    newBuilder.ensureCapacity(10)
  }

  @Test def trimToSize(): Unit = {
    // Just make sure it links
    initBuilder("hello").trimToSize()
  }

  @Test def setLength(): Unit = {
    val b = initBuilder("foobar")

    assertThrows(classOf[StringIndexOutOfBoundsException], b.setLength(-3))

    b.setLength(3)
    assertEquals("foo", b.toString)
    b.setLength(6)
    assertEquals("foo\u0000\u0000\u0000", b.toString)
  }

  @Test def charAt(): Unit = {
    def resultFor(input: String, index: Int): Char =
      initBuilder(input).charAt(index)

    assertEquals('e', resultFor("hello", 1))
    assertEquals('\ud801', resultFor("ab\ud801\udc02cd", 2))
    assertEquals('\udc02', resultFor("ab\ud801\udc02cd", 3))

    if (executingInJVM) {
      assertThrows(classOf[IndexOutOfBoundsException], resultFor("hello", -1))
      assertThrows(classOf[IndexOutOfBoundsException], resultFor("hello", 5))
      assertThrows(classOf[IndexOutOfBoundsException], resultFor("hello", 6))
    }
  }

  @Test def codePointAt(): Unit = {
    def resultFor(input: String, index: Int): Int =
      initBuilder(input).codePointAt(index)

    assertEquals(0x61, resultFor("abc\ud834\udf06def", 0))
    assertEquals(0x1d306, resultFor("abc\ud834\udf06def", 3))
    assertEquals(0xdf06, resultFor("abc\ud834\udf06def", 4))
    assertEquals(0x64, resultFor("abc\ud834\udf06def", 5))
    assertEquals(0x1d306, resultFor("\ud834\udf06def", 0))
    assertEquals(0xdf06, resultFor("\ud834\udf06def", 1))
    assertEquals(0xd834, resultFor("\ud834abc", 0))
    assertEquals(0xdf06, resultFor("\udf06abc", 0))
    assertEquals(0xd834, resultFor("abc\ud834", 3))

    if (executingInJVM) {
      assertThrows(classOf[IndexOutOfBoundsException],
          resultFor("abc\ud834\udf06def", -1))
      assertThrows(classOf[IndexOutOfBoundsException],
          resultFor("abc\ud834\udf06def", 15))
    }
  }

  @Test def codePointBefore(): Unit = {
    def resultFor(input: String, index: Int): Int =
      initBuilder(input).codePointBefore(index)

    assertEquals(0x61, resultFor("abc\ud834\udf06def", 1))
    assertEquals(0x1d306, resultFor("abc\ud834\udf06def", 5))
    assertEquals(0xd834, resultFor("abc\ud834\udf06def", 4))
    assertEquals(0x64, resultFor("abc\ud834\udf06def", 6))
    assertEquals(0x1d306, resultFor("\ud834\udf06def", 2))
    assertEquals(0xd834, resultFor("\ud834\udf06def", 1))
    assertEquals(0xd834, resultFor("\ud834abc", 1))
    assertEquals(0xdf06, resultFor("\udf06abc", 1))

    if (executingInJVM) {
      assertThrows(classOf[IndexOutOfBoundsException],
          resultFor("abc\ud834\udf06def", 0))
      assertThrows(classOf[IndexOutOfBoundsException],
          resultFor("abc\ud834\udf06def", 15))
    }
  }

  @Test def codePointCount(): Unit = {
    val sb = initBuilder(
        "abc\uD834\uDF06de\uD834\uDF06fgh\uD834ij\uDF06\uD834kl\uDF06")

    assertEquals(18, sb.codePointCount(0, sb.length))
    assertEquals(1, sb.codePointCount(3, 5))
    assertEquals(1, sb.codePointCount(2, 3))
    assertEquals(2, sb.codePointCount(2, 4))
    assertEquals(2, sb.codePointCount(2, 5))
    assertEquals(3, sb.codePointCount(2, 6))
    assertEquals(5, sb.codePointCount(12, 17))
    assertEquals(2, sb.codePointCount(8, 10))
    assertEquals(2, sb.codePointCount(7, 10))
    assertEquals(0, sb.codePointCount(7, 7))
    assertEquals(1, sb.codePointCount(sb.length - 1, sb.length))
    assertEquals(0, sb.codePointCount(sb.length - 1, sb.length - 1))
    assertEquals(0, sb.codePointCount(sb.length, sb.length))

    assertThrows(classOf[IndexOutOfBoundsException], sb.codePointCount(-3, 4))
    assertThrows(classOf[IndexOutOfBoundsException], sb.codePointCount(6, 2))
    assertThrows(classOf[IndexOutOfBoundsException], sb.codePointCount(10, 30))
  }

  @Test def offsetByCodePoints(): Unit = {
    val sb = initBuilder(
        "abc\uD834\uDF06de\uD834\uDF06fgh\uD834ij\uDF06\uD834kl\uDF06")

    assertEquals(sb.length, sb.offsetByCodePoints(0, 18))
    assertEquals(5, sb.offsetByCodePoints(3, 1))
    assertEquals(3, sb.offsetByCodePoints(2, 1))
    assertEquals(5, sb.offsetByCodePoints(2, 2))
    assertEquals(6, sb.offsetByCodePoints(2, 3))
    assertEquals(17, sb.offsetByCodePoints(12, 5))
    assertEquals(10, sb.offsetByCodePoints(8, 2))
    assertEquals(10, sb.offsetByCodePoints(7, 2))
    assertEquals(7, sb.offsetByCodePoints(7, 0))
    assertEquals(sb.length, sb.offsetByCodePoints(sb.length - 1, 1))
    assertEquals(sb.length - 1, sb.offsetByCodePoints(sb.length - 1, 0))
    assertEquals(sb.length, sb.offsetByCodePoints(sb.length, 0))

    assertThrows(classOf[IndexOutOfBoundsException], sb.offsetByCodePoints(-3, 4))
    assertThrows(classOf[IndexOutOfBoundsException], sb.offsetByCodePoints(6, 18))
    assertThrows(classOf[IndexOutOfBoundsException], sb.offsetByCodePoints(30, 2))
  }

  @Test def offsetByCodePointsBackwards(): Unit = {
    val sb = initBuilder(
        "abc\uD834\uDF06de\uD834\uDF06fgh\uD834ij\uDF06\uD834kl\uDF06")

    assertEquals(0, sb.offsetByCodePoints(sb.length, -18))
    assertEquals(3, sb.offsetByCodePoints(5, -1))
    assertEquals(2, sb.offsetByCodePoints(3, -1))
    assertEquals(2, sb.offsetByCodePoints(4, -2))
    assertEquals(2, sb.offsetByCodePoints(5, -2))
    assertEquals(2, sb.offsetByCodePoints(6, -3))
    assertEquals(12, sb.offsetByCodePoints(17, -5))
    assertEquals(7, sb.offsetByCodePoints(10, -2))
    assertEquals(7, sb.offsetByCodePoints(7, -0))
    assertEquals(sb.length - 1, sb.offsetByCodePoints(sb.length, -1))
    assertEquals(sb.length - 1, sb.offsetByCodePoints(sb.length - 1, -0))
    assertEquals(sb.length, sb.offsetByCodePoints(sb.length, -0))

    assertThrows(classOf[IndexOutOfBoundsException], sb.offsetByCodePoints(-3, 4))
    assertThrows(classOf[IndexOutOfBoundsException], sb.offsetByCodePoints(6, 18))
    assertThrows(classOf[IndexOutOfBoundsException], sb.offsetByCodePoints(30, 2))
  }

  @Test def getChars(): Unit = {
    val dst = new Array[Char](10)
    initBuilder("asdf_foo").getChars(2, 6, dst, 3)
    assertArrayEquals(Array[Char](0, 0, 0, 'd', 'f', '_', 'f', 0, 0, 0), dst)
  }

  @Test def setCharAt(): Unit = {
    def resultFor(input: String, index: Int, ch: Char): String = {
      val sb = initBuilder(input)
      sb.setCharAt(index, ch)
      sb.toString()
    }

    assertEquals("foxbar", resultFor("foobar", 2, 'x'))
    assertEquals("foobah", resultFor("foobar", 5, 'h'))

    assertThrows(classOf[StringIndexOutOfBoundsException],
        resultFor("foobar", -1, 'h'))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        resultFor("foobar", 6, 'h'))
  }

  @Test def substringStart(): Unit = {
    def resultFor(input: String, start: Int): String =
      initBuilder(input).substring(start)

    assertEquals("llo", resultFor("hello", 2))
    assertEquals("", resultFor("hello", 5))

    if (executingInJVM) {
      assertThrows(classOf[StringIndexOutOfBoundsException],
          resultFor("hello", -1))
      assertThrows(classOf[StringIndexOutOfBoundsException],
          resultFor("hello", 8))
    }
  }

  @Test def subSequence(): Unit = {
    def resultFor(input: String, start: Int, end: Int): CharSequence =
      initBuilder(input).subSequence(start, end)

    /* Note that the spec of subSequence says that it behaves exactly like
     * substring. Therefore, the returned CharSequence must necessarily be a
     * String.
     */
    assertEquals("ll", resultFor("hello", 2, 4))
    assertEquals("", resultFor("hello", 5, 5))
    assertEquals("hel", resultFor("hello", 0, 3))

    if (executingInJVM) {
      assertThrows(classOf[StringIndexOutOfBoundsException],
          resultFor("hello", -1, 3))
      assertThrows(classOf[StringIndexOutOfBoundsException],
          resultFor("hello", 8, 8))
      assertThrows(classOf[StringIndexOutOfBoundsException],
          resultFor("hello", 3, 2))
      assertThrows(classOf[StringIndexOutOfBoundsException],
          resultFor("hello", 3, 8))
    }
  }

  @Test def substringStartEnd(): Unit = {
    def resultFor(input: String, start: Int, end: Int): String =
      initBuilder(input).substring(start, end)

    assertEquals("ll", resultFor("hello", 2, 4))
    assertEquals("", resultFor("hello", 5, 5))
    assertEquals("hel", resultFor("hello", 0, 3))

    if (executingInJVM) {
      assertThrows(classOf[StringIndexOutOfBoundsException],
          resultFor("hello", -1, 3))
      assertThrows(classOf[StringIndexOutOfBoundsException],
          resultFor("hello", 8, 8))
      assertThrows(classOf[StringIndexOutOfBoundsException],
          resultFor("hello", 3, 2))
      assertThrows(classOf[StringIndexOutOfBoundsException],
          resultFor("hello", 3, 8))
    }
  }

  @Test def stringInterpolationToSurviveNullAndUndefined(): Unit = {
    assertEquals("anullb", s"a${null}b")

    if (executingInJVM)
      assertEquals("a()b", s"a${()}b")
    else
      assertEquals("aundefinedb", s"a${()}b")
  }
}
