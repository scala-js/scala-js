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

/* This file contains automatically generated snippets.
 * To regenerate them, run the sbt task `javalibInternal/regenerateUnicodeData`,
 * whose implementation is in `project/UnicodeDataGen.scala`.
 */

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform._

object CharacterTest {
  // BEGIN GENERATED: [constants]
  private final val ReferenceJDKVersion = 21
  // END GENERATED: [constants]
}

class CharacterTest {
  import CharacterTest._

  @Test def hashCodeChar(): Unit = {
    assertEquals(0, Character.hashCode('\u0000'))
    assertEquals(65, Character.hashCode('A'))
    assertEquals(0x1234, Character.hashCode('\u1234'))
    assertEquals(0xffff, Character.hashCode('\uFFFF'))
  }

  @Test def toStringChar(): Unit = {
    assertEquals("a", Character.toString('a'))
    assertEquals("\u1234", Character.toString('\u1234'))
    assertEquals("\uD845", Character.toString('\uD845')) // high surrogate
    assertEquals("\uDC54", Character.toString('\uDC54')) // low surrogate
    assertEquals("\uFFFF", Character.toString('\uFFFF'))
  }

  @Test def isValidCodePoint(): Unit = {
    assertTrue(Character.isValidCodePoint(0))
    assertTrue(Character.isValidCodePoint(1234))
    assertTrue(Character.isValidCodePoint(0xd845))
    assertTrue(Character.isValidCodePoint(0xdc54))
    assertTrue(Character.isValidCodePoint(0xffff))

    assertTrue(Character.isValidCodePoint(0x10000))
    assertTrue(Character.isValidCodePoint(0x12345))
    assertTrue(Character.isValidCodePoint(0x10ffff))

    assertFalse(Character.isValidCodePoint(0x110000))
    assertFalse(Character.isValidCodePoint(0x234567))
    assertFalse(Character.isValidCodePoint(-1))
    assertFalse(Character.isValidCodePoint(Int.MinValue))
  }

  @Test def isBmpCodePoint(): Unit = {
    assertTrue(Character.isBmpCodePoint(0))
    assertTrue(Character.isBmpCodePoint(1234))
    assertTrue(Character.isBmpCodePoint(0xd845))
    assertTrue(Character.isBmpCodePoint(0xdc54))
    assertTrue(Character.isBmpCodePoint(0xffff))

    assertFalse(Character.isBmpCodePoint(0x10000))
    assertFalse(Character.isBmpCodePoint(0x12345))
    assertFalse(Character.isBmpCodePoint(0x10ffff))

    assertFalse(Character.isBmpCodePoint(0x110000))
    assertFalse(Character.isBmpCodePoint(0x234567))
    assertFalse(Character.isBmpCodePoint(-1))
    assertFalse(Character.isBmpCodePoint(Int.MinValue))
  }

  @Test def isSupplementaryCodePoint(): Unit = {
    assertFalse(Character.isSupplementaryCodePoint(0))
    assertFalse(Character.isSupplementaryCodePoint(1234))
    assertFalse(Character.isSupplementaryCodePoint(0xd845))
    assertFalse(Character.isSupplementaryCodePoint(0xdc54))
    assertFalse(Character.isSupplementaryCodePoint(0xffff))

    assertTrue(Character.isSupplementaryCodePoint(0x10000))
    assertTrue(Character.isSupplementaryCodePoint(0x12345))
    assertTrue(Character.isSupplementaryCodePoint(0x10ffff))

    assertFalse(Character.isSupplementaryCodePoint(0x110000))
    assertFalse(Character.isSupplementaryCodePoint(0x234567))
    assertFalse(Character.isSupplementaryCodePoint(-1))
    assertFalse(Character.isSupplementaryCodePoint(Int.MinValue))
  }

  @Test def isHighSurrogate(): Unit = {
    assertFalse(Character.isHighSurrogate(0))
    assertFalse(Character.isHighSurrogate(1234))
    assertFalse(Character.isHighSurrogate(0xd7ff))

    assertTrue(Character.isHighSurrogate(0xd800))
    assertTrue(Character.isHighSurrogate(0xd954))
    assertTrue(Character.isHighSurrogate(0xdbff))

    assertFalse(Character.isHighSurrogate(0xdc00))
    assertFalse(Character.isHighSurrogate(0xdd45))
    assertFalse(Character.isHighSurrogate(0xdfff))

    assertFalse(Character.isHighSurrogate(0xe000))
    assertFalse(Character.isHighSurrogate(0xea65))
    assertFalse(Character.isHighSurrogate(0xffff))
  }

  @Test def isLowSurrogate(): Unit = {
    assertFalse(Character.isLowSurrogate(0))
    assertFalse(Character.isLowSurrogate(1234))
    assertFalse(Character.isLowSurrogate(0xd7ff))

    assertFalse(Character.isLowSurrogate(0xd800))
    assertFalse(Character.isLowSurrogate(0xd954))
    assertFalse(Character.isLowSurrogate(0xdbff))

    assertTrue(Character.isLowSurrogate(0xdc00))
    assertTrue(Character.isLowSurrogate(0xdd45))
    assertTrue(Character.isLowSurrogate(0xdfff))

    assertFalse(Character.isLowSurrogate(0xe000))
    assertFalse(Character.isLowSurrogate(0xea65))
    assertFalse(Character.isLowSurrogate(0xffff))
  }

  @Test def isSurrogate(): Unit = {
    assertFalse(Character.isSurrogate(0))
    assertFalse(Character.isSurrogate(1234))
    assertFalse(Character.isSurrogate(0xd7ff))

    assertTrue(Character.isSurrogate(0xd800))
    assertTrue(Character.isSurrogate(0xd954))
    assertTrue(Character.isSurrogate(0xdbff))

    assertTrue(Character.isSurrogate(0xdc00))
    assertTrue(Character.isSurrogate(0xdd45))
    assertTrue(Character.isSurrogate(0xdfff))

    assertFalse(Character.isSurrogate(0xe000))
    assertFalse(Character.isSurrogate(0xea65))
    assertFalse(Character.isSurrogate(0xffff))
  }

  @Test def isSurrogatePair(): Unit = {
    val chars = List[Char](0, 1234, 0xd7ff, 0xd800, 0xd954, 0xdbff, 0xdc00,
        0xdd45, 0xdfff, 0xe000, 0xea65, 0xffff)

    for {
      high <- chars
      low <- chars
    } {
      val expected = Character.isHighSurrogate(high) && Character.isLowSurrogate(low)
      assertEquals(expected, Character.isSurrogatePair(high, low))
    }
  }

  @Test def charCount(): Unit = {
    assertEquals(1, Character.charCount(0))
    assertEquals(1, Character.charCount(1234))
    assertEquals(1, Character.charCount(0xd845))
    assertEquals(1, Character.charCount(0xdc54))
    assertEquals(1, Character.charCount(0xffff))

    assertEquals(2, Character.charCount(0x10000))
    assertEquals(2, Character.charCount(0x12345))
    assertEquals(2, Character.charCount(0x10ffff))

    /* It is unclear whether the result is actually specified for invalid
     * code points. However, JDK 8 through 14 all agree on the following
     * results.
     */

    assertEquals(2, Character.charCount(0x110000))
    assertEquals(2, Character.charCount(0x234567))

    assertEquals(1, Character.charCount(-1))
    assertEquals(1, Character.charCount(Int.MinValue))
  }

  @Test def toCodePoint(): Unit = {
    assertEquals(0x10000, Character.toCodePoint(0xd800, 0xdc00))
    assertEquals(0x12345, Character.toCodePoint(0xd808, 0xdf45))
    assertEquals(0x10ffff, Character.toCodePoint(0xdbff, 0xdfff))

    // unspecified for inputs that are not surrogate pairs
  }

  @Test def highSurrogate(): Unit = {
    assertEquals(0xd800, Character.highSurrogate(0x10000))
    assertEquals(0xd808, Character.highSurrogate(0x12345))
    assertEquals(0xdbff, Character.highSurrogate(0x10ffff))

    // unspecified for non-supplementary code points
  }

  @Test def lowSurrogate(): Unit = {
    assertEquals(0xdc00, Character.lowSurrogate(0x10000))
    assertEquals(0xdf45, Character.lowSurrogate(0x12345))
    assertEquals(0xdfff, Character.lowSurrogate(0x10ffff))

    // unspecified for non-supplementary code points
  }

  @Test def digit(): Unit = {
    import Character.{MAX_RADIX, MIN_RADIX}

    assumeTrue(
        s"requires exactly the reference JDK version $ReferenceJDKVersion",
        !executingInJVM || executingInJVMWithJDKIn(ReferenceJDKVersion to ReferenceJDKVersion))

    def test(expected: Int, codePoint: Int): Unit = {
      assertEquals(expected, Character.digit(codePoint, MAX_RADIX))
      if (codePoint <= Char.MaxValue)
        assertEquals(expected, Character.digit(codePoint.toChar, MAX_RADIX))

      if (expected != -1) {
        assertEquals(expected,
            Character.digit(codePoint, Math.max(expected + 1, MIN_RADIX)))

        if (expected >= MIN_RADIX)
          assertEquals(-1, Character.digit(codePoint, expected))
      }
    }

    // Invalid radix

    assertEquals(-1, Character.digit('0', MIN_RADIX - 1))
    assertEquals(-1, Character.digit('0', MAX_RADIX + 1))
    assertEquals(-1, Character.digit('0', -1))

    assertEquals(-1, Character.digit('0'.toInt, MIN_RADIX - 1))
    assertEquals(-1, Character.digit('0'.toInt, MAX_RADIX + 1))
    assertEquals(-1, Character.digit('0'.toInt, -1))

    // A few invalid digits
    test(-1, '}')
    test(-1, -4)
    test(-1, 0xffffff)
    test(-1, '0' - 1)
    test(-1, '9' + 1)
    test(-1, 'A' - 1)
    test(-1, 'Z' + 1)
    test(-1, 'a' - 1)
    test(-1, 'z' + 1)
    test(-1, 0xff20)
    test(-1, 0xff3b)
    test(-1, 0xff40)
    test(-1, 0xff5b)
    test(-1, 0xbe5)
    test(-1, 0xbf0)
    test(-1, 0x11065)
    test(-1, 0x11070)
    test(-1, Int.MinValue)
    test(-1, Int.MaxValue)

    // Every single valid digit

    val All0s: Array[Int] = Array(
        // BEGIN GENERATED: [all-zero-digits]
        0x0030, 0x0660, 0x06f0, 0x07c0, 0x0966, 0x09e6, 0x0a66, 0x0ae6, 0x0b66,
        0x0be6, 0x0c66, 0x0ce6, 0x0d66, 0x0de6, 0x0e50, 0x0ed0, 0x0f20, 0x1040,
        0x1090, 0x17e0, 0x1810, 0x1946, 0x19d0, 0x1a80, 0x1a90, 0x1b50, 0x1bb0,
        0x1c40, 0x1c50, 0xa620, 0xa8d0, 0xa900, 0xa9d0, 0xa9f0, 0xaa50, 0xabf0,
        0xff10, 0x104a0, 0x10d30, 0x11066, 0x110f0, 0x11136, 0x111d0, 0x112f0,
        0x11450, 0x114d0, 0x11650, 0x116c0, 0x11730, 0x118e0, 0x11950, 0x11c50,
        0x11d50, 0x11da0, 0x11f50, 0x16a60, 0x16ac0, 0x16b50, 0x1d7ce, 0x1d7d8,
        0x1d7e2, 0x1d7ec, 0x1d7f6, 0x1e140, 0x1e2f0, 0x1e4f0, 0x1e950, 0x1fbf0
        // END GENERATED: [all-zero-digits]
    )

    for {
      zero <- All0s
      offset <- 0 to 9
    } {
      test(offset, zero + offset)
    }

    val AllAs = Array[Int]('A', 'a', 0xff21, 0xff41)

    for {
      a <- AllAs
      offset <- 0 to 25
    } {
      test(10 + offset, a + offset)
    }
  }

  @Test def forDigit(): Unit = {
    /* Ported from
     * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/lang/CharacterTest.java
     */
    for (i <- 0 until 36) {
      assertEquals(i, Character.digit(Character.forDigit(i, 36), 36))
    }
    assertEquals('9', Character.forDigit(9, 10))
  }

  @Test def codePointAtCharSequence(): Unit = {
    assertEquals(0x61, Character.codePointAt("abc\ud834\udf06def", 0))
    assertEquals(0x1d306, Character.codePointAt("abc\ud834\udf06def", 3))
    assertEquals(0xdf06, Character.codePointAt("abc\ud834\udf06def", 4))
    assertEquals(0x64, Character.codePointAt("abc\ud834\udf06def", 5))
    assertEquals(0x1d306, Character.codePointAt("\ud834\udf06def", 0))
    assertEquals(0xdf06, Character.codePointAt("\ud834\udf06def", 1))
    assertEquals(0xd834, Character.codePointAt("\ud834abc", 0))
    assertEquals(0xdf06, Character.codePointAt("\udf06abc", 0))
    assertEquals(0xd834, Character.codePointAt("abc\ud834", 3))
  }

  @Test def codePointAtCharSequenceIndexOutOfBounds(): Unit = {
    assumeTrue("Assuming compliant StringIndexOutOfBounds",
        hasCompliantStringIndexOutOfBounds)

    assertThrows(classOf[StringIndexOutOfBoundsException],
        Character.codePointAt("abc\ud834\udf06def", -1))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        Character.codePointAt("abc\ud834\udf06def", 8))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        Character.codePointAt("abc\ud834\udf06def", 15))
  }

  @Test def codePointAtArray(): Unit = {
    assertEquals(0x61, Character.codePointAt("abc\ud834\udf06def".toCharArray(), 0))
    assertEquals(0x1d306, Character.codePointAt("abc\ud834\udf06def".toCharArray(), 3))
    assertEquals(0xdf06, Character.codePointAt("abc\ud834\udf06def".toCharArray(), 4))
    assertEquals(0x64, Character.codePointAt("abc\ud834\udf06def".toCharArray(), 5))
    assertEquals(0x1d306, Character.codePointAt("\ud834\udf06def".toCharArray(), 0))
    assertEquals(0xdf06, Character.codePointAt("\ud834\udf06def".toCharArray(), 1))
    assertEquals(0xd834, Character.codePointAt("\ud834abc".toCharArray(), 0))
    assertEquals(0xdf06, Character.codePointAt("\udf06abc".toCharArray(), 0))
    assertEquals(0xd834, Character.codePointAt("abc\ud834".toCharArray(), 3))
  }

  @Test def codePointAtArrayIndexOutOfBounds(): Unit = {
    assumeTrue("Assuming compliant ArrayIndexOutOfBounds",
        hasCompliantArrayIndexOutOfBounds)

    assertThrows(classOf[ArrayIndexOutOfBoundsException],
        Character.codePointAt("abc\ud834\udf06def".toCharArray(), -1))
    assertThrows(classOf[ArrayIndexOutOfBoundsException],
        Character.codePointAt("abc\ud834\udf06def".toCharArray(), 8))
    assertThrows(classOf[ArrayIndexOutOfBoundsException],
        Character.codePointAt("abc\ud834\udf06def".toCharArray(), 15))
  }

  @Test def codePointAtArrayWithLimit(): Unit = {
    assertEquals(0x61, Character.codePointAt("abc\ud834\udf06def".toCharArray(), 0, 3))
    assertEquals(0x1d306, Character.codePointAt("abc\ud834\udf06def".toCharArray(), 3, 5))
    assertEquals(0xdf06, Character.codePointAt("abc\ud834\udf06def".toCharArray(), 4, 5))
    assertEquals(0x64, Character.codePointAt("abc\ud834\udf06def".toCharArray(), 5, 8))
    assertEquals(0x1d306, Character.codePointAt("\ud834\udf06def".toCharArray(), 0, 2))
    assertEquals(0xdf06, Character.codePointAt("\ud834\udf06def".toCharArray(), 1, 2))
    assertEquals(0xd834, Character.codePointAt("\ud834\udf06def".toCharArray(), 0, 1))
    assertEquals(0xdf06, Character.codePointAt("\ud834\udf06def".toCharArray(), 1, 2))
    assertEquals(0xd834, Character.codePointAt("\ud834abc".toCharArray(), 0, 3))
    assertEquals(0xdf06, Character.codePointAt("\udf06abc".toCharArray(), 0, 1))
    assertEquals(0xd834, Character.codePointAt("abc\ud834".toCharArray(), 3, 4))
  }

  @Test def codePointAtArrayWithLimitIndexOutOfBounds(): Unit = {
    assertThrows(classOf[IndexOutOfBoundsException],
        Character.codePointAt("abc\ud834\udf06def".toCharArray(), -1, 3))
    assertThrows(classOf[IndexOutOfBoundsException],
        Character.codePointAt("abc\ud834\udf06def".toCharArray(), 8, 8))
    assertThrows(classOf[IndexOutOfBoundsException],
        Character.codePointAt("abc\ud834\udf06def".toCharArray(), 15, 5))
    assertThrows(classOf[IndexOutOfBoundsException],
        Character.codePointAt("abc\ud834\udf06def".toCharArray(), 3, 1))
    assertThrows(classOf[IndexOutOfBoundsException],
        Character.codePointAt("abc\ud834\udf06def".toCharArray(), 3, 3))
    assertThrows(classOf[IndexOutOfBoundsException],
        Character.codePointAt("abc\ud834\udf06def".toCharArray(), -1, 0))
    assertThrows(classOf[IndexOutOfBoundsException],
        Character.codePointAt("abc\ud834\udf06def".toCharArray(), -5, -2))
    assertThrows(classOf[IndexOutOfBoundsException],
        Character.codePointAt("abc\ud834\udf06def".toCharArray(), 5, 10))
  }

  @Test def codePointBeforeCharSequence(): Unit = {
    assertEquals(0x61, Character.codePointBefore("abc\ud834\udf06def", 1))
    assertEquals(0x1d306, Character.codePointBefore("abc\ud834\udf06def", 5))
    assertEquals(0xd834, Character.codePointBefore("abc\ud834\udf06def", 4))
    assertEquals(0x64, Character.codePointBefore("abc\ud834\udf06def", 6))
    assertEquals('f'.toInt, Character.codePointBefore("abc\ud834\udf06def", 8))
    assertEquals(0x1d306, Character.codePointBefore("\ud834\udf06def", 2))
    assertEquals(0xd834, Character.codePointBefore("\ud834\udf06def", 1))
    assertEquals(0xd834, Character.codePointBefore("\ud834abc", 1))
    assertEquals(0xdf06, Character.codePointBefore("\udf06abc", 1))
  }

  @Test def codePointBeforeCharSequenceIndexOutOfBounds(): Unit = {
    assumeTrue("Assuming compliant StringIndexOutOfBounds",
        hasCompliantStringIndexOutOfBounds)

    assertThrows(classOf[StringIndexOutOfBoundsException],
        Character.codePointBefore("abc\ud834\udf06def", -5))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        Character.codePointBefore("abc\ud834\udf06def", 0))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        Character.codePointBefore("abc\ud834\udf06def", 9))
    assertThrows(classOf[StringIndexOutOfBoundsException],
        Character.codePointBefore("abc\ud834\udf06def", 15))
  }

  @Test def codePointBeforeArray(): Unit = {
    assertEquals(0x61, Character.codePointBefore("abc\ud834\udf06def".toCharArray(), 1))
    assertEquals(0x1d306, Character.codePointBefore("abc\ud834\udf06def".toCharArray(), 5))
    assertEquals(0xd834, Character.codePointBefore("abc\ud834\udf06def".toCharArray(), 4))
    assertEquals(0x64, Character.codePointBefore("abc\ud834\udf06def".toCharArray(), 6))
    assertEquals('f'.toInt, Character.codePointBefore("abc\ud834\udf06def".toCharArray(), 8))
    assertEquals(0x1d306, Character.codePointBefore("\ud834\udf06def".toCharArray(), 2))
    assertEquals(0xd834, Character.codePointBefore("\ud834\udf06def".toCharArray(), 1))
    assertEquals(0xd834, Character.codePointBefore("\ud834abc".toCharArray(), 1))
    assertEquals(0xdf06, Character.codePointBefore("\udf06abc".toCharArray(), 1))
  }

  @Test def codePointBeforeArrayIndexOutOfBounds(): Unit = {
    assumeTrue("Assuming compliant ArrayIndexOutOfBounds",
        hasCompliantArrayIndexOutOfBounds)

    assertThrows(classOf[ArrayIndexOutOfBoundsException],
        Character.codePointBefore("abc\ud834\udf06def".toCharArray(), -5))
    assertThrows(classOf[ArrayIndexOutOfBoundsException],
        Character.codePointBefore("abc\ud834\udf06def".toCharArray(), 0))
    assertThrows(classOf[ArrayIndexOutOfBoundsException],
        Character.codePointBefore("abc\ud834\udf06def".toCharArray(), 9))
    assertThrows(classOf[ArrayIndexOutOfBoundsException],
        Character.codePointBefore("abc\ud834\udf06def".toCharArray(), 15))
  }

  @Test def codePointBeforeArrayWithStart(): Unit = {
    assertEquals(0x61, Character.codePointBefore("abc\ud834\udf06def".toCharArray(), 1, 0))
    assertEquals(0x1d306, Character.codePointBefore("abc\ud834\udf06def".toCharArray(), 5, 0))
    assertEquals(0x1d306, Character.codePointBefore("abc\ud834\udf06def".toCharArray(), 5, 3))
    assertEquals(0xdf06, Character.codePointBefore("abc\ud834\udf06def".toCharArray(), 5, 4))
    assertEquals(0xd834, Character.codePointBefore("abc\ud834\udf06def".toCharArray(), 4, 2))
    assertEquals(0xd834, Character.codePointBefore("abc\ud834\udf06def".toCharArray(), 4, 3))
    assertEquals(0xd834, Character.codePointBefore("\ud834\udf06def".toCharArray(), 1, 0))
    assertEquals(0xd834, Character.codePointBefore("\ud834abc".toCharArray(), 1, 0))
    assertEquals(0xdf06, Character.codePointBefore("\udf06abc".toCharArray(), 1, 0))
  }

  @Test def codePointBeforeArrayWithStartIndexOutOfBounds(): Unit = {
    assertThrows(classOf[IndexOutOfBoundsException],
        Character.codePointBefore("abc\ud834\udf06def".toCharArray(), -5, 0))
    assertThrows(classOf[IndexOutOfBoundsException],
        Character.codePointBefore("abc\ud834\udf06def".toCharArray(), 0, 0))
    assertThrows(classOf[IndexOutOfBoundsException],
        Character.codePointBefore("abc\ud834\udf06def".toCharArray(), 2, 2))
    assertThrows(classOf[IndexOutOfBoundsException],
        Character.codePointBefore("abc\ud834\udf06def".toCharArray(), 2, 5))
    assertThrows(classOf[IndexOutOfBoundsException],
        Character.codePointBefore("abc\ud834\udf06def".toCharArray(), 15, 5))
  }

  @Test def toChars(): Unit = {
    assertTrue(Character.toChars(0x61) sameElements Array('a'))
    assertTrue(Character.toChars(0x10000) sameElements Array('\uD800', '\uDC00'))
    assertTrue(Character.toChars(0x10001) sameElements Array('\uD800', '\uDC01'))
    assertTrue(Character.toChars(0x10401) sameElements Array('\uD801', '\uDC01'))
    assertTrue(Character.toChars(0x10ffff) sameElements Array('\uDBFF', '\uDFFF'))

    assertThrows(classOf[IllegalArgumentException], Character.toChars(Integer.MAX_VALUE))
  }

  @Test def toCharsInPlace(): Unit = {
    locally {
      val dst = new Array[Char](2)
      assertEquals(1, Character.toChars(0x61, dst, 0))
      assertTrue(dst sameElements Array('a', 0.toChar))
    }
    locally {
      val dst = new Array[Char](2)
      assertEquals(1, Character.toChars(0x61, dst, 1))
      assertTrue(dst sameElements Array(0.toChar, 'a'))
    }
    locally {
      val dst = new Array[Char](2)
      assertEquals(2, Character.toChars(0x10000, dst, 0))
      assertTrue(dst sameElements Array('\uD800', '\uDC00'))
    }
    locally {
      val dst = new Array[Char](3)
      assertEquals(2, Character.toChars(0x10001, dst, 0))
      assertTrue(dst sameElements Array('\uD800', '\uDC01', 0.toChar))
    }
    locally {
      val dst = new Array[Char](3)
      assertEquals(2, Character.toChars(0x10401, dst, 1))
      assertTrue(dst sameElements Array(0.toChar, '\uD801', '\uDC01'))
    }
    locally {
      val dst = new Array[Char](4)
      assertEquals(2, Character.toChars(0x10ffff, dst, 2))
      assertTrue(dst sameElements Array(0.toChar, 0.toChar, '\uDBFF', '\uDFFF'))
    }

    assertThrows(
        classOf[IllegalArgumentException], Character.toChars(Integer.MAX_VALUE, new Array(2), 0))
  }

  @Test def offsetByCodePointsCharSequence(): Unit = {
    val s: CharSequence = "abc\ud834\udf06de\ud834\udf06fgh\ud834ij\udf06\ud834kl\udf06"

    assertEquals(s.length, Character.offsetByCodePoints(s, 0, 18))
    assertEquals(5, Character.offsetByCodePoints(s, 3, 1))
    assertEquals(3, Character.offsetByCodePoints(s, 2, 1))
    assertEquals(5, Character.offsetByCodePoints(s, 2, 2))
    assertEquals(6, Character.offsetByCodePoints(s, 2, 3))
    assertEquals(17, Character.offsetByCodePoints(s, 12, 5))
    assertEquals(10, Character.offsetByCodePoints(s, 8, 2))
    assertEquals(10, Character.offsetByCodePoints(s, 7, 2))
    assertEquals(7, Character.offsetByCodePoints(s, 7, 0))
    assertEquals(s.length, Character.offsetByCodePoints(s, s.length - 1, 1))
    assertEquals(s.length - 1, Character.offsetByCodePoints(s, s.length - 1, 0))
    assertEquals(s.length, Character.offsetByCodePoints(s, s.length, 0))

    assertThrows(classOf[IndexOutOfBoundsException], Character.offsetByCodePoints(s, -3, 0))
    assertThrows(classOf[IndexOutOfBoundsException], Character.offsetByCodePoints(s, -3, 4))
    assertThrows(classOf[IndexOutOfBoundsException], Character.offsetByCodePoints(s, 6, 18))
    assertThrows(classOf[IndexOutOfBoundsException], Character.offsetByCodePoints(s, 30, 2))
    assertThrows(classOf[IndexOutOfBoundsException], Character.offsetByCodePoints(s, 30, 0))
  }

  @Test def offsetByCodePointsCharSequenceBackwards(): Unit = {
    val s: CharSequence = "abc\ud834\udf06de\ud834\udf06fgh\ud834ij\udf06\ud834kl\udf06"

    assertEquals(0, Character.offsetByCodePoints(s, s.length, -18))
    assertEquals(3, Character.offsetByCodePoints(s, 5, -1))
    assertEquals(2, Character.offsetByCodePoints(s, 3, -1))
    assertEquals(2, Character.offsetByCodePoints(s, 4, -2))
    assertEquals(2, Character.offsetByCodePoints(s, 5, -2))
    assertEquals(2, Character.offsetByCodePoints(s, 6, -3))
    assertEquals(12, Character.offsetByCodePoints(s, 17, -5))
    assertEquals(7, Character.offsetByCodePoints(s, 10, -2))
    assertEquals(7, Character.offsetByCodePoints(s, 7, -0))
    assertEquals(s.length - 1, Character.offsetByCodePoints(s, s.length, -1))
    assertEquals(s.length - 1, Character.offsetByCodePoints(s, s.length - 1, -0))
    assertEquals(s.length, Character.offsetByCodePoints(s, s.length, -0))

    assertThrows(classOf[IndexOutOfBoundsException], Character.offsetByCodePoints(s, -3, -4))
    assertThrows(classOf[IndexOutOfBoundsException], Character.offsetByCodePoints(s, 6, -18))
    assertThrows(classOf[IndexOutOfBoundsException], Character.offsetByCodePoints(s, 30, -2))
  }

  @Test def offsetByCodePointsArray(): Unit = {
    val a: Array[Char] =
      "abc\ud834\udf06de\ud834\udf06fgh\ud834ij\udf06\ud834kl\udf06".toCharArray()
    val len = a.length

    assertEquals(len, Character.offsetByCodePoints(a, 0, len, 0, 18))
    assertEquals(5, Character.offsetByCodePoints(a, 0, len, 3, 1))
    assertEquals(3, Character.offsetByCodePoints(a, 0, 5, 2, 1))
    assertEquals(5, Character.offsetByCodePoints(a, 0, 5, 2, 2))
    assertEquals(6, Character.offsetByCodePoints(a, 0, len, 2, 3))
    assertEquals(17, Character.offsetByCodePoints(a, 5, 12, 12, 5))

    assertEquals(10, Character.offsetByCodePoints(a, 5, 8, 8, 2))
    assertEquals(10, Character.offsetByCodePoints(a, 5, 8, 7, 2))

    assertEquals(9, Character.offsetByCodePoints(a, 4, 5, 6, 2))
    assertEquals(8, Character.offsetByCodePoints(a, 4, 4, 6, 2))

    assertThrows(classOf[IndexOutOfBoundsException], Character.offsetByCodePoints(a, 0, len, -3, 0))
    assertThrows(classOf[IndexOutOfBoundsException], Character.offsetByCodePoints(a, 0, len, -3, 4))
    assertThrows(classOf[IndexOutOfBoundsException], Character.offsetByCodePoints(a, 0, len, 6, 18))
    assertThrows(classOf[IndexOutOfBoundsException], Character.offsetByCodePoints(a, 0, len, 30, 2))
    assertThrows(classOf[IndexOutOfBoundsException], Character.offsetByCodePoints(a, 0, len, 30, 0))

    assertThrows(classOf[IndexOutOfBoundsException], Character.offsetByCodePoints(a, 3, 6, 2, 0))
    assertThrows(classOf[IndexOutOfBoundsException], Character.offsetByCodePoints(a, 3, 6, 2, 2))
    assertThrows(classOf[IndexOutOfBoundsException], Character.offsetByCodePoints(a, 3, 6, 7, 5))
    assertThrows(classOf[IndexOutOfBoundsException], Character.offsetByCodePoints(a, 3, 6, 10, 2))
    assertThrows(classOf[IndexOutOfBoundsException], Character.offsetByCodePoints(a, 3, 6, 10, 0))

    assertThrows(classOf[IndexOutOfBoundsException], Character.offsetByCodePoints(a, -1, 6, 2, 0))
    assertThrows(classOf[IndexOutOfBoundsException], Character.offsetByCodePoints(a, 0, 30, 2, 0))
    assertThrows(classOf[IndexOutOfBoundsException], Character.offsetByCodePoints(a, 3, -2, 2, 0))
  }

  @Test def offsetByCodePointsArrayBackwards(): Unit = {
    val a: Array[Char] =
      "abc\ud834\udf06de\ud834\udf06fgh\ud834ij\udf06\ud834kl\udf06".toCharArray()
    val len = a.length

    assertEquals(0, Character.offsetByCodePoints(a, 0, len, len, -18))
    assertEquals(3, Character.offsetByCodePoints(a, 0, len, 5, -1))
    assertEquals(2, Character.offsetByCodePoints(a, 0, 5, 3, -1))
    assertEquals(2, Character.offsetByCodePoints(a, 0, 5, 5, -2))
    assertEquals(2, Character.offsetByCodePoints(a, 0, len, 6, -3))
    assertEquals(12, Character.offsetByCodePoints(a, 5, 12, 17, -5))

    assertEquals(6, Character.offsetByCodePoints(a, 5, 8, 8, -2))
    assertEquals(6, Character.offsetByCodePoints(a, 5, 8, 9, -2))

    assertEquals(3, Character.offsetByCodePoints(a, 3, 5, 6, -2))
    assertEquals(4, Character.offsetByCodePoints(a, 4, 4, 6, -2))

    assertThrows(classOf[IndexOutOfBoundsException], Character.offsetByCodePoints(a, 0, len, -3, -4))
    assertThrows(classOf[IndexOutOfBoundsException], Character.offsetByCodePoints(a, 0, len, 6, -18))
    assertThrows(classOf[IndexOutOfBoundsException], Character.offsetByCodePoints(a, 0, len, 30, -2))

    assertThrows(classOf[IndexOutOfBoundsException], Character.offsetByCodePoints(a, 3, 6, 2, -2))
    assertThrows(classOf[IndexOutOfBoundsException], Character.offsetByCodePoints(a, 3, 6, 7, -5))
    assertThrows(classOf[IndexOutOfBoundsException], Character.offsetByCodePoints(a, 3, 6, 10, -2))
  }

  @Test def toLowerCaseCompareCharAndCodepoint(): Unit = {
    for (ch <- Character.MIN_VALUE to Character.MAX_VALUE)
      assertEquals(Character.toLowerCase(ch), Character.toLowerCase(ch.toInt).toChar)
  }

  @Test def toLowerCaseInt(): Unit = {
    // ASCII, not a letter
    assertEquals(0x000a, Character.toLowerCase(0x000a)) // '\n' => '\n'
    assertEquals(0x0037, Character.toLowerCase(0x0037)) // '7' => '7'
    assertEquals(0xff3b, Character.toLowerCase(0xff3b)) // ［ => ［

    // ASCII, letters
    assertEquals(0x0061, Character.toLowerCase(0x0041)) // A => a
    assertEquals(0x0066, Character.toLowerCase(0x0046)) // F => f
    assertEquals(0x007a, Character.toLowerCase(0x005a)) // Z => z
    assertEquals(0x03b2, Character.toLowerCase(0x0392)) // Β => β
    assertEquals(0x03bc, Character.toLowerCase(0x039c)) // Μ => μ
    assertEquals(0x0101, Character.toLowerCase(0x0100)) // Ā => ā
    assertEquals(0x0103, Character.toLowerCase(0x0102)) // Ă => ă
    assertEquals(0x012f, Character.toLowerCase(0x012e)) // Į => į
    assertEquals(0xff41, Character.toLowerCase(0xff21)) // Ａ => ａ
    assertEquals(0xff5a, Character.toLowerCase(0xff3a)) // Ｚ => ｚ

    // ASCII, letters, no change
    assertEquals(0x0061, Character.toLowerCase(0x0061)) // a => a
    assertEquals(0x0066, Character.toLowerCase(0x0066)) // f => f
    assertEquals(0x007a, Character.toLowerCase(0x007a)) // z => z
    assertEquals(0x0101, Character.toLowerCase(0x0101)) // ā => ā

    // BMP, letters & titlecase
    assertEquals(0x10428, Character.toLowerCase(0x10400)) // 𐐨 => 𐐀
    assertEquals(0x10429, Character.toLowerCase(0x10401)) // 𐐁 => 𐐩
    assertEquals(0x1044e, Character.toLowerCase(0x10426)) // 𐐦 => 𐑎
    assertEquals(0x1044f, Character.toLowerCase(0x10427)) // 𐐧 => 𐑏

    // BMP, no change
    assertEquals(0xffff, Character.toLowerCase(0xffff)) // ￿ => ￿
    assertEquals(0x10000, Character.toLowerCase(0x10000)) // 𐀀 => 𐀀
    assertEquals(0x10001, Character.toLowerCase(0x10001)) // 𐀁 => 𐀁
    assertEquals(0x10fffe, Character.toLowerCase(0x10fffe)) // 􏿾 => 􏿾
    assertEquals(0x10ffff, Character.toLowerCase(0x10ffff)) // 􏿿 => 􏿿
  }

  /* Test all the code points that are specially handled in our implementation
   * of `toLowerCase(codePoint: Int)`.
   *
   * This list happens to coincide with the code points tested in the following
   * test.
   */
  @Test def toLowerCaseCodePointSpecialCases(): Unit =
    assertEquals(0x0069, Character.toLowerCase(0x0130))

  /* Test all the code points for which delegating to `String.toLowerCase()`
   * is not a valid implementation.
   */
  @Test def toLowerCaseCodePointStringLowerCaseDiffCharacterLowerCase(): Unit = {
    assumeTrue(
        s"requires exactly the reference JDK version $ReferenceJDKVersion",
        !executingInJVM || executingInJVMWithJDKIn(ReferenceJDKVersion to ReferenceJDKVersion))

    // BEGIN GENERATED: [tolowercase-code-point-diff-string]
    assertEquals(0x0069, Character.toLowerCase(0x0130)) // İ => i
    // END GENERATED: [tolowercase-code-point-diff-string]
  }

  @Test def toUpperCaseCompareCharAndCodepoint(): Unit = {
    for (ch <- Character.MIN_VALUE to Character.MAX_VALUE)
      assertEquals(Character.toUpperCase(ch), Character.toUpperCase(ch.toInt).toChar)
  }

  @Test def toUpperCaseInt(): Unit = {
    // ASCII, not a letter
    assertEquals(0x000a, Character.toUpperCase(0x000a)) // '\n' => '\n'
    assertEquals(0x0037, Character.toUpperCase(0x0037)) // '7' => '7'
    assertEquals(0xff3b, Character.toUpperCase(0xff3b)) // ［ => ［

    // ASCII, letters
    assertEquals(0x0041, Character.toUpperCase(0x0061)) // a => A
    assertEquals(0x0046, Character.toUpperCase(0x0066)) // f => F
    assertEquals(0x005a, Character.toUpperCase(0x007a)) // z => Z
    assertEquals(0x0392, Character.toUpperCase(0x03d0)) // β => Β
    assertEquals(0x039c, Character.toUpperCase(0x00b5)) // μ => Μ

    // ASCII, letters, no change
    assertEquals(0x0041, Character.toUpperCase(0x0041)) // A => A
    assertEquals(0x0046, Character.toUpperCase(0x0046)) // F => F
    assertEquals(0x005a, Character.toUpperCase(0x005a)) // Z => Z

    // BMP, letters & titlecase
    assertEquals(0x10400, Character.toUpperCase(0x10428)) // 𐐨 => 𐐀
    assertEquals(0x10401, Character.toUpperCase(0x10429)) // 𐐩 => 𐐁
    assertEquals(0x10426, Character.toUpperCase(0x1044e)) // 𐑎 => 𐐦
    assertEquals(0x10427, Character.toUpperCase(0x1044f)) // 𐑏 => 𐐧

    // BMP, no change
    assertEquals(0xffff, Character.toUpperCase(0xffff)) // ￿ => ￿
    assertEquals(0x10000, Character.toUpperCase(0x10000)) // 𐀀 => 𐀀
    assertEquals(0x10001, Character.toUpperCase(0x10001)) // 𐀁 => 𐀁
    assertEquals(0x10fffe, Character.toUpperCase(0x10fffe)) // 􏿾 => 􏿾
    assertEquals(0x10ffff, Character.toUpperCase(0x10ffff)) // 􏿿 => 􏿿
  }

  /* Test all the code points that are specially handled in our implementation
   * of `toUpperCase(codePoint: Int)`.
   */
  @Test def toUpperCaseCodePointSpecialCases(): Unit = {
    assertEquals(0x1fbc, Character.toUpperCase(0x1fb3))
    assertEquals(0x1fcc, Character.toUpperCase(0x1fc3))
    assertEquals(0x1ffc, Character.toUpperCase(0x1ff3))

    assertEquals(0x1f88, Character.toUpperCase(0x1f80))
    assertEquals(0x1f89, Character.toUpperCase(0x1f81))
    assertEquals(0x1f8a, Character.toUpperCase(0x1f82))
    assertEquals(0x1f8b, Character.toUpperCase(0x1f83))
    assertEquals(0x1f8c, Character.toUpperCase(0x1f84))
    assertEquals(0x1f8d, Character.toUpperCase(0x1f85))
    assertEquals(0x1f8e, Character.toUpperCase(0x1f86))
    assertEquals(0x1f8f, Character.toUpperCase(0x1f87))
    assertEquals(0x1f88, Character.toUpperCase(0x1f88))
    assertEquals(0x1f89, Character.toUpperCase(0x1f89))
    assertEquals(0x1f8a, Character.toUpperCase(0x1f8a))
    assertEquals(0x1f8b, Character.toUpperCase(0x1f8b))
    assertEquals(0x1f8c, Character.toUpperCase(0x1f8c))
    assertEquals(0x1f8d, Character.toUpperCase(0x1f8d))
    assertEquals(0x1f8e, Character.toUpperCase(0x1f8e))
    assertEquals(0x1f8f, Character.toUpperCase(0x1f8f))
    assertEquals(0x1f98, Character.toUpperCase(0x1f90))
    assertEquals(0x1f99, Character.toUpperCase(0x1f91))
    assertEquals(0x1f9a, Character.toUpperCase(0x1f92))
    assertEquals(0x1f9b, Character.toUpperCase(0x1f93))
    assertEquals(0x1f9c, Character.toUpperCase(0x1f94))
    assertEquals(0x1f9d, Character.toUpperCase(0x1f95))
    assertEquals(0x1f9e, Character.toUpperCase(0x1f96))
    assertEquals(0x1f9f, Character.toUpperCase(0x1f97))
    assertEquals(0x1f98, Character.toUpperCase(0x1f98))
    assertEquals(0x1f99, Character.toUpperCase(0x1f99))
    assertEquals(0x1f9a, Character.toUpperCase(0x1f9a))
    assertEquals(0x1f9b, Character.toUpperCase(0x1f9b))
    assertEquals(0x1f9c, Character.toUpperCase(0x1f9c))
    assertEquals(0x1f9d, Character.toUpperCase(0x1f9d))
    assertEquals(0x1f9e, Character.toUpperCase(0x1f9e))
    assertEquals(0x1f9f, Character.toUpperCase(0x1f9f))
    assertEquals(0x1fa8, Character.toUpperCase(0x1fa0))
    assertEquals(0x1fa9, Character.toUpperCase(0x1fa1))
    assertEquals(0x1faa, Character.toUpperCase(0x1fa2))
    assertEquals(0x1fab, Character.toUpperCase(0x1fa3))
    assertEquals(0x1fac, Character.toUpperCase(0x1fa4))
    assertEquals(0x1fad, Character.toUpperCase(0x1fa5))
    assertEquals(0x1fae, Character.toUpperCase(0x1fa6))
    assertEquals(0x1faf, Character.toUpperCase(0x1fa7))
    assertEquals(0x1fa8, Character.toUpperCase(0x1fa8))
    assertEquals(0x1fa9, Character.toUpperCase(0x1fa9))
    assertEquals(0x1faa, Character.toUpperCase(0x1faa))
    assertEquals(0x1fab, Character.toUpperCase(0x1fab))
    assertEquals(0x1fac, Character.toUpperCase(0x1fac))
    assertEquals(0x1fad, Character.toUpperCase(0x1fad))
    assertEquals(0x1fae, Character.toUpperCase(0x1fae))
    assertEquals(0x1faf, Character.toUpperCase(0x1faf))
  }

  /* Test all the code points for which delegating to `String.toUpperCase()`
   * is not a valid implementation.
   */
  @Test def toUpperCaseCodePointStringUpperCaseDiffCharacterUpperCase(): Unit = {
    assumeTrue(
        s"requires exactly the reference JDK version $ReferenceJDKVersion",
        !executingInJVM || executingInJVMWithJDKIn(ReferenceJDKVersion to ReferenceJDKVersion))

    // BEGIN GENERATED: [touppercase-code-point-diff-string]
    assertEquals(0x00df, Character.toUpperCase(0x00df)) // ß => ß
    assertEquals(0x0149, Character.toUpperCase(0x0149)) // ŉ => ŉ
    assertEquals(0x01f0, Character.toUpperCase(0x01f0)) // ǰ => ǰ
    assertEquals(0x0390, Character.toUpperCase(0x0390)) // ΐ => ΐ
    assertEquals(0x03b0, Character.toUpperCase(0x03b0)) // ΰ => ΰ
    assertEquals(0x0587, Character.toUpperCase(0x0587)) // և => և
    assertEquals(0x1e96, Character.toUpperCase(0x1e96)) // ẖ => ẖ
    assertEquals(0x1e97, Character.toUpperCase(0x1e97)) // ẗ => ẗ
    assertEquals(0x1e98, Character.toUpperCase(0x1e98)) // ẘ => ẘ
    assertEquals(0x1e99, Character.toUpperCase(0x1e99)) // ẙ => ẙ
    assertEquals(0x1e9a, Character.toUpperCase(0x1e9a)) // ẚ => ẚ
    assertEquals(0x1f50, Character.toUpperCase(0x1f50)) // ὐ => ὐ
    assertEquals(0x1f52, Character.toUpperCase(0x1f52)) // ὒ => ὒ
    assertEquals(0x1f54, Character.toUpperCase(0x1f54)) // ὔ => ὔ
    assertEquals(0x1f56, Character.toUpperCase(0x1f56)) // ὖ => ὖ
    assertEquals(0x1f88, Character.toUpperCase(0x1f80)) // ᾀ => ᾈ
    assertEquals(0x1f89, Character.toUpperCase(0x1f81)) // ᾁ => ᾉ
    assertEquals(0x1f8a, Character.toUpperCase(0x1f82)) // ᾂ => ᾊ
    assertEquals(0x1f8b, Character.toUpperCase(0x1f83)) // ᾃ => ᾋ
    assertEquals(0x1f8c, Character.toUpperCase(0x1f84)) // ᾄ => ᾌ
    assertEquals(0x1f8d, Character.toUpperCase(0x1f85)) // ᾅ => ᾍ
    assertEquals(0x1f8e, Character.toUpperCase(0x1f86)) // ᾆ => ᾎ
    assertEquals(0x1f8f, Character.toUpperCase(0x1f87)) // ᾇ => ᾏ
    assertEquals(0x1f88, Character.toUpperCase(0x1f88)) // ᾈ => ᾈ
    assertEquals(0x1f89, Character.toUpperCase(0x1f89)) // ᾉ => ᾉ
    assertEquals(0x1f8a, Character.toUpperCase(0x1f8a)) // ᾊ => ᾊ
    assertEquals(0x1f8b, Character.toUpperCase(0x1f8b)) // ᾋ => ᾋ
    assertEquals(0x1f8c, Character.toUpperCase(0x1f8c)) // ᾌ => ᾌ
    assertEquals(0x1f8d, Character.toUpperCase(0x1f8d)) // ᾍ => ᾍ
    assertEquals(0x1f8e, Character.toUpperCase(0x1f8e)) // ᾎ => ᾎ
    assertEquals(0x1f8f, Character.toUpperCase(0x1f8f)) // ᾏ => ᾏ
    assertEquals(0x1f98, Character.toUpperCase(0x1f90)) // ᾐ => ᾘ
    assertEquals(0x1f99, Character.toUpperCase(0x1f91)) // ᾑ => ᾙ
    assertEquals(0x1f9a, Character.toUpperCase(0x1f92)) // ᾒ => ᾚ
    assertEquals(0x1f9b, Character.toUpperCase(0x1f93)) // ᾓ => ᾛ
    assertEquals(0x1f9c, Character.toUpperCase(0x1f94)) // ᾔ => ᾜ
    assertEquals(0x1f9d, Character.toUpperCase(0x1f95)) // ᾕ => ᾝ
    assertEquals(0x1f9e, Character.toUpperCase(0x1f96)) // ᾖ => ᾞ
    assertEquals(0x1f9f, Character.toUpperCase(0x1f97)) // ᾗ => ᾟ
    assertEquals(0x1f98, Character.toUpperCase(0x1f98)) // ᾘ => ᾘ
    assertEquals(0x1f99, Character.toUpperCase(0x1f99)) // ᾙ => ᾙ
    assertEquals(0x1f9a, Character.toUpperCase(0x1f9a)) // ᾚ => ᾚ
    assertEquals(0x1f9b, Character.toUpperCase(0x1f9b)) // ᾛ => ᾛ
    assertEquals(0x1f9c, Character.toUpperCase(0x1f9c)) // ᾜ => ᾜ
    assertEquals(0x1f9d, Character.toUpperCase(0x1f9d)) // ᾝ => ᾝ
    assertEquals(0x1f9e, Character.toUpperCase(0x1f9e)) // ᾞ => ᾞ
    assertEquals(0x1f9f, Character.toUpperCase(0x1f9f)) // ᾟ => ᾟ
    assertEquals(0x1fa8, Character.toUpperCase(0x1fa0)) // ᾠ => ᾨ
    assertEquals(0x1fa9, Character.toUpperCase(0x1fa1)) // ᾡ => ᾩ
    assertEquals(0x1faa, Character.toUpperCase(0x1fa2)) // ᾢ => ᾪ
    assertEquals(0x1fab, Character.toUpperCase(0x1fa3)) // ᾣ => ᾫ
    assertEquals(0x1fac, Character.toUpperCase(0x1fa4)) // ᾤ => ᾬ
    assertEquals(0x1fad, Character.toUpperCase(0x1fa5)) // ᾥ => ᾭ
    assertEquals(0x1fae, Character.toUpperCase(0x1fa6)) // ᾦ => ᾮ
    assertEquals(0x1faf, Character.toUpperCase(0x1fa7)) // ᾧ => ᾯ
    assertEquals(0x1fa8, Character.toUpperCase(0x1fa8)) // ᾨ => ᾨ
    assertEquals(0x1fa9, Character.toUpperCase(0x1fa9)) // ᾩ => ᾩ
    assertEquals(0x1faa, Character.toUpperCase(0x1faa)) // ᾪ => ᾪ
    assertEquals(0x1fab, Character.toUpperCase(0x1fab)) // ᾫ => ᾫ
    assertEquals(0x1fac, Character.toUpperCase(0x1fac)) // ᾬ => ᾬ
    assertEquals(0x1fad, Character.toUpperCase(0x1fad)) // ᾭ => ᾭ
    assertEquals(0x1fae, Character.toUpperCase(0x1fae)) // ᾮ => ᾮ
    assertEquals(0x1faf, Character.toUpperCase(0x1faf)) // ᾯ => ᾯ
    assertEquals(0x1fb2, Character.toUpperCase(0x1fb2)) // ᾲ => ᾲ
    assertEquals(0x1fbc, Character.toUpperCase(0x1fb3)) // ᾳ => ᾼ
    assertEquals(0x1fb4, Character.toUpperCase(0x1fb4)) // ᾴ => ᾴ
    assertEquals(0x1fb6, Character.toUpperCase(0x1fb6)) // ᾶ => ᾶ
    assertEquals(0x1fb7, Character.toUpperCase(0x1fb7)) // ᾷ => ᾷ
    assertEquals(0x1fbc, Character.toUpperCase(0x1fbc)) // ᾼ => ᾼ
    assertEquals(0x1fc2, Character.toUpperCase(0x1fc2)) // ῂ => ῂ
    assertEquals(0x1fcc, Character.toUpperCase(0x1fc3)) // ῃ => ῌ
    assertEquals(0x1fc4, Character.toUpperCase(0x1fc4)) // ῄ => ῄ
    assertEquals(0x1fc6, Character.toUpperCase(0x1fc6)) // ῆ => ῆ
    assertEquals(0x1fc7, Character.toUpperCase(0x1fc7)) // ῇ => ῇ
    assertEquals(0x1fcc, Character.toUpperCase(0x1fcc)) // ῌ => ῌ
    assertEquals(0x1fd2, Character.toUpperCase(0x1fd2)) // ῒ => ῒ
    assertEquals(0x1fd3, Character.toUpperCase(0x1fd3)) // ΐ => ΐ
    assertEquals(0x1fd6, Character.toUpperCase(0x1fd6)) // ῖ => ῖ
    assertEquals(0x1fd7, Character.toUpperCase(0x1fd7)) // ῗ => ῗ
    assertEquals(0x1fe2, Character.toUpperCase(0x1fe2)) // ῢ => ῢ
    assertEquals(0x1fe3, Character.toUpperCase(0x1fe3)) // ΰ => ΰ
    assertEquals(0x1fe4, Character.toUpperCase(0x1fe4)) // ῤ => ῤ
    assertEquals(0x1fe6, Character.toUpperCase(0x1fe6)) // ῦ => ῦ
    assertEquals(0x1fe7, Character.toUpperCase(0x1fe7)) // ῧ => ῧ
    assertEquals(0x1ff2, Character.toUpperCase(0x1ff2)) // ῲ => ῲ
    assertEquals(0x1ffc, Character.toUpperCase(0x1ff3)) // ῳ => ῼ
    assertEquals(0x1ff4, Character.toUpperCase(0x1ff4)) // ῴ => ῴ
    assertEquals(0x1ff6, Character.toUpperCase(0x1ff6)) // ῶ => ῶ
    assertEquals(0x1ff7, Character.toUpperCase(0x1ff7)) // ῷ => ῷ
    assertEquals(0x1ffc, Character.toUpperCase(0x1ffc)) // ῼ => ῼ
    assertEquals(0xfb00, Character.toUpperCase(0xfb00)) // ﬀ => ﬀ
    assertEquals(0xfb01, Character.toUpperCase(0xfb01)) // ﬁ => ﬁ
    assertEquals(0xfb02, Character.toUpperCase(0xfb02)) // ﬂ => ﬂ
    assertEquals(0xfb03, Character.toUpperCase(0xfb03)) // ﬃ => ﬃ
    assertEquals(0xfb04, Character.toUpperCase(0xfb04)) // ﬄ => ﬄ
    assertEquals(0xfb05, Character.toUpperCase(0xfb05)) // ﬅ => ﬅ
    assertEquals(0xfb06, Character.toUpperCase(0xfb06)) // ﬆ => ﬆ
    assertEquals(0xfb13, Character.toUpperCase(0xfb13)) // ﬓ => ﬓ
    assertEquals(0xfb14, Character.toUpperCase(0xfb14)) // ﬔ => ﬔ
    assertEquals(0xfb15, Character.toUpperCase(0xfb15)) // ﬕ => ﬕ
    assertEquals(0xfb16, Character.toUpperCase(0xfb16)) // ﬖ => ﬖ
    assertEquals(0xfb17, Character.toUpperCase(0xfb17)) // ﬗ => ﬗ
    // END GENERATED: [touppercase-code-point-diff-string]
  }

  @Test def toTitleCaseCompareCharAndCodepoint(): Unit = {
    for (ch <- Character.MIN_VALUE to Character.MAX_VALUE)
      assertEquals(Character.toTitleCase(ch), Character.toTitleCase(ch.toInt).toChar)
  }

  @Test def toTitleCaseCodePointSpecialCases(): Unit = {
    assertEquals(0x1fbc, Character.toTitleCase(0x1fb3))
    assertEquals(0x1fcc, Character.toTitleCase(0x1fc3))
    assertEquals(0x1ffc, Character.toTitleCase(0x1ff3))

    assertEquals(0x1f88, Character.toTitleCase(0x1f80))
    assertEquals(0x1f89, Character.toTitleCase(0x1f81))
    assertEquals(0x1f8a, Character.toTitleCase(0x1f82))
    assertEquals(0x1f8b, Character.toTitleCase(0x1f83))
    assertEquals(0x1f8c, Character.toTitleCase(0x1f84))
    assertEquals(0x1f8d, Character.toTitleCase(0x1f85))
    assertEquals(0x1f8e, Character.toTitleCase(0x1f86))
    assertEquals(0x1f8f, Character.toTitleCase(0x1f87))
    assertEquals(0x1f88, Character.toTitleCase(0x1f88))
    assertEquals(0x1f89, Character.toTitleCase(0x1f89))
    assertEquals(0x1f8a, Character.toTitleCase(0x1f8a))
    assertEquals(0x1f8b, Character.toTitleCase(0x1f8b))
    assertEquals(0x1f8c, Character.toTitleCase(0x1f8c))
    assertEquals(0x1f8d, Character.toTitleCase(0x1f8d))
    assertEquals(0x1f8e, Character.toTitleCase(0x1f8e))
    assertEquals(0x1f8f, Character.toTitleCase(0x1f8f))
    assertEquals(0x1f98, Character.toTitleCase(0x1f90))
    assertEquals(0x1f99, Character.toTitleCase(0x1f91))
    assertEquals(0x1f9a, Character.toTitleCase(0x1f92))
    assertEquals(0x1f9b, Character.toTitleCase(0x1f93))
    assertEquals(0x1f9c, Character.toTitleCase(0x1f94))
    assertEquals(0x1f9d, Character.toTitleCase(0x1f95))
    assertEquals(0x1f9e, Character.toTitleCase(0x1f96))
    assertEquals(0x1f9f, Character.toTitleCase(0x1f97))
    assertEquals(0x1f98, Character.toTitleCase(0x1f98))
    assertEquals(0x1f99, Character.toTitleCase(0x1f99))
    assertEquals(0x1f9a, Character.toTitleCase(0x1f9a))
    assertEquals(0x1f9b, Character.toTitleCase(0x1f9b))
    assertEquals(0x1f9c, Character.toTitleCase(0x1f9c))
    assertEquals(0x1f9d, Character.toTitleCase(0x1f9d))
    assertEquals(0x1f9e, Character.toTitleCase(0x1f9e))
    assertEquals(0x1f9f, Character.toTitleCase(0x1f9f))
    assertEquals(0x1fa8, Character.toTitleCase(0x1fa0))
    assertEquals(0x1fa9, Character.toTitleCase(0x1fa1))
    assertEquals(0x1faa, Character.toTitleCase(0x1fa2))
    assertEquals(0x1fab, Character.toTitleCase(0x1fa3))
    assertEquals(0x1fac, Character.toTitleCase(0x1fa4))
    assertEquals(0x1fad, Character.toTitleCase(0x1fa5))
    assertEquals(0x1fae, Character.toTitleCase(0x1fa6))
    assertEquals(0x1faf, Character.toTitleCase(0x1fa7))
    assertEquals(0x1fa8, Character.toTitleCase(0x1fa8))
    assertEquals(0x1fa9, Character.toTitleCase(0x1fa9))
    assertEquals(0x1faa, Character.toTitleCase(0x1faa))
    assertEquals(0x1fab, Character.toTitleCase(0x1fab))
    assertEquals(0x1fac, Character.toTitleCase(0x1fac))
    assertEquals(0x1fad, Character.toTitleCase(0x1fad))
    assertEquals(0x1fae, Character.toTitleCase(0x1fae))
    assertEquals(0x1faf, Character.toTitleCase(0x1faf))
  }

  @Test def toTitleCaseCodePointStringUpperCaseDiffCharacterTitleCase(): Unit = {
    assumeTrue(
        s"requires exactly the reference JDK version $ReferenceJDKVersion",
        !executingInJVM || executingInJVMWithJDKIn(ReferenceJDKVersion to ReferenceJDKVersion))

    // BEGIN GENERATED: [totitlecase-code-point-diff-string-touppercase]
    assertEquals(0x00df, Character.toTitleCase(0x00df)) // ß => ß
    assertEquals(0x0149, Character.toTitleCase(0x0149)) // ŉ => ŉ
    assertEquals(0x01c5, Character.toTitleCase(0x01c4)) // Ǆ => ǅ
    assertEquals(0x01c5, Character.toTitleCase(0x01c5)) // ǅ => ǅ
    assertEquals(0x01c5, Character.toTitleCase(0x01c6)) // ǆ => ǅ
    assertEquals(0x01c8, Character.toTitleCase(0x01c7)) // Ǉ => ǈ
    assertEquals(0x01c8, Character.toTitleCase(0x01c8)) // ǈ => ǈ
    assertEquals(0x01c8, Character.toTitleCase(0x01c9)) // ǉ => ǈ
    assertEquals(0x01cb, Character.toTitleCase(0x01ca)) // Ǌ => ǋ
    assertEquals(0x01cb, Character.toTitleCase(0x01cb)) // ǋ => ǋ
    assertEquals(0x01cb, Character.toTitleCase(0x01cc)) // ǌ => ǋ
    assertEquals(0x01f0, Character.toTitleCase(0x01f0)) // ǰ => ǰ
    assertEquals(0x01f2, Character.toTitleCase(0x01f1)) // Ǳ => ǲ
    assertEquals(0x01f2, Character.toTitleCase(0x01f2)) // ǲ => ǲ
    assertEquals(0x01f2, Character.toTitleCase(0x01f3)) // ǳ => ǲ
    assertEquals(0x0390, Character.toTitleCase(0x0390)) // ΐ => ΐ
    assertEquals(0x03b0, Character.toTitleCase(0x03b0)) // ΰ => ΰ
    assertEquals(0x0587, Character.toTitleCase(0x0587)) // և => և
    assertEquals(0x10d0, Character.toTitleCase(0x10d0)) // ა => ა
    assertEquals(0x10d1, Character.toTitleCase(0x10d1)) // ბ => ბ
    assertEquals(0x10d2, Character.toTitleCase(0x10d2)) // გ => გ
    assertEquals(0x10d3, Character.toTitleCase(0x10d3)) // დ => დ
    assertEquals(0x10d4, Character.toTitleCase(0x10d4)) // ე => ე
    assertEquals(0x10d5, Character.toTitleCase(0x10d5)) // ვ => ვ
    assertEquals(0x10d6, Character.toTitleCase(0x10d6)) // ზ => ზ
    assertEquals(0x10d7, Character.toTitleCase(0x10d7)) // თ => თ
    assertEquals(0x10d8, Character.toTitleCase(0x10d8)) // ი => ი
    assertEquals(0x10d9, Character.toTitleCase(0x10d9)) // კ => კ
    assertEquals(0x10da, Character.toTitleCase(0x10da)) // ლ => ლ
    assertEquals(0x10db, Character.toTitleCase(0x10db)) // მ => მ
    assertEquals(0x10dc, Character.toTitleCase(0x10dc)) // ნ => ნ
    assertEquals(0x10dd, Character.toTitleCase(0x10dd)) // ო => ო
    assertEquals(0x10de, Character.toTitleCase(0x10de)) // პ => პ
    assertEquals(0x10df, Character.toTitleCase(0x10df)) // ჟ => ჟ
    assertEquals(0x10e0, Character.toTitleCase(0x10e0)) // რ => რ
    assertEquals(0x10e1, Character.toTitleCase(0x10e1)) // ს => ს
    assertEquals(0x10e2, Character.toTitleCase(0x10e2)) // ტ => ტ
    assertEquals(0x10e3, Character.toTitleCase(0x10e3)) // უ => უ
    assertEquals(0x10e4, Character.toTitleCase(0x10e4)) // ფ => ფ
    assertEquals(0x10e5, Character.toTitleCase(0x10e5)) // ქ => ქ
    assertEquals(0x10e6, Character.toTitleCase(0x10e6)) // ღ => ღ
    assertEquals(0x10e7, Character.toTitleCase(0x10e7)) // ყ => ყ
    assertEquals(0x10e8, Character.toTitleCase(0x10e8)) // შ => შ
    assertEquals(0x10e9, Character.toTitleCase(0x10e9)) // ჩ => ჩ
    assertEquals(0x10ea, Character.toTitleCase(0x10ea)) // ც => ც
    assertEquals(0x10eb, Character.toTitleCase(0x10eb)) // ძ => ძ
    assertEquals(0x10ec, Character.toTitleCase(0x10ec)) // წ => წ
    assertEquals(0x10ed, Character.toTitleCase(0x10ed)) // ჭ => ჭ
    assertEquals(0x10ee, Character.toTitleCase(0x10ee)) // ხ => ხ
    assertEquals(0x10ef, Character.toTitleCase(0x10ef)) // ჯ => ჯ
    assertEquals(0x10f0, Character.toTitleCase(0x10f0)) // ჰ => ჰ
    assertEquals(0x10f1, Character.toTitleCase(0x10f1)) // ჱ => ჱ
    assertEquals(0x10f2, Character.toTitleCase(0x10f2)) // ჲ => ჲ
    assertEquals(0x10f3, Character.toTitleCase(0x10f3)) // ჳ => ჳ
    assertEquals(0x10f4, Character.toTitleCase(0x10f4)) // ჴ => ჴ
    assertEquals(0x10f5, Character.toTitleCase(0x10f5)) // ჵ => ჵ
    assertEquals(0x10f6, Character.toTitleCase(0x10f6)) // ჶ => ჶ
    assertEquals(0x10f7, Character.toTitleCase(0x10f7)) // ჷ => ჷ
    assertEquals(0x10f8, Character.toTitleCase(0x10f8)) // ჸ => ჸ
    assertEquals(0x10f9, Character.toTitleCase(0x10f9)) // ჹ => ჹ
    assertEquals(0x10fa, Character.toTitleCase(0x10fa)) // ჺ => ჺ
    assertEquals(0x10fd, Character.toTitleCase(0x10fd)) // ჽ => ჽ
    assertEquals(0x10fe, Character.toTitleCase(0x10fe)) // ჾ => ჾ
    assertEquals(0x10ff, Character.toTitleCase(0x10ff)) // ჿ => ჿ
    assertEquals(0x1e96, Character.toTitleCase(0x1e96)) // ẖ => ẖ
    assertEquals(0x1e97, Character.toTitleCase(0x1e97)) // ẗ => ẗ
    assertEquals(0x1e98, Character.toTitleCase(0x1e98)) // ẘ => ẘ
    assertEquals(0x1e99, Character.toTitleCase(0x1e99)) // ẙ => ẙ
    assertEquals(0x1e9a, Character.toTitleCase(0x1e9a)) // ẚ => ẚ
    assertEquals(0x1f50, Character.toTitleCase(0x1f50)) // ὐ => ὐ
    assertEquals(0x1f52, Character.toTitleCase(0x1f52)) // ὒ => ὒ
    assertEquals(0x1f54, Character.toTitleCase(0x1f54)) // ὔ => ὔ
    assertEquals(0x1f56, Character.toTitleCase(0x1f56)) // ὖ => ὖ
    assertEquals(0x1f88, Character.toTitleCase(0x1f80)) // ᾀ => ᾈ
    assertEquals(0x1f89, Character.toTitleCase(0x1f81)) // ᾁ => ᾉ
    assertEquals(0x1f8a, Character.toTitleCase(0x1f82)) // ᾂ => ᾊ
    assertEquals(0x1f8b, Character.toTitleCase(0x1f83)) // ᾃ => ᾋ
    assertEquals(0x1f8c, Character.toTitleCase(0x1f84)) // ᾄ => ᾌ
    assertEquals(0x1f8d, Character.toTitleCase(0x1f85)) // ᾅ => ᾍ
    assertEquals(0x1f8e, Character.toTitleCase(0x1f86)) // ᾆ => ᾎ
    assertEquals(0x1f8f, Character.toTitleCase(0x1f87)) // ᾇ => ᾏ
    assertEquals(0x1f88, Character.toTitleCase(0x1f88)) // ᾈ => ᾈ
    assertEquals(0x1f89, Character.toTitleCase(0x1f89)) // ᾉ => ᾉ
    assertEquals(0x1f8a, Character.toTitleCase(0x1f8a)) // ᾊ => ᾊ
    assertEquals(0x1f8b, Character.toTitleCase(0x1f8b)) // ᾋ => ᾋ
    assertEquals(0x1f8c, Character.toTitleCase(0x1f8c)) // ᾌ => ᾌ
    assertEquals(0x1f8d, Character.toTitleCase(0x1f8d)) // ᾍ => ᾍ
    assertEquals(0x1f8e, Character.toTitleCase(0x1f8e)) // ᾎ => ᾎ
    assertEquals(0x1f8f, Character.toTitleCase(0x1f8f)) // ᾏ => ᾏ
    assertEquals(0x1f98, Character.toTitleCase(0x1f90)) // ᾐ => ᾘ
    assertEquals(0x1f99, Character.toTitleCase(0x1f91)) // ᾑ => ᾙ
    assertEquals(0x1f9a, Character.toTitleCase(0x1f92)) // ᾒ => ᾚ
    assertEquals(0x1f9b, Character.toTitleCase(0x1f93)) // ᾓ => ᾛ
    assertEquals(0x1f9c, Character.toTitleCase(0x1f94)) // ᾔ => ᾜ
    assertEquals(0x1f9d, Character.toTitleCase(0x1f95)) // ᾕ => ᾝ
    assertEquals(0x1f9e, Character.toTitleCase(0x1f96)) // ᾖ => ᾞ
    assertEquals(0x1f9f, Character.toTitleCase(0x1f97)) // ᾗ => ᾟ
    assertEquals(0x1f98, Character.toTitleCase(0x1f98)) // ᾘ => ᾘ
    assertEquals(0x1f99, Character.toTitleCase(0x1f99)) // ᾙ => ᾙ
    assertEquals(0x1f9a, Character.toTitleCase(0x1f9a)) // ᾚ => ᾚ
    assertEquals(0x1f9b, Character.toTitleCase(0x1f9b)) // ᾛ => ᾛ
    assertEquals(0x1f9c, Character.toTitleCase(0x1f9c)) // ᾜ => ᾜ
    assertEquals(0x1f9d, Character.toTitleCase(0x1f9d)) // ᾝ => ᾝ
    assertEquals(0x1f9e, Character.toTitleCase(0x1f9e)) // ᾞ => ᾞ
    assertEquals(0x1f9f, Character.toTitleCase(0x1f9f)) // ᾟ => ᾟ
    assertEquals(0x1fa8, Character.toTitleCase(0x1fa0)) // ᾠ => ᾨ
    assertEquals(0x1fa9, Character.toTitleCase(0x1fa1)) // ᾡ => ᾩ
    assertEquals(0x1faa, Character.toTitleCase(0x1fa2)) // ᾢ => ᾪ
    assertEquals(0x1fab, Character.toTitleCase(0x1fa3)) // ᾣ => ᾫ
    assertEquals(0x1fac, Character.toTitleCase(0x1fa4)) // ᾤ => ᾬ
    assertEquals(0x1fad, Character.toTitleCase(0x1fa5)) // ᾥ => ᾭ
    assertEquals(0x1fae, Character.toTitleCase(0x1fa6)) // ᾦ => ᾮ
    assertEquals(0x1faf, Character.toTitleCase(0x1fa7)) // ᾧ => ᾯ
    assertEquals(0x1fa8, Character.toTitleCase(0x1fa8)) // ᾨ => ᾨ
    assertEquals(0x1fa9, Character.toTitleCase(0x1fa9)) // ᾩ => ᾩ
    assertEquals(0x1faa, Character.toTitleCase(0x1faa)) // ᾪ => ᾪ
    assertEquals(0x1fab, Character.toTitleCase(0x1fab)) // ᾫ => ᾫ
    assertEquals(0x1fac, Character.toTitleCase(0x1fac)) // ᾬ => ᾬ
    assertEquals(0x1fad, Character.toTitleCase(0x1fad)) // ᾭ => ᾭ
    assertEquals(0x1fae, Character.toTitleCase(0x1fae)) // ᾮ => ᾮ
    assertEquals(0x1faf, Character.toTitleCase(0x1faf)) // ᾯ => ᾯ
    assertEquals(0x1fb2, Character.toTitleCase(0x1fb2)) // ᾲ => ᾲ
    assertEquals(0x1fbc, Character.toTitleCase(0x1fb3)) // ᾳ => ᾼ
    assertEquals(0x1fb4, Character.toTitleCase(0x1fb4)) // ᾴ => ᾴ
    assertEquals(0x1fb6, Character.toTitleCase(0x1fb6)) // ᾶ => ᾶ
    assertEquals(0x1fb7, Character.toTitleCase(0x1fb7)) // ᾷ => ᾷ
    assertEquals(0x1fbc, Character.toTitleCase(0x1fbc)) // ᾼ => ᾼ
    assertEquals(0x1fc2, Character.toTitleCase(0x1fc2)) // ῂ => ῂ
    assertEquals(0x1fcc, Character.toTitleCase(0x1fc3)) // ῃ => ῌ
    assertEquals(0x1fc4, Character.toTitleCase(0x1fc4)) // ῄ => ῄ
    assertEquals(0x1fc6, Character.toTitleCase(0x1fc6)) // ῆ => ῆ
    assertEquals(0x1fc7, Character.toTitleCase(0x1fc7)) // ῇ => ῇ
    assertEquals(0x1fcc, Character.toTitleCase(0x1fcc)) // ῌ => ῌ
    assertEquals(0x1fd2, Character.toTitleCase(0x1fd2)) // ῒ => ῒ
    assertEquals(0x1fd3, Character.toTitleCase(0x1fd3)) // ΐ => ΐ
    assertEquals(0x1fd6, Character.toTitleCase(0x1fd6)) // ῖ => ῖ
    assertEquals(0x1fd7, Character.toTitleCase(0x1fd7)) // ῗ => ῗ
    assertEquals(0x1fe2, Character.toTitleCase(0x1fe2)) // ῢ => ῢ
    assertEquals(0x1fe3, Character.toTitleCase(0x1fe3)) // ΰ => ΰ
    assertEquals(0x1fe4, Character.toTitleCase(0x1fe4)) // ῤ => ῤ
    assertEquals(0x1fe6, Character.toTitleCase(0x1fe6)) // ῦ => ῦ
    assertEquals(0x1fe7, Character.toTitleCase(0x1fe7)) // ῧ => ῧ
    assertEquals(0x1ff2, Character.toTitleCase(0x1ff2)) // ῲ => ῲ
    assertEquals(0x1ffc, Character.toTitleCase(0x1ff3)) // ῳ => ῼ
    assertEquals(0x1ff4, Character.toTitleCase(0x1ff4)) // ῴ => ῴ
    assertEquals(0x1ff6, Character.toTitleCase(0x1ff6)) // ῶ => ῶ
    assertEquals(0x1ff7, Character.toTitleCase(0x1ff7)) // ῷ => ῷ
    assertEquals(0x1ffc, Character.toTitleCase(0x1ffc)) // ῼ => ῼ
    assertEquals(0xfb00, Character.toTitleCase(0xfb00)) // ﬀ => ﬀ
    assertEquals(0xfb01, Character.toTitleCase(0xfb01)) // ﬁ => ﬁ
    assertEquals(0xfb02, Character.toTitleCase(0xfb02)) // ﬂ => ﬂ
    assertEquals(0xfb03, Character.toTitleCase(0xfb03)) // ﬃ => ﬃ
    assertEquals(0xfb04, Character.toTitleCase(0xfb04)) // ﬄ => ﬄ
    assertEquals(0xfb05, Character.toTitleCase(0xfb05)) // ﬅ => ﬅ
    assertEquals(0xfb06, Character.toTitleCase(0xfb06)) // ﬆ => ﬆ
    assertEquals(0xfb13, Character.toTitleCase(0xfb13)) // ﬓ => ﬓ
    assertEquals(0xfb14, Character.toTitleCase(0xfb14)) // ﬔ => ﬔ
    assertEquals(0xfb15, Character.toTitleCase(0xfb15)) // ﬕ => ﬕ
    assertEquals(0xfb16, Character.toTitleCase(0xfb16)) // ﬖ => ﬖ
    assertEquals(0xfb17, Character.toTitleCase(0xfb17)) // ﬗ => ﬗ
    // END GENERATED: [totitlecase-code-point-diff-string-touppercase]
  }

  @Test def codePointCountString(): Unit = {
    val s: String =
      "abc\uD834\uDF06de\uD834\uDF06fgh\uD834ij\uDF06\uD834kl\uDF06"

    assertEquals(18, Character.codePointCount(s, 0, s.length))
    assertEquals(1, Character.codePointCount(s, 3, 5))
    assertEquals(1, Character.codePointCount(s, 2, 3))
    assertEquals(2, Character.codePointCount(s, 2, 4))
    assertEquals(2, Character.codePointCount(s, 2, 5))
    assertEquals(3, Character.codePointCount(s, 2, 6))
    assertEquals(5, Character.codePointCount(s, 12, 17))
    assertEquals(2, Character.codePointCount(s, 8, 10))
    assertEquals(2, Character.codePointCount(s, 7, 10))
    assertEquals(0, Character.codePointCount(s, 7, 7))
    assertEquals(1, Character.codePointCount(s, s.length - 1, s.length))
    assertEquals(0, Character.codePointCount(s, s.length - 1, s.length - 1))
    assertEquals(0, Character.codePointCount(s, s.length, s.length))

    assertThrows(classOf[IndexOutOfBoundsException], Character.codePointCount(s, -3, 4))
    assertThrows(classOf[IndexOutOfBoundsException], Character.codePointCount(s, 6, 2))
    assertThrows(classOf[IndexOutOfBoundsException], Character.codePointCount(s, 10, 30))
  }

  @Test def codePointCountCharSequence(): Unit = {
    import WrappedStringCharSequence.charSequence

    val cs: CharSequence =
      charSequence("abc\uD834\uDF06de\uD834\uDF06fgh\uD834ij\uDF06\uD834kl\uDF06")

    assertEquals(18, Character.codePointCount(cs, 0, cs.length))
    assertEquals(1, Character.codePointCount(cs, 3, 5))
    assertEquals(1, Character.codePointCount(cs, 2, 3))
    assertEquals(2, Character.codePointCount(cs, 2, 4))
    assertEquals(2, Character.codePointCount(cs, 2, 5))
    assertEquals(3, Character.codePointCount(cs, 2, 6))
    assertEquals(5, Character.codePointCount(cs, 12, 17))
    assertEquals(2, Character.codePointCount(cs, 8, 10))
    assertEquals(2, Character.codePointCount(cs, 7, 10))
    assertEquals(0, Character.codePointCount(cs, 7, 7))
    assertEquals(1, Character.codePointCount(cs, cs.length - 1, cs.length))
    assertEquals(0, Character.codePointCount(cs, cs.length - 1, cs.length - 1))
    assertEquals(0, Character.codePointCount(cs, cs.length, cs.length))

    assertThrows(classOf[IndexOutOfBoundsException], Character.codePointCount(cs, -3, 4))
    assertThrows(classOf[IndexOutOfBoundsException], Character.codePointCount(cs, 6, 2))
    assertThrows(classOf[IndexOutOfBoundsException], Character.codePointCount(cs, 10, 30))
  }

  @Test def codePointCountArray(): Unit = {
    /* Attention! The third argument is a count/length value, not an end/limit value.
     * To keep consistency with the tests above, we use the same values and
     * subtract the start offset.
     */

    val a: Array[Char] =
      "abc\uD834\uDF06de\uD834\uDF06fgh\uD834ij\uDF06\uD834kl\uDF06".toCharArray()

    assertEquals(18, Character.codePointCount(a, 0, a.length - 0))
    assertEquals(1, Character.codePointCount(a, 3, 5 - 3))
    assertEquals(1, Character.codePointCount(a, 2, 3 - 2))
    assertEquals(2, Character.codePointCount(a, 2, 4 - 2))
    assertEquals(2, Character.codePointCount(a, 2, 5 - 2))
    assertEquals(3, Character.codePointCount(a, 2, 6 - 2))
    assertEquals(5, Character.codePointCount(a, 12, 17 - 12))
    assertEquals(2, Character.codePointCount(a, 8, 10 - 8))
    assertEquals(2, Character.codePointCount(a, 7, 10 - 7))
    assertEquals(0, Character.codePointCount(a, 7, 7 - 7))
    assertEquals(1, Character.codePointCount(a, a.length - 1, a.length - (a.length - 1)))
    assertEquals(0, Character.codePointCount(a, a.length - 1, a.length - 1 - (a.length - 1)))
    assertEquals(0, Character.codePointCount(a, a.length, a.length - a.length))

    assertThrows(classOf[IndexOutOfBoundsException], Character.codePointCount(a, -3, 4 - (-3)))
    assertThrows(classOf[IndexOutOfBoundsException], Character.codePointCount(a, 6, 2 - 6))
    assertThrows(classOf[IndexOutOfBoundsException], Character.codePointCount(a, 10, 30 - 10))
  }

  @Test def compare(): Unit = {
    assertTrue(Character.compare('0', '5') < 0)
    assertTrue(Character.compare('o', 'g') > 0)
    assertTrue(Character.compare('A', 'a') < 0)
    assertEquals(0, Character.compare('b', 'b'))
  }

  @Test def compareTo(): Unit = {
    def compare(x: Char, y: Char): Int =
      new Character(x).compareTo(new Character(y))

    assertTrue(compare('0', '5') < 0)
    assertTrue(compare('o', 'g') > 0)
    assertTrue(compare('A', 'a') < 0)
    assertEquals(0, compare('b', 'b'))
  }

  @Test def compareToAnyAny(): Unit = {
    def compare(x: Any, y: Any): Int =
      x.asInstanceOf[Comparable[Any]].compareTo(y)

    assertTrue(compare('0', '5') < 0)
    assertTrue(compare('o', 'g') > 0)
    assertTrue(compare('A', 'a') < 0)
    assertEquals(0, compare('b', 'b'))
  }

  @Test def isSpace(): Unit = {
    def reference(ch: Char): Boolean = ch match {
      case '\t' | '\n' | '\f' | '\r' | ' ' => true
      case _                               => false
    }

    for (ch <- Char.MinValue to Char.MaxValue)
      assertEquals(reference(ch), Character.isSpace(ch))
  }

  @Test def isJavaLetter(): Unit = {
    def reference(ch: Char): Boolean =
      Character.isJavaIdentifierStart(ch)

    for (ch <- Char.MinValue to Char.MaxValue)
      assertEquals(reference(ch), Character.isJavaLetter(ch))
  }

  @Test def isJavaLetterOrDigit(): Unit = {
    def reference(ch: Char): Boolean =
      Character.isJavaIdentifierPart(ch)

    for (ch <- Char.MinValue to Char.MaxValue)
      assertEquals(reference(ch), Character.isJavaLetterOrDigit(ch))
  }

  @Test def reverseBytes(): Unit =
    assertEquals('\u3412', Character.reverseBytes('\u1234'))
}
