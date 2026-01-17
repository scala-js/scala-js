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

  @Test def isISOControl(): Unit = {
    val isoControlChars = (('\u0000' to '\u001F') ++
      ('\u007F' to '\u009F')).map(_.toInt).toSet
    isoControlChars foreach { c =>
      assertEquals(true, Character.isISOControl(c))
    }

    val randomInts = List.fill(100)(scala.util.Random.nextInt)
    ((-1000 to 1000) ++ randomInts).filterNot(isoControlChars) foreach { c =>
      assertEquals(false, Character.isISOControl(c))
    }
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

  @Test def isDigit(): Unit = {
    assertFalse(Character.isDigit('a'))
    assertTrue(Character.isDigit('0'))
    assertTrue(Character.isDigit('5'))
    assertTrue(Character.isDigit('9'))
    assertFalse(Character.isDigit('z'))
    assertFalse(Character.isDigit(' '))

    // 100 randomly chosen characters that produce true
    assertTrue(Character.isDigit('\u0661'))
    assertTrue(Character.isDigit('\u0663'))
    assertTrue(Character.isDigit('\u0664'))
    assertTrue(Character.isDigit('\u0665'))
    assertTrue(Character.isDigit('\u0669'))
    assertTrue(Character.isDigit('\u06F1'))
    assertTrue(Character.isDigit('\u06F3'))
    assertTrue(Character.isDigit('\u06F5'))
    assertTrue(Character.isDigit('\u07C3'))
    assertTrue(Character.isDigit('\u07C7'))
    assertTrue(Character.isDigit('\u0967'))
    assertTrue(Character.isDigit('\u096C'))
    assertTrue(Character.isDigit('\u096F'))
    assertTrue(Character.isDigit('\u09E6'))
    assertTrue(Character.isDigit('\u09E8'))
    assertTrue(Character.isDigit('\u09EA'))
    assertTrue(Character.isDigit('\u09ED'))
    assertTrue(Character.isDigit('\u0A66'))
    assertTrue(Character.isDigit('\u0A69'))
    assertTrue(Character.isDigit('\u0A6D'))
    assertTrue(Character.isDigit('\u0BE9'))
    assertTrue(Character.isDigit('\u0C67'))
    assertTrue(Character.isDigit('\u0C6A'))
    assertTrue(Character.isDigit('\u0CE6'))
    assertTrue(Character.isDigit('\u0CE9'))
    assertTrue(Character.isDigit('\u0CEC'))
    assertTrue(Character.isDigit('\u0CED'))
    assertTrue(Character.isDigit('\u0CEF'))
    assertTrue(Character.isDigit('\u0D6B'))
    assertTrue(Character.isDigit('\u0D6C'))
    assertTrue(Character.isDigit('\u0E51'))
    assertTrue(Character.isDigit('\u0E53'))
    assertTrue(Character.isDigit('\u0E54'))
    assertTrue(Character.isDigit('\u0ED3'))
    assertTrue(Character.isDigit('\u0ED6'))
    assertTrue(Character.isDigit('\u0F20'))
    assertTrue(Character.isDigit('\u0F21'))
    assertTrue(Character.isDigit('\u1040'))
    assertTrue(Character.isDigit('\u1048'))
    assertTrue(Character.isDigit('\u1091'))
    assertTrue(Character.isDigit('\u1094'))
    assertTrue(Character.isDigit('\u17E0'))
    assertTrue(Character.isDigit('\u1811'))
    assertTrue(Character.isDigit('\u1814'))
    assertTrue(Character.isDigit('\u1815'))
    assertTrue(Character.isDigit('\u1946'))
    assertTrue(Character.isDigit('\u1947'))
    assertTrue(Character.isDigit('\u1948'))
    assertTrue(Character.isDigit('\u194A'))
    assertTrue(Character.isDigit('\u194C'))
    assertTrue(Character.isDigit('\u19D4'))
    assertTrue(Character.isDigit('\u19D5'))
    assertTrue(Character.isDigit('\u19D6'))
    assertTrue(Character.isDigit('\u19D9'))
    assertTrue(Character.isDigit('\u1A81'))
    assertTrue(Character.isDigit('\u1A84'))
    assertTrue(Character.isDigit('\u1A86'))
    assertTrue(Character.isDigit('\u1A87'))
    assertTrue(Character.isDigit('\u1A91'))
    assertTrue(Character.isDigit('\u1A92'))
    assertTrue(Character.isDigit('\u1A96'))
    assertTrue(Character.isDigit('\u1A98'))
    assertTrue(Character.isDigit('\u1B56'))
    assertTrue(Character.isDigit('\u1B58'))
    assertTrue(Character.isDigit('\u1BB0'))
    assertTrue(Character.isDigit('\u1BB2'))
    assertTrue(Character.isDigit('\u1C43'))
    assertTrue(Character.isDigit('\u1C51'))
    assertTrue(Character.isDigit('\uA622'))
    assertTrue(Character.isDigit('\uA624'))
    assertTrue(Character.isDigit('\uA8D1'))
    assertTrue(Character.isDigit('\uA8D6'))
    assertTrue(Character.isDigit('\uA8D8'))
    assertTrue(Character.isDigit('\uA9D2'))
    assertTrue(Character.isDigit('\uA9D8'))
    assertTrue(Character.isDigit('\uAA50'))
    assertTrue(Character.isDigit('\uAA52'))
    assertTrue(Character.isDigit('\uAA56'))
    assertTrue(Character.isDigit('\uABF5'))
    assertTrue(Character.isDigit('\uFF12'))
    assertTrue(Character.isDigit('\uFF14'))
    assertTrue(Character.isDigit('\u0033'))

    // 100 randomly chosen characters that produce false
    assertFalse(Character.isDigit('\u04A3'))
    assertFalse(Character.isDigit('\u064B'))
    assertFalse(Character.isDigit('\u078A'))
    assertFalse(Character.isDigit('\u0983'))
    assertFalse(Character.isDigit('\u0AD0'))
    assertFalse(Character.isDigit('\u0B3D'))
    assertFalse(Character.isDigit('\u0E18'))
    assertFalse(Character.isDigit('\u0F53'))
    assertFalse(Character.isDigit('\u1433'))
    assertFalse(Character.isDigit('\u1AD1'))
    assertFalse(Character.isDigit('\u1D41'))
    assertFalse(Character.isDigit('\u1FB6'))
    assertFalse(Character.isDigit('\u2351'))
    assertFalse(Character.isDigit('\u2F86'))
    assertFalse(Character.isDigit('\u3198'))
    assertFalse(Character.isDigit('\u3533'))
    assertFalse(Character.isDigit('\u3FE1'))
    assertFalse(Character.isDigit('\u4717'))
    assertFalse(Character.isDigit('\u47BE'))
    assertFalse(Character.isDigit('\u4A75'))
    assertFalse(Character.isDigit('\u4B4E'))
    assertFalse(Character.isDigit('\u4C18'))
    assertFalse(Character.isDigit('\u531E'))
    assertFalse(Character.isDigit('\u5445'))
    assertFalse(Character.isDigit('\u54CE'))
    assertFalse(Character.isDigit('\u556E'))
    assertFalse(Character.isDigit('\u56A6'))
    assertFalse(Character.isDigit('\u587C'))
    assertFalse(Character.isDigit('\u5972'))
    assertFalse(Character.isDigit('\u5A9A'))
    assertFalse(Character.isDigit('\u5D39'))
    assertFalse(Character.isDigit('\u60CD'))
    assertFalse(Character.isDigit('\u60FC'))
    assertFalse(Character.isDigit('\u611A'))
    assertFalse(Character.isDigit('\u61DC'))
    assertFalse(Character.isDigit('\u61EE'))
    assertFalse(Character.isDigit('\u6750'))
    assertFalse(Character.isDigit('\u683B'))
    assertFalse(Character.isDigit('\u6878'))
    assertFalse(Character.isDigit('\u68A2'))
    assertFalse(Character.isDigit('\u6B6B'))
    assertFalse(Character.isDigit('\u6E83'))
    assertFalse(Character.isDigit('\u7099'))
    assertFalse(Character.isDigit('\u7717'))
    assertFalse(Character.isDigit('\u7AAC'))
    assertFalse(Character.isDigit('\u7B96'))
    assertFalse(Character.isDigit('\u80C3'))
    assertFalse(Character.isDigit('\u828F'))
    assertFalse(Character.isDigit('\u829D'))
    assertFalse(Character.isDigit('\u876B'))
    assertFalse(Character.isDigit('\u8913'))
    assertFalse(Character.isDigit('\u8D2A'))
    assertFalse(Character.isDigit('\u8D82'))
    assertFalse(Character.isDigit('\u8E71'))
    assertFalse(Character.isDigit('\u8F63'))
    assertFalse(Character.isDigit('\u94D9'))
    assertFalse(Character.isDigit('\u9859'))
    assertFalse(Character.isDigit('\u99F9'))
    assertFalse(Character.isDigit('\u9A1A'))
    assertFalse(Character.isDigit('\u9B5C'))
    assertFalse(Character.isDigit('\u9E00'))
    assertFalse(Character.isDigit('\u9E3B'))
    assertFalse(Character.isDigit('\u9E5A'))
    assertFalse(Character.isDigit('\u9F49'))
    assertFalse(Character.isDigit('\u9FED'))
    assertFalse(Character.isDigit('\uA28A'))
    assertFalse(Character.isDigit('\uA6A5'))
    assertFalse(Character.isDigit('\uA84F'))
    assertFalse(Character.isDigit('\uA8A1'))
    assertFalse(Character.isDigit('\uA94C'))
    assertFalse(Character.isDigit('\uA964'))
    assertFalse(Character.isDigit('\uABC1'))
    assertFalse(Character.isDigit('\uAC5A'))
    assertFalse(Character.isDigit('\uAE22'))
    assertFalse(Character.isDigit('\uAE82'))
    assertFalse(Character.isDigit('\uAF4E'))
    assertFalse(Character.isDigit('\uB680'))
    assertFalse(Character.isDigit('\uBC1B'))
    assertFalse(Character.isDigit('\uBDF1'))
    assertFalse(Character.isDigit('\uC1F3'))
    assertFalse(Character.isDigit('\uC71B'))
    assertFalse(Character.isDigit('\uC91F'))
    assertFalse(Character.isDigit('\uC97F'))
    assertFalse(Character.isDigit('\uCA08'))
    assertFalse(Character.isDigit('\uCA4C'))
    assertFalse(Character.isDigit('\uCCEC'))
    assertFalse(Character.isDigit('\uCEA2'))
    assertFalse(Character.isDigit('\uD084'))
    assertFalse(Character.isDigit('\uD24E'))
    assertFalse(Character.isDigit('\uD8F5'))
    assertFalse(Character.isDigit('\uE20A'))
    assertFalse(Character.isDigit('\uED86'))
    assertFalse(Character.isDigit('\uEDF0'))
    assertFalse(Character.isDigit('\uF067'))
    assertFalse(Character.isDigit('\uF121'))
    assertFalse(Character.isDigit('\uF448'))
    assertFalse(Character.isDigit('\uF560'))
    assertFalse(Character.isDigit('\uF79C'))
    assertFalse(Character.isDigit('\uF82D'))
    assertFalse(Character.isDigit('\uFBFC'))
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

  @Test def isIdentifierIgnorable(): Unit = {
    for (c <- '\u0000' to '\u0008')
      assertTrue(Character.isIdentifierIgnorable(c))

    for (c <- '\u000E' to '\u001B')
      assertTrue(Character.isIdentifierIgnorable(c))

    for (c <- '\u007F' to '\u009F')
      assertTrue(Character.isIdentifierIgnorable(c))

    // Exhaustive list of Cf category. Unicode 7.0.0
    assertTrue(Character.isIdentifierIgnorable('\u00AD'))
    assertTrue(Character.isIdentifierIgnorable('\u0600'))
    assertTrue(Character.isIdentifierIgnorable('\u0601'))
    assertTrue(Character.isIdentifierIgnorable('\u0602'))
    assertTrue(Character.isIdentifierIgnorable('\u0603'))
    assertTrue(Character.isIdentifierIgnorable('\u0604'))
    // assertTrue(Character.isIdentifierIgnorable('\u0605')) returns false on JVM
    // assertTrue(Character.isIdentifierIgnorable('\u061C')) returns false on JVM
    assertTrue(Character.isIdentifierIgnorable('\u06DD'))
    assertTrue(Character.isIdentifierIgnorable('\u070F'))
    // assertTrue(Character.isIdentifierIgnorable('\u180E'))
    assertTrue(Character.isIdentifierIgnorable('\u200B'))
    assertTrue(Character.isIdentifierIgnorable('\u200C'))
    assertTrue(Character.isIdentifierIgnorable('\u200D'))
    assertTrue(Character.isIdentifierIgnorable('\u200E'))
    assertTrue(Character.isIdentifierIgnorable('\u200F'))
    assertTrue(Character.isIdentifierIgnorable('\u202A'))
    assertTrue(Character.isIdentifierIgnorable('\u202B'))
    assertTrue(Character.isIdentifierIgnorable('\u202C'))
    assertTrue(Character.isIdentifierIgnorable('\u202D'))
    assertTrue(Character.isIdentifierIgnorable('\u202E'))
    assertTrue(Character.isIdentifierIgnorable('\u2060'))
    assertTrue(Character.isIdentifierIgnorable('\u2061'))
    assertTrue(Character.isIdentifierIgnorable('\u2062'))
    assertTrue(Character.isIdentifierIgnorable('\u2063'))
    assertTrue(Character.isIdentifierIgnorable('\u2064'))
    // assertTrue(Character.isIdentifierIgnorable('\u2066')) returns false on JVM
    // assertTrue(Character.isIdentifierIgnorable('\u2067')) returns false on JVM
    // assertTrue(Character.isIdentifierIgnorable('\u2068')) returns false on JVM
    // assertTrue(Character.isIdentifierIgnorable('\u2069')) returns false on JVM
    assertTrue(Character.isIdentifierIgnorable('\u206A'))
    assertTrue(Character.isIdentifierIgnorable('\u206B'))
    assertTrue(Character.isIdentifierIgnorable('\u206C'))
    assertTrue(Character.isIdentifierIgnorable('\u206D'))
    assertTrue(Character.isIdentifierIgnorable('\u206E'))
    assertTrue(Character.isIdentifierIgnorable('\u206F'))
    assertTrue(Character.isIdentifierIgnorable('\uFEFF'))
    assertTrue(Character.isIdentifierIgnorable('\uFFF9'))
    assertTrue(Character.isIdentifierIgnorable('\uFFFA'))
    assertTrue(Character.isIdentifierIgnorable('\uFFFB'))

    // BUG in JDK? 17B4 should be "Mn", Java says "Cf"
    // assertTrue(Character.isIdentifierIgnorable('\u17b4'))

    // 100 randomly generated negatives
    assertFalse(Character.isIdentifierIgnorable('\u745a'))
    assertFalse(Character.isIdentifierIgnorable('\ub445'))
    assertFalse(Character.isIdentifierIgnorable('\ub23a'))
    assertFalse(Character.isIdentifierIgnorable('\ub029'))
    assertFalse(Character.isIdentifierIgnorable('\ufb5c'))
    assertFalse(Character.isIdentifierIgnorable('\u1b67'))
    assertFalse(Character.isIdentifierIgnorable('\u943b'))
    assertFalse(Character.isIdentifierIgnorable('\ue766'))
    assertFalse(Character.isIdentifierIgnorable('\uad12'))
    assertFalse(Character.isIdentifierIgnorable('\ub80b'))
    assertFalse(Character.isIdentifierIgnorable('\u7341'))
    assertFalse(Character.isIdentifierIgnorable('\ubc73'))
    assertFalse(Character.isIdentifierIgnorable('\uabb9'))
    assertFalse(Character.isIdentifierIgnorable('\ub34b'))
    assertFalse(Character.isIdentifierIgnorable('\u1063'))
    assertFalse(Character.isIdentifierIgnorable('\u272f'))
    assertFalse(Character.isIdentifierIgnorable('\u3801'))
    assertFalse(Character.isIdentifierIgnorable('\u53a6'))
    assertFalse(Character.isIdentifierIgnorable('\u2ec2'))
    assertFalse(Character.isIdentifierIgnorable('\u540c'))
    assertFalse(Character.isIdentifierIgnorable('\uc85f'))
    assertFalse(Character.isIdentifierIgnorable('\ud2c8'))
    assertFalse(Character.isIdentifierIgnorable('\u551b'))
    assertFalse(Character.isIdentifierIgnorable('\uc0a1'))
    assertFalse(Character.isIdentifierIgnorable('\ud25a'))
    assertFalse(Character.isIdentifierIgnorable('\u2b98'))
    assertFalse(Character.isIdentifierIgnorable('\u398b'))
    assertFalse(Character.isIdentifierIgnorable('\ubc77'))
    assertFalse(Character.isIdentifierIgnorable('\u54cc'))
    assertFalse(Character.isIdentifierIgnorable('\uc9a0'))
    assertFalse(Character.isIdentifierIgnorable('\ud10f'))
    assertFalse(Character.isIdentifierIgnorable('\uf7e1'))
    assertFalse(Character.isIdentifierIgnorable('\u0f29'))
    assertFalse(Character.isIdentifierIgnorable('\uafcd'))
    assertFalse(Character.isIdentifierIgnorable('\uf187'))
    assertFalse(Character.isIdentifierIgnorable('\u6287'))
    assertFalse(Character.isIdentifierIgnorable('\uacb6'))
    assertFalse(Character.isIdentifierIgnorable('\uff99'))
    assertFalse(Character.isIdentifierIgnorable('\ub59e'))
    assertFalse(Character.isIdentifierIgnorable('\uf630'))
    assertFalse(Character.isIdentifierIgnorable('\ufaec'))
    assertFalse(Character.isIdentifierIgnorable('\ua7d7'))
    assertFalse(Character.isIdentifierIgnorable('\u3eab'))
    assertFalse(Character.isIdentifierIgnorable('\u54a5'))
    assertFalse(Character.isIdentifierIgnorable('\u393a'))
    assertFalse(Character.isIdentifierIgnorable('\uc621'))
    assertFalse(Character.isIdentifierIgnorable('\u766c'))
    assertFalse(Character.isIdentifierIgnorable('\ud64c'))
    assertFalse(Character.isIdentifierIgnorable('\u8beb'))
    assertFalse(Character.isIdentifierIgnorable('\u44e2'))
    assertFalse(Character.isIdentifierIgnorable('\ub6f6'))
    assertFalse(Character.isIdentifierIgnorable('\u58b6'))
    assertFalse(Character.isIdentifierIgnorable('\u3bad'))
    assertFalse(Character.isIdentifierIgnorable('\u3c28'))
    assertFalse(Character.isIdentifierIgnorable('\ufbfd'))
    assertFalse(Character.isIdentifierIgnorable('\u585f'))
    assertFalse(Character.isIdentifierIgnorable('\u7227'))
    assertFalse(Character.isIdentifierIgnorable('\ucea7'))
    assertFalse(Character.isIdentifierIgnorable('\u2c82'))
    assertFalse(Character.isIdentifierIgnorable('\u686d'))
    assertFalse(Character.isIdentifierIgnorable('\u120d'))
    assertFalse(Character.isIdentifierIgnorable('\uf3db'))
    assertFalse(Character.isIdentifierIgnorable('\u320a'))
    assertFalse(Character.isIdentifierIgnorable('\ud96e'))
    assertFalse(Character.isIdentifierIgnorable('\u85eb'))
    assertFalse(Character.isIdentifierIgnorable('\u9648'))
    assertFalse(Character.isIdentifierIgnorable('\u08a4'))
    assertFalse(Character.isIdentifierIgnorable('\u9db7'))
    assertFalse(Character.isIdentifierIgnorable('\u82c7'))
    assertFalse(Character.isIdentifierIgnorable('\ufe12'))
    assertFalse(Character.isIdentifierIgnorable('\u0eaf'))
    assertFalse(Character.isIdentifierIgnorable('\u96dc'))
    assertFalse(Character.isIdentifierIgnorable('\u3a2a'))
    assertFalse(Character.isIdentifierIgnorable('\uc72e'))
    assertFalse(Character.isIdentifierIgnorable('\u3745'))
    assertFalse(Character.isIdentifierIgnorable('\ubcf9'))
    assertFalse(Character.isIdentifierIgnorable('\u5f66'))
    assertFalse(Character.isIdentifierIgnorable('\u9be1'))
    assertFalse(Character.isIdentifierIgnorable('\ud81d'))
    assertFalse(Character.isIdentifierIgnorable('\u3ca3'))
    assertFalse(Character.isIdentifierIgnorable('\u3e82'))
    assertFalse(Character.isIdentifierIgnorable('\u7ce4'))
    assertFalse(Character.isIdentifierIgnorable('\u33ca'))
    assertFalse(Character.isIdentifierIgnorable('\ue725'))
    assertFalse(Character.isIdentifierIgnorable('\uef49'))
    assertFalse(Character.isIdentifierIgnorable('\ue2cf'))
    assertFalse(Character.isIdentifierIgnorable('\udcf0'))
    assertFalse(Character.isIdentifierIgnorable('\u5f2e'))
    assertFalse(Character.isIdentifierIgnorable('\u2a63'))
    assertFalse(Character.isIdentifierIgnorable('\ud2d2'))
    assertFalse(Character.isIdentifierIgnorable('\u8023'))
    assertFalse(Character.isIdentifierIgnorable('\ua957'))
    assertFalse(Character.isIdentifierIgnorable('\u10ba'))
    assertFalse(Character.isIdentifierIgnorable('\uf85f'))
    assertFalse(Character.isIdentifierIgnorable('\uc40d'))
    assertFalse(Character.isIdentifierIgnorable('\u2509'))
    assertFalse(Character.isIdentifierIgnorable('\u0d8e'))
    assertFalse(Character.isIdentifierIgnorable('\u9db8'))
    assertFalse(Character.isIdentifierIgnorable('\u824d'))
    assertFalse(Character.isIdentifierIgnorable('\u5670'))
    assertFalse(Character.isIdentifierIgnorable('\u6005'))
    assertFalse(Character.isIdentifierIgnorable('\ub8de'))
    assertFalse(Character.isIdentifierIgnorable('\uff5c'))
    assertFalse(Character.isIdentifierIgnorable('\ub36d'))
    assertFalse(Character.isIdentifierIgnorable('\u0cf2'))
    assertFalse(Character.isIdentifierIgnorable('\u82f6'))
    assertFalse(Character.isIdentifierIgnorable('\u9206'))
    assertFalse(Character.isIdentifierIgnorable('\u95e1'))
    assertFalse(Character.isIdentifierIgnorable('\u990f'))
    assertFalse(Character.isIdentifierIgnorable('\u9fc7'))
    assertFalse(Character.isIdentifierIgnorable('\udffb'))
    assertFalse(Character.isIdentifierIgnorable('\u0ecb'))
    assertFalse(Character.isIdentifierIgnorable('\u7563'))
    assertFalse(Character.isIdentifierIgnorable('\uf0ff'))
    assertFalse(Character.isIdentifierIgnorable('\u6b2e'))
    assertFalse(Character.isIdentifierIgnorable('\u894c'))
    assertFalse(Character.isIdentifierIgnorable('\u8f06'))
    assertFalse(Character.isIdentifierIgnorable('\uffa9'))
    assertFalse(Character.isIdentifierIgnorable('\u37b0'))
    assertFalse(Character.isIdentifierIgnorable('\u3e04'))

  }

  @Test def isUnicodeIdentifierStart(): Unit = {
    /* 100 randomly generated positives and 100 randomly generated negatives,
     * minus negative ones that became positive later.
     */

    assertTrue(Character.isUnicodeIdentifierStart('\ud6d5'))
    assertTrue(Character.isUnicodeIdentifierStart('\u3f9c'))
    assertTrue(Character.isUnicodeIdentifierStart('\u3a40'))
    assertTrue(Character.isUnicodeIdentifierStart('\u53af'))
    assertTrue(Character.isUnicodeIdentifierStart('\u1636'))
    assertTrue(Character.isUnicodeIdentifierStart('\u4884'))
    assertTrue(Character.isUnicodeIdentifierStart('\ucba4'))
    assertTrue(Character.isUnicodeIdentifierStart('\u1ee4'))
    assertTrue(Character.isUnicodeIdentifierStart('\u6dec'))
    assertTrue(Character.isUnicodeIdentifierStart('\u10d4'))
    assertTrue(Character.isUnicodeIdentifierStart('\u631f'))
    assertTrue(Character.isUnicodeIdentifierStart('\u3661'))
    assertTrue(Character.isUnicodeIdentifierStart('\u55f8'))
    assertTrue(Character.isUnicodeIdentifierStart('\ub4ef'))
    assertTrue(Character.isUnicodeIdentifierStart('\ud509'))
    assertTrue(Character.isUnicodeIdentifierStart('\u65b5'))
    assertTrue(Character.isUnicodeIdentifierStart('\u316b'))
    assertTrue(Character.isUnicodeIdentifierStart('\ub270'))
    assertTrue(Character.isUnicodeIdentifierStart('\u7f0f'))
    assertTrue(Character.isUnicodeIdentifierStart('\uff84'))
    assertTrue(Character.isUnicodeIdentifierStart('\u11cc'))
    assertTrue(Character.isUnicodeIdentifierStart('\u0294'))
    assertTrue(Character.isUnicodeIdentifierStart('\u51b1'))
    assertTrue(Character.isUnicodeIdentifierStart('\u9ae2'))
    assertTrue(Character.isUnicodeIdentifierStart('\u304a'))
    assertTrue(Character.isUnicodeIdentifierStart('\ud5c7'))
    assertTrue(Character.isUnicodeIdentifierStart('\u3b4b'))
    assertTrue(Character.isUnicodeIdentifierStart('\u5e42'))
    assertTrue(Character.isUnicodeIdentifierStart('\u51fc'))
    assertTrue(Character.isUnicodeIdentifierStart('\uc148'))
    assertTrue(Character.isUnicodeIdentifierStart('\uc1ae'))
    assertTrue(Character.isUnicodeIdentifierStart('\u7372'))
    assertTrue(Character.isUnicodeIdentifierStart('\uc116'))
    assertTrue(Character.isUnicodeIdentifierStart('\u5d29'))
    assertTrue(Character.isUnicodeIdentifierStart('\u8753'))
    assertTrue(Character.isUnicodeIdentifierStart('\u50f8'))
    assertTrue(Character.isUnicodeIdentifierStart('\u3f9d'))
    assertTrue(Character.isUnicodeIdentifierStart('\u1f44'))
    assertTrue(Character.isUnicodeIdentifierStart('\ucd43'))
    assertTrue(Character.isUnicodeIdentifierStart('\u9126'))
    assertTrue(Character.isUnicodeIdentifierStart('\u8d2e'))
    assertTrue(Character.isUnicodeIdentifierStart('\u4f5c'))
    assertTrue(Character.isUnicodeIdentifierStart('\u66d7'))
    assertTrue(Character.isUnicodeIdentifierStart('\ua30b'))
    assertTrue(Character.isUnicodeIdentifierStart('\u140b'))
    assertTrue(Character.isUnicodeIdentifierStart('\ub264'))
    assertTrue(Character.isUnicodeIdentifierStart('\u7b35'))
    assertTrue(Character.isUnicodeIdentifierStart('\u15e4'))
    assertTrue(Character.isUnicodeIdentifierStart('\ubb37'))
    assertTrue(Character.isUnicodeIdentifierStart('\u34e3'))
    assertTrue(Character.isUnicodeIdentifierStart('\uac3e'))
    assertTrue(Character.isUnicodeIdentifierStart('\ubd0e'))
    assertTrue(Character.isUnicodeIdentifierStart('\ub641'))
    assertTrue(Character.isUnicodeIdentifierStart('\u1580'))
    assertTrue(Character.isUnicodeIdentifierStart('\u30c1'))
    assertTrue(Character.isUnicodeIdentifierStart('\ub0c8'))
    assertTrue(Character.isUnicodeIdentifierStart('\u8681'))
    assertTrue(Character.isUnicodeIdentifierStart('\u7f14'))
    assertTrue(Character.isUnicodeIdentifierStart('\u4142'))
    assertTrue(Character.isUnicodeIdentifierStart('\u56c1'))
    assertTrue(Character.isUnicodeIdentifierStart('\u0444'))
    assertTrue(Character.isUnicodeIdentifierStart('\u9964'))
    assertTrue(Character.isUnicodeIdentifierStart('\ub5c0'))
    assertTrue(Character.isUnicodeIdentifierStart('\u43d8'))
    assertTrue(Character.isUnicodeIdentifierStart('\u479e'))
    assertTrue(Character.isUnicodeIdentifierStart('\u0853'))
    assertTrue(Character.isUnicodeIdentifierStart('\ube08'))
    assertTrue(Character.isUnicodeIdentifierStart('\u9346'))
    assertTrue(Character.isUnicodeIdentifierStart('\uf9c1'))
    assertTrue(Character.isUnicodeIdentifierStart('\u0e8a'))
    assertTrue(Character.isUnicodeIdentifierStart('\u212c'))
    assertTrue(Character.isUnicodeIdentifierStart('\u810c'))
    assertTrue(Character.isUnicodeIdentifierStart('\u8089'))
    assertTrue(Character.isUnicodeIdentifierStart('\u1331'))
    assertTrue(Character.isUnicodeIdentifierStart('\ua5f7'))
    assertTrue(Character.isUnicodeIdentifierStart('\u5e5e'))
    assertTrue(Character.isUnicodeIdentifierStart('\u613b'))
    assertTrue(Character.isUnicodeIdentifierStart('\u34a7'))
    assertTrue(Character.isUnicodeIdentifierStart('\ud15b'))
    assertTrue(Character.isUnicodeIdentifierStart('\uc1fc'))
    assertTrue(Character.isUnicodeIdentifierStart('\u92f1'))
    assertTrue(Character.isUnicodeIdentifierStart('\u3ae6'))
    assertTrue(Character.isUnicodeIdentifierStart('\ufceb'))
    assertTrue(Character.isUnicodeIdentifierStart('\u7584'))
    assertTrue(Character.isUnicodeIdentifierStart('\ufe98'))
    assertTrue(Character.isUnicodeIdentifierStart('\ubb23'))
    assertTrue(Character.isUnicodeIdentifierStart('\u7961'))
    assertTrue(Character.isUnicodeIdentifierStart('\u4445'))
    assertTrue(Character.isUnicodeIdentifierStart('\u4d5f'))
    assertTrue(Character.isUnicodeIdentifierStart('\u61cb'))
    assertTrue(Character.isUnicodeIdentifierStart('\u5176'))
    assertTrue(Character.isUnicodeIdentifierStart('\ub987'))
    assertTrue(Character.isUnicodeIdentifierStart('\u906a'))
    assertTrue(Character.isUnicodeIdentifierStart('\u4317'))
    assertTrue(Character.isUnicodeIdentifierStart('\u93ad'))
    assertTrue(Character.isUnicodeIdentifierStart('\u825a'))
    assertTrue(Character.isUnicodeIdentifierStart('\u7ff8'))
    assertTrue(Character.isUnicodeIdentifierStart('\u533a'))
    assertTrue(Character.isUnicodeIdentifierStart('\u5617'))
    assertTrue(Character.isUnicodeIdentifierStart('\ufcc6'))
    assertFalse(Character.isUnicodeIdentifierStart('\ue398'))
    assertFalse(Character.isUnicodeIdentifierStart('\ueab6'))
    assertFalse(Character.isUnicodeIdentifierStart('\ue7bc'))
    assertFalse(Character.isUnicodeIdentifierStart('\uf8ab'))
    assertFalse(Character.isUnicodeIdentifierStart('\ue27f'))
    assertFalse(Character.isUnicodeIdentifierStart('\uebea'))
    assertFalse(Character.isUnicodeIdentifierStart('\ueedc'))
    assertFalse(Character.isUnicodeIdentifierStart('\uf091'))
    assertFalse(Character.isUnicodeIdentifierStart('\u2785'))
    assertFalse(Character.isUnicodeIdentifierStart('\u287b'))
    assertFalse(Character.isUnicodeIdentifierStart('\uf042'))
    assertFalse(Character.isUnicodeIdentifierStart('\u20f9'))
    assertFalse(Character.isUnicodeIdentifierStart('\u23d6'))
    assertFalse(Character.isUnicodeIdentifierStart('\udc5b'))
    assertFalse(Character.isUnicodeIdentifierStart('\ued16'))
    assertFalse(Character.isUnicodeIdentifierStart('\u1b6b'))
    assertFalse(Character.isUnicodeIdentifierStart('\ue7ba'))
    assertFalse(Character.isUnicodeIdentifierStart('\uf7fa'))
    assertFalse(Character.isUnicodeIdentifierStart('\u2125'))
    assertFalse(Character.isUnicodeIdentifierStart('\uea97'))
    assertFalse(Character.isUnicodeIdentifierStart('\ue624'))
    assertFalse(Character.isUnicodeIdentifierStart('\ufbb8'))
    assertFalse(Character.isUnicodeIdentifierStart('\u2730'))
    assertFalse(Character.isUnicodeIdentifierStart('\udb89'))
    assertFalse(Character.isUnicodeIdentifierStart('\ue30d'))
    assertFalse(Character.isUnicodeIdentifierStart('\u2e24'))
    assertFalse(Character.isUnicodeIdentifierStart('\uf03e'))
    assertFalse(Character.isUnicodeIdentifierStart('\uda27'))
    assertFalse(Character.isUnicodeIdentifierStart('\u28fc'))
    assertFalse(Character.isUnicodeIdentifierStart('\ude19'))
    assertFalse(Character.isUnicodeIdentifierStart('\u0b70'))
    assertFalse(Character.isUnicodeIdentifierStart('\uddfc'))
    assertFalse(Character.isUnicodeIdentifierStart('\ued53'))
    assertFalse(Character.isUnicodeIdentifierStart('\ue8cb'))
    assertFalse(Character.isUnicodeIdentifierStart('\udccc'))
    assertFalse(Character.isUnicodeIdentifierStart('\u00a3'))
    assertFalse(Character.isUnicodeIdentifierStart('\u0bed'))
    assertFalse(Character.isUnicodeIdentifierStart('\u0c68'))
    assertFalse(Character.isUnicodeIdentifierStart('\uf47b'))
    assertFalse(Character.isUnicodeIdentifierStart('\u0f96'))
    assertFalse(Character.isUnicodeIdentifierStart('\ue9c3'))
    assertFalse(Character.isUnicodeIdentifierStart('\uf784'))
    assertFalse(Character.isUnicodeIdentifierStart('\uef4b'))
    assertFalse(Character.isUnicodeIdentifierStart('\udee1'))
    assertFalse(Character.isUnicodeIdentifierStart('\u2f61'))
    assertFalse(Character.isUnicodeIdentifierStart('\uf622'))
    assertFalse(Character.isUnicodeIdentifierStart('\u19f9'))
    assertFalse(Character.isUnicodeIdentifierStart('\ud86a'))
    assertFalse(Character.isUnicodeIdentifierStart('\ued83'))
    assertFalse(Character.isUnicodeIdentifierStart('\uf7e4'))
    assertFalse(Character.isUnicodeIdentifierStart('\uecce'))

    // BUG in JDK? A699 should be "Ll", Java says "Cn"
    // assertFalse(Character.isUnicodeIdentifierStart('\ua699'))

    assertFalse(Character.isUnicodeIdentifierStart('\uaa5f'))
    assertFalse(Character.isUnicodeIdentifierStart('\udf24'))
    assertFalse(Character.isUnicodeIdentifierStart('\u2e0e'))
    assertFalse(Character.isUnicodeIdentifierStart('\uf322'))
    assertFalse(Character.isUnicodeIdentifierStart('\ue137'))
    assertFalse(Character.isUnicodeIdentifierStart('\ued19'))
    assertFalse(Character.isUnicodeIdentifierStart('\u21ab'))
    assertFalse(Character.isUnicodeIdentifierStart('\ue972'))
    assertFalse(Character.isUnicodeIdentifierStart('\udbf2'))
    assertFalse(Character.isUnicodeIdentifierStart('\uf54c'))
    assertFalse(Character.isUnicodeIdentifierStart('\u4dd3'))
    assertFalse(Character.isUnicodeIdentifierStart('\u2769'))
    assertFalse(Character.isUnicodeIdentifierStart('\ue363'))

    // BUG in JDK? 1BBB should be "Lo", Java says "Cn"
    // assertFalse(Character.isUnicodeIdentifierStart('\u1bbb'))

    assertFalse(Character.isUnicodeIdentifierStart('\ueae7'))
    assertFalse(Character.isUnicodeIdentifierStart('\u2bf3'))
    assertFalse(Character.isUnicodeIdentifierStart('\ue704'))
    assertFalse(Character.isUnicodeIdentifierStart('\u1c7f'))
    assertFalse(Character.isUnicodeIdentifierStart('\uf52b'))
    assertFalse(Character.isUnicodeIdentifierStart('\ue9e3'))
    assertFalse(Character.isUnicodeIdentifierStart('\u259b'))
    assertFalse(Character.isUnicodeIdentifierStart('\uf250'))
    assertFalse(Character.isUnicodeIdentifierStart('\uf42f'))
    assertFalse(Character.isUnicodeIdentifierStart('\ue244'))
    assertFalse(Character.isUnicodeIdentifierStart('\u20d9'))
    assertFalse(Character.isUnicodeIdentifierStart('\ua881'))
    assertFalse(Character.isUnicodeIdentifierStart('\u0ee6'))
    assertFalse(Character.isUnicodeIdentifierStart('\u2203'))
    assertFalse(Character.isUnicodeIdentifierStart('\u0fc7'))
    assertFalse(Character.isUnicodeIdentifierStart('\u07fc'))
    assertFalse(Character.isUnicodeIdentifierStart('\udb86'))
    assertFalse(Character.isUnicodeIdentifierStart('\u2a70'))
    assertFalse(Character.isUnicodeIdentifierStart('\u2bb7'))
    assertFalse(Character.isUnicodeIdentifierStart('\uecf0'))
    assertFalse(Character.isUnicodeIdentifierStart('\ude48'))
    assertFalse(Character.isUnicodeIdentifierStart('\u0a3b'))
    assertFalse(Character.isUnicodeIdentifierStart('\u20b8'))
    assertFalse(Character.isUnicodeIdentifierStart('\uf898'))
    assertFalse(Character.isUnicodeIdentifierStart('\u23e6'))
    assertFalse(Character.isUnicodeIdentifierStart('\ud8ba'))
    assertFalse(Character.isUnicodeIdentifierStart('\uda1e'))
    assertFalse(Character.isUnicodeIdentifierStart('\udc12'))
    assertFalse(Character.isUnicodeIdentifierStart('\u2a06'))
    assertFalse(Character.isUnicodeIdentifierStart('\u0888'))
    assertFalse(Character.isUnicodeIdentifierStart('\ud9ec'))
    assertFalse(Character.isUnicodeIdentifierStart('\uf81f'))
    assertFalse(Character.isUnicodeIdentifierStart('\uf817'))
  }

  @Test def isUnicodeIdentifierPart(): Unit = {
    /* 100 randomly generated positives and 100 randomly generated negatives,
     * minus those that became positive in JDK 11 or later.
     */

    assertTrue(Character.isUnicodeIdentifierPart('\u48d3'))
    assertTrue(Character.isUnicodeIdentifierPart('\u0905'))
    assertTrue(Character.isUnicodeIdentifierPart('\u8f51'))
    assertTrue(Character.isUnicodeIdentifierPart('\u9bcb'))
    assertTrue(Character.isUnicodeIdentifierPart('\ud358'))
    assertTrue(Character.isUnicodeIdentifierPart('\u1538'))
    assertTrue(Character.isUnicodeIdentifierPart('\uffcf'))
    assertTrue(Character.isUnicodeIdentifierPart('\u83ec'))
    assertTrue(Character.isUnicodeIdentifierPart('\u3a89'))
    assertTrue(Character.isUnicodeIdentifierPart('\ub63a'))
    assertTrue(Character.isUnicodeIdentifierPart('\ufe24'))
    assertTrue(Character.isUnicodeIdentifierPart('\u2d62'))
    assertTrue(Character.isUnicodeIdentifierPart('\u15ca'))
    assertTrue(Character.isUnicodeIdentifierPart('\u4fa4'))
    assertTrue(Character.isUnicodeIdentifierPart('\u47d1'))
    assertTrue(Character.isUnicodeIdentifierPart('\u831c'))
    assertTrue(Character.isUnicodeIdentifierPart('\u84e6'))
    assertTrue(Character.isUnicodeIdentifierPart('\u7783'))
    assertTrue(Character.isUnicodeIdentifierPart('\ua03c'))
    assertTrue(Character.isUnicodeIdentifierPart('\u6ecf'))
    assertTrue(Character.isUnicodeIdentifierPart('\u147f'))
    assertTrue(Character.isUnicodeIdentifierPart('\u67a9'))
    assertTrue(Character.isUnicodeIdentifierPart('\u8b6c'))
    assertTrue(Character.isUnicodeIdentifierPart('\u3410'))
    assertTrue(Character.isUnicodeIdentifierPart('\u2cc0'))
    assertTrue(Character.isUnicodeIdentifierPart('\ua332'))
    assertTrue(Character.isUnicodeIdentifierPart('\u9733'))
    assertTrue(Character.isUnicodeIdentifierPart('\u5df3'))
    assertTrue(Character.isUnicodeIdentifierPart('\u3fd7'))
    assertTrue(Character.isUnicodeIdentifierPart('\u6611'))
    assertTrue(Character.isUnicodeIdentifierPart('\u55b4'))
    assertTrue(Character.isUnicodeIdentifierPart('\u8bc8'))
    assertTrue(Character.isUnicodeIdentifierPart('\u6f74'))
    assertTrue(Character.isUnicodeIdentifierPart('\u6c97'))
    assertTrue(Character.isUnicodeIdentifierPart('\u6a86'))
    assertTrue(Character.isUnicodeIdentifierPart('\u6000'))
    assertTrue(Character.isUnicodeIdentifierPart('\u614f'))
    assertTrue(Character.isUnicodeIdentifierPart('\u206e'))
    assertTrue(Character.isUnicodeIdentifierPart('\ua801'))
    assertTrue(Character.isUnicodeIdentifierPart('\u9edf'))
    assertTrue(Character.isUnicodeIdentifierPart('\ub42c'))
    assertTrue(Character.isUnicodeIdentifierPart('\u7fcd'))
    assertTrue(Character.isUnicodeIdentifierPart('\u8a60'))
    assertTrue(Character.isUnicodeIdentifierPart('\u182f'))
    assertTrue(Character.isUnicodeIdentifierPart('\u5d0a'))
    assertTrue(Character.isUnicodeIdentifierPart('\uaf9c'))
    assertTrue(Character.isUnicodeIdentifierPart('\u9d4b'))
    assertTrue(Character.isUnicodeIdentifierPart('\u5088'))
    assertTrue(Character.isUnicodeIdentifierPart('\uc1a6'))
    assertTrue(Character.isUnicodeIdentifierPart('\ubbe4'))
    assertTrue(Character.isUnicodeIdentifierPart('\uad25'))
    assertTrue(Character.isUnicodeIdentifierPart('\u4653'))
    assertTrue(Character.isUnicodeIdentifierPart('\u8add'))
    assertTrue(Character.isUnicodeIdentifierPart('\u3d1c'))
    assertTrue(Character.isUnicodeIdentifierPart('\u80a8'))
    assertTrue(Character.isUnicodeIdentifierPart('\u810e'))
    assertTrue(Character.isUnicodeIdentifierPart('\uc1d2'))
    assertTrue(Character.isUnicodeIdentifierPart('\ub984'))
    assertTrue(Character.isUnicodeIdentifierPart('\u9d13'))
    assertTrue(Character.isUnicodeIdentifierPart('\u37c2'))
    assertTrue(Character.isUnicodeIdentifierPart('\u13cd'))
    assertTrue(Character.isUnicodeIdentifierPart('\u53f9'))
    assertTrue(Character.isUnicodeIdentifierPart('\u98b7'))
    assertTrue(Character.isUnicodeIdentifierPart('\u57f3'))
    assertTrue(Character.isUnicodeIdentifierPart('\ub554'))
    assertTrue(Character.isUnicodeIdentifierPart('\u0176'))
    assertTrue(Character.isUnicodeIdentifierPart('\ua318'))
    assertTrue(Character.isUnicodeIdentifierPart('\u9704'))
    assertTrue(Character.isUnicodeIdentifierPart('\u8d52'))
    assertTrue(Character.isUnicodeIdentifierPart('\u940a'))
    assertTrue(Character.isUnicodeIdentifierPart('\u0fa5'))
    assertTrue(Character.isUnicodeIdentifierPart('\u38d1'))
    assertTrue(Character.isUnicodeIdentifierPart('\u3b33'))
    assertTrue(Character.isUnicodeIdentifierPart('\u93bb'))
    assertTrue(Character.isUnicodeIdentifierPart('\u03bd'))
    assertTrue(Character.isUnicodeIdentifierPart('\u4c88'))
    assertTrue(Character.isUnicodeIdentifierPart('\ud67d'))
    assertTrue(Character.isUnicodeIdentifierPart('\ubcbf'))
    assertTrue(Character.isUnicodeIdentifierPart('\u3867'))
    assertTrue(Character.isUnicodeIdentifierPart('\u4368'))
    assertTrue(Character.isUnicodeIdentifierPart('\u8f2d'))
    assertTrue(Character.isUnicodeIdentifierPart('\u049a'))
    assertTrue(Character.isUnicodeIdentifierPart('\u4c01'))
    assertTrue(Character.isUnicodeIdentifierPart('\u5589'))
    assertTrue(Character.isUnicodeIdentifierPart('\u5e71'))
    assertTrue(Character.isUnicodeIdentifierPart('\ua1fd'))
    assertTrue(Character.isUnicodeIdentifierPart('\u3a4a'))
    assertTrue(Character.isUnicodeIdentifierPart('\uc111'))
    assertTrue(Character.isUnicodeIdentifierPart('\ub465'))
    assertTrue(Character.isUnicodeIdentifierPart('\u95af'))
    assertTrue(Character.isUnicodeIdentifierPart('\ubf2c'))
    assertTrue(Character.isUnicodeIdentifierPart('\u8488'))
    assertTrue(Character.isUnicodeIdentifierPart('\u4317'))
    assertTrue(Character.isUnicodeIdentifierPart('\u6b77'))
    assertTrue(Character.isUnicodeIdentifierPart('\u8995'))
    assertTrue(Character.isUnicodeIdentifierPart('\u7467'))
    assertTrue(Character.isUnicodeIdentifierPart('\u16b7'))
    assertTrue(Character.isUnicodeIdentifierPart('\u3ca0'))
    assertTrue(Character.isUnicodeIdentifierPart('\u5332'))
    assertTrue(Character.isUnicodeIdentifierPart('\u8654'))
    assertFalse(Character.isUnicodeIdentifierPart('\ua8c8'))
    assertFalse(Character.isUnicodeIdentifierPart('\ue3ca'))
    assertFalse(Character.isUnicodeIdentifierPart('\uebee'))
    assertFalse(Character.isUnicodeIdentifierPart('\u270e'))
    assertFalse(Character.isUnicodeIdentifierPart('\uf0ac'))
    assertFalse(Character.isUnicodeIdentifierPart('\ue9ec'))
    assertFalse(Character.isUnicodeIdentifierPart('\u296a'))
    assertFalse(Character.isUnicodeIdentifierPart('\u33fd'))
    assertFalse(Character.isUnicodeIdentifierPart('\ue5f4'))
    assertFalse(Character.isUnicodeIdentifierPart('\ueb01'))
    assertFalse(Character.isUnicodeIdentifierPart('\uf38b'))
    assertFalse(Character.isUnicodeIdentifierPart('\u2e6f'))
    assertFalse(Character.isUnicodeIdentifierPart('\uea69'))
    assertFalse(Character.isUnicodeIdentifierPart('\uf155'))
    assertFalse(Character.isUnicodeIdentifierPart('\u0f0e'))
    assertFalse(Character.isUnicodeIdentifierPart('\ueb80'))
    assertFalse(Character.isUnicodeIdentifierPart('\ud959'))
    assertFalse(Character.isUnicodeIdentifierPart('\ue25e'))
    assertFalse(Character.isUnicodeIdentifierPart('\uf566'))
    assertFalse(Character.isUnicodeIdentifierPart('\ue4a3'))
    assertFalse(Character.isUnicodeIdentifierPart('\uec44'))
    assertFalse(Character.isUnicodeIdentifierPart('\u3297'))
    assertFalse(Character.isUnicodeIdentifierPart('\u3214'))
    assertFalse(Character.isUnicodeIdentifierPart('\u1bfd'))
    assertFalse(Character.isUnicodeIdentifierPart('\u4dd0'))
    assertFalse(Character.isUnicodeIdentifierPart('\uea99'))
    assertFalse(Character.isUnicodeIdentifierPart('\uf592'))
    assertFalse(Character.isUnicodeIdentifierPart('\uf4dd'))
    assertFalse(Character.isUnicodeIdentifierPart('\udfaf'))
    assertFalse(Character.isUnicodeIdentifierPart('\udd38'))
    assertFalse(Character.isUnicodeIdentifierPart('\uf820'))
    assertFalse(Character.isUnicodeIdentifierPart('\uaacd'))
    assertFalse(Character.isUnicodeIdentifierPart('\uff5b'))
    assertFalse(Character.isUnicodeIdentifierPart('\ude36'))
    assertFalse(Character.isUnicodeIdentifierPart('\ue33b'))
    assertFalse(Character.isUnicodeIdentifierPart('\udbce'))
    assertFalse(Character.isUnicodeIdentifierPart('\ue1f6'))
    assertFalse(Character.isUnicodeIdentifierPart('\uf78a'))
    assertFalse(Character.isUnicodeIdentifierPart('\ueb44'))
    assertFalse(Character.isUnicodeIdentifierPart('\uebd4'))
    assertFalse(Character.isUnicodeIdentifierPart('\u2f10'))
    assertFalse(Character.isUnicodeIdentifierPart('\u2362'))
    assertFalse(Character.isUnicodeIdentifierPart('\uebeb'))
    assertFalse(Character.isUnicodeIdentifierPart('\u2ede'))
    assertFalse(Character.isUnicodeIdentifierPart('\u221d'))
    assertFalse(Character.isUnicodeIdentifierPart('\u2021'))
    assertFalse(Character.isUnicodeIdentifierPart('\udf41'))
    assertFalse(Character.isUnicodeIdentifierPart('\u05f5'))
    assertFalse(Character.isUnicodeIdentifierPart('\u24ab'))
    assertFalse(Character.isUnicodeIdentifierPart('\uee15'))
    assertFalse(Character.isUnicodeIdentifierPart('\uf175'))
    assertFalse(Character.isUnicodeIdentifierPart('\uf35c'))
    assertFalse(Character.isUnicodeIdentifierPart('\udc7b'))
    assertFalse(Character.isUnicodeIdentifierPart('\ud883'))
    assertFalse(Character.isUnicodeIdentifierPart('\uf341'))
    assertFalse(Character.isUnicodeIdentifierPart('\ueec6'))
    assertFalse(Character.isUnicodeIdentifierPart('\u2f57'))
    assertFalse(Character.isUnicodeIdentifierPart('\uff64'))
    assertFalse(Character.isUnicodeIdentifierPart('\ue6a4'))
    assertFalse(Character.isUnicodeIdentifierPart('\uec34'))
    assertFalse(Character.isUnicodeIdentifierPart('\u22a5'))
    assertFalse(Character.isUnicodeIdentifierPart('\uf5ac'))
    assertFalse(Character.isUnicodeIdentifierPart('\u3360'))
    assertFalse(Character.isUnicodeIdentifierPart('\u28b0'))
    assertFalse(Character.isUnicodeIdentifierPart('\uf678'))
    assertFalse(Character.isUnicodeIdentifierPart('\ue0e4'))
    assertFalse(Character.isUnicodeIdentifierPart('\u233f'))
    assertFalse(Character.isUnicodeIdentifierPart('\u2013'))
    assertFalse(Character.isUnicodeIdentifierPart('\ud7af'))
    assertFalse(Character.isUnicodeIdentifierPart('\ud98e'))
    assertFalse(Character.isUnicodeIdentifierPart('\ud8a5'))

    // BUG in JDK? A79E should be "Lu", Java says "Cn"
    // assertFalse(Character.isUnicodeIdentifierPart('\ua79e'))

    assertFalse(Character.isUnicodeIdentifierPart('\u1806'))
    assertFalse(Character.isUnicodeIdentifierPart('\ue07a'))
    assertFalse(Character.isUnicodeIdentifierPart('\u2748'))
    assertFalse(Character.isUnicodeIdentifierPart('\uec5c'))
    assertFalse(Character.isUnicodeIdentifierPart('\ue832'))

    // BUG in JDK? 08A9 should be "Lo", Java says "Cn"
    // assertFalse(Character.isUnicodeIdentifierPart('\u08a9'))

    assertFalse(Character.isUnicodeIdentifierPart('\ue4bd'))
    assertFalse(Character.isUnicodeIdentifierPart('\u208a'))
    assertFalse(Character.isUnicodeIdentifierPart('\uf840'))
    assertFalse(Character.isUnicodeIdentifierPart('\uf570'))
    assertFalse(Character.isUnicodeIdentifierPart('\uef1e'))
    assertFalse(Character.isUnicodeIdentifierPart('\u2bd4'))
    assertFalse(Character.isUnicodeIdentifierPart('\ue385'))
    assertFalse(Character.isUnicodeIdentifierPart('\udc18'))
    assertFalse(Character.isUnicodeIdentifierPart('\u0af0'))
    assertFalse(Character.isUnicodeIdentifierPart('\u244a'))
    assertFalse(Character.isUnicodeIdentifierPart('\uf01e'))
    assertFalse(Character.isUnicodeIdentifierPart('\uf114'))
    assertFalse(Character.isUnicodeIdentifierPart('\ue9c4'))

    // BUG in JDK? AAF4 should be "Lm", Java says "Cn"
    // assertFalse(Character.isUnicodeIdentifierPart('\uaaf4'))

    assertFalse(Character.isUnicodeIdentifierPart('\uf7b9'))
    assertFalse(Character.isUnicodeIdentifierPart('\udd2f'))
    assertFalse(Character.isUnicodeIdentifierPart('\u2d2c'))
  }

  @Test def isUpperCase(): Unit = {
    for (c <- 'a' to 'z')
      assertFalse(Character.isUpperCase(c))
    for (c <- 'A' to 'Z')
      assertTrue(Character.isUpperCase(c))
    for (c <- '0' to '9')
      assertFalse(Character.isUpperCase(c))

    // example form issue #1646
    assertFalse(Character.isUpperCase('('))
    assertFalse(Character.isUpperCase(')'))

    // 100 randomly chosen uppeer cases
    assertTrue(Character.isUpperCase('\uA78B'))
    assertTrue(Character.isUpperCase('\u01D1'))
    assertTrue(Character.isUpperCase('\uA680'))
    assertTrue(Character.isUpperCase('\u01E2'))
    assertTrue(Character.isUpperCase('\u00C1'))
    assertTrue(Character.isUpperCase('\u038F'))
    assertTrue(Character.isUpperCase('\u04FC'))
    assertTrue(Character.isUpperCase('\u0232'))
    assertTrue(Character.isUpperCase('\u1E86'))
    assertTrue(Character.isUpperCase('\u1E66'))
    assertTrue(Character.isUpperCase('\u1E12'))
    assertTrue(Character.isUpperCase('\u1F09'))
    assertTrue(Character.isUpperCase('\uA758'))
    assertTrue(Character.isUpperCase('\u01B1'))
    assertTrue(Character.isUpperCase('\u0230'))
    assertTrue(Character.isUpperCase('\u2CA2'))
    assertTrue(Character.isUpperCase('\u0154'))
    assertTrue(Character.isUpperCase('\uA77E'))
    assertTrue(Character.isUpperCase('\u2C69'))
    assertTrue(Character.isUpperCase('\u1EDE'))
    assertTrue(Character.isUpperCase('\u2CBC'))
    assertTrue(Character.isUpperCase('\u2CED'))
    assertTrue(Character.isUpperCase('\u2CAC'))
    assertTrue(Character.isUpperCase('\u0549'))
    assertTrue(Character.isUpperCase('\u00D5'))
    assertTrue(Character.isUpperCase('\u0494'))
    assertTrue(Character.isUpperCase('\u2CCC'))
    assertTrue(Character.isUpperCase('\u0162'))
    assertTrue(Character.isUpperCase('\uFF22'))
    assertTrue(Character.isUpperCase('\u24C4'))
    assertTrue(Character.isUpperCase('\u041A'))
    assertTrue(Character.isUpperCase('\u2124'))
    assertTrue(Character.isUpperCase('\u1FBB'))
    assertTrue(Character.isUpperCase('\u1FD9'))
    assertTrue(Character.isUpperCase('\uA662'))
    assertTrue(Character.isUpperCase('\u0504'))
    assertTrue(Character.isUpperCase('\u2C14'))
    assertTrue(Character.isUpperCase('\uA760'))
    assertTrue(Character.isUpperCase('\u1E44'))
    assertTrue(Character.isUpperCase('\u10A2'))
    assertTrue(Character.isUpperCase('\u017B'))
    assertTrue(Character.isUpperCase('\u054D'))
    assertTrue(Character.isUpperCase('\u053E'))
    assertTrue(Character.isUpperCase('\u1E22'))
    assertTrue(Character.isUpperCase('\u013F'))
    assertTrue(Character.isUpperCase('\u1E2A'))
    assertTrue(Character.isUpperCase('\uA760'))
    assertTrue(Character.isUpperCase('\u24C3'))
    assertTrue(Character.isUpperCase('\u049A'))
    assertTrue(Character.isUpperCase('\u10BD'))
    assertTrue(Character.isUpperCase('\u1EEA'))
    assertTrue(Character.isUpperCase('\u0397'))
    assertTrue(Character.isUpperCase('\u1F3D'))
    assertTrue(Character.isUpperCase('\u050A'))
    assertTrue(Character.isUpperCase('\uFF23'))
    assertTrue(Character.isUpperCase('\u2C01'))
    assertTrue(Character.isUpperCase('\u10C4'))
    assertTrue(Character.isUpperCase('\u1EA2'))
    assertTrue(Character.isUpperCase('\u2C72'))
    assertTrue(Character.isUpperCase('\u0554'))
    assertTrue(Character.isUpperCase('\u01E8'))
    assertTrue(Character.isUpperCase('\u10A4'))
    assertTrue(Character.isUpperCase('\u1F0C'))
    assertTrue(Character.isUpperCase('\u0520'))
    assertTrue(Character.isUpperCase('\u00C5'))
    assertTrue(Character.isUpperCase('\u10AC'))
    assertTrue(Character.isUpperCase('\u2CA4'))
    assertTrue(Character.isUpperCase('\uA73A'))
    assertTrue(Character.isUpperCase('\u1EFC'))
    assertTrue(Character.isUpperCase('\u0049'))
    assertTrue(Character.isUpperCase('\u10A4'))
    assertTrue(Character.isUpperCase('\uA652'))
    assertTrue(Character.isUpperCase('\u1E0E'))
    assertTrue(Character.isUpperCase('\u04E2'))
    assertTrue(Character.isUpperCase('\u0174'))
    assertTrue(Character.isUpperCase('\u00CE'))
    assertTrue(Character.isUpperCase('\u042F'))
    assertTrue(Character.isUpperCase('\u053F'))
    assertTrue(Character.isUpperCase('\u0222'))
    assertTrue(Character.isUpperCase('\u1F3F'))
    assertTrue(Character.isUpperCase('\uA77E'))
    assertTrue(Character.isUpperCase('\u01AC'))
    assertTrue(Character.isUpperCase('\u2C20'))
    assertTrue(Character.isUpperCase('\u1ECC'))
    assertTrue(Character.isUpperCase('\u04DE'))
    assertTrue(Character.isUpperCase('\u004F'))
    assertTrue(Character.isUpperCase('\u2CE2'))
    assertTrue(Character.isUpperCase('\u0124'))
    assertTrue(Character.isUpperCase('\u1F4A'))
    assertTrue(Character.isUpperCase('\u0112'))
    assertTrue(Character.isUpperCase('\u016E'))
    assertTrue(Character.isUpperCase('\u24B7'))
    assertTrue(Character.isUpperCase('\u211A'))
    assertTrue(Character.isUpperCase('\uA72A'))
    assertTrue(Character.isUpperCase('\u0388'))
    assertTrue(Character.isUpperCase('\uA726'))
    assertTrue(Character.isUpperCase('\u0052'))
    assertTrue(Character.isUpperCase('\u1EB0'))
    assertTrue(Character.isUpperCase('\u0197'))
    assertTrue(Character.isUpperCase('\u014A'))

    // 100 randomly chosen non uppeer cases
    assertFalse(Character.isUpperCase('\u58C5'))
    assertFalse(Character.isUpperCase('\uF319'))
    assertFalse(Character.isUpperCase('\u1FBC'))
    assertFalse(Character.isUpperCase('\u8143'))
    assertFalse(Character.isUpperCase('\u468A'))
    assertFalse(Character.isUpperCase('\uEAA8'))
    assertFalse(Character.isUpperCase('\u3D3A'))
    assertFalse(Character.isUpperCase('\uB168'))
    assertFalse(Character.isUpperCase('\u6747'))
    assertFalse(Character.isUpperCase('\uAA8D'))
    assertFalse(Character.isUpperCase('\u26B7'))
    assertFalse(Character.isUpperCase('\uDFE3'))
    assertFalse(Character.isUpperCase('\u51AD'))
    assertFalse(Character.isUpperCase('\uB4FC'))
    assertFalse(Character.isUpperCase('\uE23A'))
    assertFalse(Character.isUpperCase('\u9465'))
    assertFalse(Character.isUpperCase('\uAE66'))
    assertFalse(Character.isUpperCase('\u2EA3'))
    assertFalse(Character.isUpperCase('\u2D05'))
    assertFalse(Character.isUpperCase('\u2033'))
    assertFalse(Character.isUpperCase('\u4BFA'))
    assertFalse(Character.isUpperCase('\u89B6'))
    assertFalse(Character.isUpperCase('\u9A3F'))
    assertFalse(Character.isUpperCase('\u4FCD'))
    assertFalse(Character.isUpperCase('\u770B'))
    assertFalse(Character.isUpperCase('\u17B4'))
    assertFalse(Character.isUpperCase('\u08A6'))
    assertFalse(Character.isUpperCase('\uA202'))
    assertFalse(Character.isUpperCase('\uE726'))
    assertFalse(Character.isUpperCase('\u8605'))
    assertFalse(Character.isUpperCase('\uE24F'))
    assertFalse(Character.isUpperCase('\u588C'))
    assertFalse(Character.isUpperCase('\u3AF6'))
    assertFalse(Character.isUpperCase('\u313F'))
    assertFalse(Character.isUpperCase('\u8804'))
    assertFalse(Character.isUpperCase('\uE02A'))
    assertFalse(Character.isUpperCase('\u89C5'))
    assertFalse(Character.isUpperCase('\u3B9E'))
    assertFalse(Character.isUpperCase('\uF678'))
    assertFalse(Character.isUpperCase('\uD142'))
    assertFalse(Character.isUpperCase('\u19E5'))
    assertFalse(Character.isUpperCase('\u8C7A'))
    assertFalse(Character.isUpperCase('\uBF0C'))
    assertFalse(Character.isUpperCase('\u5EC6'))
    assertFalse(Character.isUpperCase('\u6EA3'))
    assertFalse(Character.isUpperCase('\uCD19'))
    assertFalse(Character.isUpperCase('\uA6F3'))
    assertFalse(Character.isUpperCase('\uA5FD'))
    assertFalse(Character.isUpperCase('\uEAA7'))
    assertFalse(Character.isUpperCase('\u89DB'))
    assertFalse(Character.isUpperCase('\u2657'))
    assertFalse(Character.isUpperCase('\u667A'))
    assertFalse(Character.isUpperCase('\u131C'))
    assertFalse(Character.isUpperCase('\u5906'))
    assertFalse(Character.isUpperCase('\uAA61'))
    assertFalse(Character.isUpperCase('\uCDD3'))
    assertFalse(Character.isUpperCase('\u1952'))
    assertFalse(Character.isUpperCase('\uD93B'))
    assertFalse(Character.isUpperCase('\uE5B4'))
    assertFalse(Character.isUpperCase('\uF5D1'))
    assertFalse(Character.isUpperCase('\uFC49'))
    assertFalse(Character.isUpperCase('\u51D6'))
    assertFalse(Character.isUpperCase('\u1D29'))
    assertFalse(Character.isUpperCase('\u2A30'))
    assertFalse(Character.isUpperCase('\u881A'))
    assertFalse(Character.isUpperCase('\u2EA5'))
    assertFalse(Character.isUpperCase('\u866C'))
    assertFalse(Character.isUpperCase('\uCAF5'))
    assertFalse(Character.isUpperCase('\uF5F1'))
    assertFalse(Character.isUpperCase('\u97B4'))
    assertFalse(Character.isUpperCase('\uF5F5'))
    assertFalse(Character.isUpperCase('\u46FD'))
    assertFalse(Character.isUpperCase('\u538B'))
    assertFalse(Character.isUpperCase('\u320B'))
    assertFalse(Character.isUpperCase('\u6F1F'))
    assertFalse(Character.isUpperCase('\u85B4'))
    assertFalse(Character.isUpperCase('\u887E'))
    assertFalse(Character.isUpperCase('\uEA84'))
    assertFalse(Character.isUpperCase('\uFFE2'))
    assertFalse(Character.isUpperCase('\u48B2'))
    assertFalse(Character.isUpperCase('\u3D8D'))
    assertFalse(Character.isUpperCase('\u071B'))
    assertFalse(Character.isUpperCase('\u70D9'))
    assertFalse(Character.isUpperCase('\u86FD'))
    assertFalse(Character.isUpperCase('\u9303'))
    assertFalse(Character.isUpperCase('\u6620'))
    assertFalse(Character.isUpperCase('\uB503'))
    assertFalse(Character.isUpperCase('\u9D5A'))
    assertFalse(Character.isUpperCase('\u7909'))
    assertFalse(Character.isUpperCase('\u772C'))
    assertFalse(Character.isUpperCase('\u93D8'))
    assertFalse(Character.isUpperCase('\uE969'))
    assertFalse(Character.isUpperCase('\u327F'))
    assertFalse(Character.isUpperCase('\u8473'))
    assertFalse(Character.isUpperCase('\u478B'))
    assertFalse(Character.isUpperCase('\uB916'))
    assertFalse(Character.isUpperCase('\uBBFA'))
    assertFalse(Character.isUpperCase('\uDC65'))
    assertFalse(Character.isUpperCase('\u65E9'))
    assertFalse(Character.isUpperCase('\u97FF'))
  }

  @Test def isLowerCase(): Unit = {
    for (c <- 'a' to 'z')
      assertTrue(Character.isLowerCase(c))
    for (c <- 'A' to 'Z')
      assertFalse(Character.isLowerCase(c))
    for (c <- '0' to '9')
      assertFalse(Character.isLowerCase(c))

    // example form issue #1646
    assertFalse(Character.isLowerCase('('))
    assertFalse(Character.isLowerCase(')'))

    // 50 randomly chosen characters that produce true
    assertTrue(Character.isLowerCase('\u00BA'))
    assertTrue(Character.isLowerCase('\u02B0'))
    assertTrue(Character.isLowerCase('\u02B2'))
    assertTrue(Character.isLowerCase('\u02E1'))
    assertTrue(Character.isLowerCase('\u02E3'))
    assertTrue(Character.isLowerCase('\u1D2D'))
    assertTrue(Character.isLowerCase('\u1D34'))
    assertTrue(Character.isLowerCase('\u1D36'))
    assertTrue(Character.isLowerCase('\u1D3B'))
    assertTrue(Character.isLowerCase('\u1D3D'))
    assertTrue(Character.isLowerCase('\u1D42'))
    assertTrue(Character.isLowerCase('\u1D43'))
    assertTrue(Character.isLowerCase('\u1D48'))
    assertTrue(Character.isLowerCase('\u1D4C'))
    assertTrue(Character.isLowerCase('\u1D51'))
    assertTrue(Character.isLowerCase('\u1D54'))
    assertTrue(Character.isLowerCase('\u1D61'))
    assertTrue(Character.isLowerCase('\u1D64'))
    assertTrue(Character.isLowerCase('\u1D65'))
    assertTrue(Character.isLowerCase('\u1D66'))
    assertTrue(Character.isLowerCase('\u1D67'))
    assertTrue(Character.isLowerCase('\u1D9B'))
    assertTrue(Character.isLowerCase('\u1D9C'))
    assertTrue(Character.isLowerCase('\u1DA2'))
    assertTrue(Character.isLowerCase('\u1DAB'))
    assertTrue(Character.isLowerCase('\u1DB0'))
    assertTrue(Character.isLowerCase('\u1DB2'))
    assertTrue(Character.isLowerCase('\u1DBA'))
    assertTrue(Character.isLowerCase('\u1DBB'))
    assertTrue(Character.isLowerCase('\u2092'))
    assertTrue(Character.isLowerCase('\u2096'))
    assertTrue(Character.isLowerCase('\u2098'))
    assertTrue(Character.isLowerCase('\u2099'))
    assertTrue(Character.isLowerCase('\u2173'))
    assertTrue(Character.isLowerCase('\u2176'))
    assertTrue(Character.isLowerCase('\u2177'))
    assertTrue(Character.isLowerCase('\u217D'))
    assertTrue(Character.isLowerCase('\u217F'))
    assertTrue(Character.isLowerCase('\u24D5'))
    assertTrue(Character.isLowerCase('\u24D9'))
    assertTrue(Character.isLowerCase('\u24DE'))
    assertTrue(Character.isLowerCase('\u24E4'))

    // 50 randomly chosen characters that produce false
    assertFalse(Character.isLowerCase('\u0EB1'))
    assertFalse(Character.isLowerCase('\u8081'))
    assertFalse(Character.isLowerCase('\u9CC7'))
    assertFalse(Character.isLowerCase('\uF690'))
    assertFalse(Character.isLowerCase(1000364))
    assertFalse(Character.isLowerCase(1014512))
    assertFalse(Character.isLowerCase(1015728))
    assertFalse(Character.isLowerCase(1059585))
    assertFalse(Character.isLowerCase(1060648))
    assertFalse(Character.isLowerCase(1072459))
    assertFalse(Character.isLowerCase(158456))
    assertFalse(Character.isLowerCase(202025))
    assertFalse(Character.isLowerCase(210769))
    assertFalse(Character.isLowerCase(229801))
    assertFalse(Character.isLowerCase(297040))
    assertFalse(Character.isLowerCase(336465))
    assertFalse(Character.isLowerCase(371554))
    assertFalse(Character.isLowerCase(375989))
    assertFalse(Character.isLowerCase(400272))
    assertFalse(Character.isLowerCase(416137))
    assertFalse(Character.isLowerCase(438321))
    assertFalse(Character.isLowerCase(440405))
    assertFalse(Character.isLowerCase(443020))
    assertFalse(Character.isLowerCase(459145))
    assertFalse(Character.isLowerCase(514186))
    assertFalse(Character.isLowerCase(540856))
    assertFalse(Character.isLowerCase(596077))
    assertFalse(Character.isLowerCase(599610))
    assertFalse(Character.isLowerCase(606387))
    assertFalse(Character.isLowerCase(669554))
    assertFalse(Character.isLowerCase(707507))
    assertFalse(Character.isLowerCase(712083))
    assertFalse(Character.isLowerCase(725257))
    assertFalse(Character.isLowerCase(742132))
    assertFalse(Character.isLowerCase(767248))
    assertFalse(Character.isLowerCase(767605))
    assertFalse(Character.isLowerCase(787060))
    assertFalse(Character.isLowerCase(801428))
    assertFalse(Character.isLowerCase(816223))
    assertFalse(Character.isLowerCase(858580))
    assertFalse(Character.isLowerCase(870913))
    assertFalse(Character.isLowerCase(907591))
    assertFalse(Character.isLowerCase(911179))
    assertFalse(Character.isLowerCase(919865))
    assertFalse(Character.isLowerCase(923549))
    assertFalse(Character.isLowerCase(932196))
    assertFalse(Character.isLowerCase(947222))
    assertFalse(Character.isLowerCase(967074))
    assertFalse(Character.isLowerCase(968024))
    assertFalse(Character.isLowerCase(989574))
  }

  @Test def isLetter(): Unit = {
    for (c <- 'a' to 'z')
      assertTrue(Character.isLetter(c))
    for (c <- 'A' to 'Z')
      assertTrue(Character.isLetter(c))

    // 100 randomly chosen characters that produce true
    assertTrue(Character.isLetter('\u0931'))
    assertTrue(Character.isLetter('\u0E2D'))
    assertTrue(Character.isLetter('\u1E4F'))
    assertTrue(Character.isLetter('\u2DA5'))
    assertTrue(Character.isLetter('\u36AF'))
    assertTrue(Character.isLetter('\u38BE'))
    assertTrue(Character.isLetter('\u38DD'))
    assertTrue(Character.isLetter('\u39CE'))
    assertTrue(Character.isLetter('\u3D44'))
    assertTrue(Character.isLetter('\u3D63'))
    assertTrue(Character.isLetter('\u4821'))
    assertTrue(Character.isLetter('\u53F3'))
    assertTrue(Character.isLetter('\u5860'))
    assertTrue(Character.isLetter('\u588E'))
    assertTrue(Character.isLetter('\u58FE'))
    assertTrue(Character.isLetter('\u596C'))
    assertTrue(Character.isLetter('\u59DE'))
    assertTrue(Character.isLetter('\u670F'))
    assertTrue(Character.isLetter('\u6E9A'))
    assertTrue(Character.isLetter('\u7072'))
    assertTrue(Character.isLetter('\u7542'))
    assertTrue(Character.isLetter('\u8145'))
    assertTrue(Character.isLetter('\u8494'))
    assertTrue(Character.isLetter('\u8495'))
    assertTrue(Character.isLetter('\u8A95'))
    assertTrue(Character.isLetter('\u8F1C'))
    assertTrue(Character.isLetter('\u8FBB'))
    assertTrue(Character.isLetter('\u9134'))
    assertTrue(Character.isLetter('\u9503'))
    assertTrue(Character.isLetter('\u951E'))
    assertTrue(Character.isLetter('\u9EBA'))
    assertTrue(Character.isLetter('\uA13C'))
    assertTrue(Character.isLetter('\uA5E6'))
    assertTrue(Character.isLetter('\uA810'))
    assertTrue(Character.isLetter('\uB3B5'))
    assertTrue(Character.isLetter('\uB4BA'))
    assertTrue(Character.isLetter('\uB66A'))
    assertTrue(Character.isLetter('\uCC72'))
    assertTrue(Character.isLetter('\uCFC5'))
    assertTrue(Character.isLetter('\uD0EA'))
    assertTrue(Character.isLetter('\uD3DB'))
    assertTrue(Character.isLetter('\uD421'))
    assertTrue(Character.isLetter('\uD4C7'))
    assertTrue(Character.isLetter('\uD5C9'))
    assertTrue(Character.isLetter('\uD645'))
    assertTrue(Character.isLetter('\uFEEC'))
    assertTrue(Character.isLetter(126614))
    assertTrue(Character.isLetter(131284))
    assertTrue(Character.isLetter(132125))
    assertTrue(Character.isLetter(133027))
    assertTrue(Character.isLetter(134132))
    assertTrue(Character.isLetter(135009))
    assertTrue(Character.isLetter(136707))
    assertTrue(Character.isLetter(137302))
    assertTrue(Character.isLetter(139777))
    assertTrue(Character.isLetter(139817))
    assertTrue(Character.isLetter(140291))
    assertTrue(Character.isLetter(140662))
    assertTrue(Character.isLetter(141121))
    assertTrue(Character.isLetter(142226))
    assertTrue(Character.isLetter(142357))
    assertTrue(Character.isLetter(143348))
    assertTrue(Character.isLetter(143406))
    assertTrue(Character.isLetter(144320))
    assertTrue(Character.isLetter(150432))
    assertTrue(Character.isLetter(150647))
    assertTrue(Character.isLetter(150718))
    assertTrue(Character.isLetter(154065))
    assertTrue(Character.isLetter(154216))
    assertTrue(Character.isLetter(154406))
    assertTrue(Character.isLetter(154981))
    assertTrue(Character.isLetter(156075))
    assertTrue(Character.isLetter(156323))
    assertTrue(Character.isLetter(156813))
    assertTrue(Character.isLetter(157428))
    assertTrue(Character.isLetter(159375))
    assertTrue(Character.isLetter(159743))
    assertTrue(Character.isLetter(160356))
    assertTrue(Character.isLetter(160611))
    assertTrue(Character.isLetter(160671))
    assertTrue(Character.isLetter(161921))
    assertTrue(Character.isLetter(163295))
    assertTrue(Character.isLetter(163637))
    assertTrue(Character.isLetter(163690))
    assertTrue(Character.isLetter(164083))
    assertTrue(Character.isLetter(166120))
    assertTrue(Character.isLetter(166691))
    assertTrue(Character.isLetter(167425))
    assertTrue(Character.isLetter(168930))
    assertTrue(Character.isLetter(169746))
    assertTrue(Character.isLetter(171107))
    assertTrue(Character.isLetter(171334))
    assertTrue(Character.isLetter(172074))
    assertTrue(Character.isLetter(173301))
    assertTrue(Character.isLetter(175989))
    assertTrue(Character.isLetter(176347))
    assertTrue(Character.isLetter(176935))
    assertTrue(Character.isLetter(66253))
    assertTrue(Character.isLetter(74015))
    assertTrue(Character.isLetter(93997))

    // 100 randomly chosen assigned characters that produce false
    assertFalse(Character.isLetter('\u0b6a'))
    assertFalse(Character.isLetter('\u203a'))
    assertFalse(Character.isLetter('\u2e31'))
    assertFalse(Character.isLetter('\ue0ff'))
    assertFalse(Character.isLetter('\ue214'))
    assertFalse(Character.isLetter('\ue6f2'))
    assertFalse(Character.isLetter('\ue7b2'))
    assertFalse(Character.isLetter('\ueb4e'))
    assertFalse(Character.isLetter(0x10a58))
    assertFalse(Character.isLetter(0x1d983))
    assertFalse(Character.isLetter(0x1f03c))
    assertFalse(Character.isLetter(0x1f220))
    assertFalse(Character.isLetter(0xf0832))
    assertFalse(Character.isLetter(0xf0feb))
    assertFalse(Character.isLetter(0xf1088))
    assertFalse(Character.isLetter(0xf14fe))
    assertFalse(Character.isLetter(0xf1931))
    assertFalse(Character.isLetter(0xf243c))
    assertFalse(Character.isLetter(0xf2a73))
    assertFalse(Character.isLetter(0xf33e9))
    assertFalse(Character.isLetter(0xf39af))
    assertFalse(Character.isLetter(0xf3d84))
    assertFalse(Character.isLetter(0xf4015))
    assertFalse(Character.isLetter(0xf4f21))
    assertFalse(Character.isLetter(0xf533f))
    assertFalse(Character.isLetter(0xf5759))
    assertFalse(Character.isLetter(0xf5e1d))
    assertFalse(Character.isLetter(0xf5eb5))
    assertFalse(Character.isLetter(0xf6d80))
    assertFalse(Character.isLetter(0xf77fc))
    assertFalse(Character.isLetter(0xf7cc7))
    assertFalse(Character.isLetter(0xf8072))
    assertFalse(Character.isLetter(0xf84d1))
    assertFalse(Character.isLetter(0xf853f))
    assertFalse(Character.isLetter(0xf90b8))
    assertFalse(Character.isLetter(0xfa489))
    assertFalse(Character.isLetter(0xfae60))
    assertFalse(Character.isLetter(0xfda56))
    assertFalse(Character.isLetter(0xfdc92))
    assertFalse(Character.isLetter(0xfde67))
    assertFalse(Character.isLetter(0xfe464))
    assertFalse(Character.isLetter(0xfe7d2))
    assertFalse(Character.isLetter(0xfe7e9))
    assertFalse(Character.isLetter(0xfe8d8))
    assertFalse(Character.isLetter(0xfecb6))
    assertFalse(Character.isLetter(0xfee21))
    assertFalse(Character.isLetter(0xfef92))
    assertFalse(Character.isLetter(0xff593))
    assertFalse(Character.isLetter(0xff8f4))
    assertFalse(Character.isLetter(0x1002e0))
    assertFalse(Character.isLetter(0x1016c0))
    assertFalse(Character.isLetter(0x1017c5))
    assertFalse(Character.isLetter(0x101a24))
    assertFalse(Character.isLetter(0x101d75))
    assertFalse(Character.isLetter(0x1024b7))
    assertFalse(Character.isLetter(0x102922))
    assertFalse(Character.isLetter(0x103656))
    assertFalse(Character.isLetter(0x103efc))
    assertFalse(Character.isLetter(0x104618))
    assertFalse(Character.isLetter(0x1047ee))
    assertFalse(Character.isLetter(0x10517c))
    assertFalse(Character.isLetter(0x1058fb))
    assertFalse(Character.isLetter(0x105f5c))
    assertFalse(Character.isLetter(0x10637c))
    assertFalse(Character.isLetter(0x10648a))
    assertFalse(Character.isLetter(0x1065f9))
    assertFalse(Character.isLetter(0x106a6a))
    assertFalse(Character.isLetter(0x106e01))
    assertFalse(Character.isLetter(0x107412))
    assertFalse(Character.isLetter(0x1077b0))
    assertFalse(Character.isLetter(0x107968))
    assertFalse(Character.isLetter(0x107dc5))
    assertFalse(Character.isLetter(0x10827d))
    assertFalse(Character.isLetter(0x108339))
    assertFalse(Character.isLetter(0x10854e))
    assertFalse(Character.isLetter(0x10893d))
    assertFalse(Character.isLetter(0x109f61))
    assertFalse(Character.isLetter(0x10a0c5))
    assertFalse(Character.isLetter(0x10a12f))
    assertFalse(Character.isLetter(0x10a574))
    assertFalse(Character.isLetter(0x10a8bb))
    assertFalse(Character.isLetter(0x10aa14))
    assertFalse(Character.isLetter(0x10abd7))
    assertFalse(Character.isLetter(0x10be0d))
    assertFalse(Character.isLetter(0x10be13))
    assertFalse(Character.isLetter(0x10c2e6))
    assertFalse(Character.isLetter(0x10c370))
    assertFalse(Character.isLetter(0x10c388))
    assertFalse(Character.isLetter(0x10cb0a))
    assertFalse(Character.isLetter(0x10cbad))
    assertFalse(Character.isLetter(0x10cc78))
    assertFalse(Character.isLetter(0x10d027))
    assertFalse(Character.isLetter(0x10d06f))
    assertFalse(Character.isLetter(0x10d486))
    assertFalse(Character.isLetter(0x10d677))
    assertFalse(Character.isLetter(0x10d976))
    assertFalse(Character.isLetter(0x10e9df))
    assertFalse(Character.isLetter(0x10eff5))
    assertFalse(Character.isLetter(0x10f0fd))
    assertFalse(Character.isLetter(0x10f739))
  }

  @Test def isLetterOrDigit(): Unit = {
    // 100 randomly chosen characters that produce true
    assertTrue(Character.isLetterOrDigit('\u0209'))
    assertTrue(Character.isLetterOrDigit('\u0377'))
    assertTrue(Character.isLetterOrDigit('\u0497'))
    assertTrue(Character.isLetterOrDigit('\u053E'))
    assertTrue(Character.isLetterOrDigit('\u0726'))
    assertTrue(Character.isLetterOrDigit('\u075A'))
    assertTrue(Character.isLetterOrDigit('\u0A91'))
    assertTrue(Character.isLetterOrDigit('\u12CB'))
    assertTrue(Character.isLetterOrDigit('\u182B'))
    assertTrue(Character.isLetterOrDigit('\u1866'))
    assertTrue(Character.isLetterOrDigit('\u1F1C'))
    assertTrue(Character.isLetterOrDigit('\u1F74'))
    assertTrue(Character.isLetterOrDigit('\u1F7C'))
    assertTrue(Character.isLetterOrDigit('\u2D53'))
    assertTrue(Character.isLetterOrDigit('\u30DF'))
    assertTrue(Character.isLetterOrDigit('\u3772'))
    assertTrue(Character.isLetterOrDigit('\u3A13'))
    assertTrue(Character.isLetterOrDigit('\u3C1D'))
    assertTrue(Character.isLetterOrDigit('\u3C4C'))
    assertTrue(Character.isLetterOrDigit('\u3DEC'))
    assertTrue(Character.isLetterOrDigit('\u3F1E'))
    assertTrue(Character.isLetterOrDigit('\u3F4E'))
    assertTrue(Character.isLetterOrDigit('\u425A'))
    assertTrue(Character.isLetterOrDigit('\u46D7'))
    assertTrue(Character.isLetterOrDigit('\u4822'))
    assertTrue(Character.isLetterOrDigit('\u4855'))
    assertTrue(Character.isLetterOrDigit('\u485D'))
    assertTrue(Character.isLetterOrDigit('\u4970'))
    assertTrue(Character.isLetterOrDigit('\u4C3A'))
    assertTrue(Character.isLetterOrDigit('\u5188'))
    assertTrue(Character.isLetterOrDigit('\u5412'))
    assertTrue(Character.isLetterOrDigit('\u585A'))
    assertTrue(Character.isLetterOrDigit('\u5933'))
    assertTrue(Character.isLetterOrDigit('\u5966'))
    assertTrue(Character.isLetterOrDigit('\u5B15'))
    assertTrue(Character.isLetterOrDigit('\u5C8B'))
    assertTrue(Character.isLetterOrDigit('\u5DA2'))
    assertTrue(Character.isLetterOrDigit('\u6053'))
    assertTrue(Character.isLetterOrDigit('\u63BB'))
    assertTrue(Character.isLetterOrDigit('\u6476'))
    assertTrue(Character.isLetterOrDigit('\u64B9'))
    assertTrue(Character.isLetterOrDigit('\u65B6'))
    assertTrue(Character.isLetterOrDigit('\u67EB'))
    assertTrue(Character.isLetterOrDigit('\u693B'))
    assertTrue(Character.isLetterOrDigit('\u698C'))
    assertTrue(Character.isLetterOrDigit('\u6B86'))
    assertTrue(Character.isLetterOrDigit('\u6DA1'))
    assertTrue(Character.isLetterOrDigit('\u6E5E'))
    assertTrue(Character.isLetterOrDigit('\u6EFA'))
    assertTrue(Character.isLetterOrDigit('\u73BE'))
    assertTrue(Character.isLetterOrDigit('\u7403'))
    assertTrue(Character.isLetterOrDigit('\u7611'))
    assertTrue(Character.isLetterOrDigit('\u76C9'))
    assertTrue(Character.isLetterOrDigit('\u76F6'))
    assertTrue(Character.isLetterOrDigit('\u7943'))
    assertTrue(Character.isLetterOrDigit('\u7A34'))
    assertTrue(Character.isLetterOrDigit('\u7CA0'))
    assertTrue(Character.isLetterOrDigit('\u7ED2'))
    assertTrue(Character.isLetterOrDigit('\u82F6'))
    assertTrue(Character.isLetterOrDigit('\u83F2'))
    assertTrue(Character.isLetterOrDigit('\u8401'))
    assertTrue(Character.isLetterOrDigit('\u8420'))
    assertTrue(Character.isLetterOrDigit('\u848C'))
    assertTrue(Character.isLetterOrDigit('\u8602'))
    assertTrue(Character.isLetterOrDigit('\u8638'))
    assertTrue(Character.isLetterOrDigit('\u88E7'))
    assertTrue(Character.isLetterOrDigit('\u8A0B'))
    assertTrue(Character.isLetterOrDigit('\u9180'))
    assertTrue(Character.isLetterOrDigit('\u920B'))
    assertTrue(Character.isLetterOrDigit('\u94C9'))
    assertTrue(Character.isLetterOrDigit('\u94CE'))
    assertTrue(Character.isLetterOrDigit('\u95A4'))
    assertTrue(Character.isLetterOrDigit('\u979E'))
    assertTrue(Character.isLetterOrDigit('\u9D66'))
    assertTrue(Character.isLetterOrDigit('\u9ED0'))
    assertTrue(Character.isLetterOrDigit('\uA047'))
    assertTrue(Character.isLetterOrDigit('\uA65A'))
    assertTrue(Character.isLetterOrDigit('\uAEA3'))
    assertTrue(Character.isLetterOrDigit('\uAEC5'))
    assertTrue(Character.isLetterOrDigit('\uB583'))
    assertTrue(Character.isLetterOrDigit('\uBADA'))
    assertTrue(Character.isLetterOrDigit('\uC56F'))
    assertTrue(Character.isLetterOrDigit('\uC857'))
    assertTrue(Character.isLetterOrDigit('\uC97C'))
    assertTrue(Character.isLetterOrDigit('\uC9BF'))
    assertTrue(Character.isLetterOrDigit('\uCC2B'))
    assertTrue(Character.isLetterOrDigit('\uCC60'))
    assertTrue(Character.isLetterOrDigit('\uCF31'))
    assertTrue(Character.isLetterOrDigit('\uCF9E'))
    assertTrue(Character.isLetterOrDigit('\uD2F6'))
    assertTrue(Character.isLetterOrDigit('\uD353'))
    assertTrue(Character.isLetterOrDigit('\uD415'))
    assertTrue(Character.isLetterOrDigit('\uD4B2'))
    assertTrue(Character.isLetterOrDigit('\uD4D3'))
    assertTrue(Character.isLetterOrDigit('\uD64E'))
    assertTrue(Character.isLetterOrDigit('\uD76B'))
    assertTrue(Character.isLetterOrDigit('\uFA9F'))
    assertTrue(Character.isLetterOrDigit('\uFB7C'))
    assertTrue(Character.isLetterOrDigit('\uFCDD'))
    assertTrue(Character.isLetterOrDigit('\uFEBD'))

    /* 100 randomly chosen characters that produce false,
     * minus those that became true in JDK 11 or later.
     */
    assertFalse(Character.isLetterOrDigit('\u02D8'))
    assertFalse(Character.isLetterOrDigit('\u0312'))
    assertFalse(Character.isLetterOrDigit('\u0361'))
    assertFalse(Character.isLetterOrDigit('\u05EB'))
    assertFalse(Character.isLetterOrDigit('\u0618'))
    assertFalse(Character.isLetterOrDigit('\u070B'))
    assertFalse(Character.isLetterOrDigit('\u08D7'))
    assertFalse(Character.isLetterOrDigit('\u0947'))
    assertFalse(Character.isLetterOrDigit('\u0B56'))
    assertFalse(Character.isLetterOrDigit('\u0B77'))
    assertFalse(Character.isLetterOrDigit('\u0DDA'))
    assertFalse(Character.isLetterOrDigit('\u0E7B'))
    assertFalse(Character.isLetterOrDigit('\u0EBA'))
    assertFalse(Character.isLetterOrDigit('\u0F0B'))
    assertFalse(Character.isLetterOrDigit('\u109B'))
    assertFalse(Character.isLetterOrDigit('\u13F7'))
    assertFalse(Character.isLetterOrDigit('\u181B'))
    assertFalse(Character.isLetterOrDigit('\u193F'))
    assertFalse(Character.isLetterOrDigit('\u1FBF'))
    assertFalse(Character.isLetterOrDigit('\u2103'))
    assertFalse(Character.isLetterOrDigit('\u219A'))
    assertFalse(Character.isLetterOrDigit('\u21C4'))
    assertFalse(Character.isLetterOrDigit('\u2220'))
    assertFalse(Character.isLetterOrDigit('\u2250'))
    assertFalse(Character.isLetterOrDigit('\u228D'))
    assertFalse(Character.isLetterOrDigit('\u231B'))
    assertFalse(Character.isLetterOrDigit('\u23BC'))
    assertFalse(Character.isLetterOrDigit('\u2413'))
    assertFalse(Character.isLetterOrDigit('\u24BF'))
    assertFalse(Character.isLetterOrDigit('\u258F'))
    assertFalse(Character.isLetterOrDigit('\u25C2'))
    assertFalse(Character.isLetterOrDigit('\u27F2'))
    assertFalse(Character.isLetterOrDigit('\u2887'))
    assertFalse(Character.isLetterOrDigit('\u2998'))
    assertFalse(Character.isLetterOrDigit('\u29A5'))
    assertFalse(Character.isLetterOrDigit('\u29D2'))
    assertFalse(Character.isLetterOrDigit('\u2A53'))
    assertFalse(Character.isLetterOrDigit('\u2A70'))
    assertFalse(Character.isLetterOrDigit('\u2B0E'))
    assertFalse(Character.isLetterOrDigit('\u2B5E'))
    assertFalse(Character.isLetterOrDigit('\u2BAF'))
    assertFalse(Character.isLetterOrDigit('\u2EAE'))
    assertFalse(Character.isLetterOrDigit('\u2EB1'))
    assertFalse(Character.isLetterOrDigit('\u32E8'))
    assertFalse(Character.isLetterOrDigit('\uA837'))
    assertFalse(Character.isLetterOrDigit('\uA8B4'))
    assertFalse(Character.isLetterOrDigit('\uA8B7'))
    assertFalse(Character.isLetterOrDigit('\uAA30'))
    assertFalse(Character.isLetterOrDigit('\uAAEB'))
    assertFalse(Character.isLetterOrDigit('\uD859'))
    assertFalse(Character.isLetterOrDigit('\uDADF'))
    assertFalse(Character.isLetterOrDigit('\uDC4C'))
    assertFalse(Character.isLetterOrDigit('\uDC80'))
    assertFalse(Character.isLetterOrDigit('\uDD5E'))
    assertFalse(Character.isLetterOrDigit('\uDD9C'))
    assertFalse(Character.isLetterOrDigit('\uDDC5'))
    assertFalse(Character.isLetterOrDigit('\uDE97'))
    assertFalse(Character.isLetterOrDigit('\uDF78'))
    assertFalse(Character.isLetterOrDigit('\uDF90'))
    assertFalse(Character.isLetterOrDigit('\uE044'))
    assertFalse(Character.isLetterOrDigit('\uE0D9'))
    assertFalse(Character.isLetterOrDigit('\uE158'))
    assertFalse(Character.isLetterOrDigit('\uE19E'))
    assertFalse(Character.isLetterOrDigit('\uE2AF'))
    assertFalse(Character.isLetterOrDigit('\uE317'))
    assertFalse(Character.isLetterOrDigit('\uE351'))
    assertFalse(Character.isLetterOrDigit('\uE3C6'))
    assertFalse(Character.isLetterOrDigit('\uE47F'))
    assertFalse(Character.isLetterOrDigit('\uE54C'))
    assertFalse(Character.isLetterOrDigit('\uE67A'))
    assertFalse(Character.isLetterOrDigit('\uE69C'))
    assertFalse(Character.isLetterOrDigit('\uE6D1'))
    assertFalse(Character.isLetterOrDigit('\uE6E2'))
    assertFalse(Character.isLetterOrDigit('\uE744'))
    assertFalse(Character.isLetterOrDigit('\uE780'))
    assertFalse(Character.isLetterOrDigit('\uE7A1'))
    assertFalse(Character.isLetterOrDigit('\uE817'))
    assertFalse(Character.isLetterOrDigit('\uE81A'))
    assertFalse(Character.isLetterOrDigit('\uE8DC'))
    assertFalse(Character.isLetterOrDigit('\uE916'))
    assertFalse(Character.isLetterOrDigit('\uEDD3'))
    assertFalse(Character.isLetterOrDigit('\uEF7C'))
    assertFalse(Character.isLetterOrDigit('\uF047'))
    assertFalse(Character.isLetterOrDigit('\uF0D0'))
    assertFalse(Character.isLetterOrDigit('\uF160'))
    assertFalse(Character.isLetterOrDigit('\uF27C'))
    assertFalse(Character.isLetterOrDigit('\uF319'))
    assertFalse(Character.isLetterOrDigit('\uF46B'))
    assertFalse(Character.isLetterOrDigit('\uF594'))
    assertFalse(Character.isLetterOrDigit('\uF6D1'))
    assertFalse(Character.isLetterOrDigit('\uF7F2'))
    assertFalse(Character.isLetterOrDigit('\uF889'))
    assertFalse(Character.isLetterOrDigit('\uF8F6'))
    assertFalse(Character.isLetterOrDigit('\uF8FF'))
    assertFalse(Character.isLetterOrDigit('\uFAE2'))
    assertFalse(Character.isLetterOrDigit('\uFE4B'))
  }

  @Test def isAlphabetic(): Unit = {
    // 50 randomly chosen characters that produce true
    assertTrue(Character.isAlphabetic('\u04F8'))
    assertTrue(Character.isAlphabetic('\u05DB'))
    assertTrue(Character.isAlphabetic('\u1314'))
    assertTrue(Character.isAlphabetic('\u3515'))
    assertTrue(Character.isAlphabetic('\u3780'))
    assertTrue(Character.isAlphabetic('\u391C'))
    assertTrue(Character.isAlphabetic('\u3B06'))
    assertTrue(Character.isAlphabetic('\u3FEF'))
    assertTrue(Character.isAlphabetic('\u47CF'))
    assertTrue(Character.isAlphabetic('\u5076'))
    assertTrue(Character.isAlphabetic('\u5684'))
    assertTrue(Character.isAlphabetic('\u5773'))
    assertTrue(Character.isAlphabetic('\u591C'))
    assertTrue(Character.isAlphabetic('\u59A0'))
    assertTrue(Character.isAlphabetic('\u5B09'))
    assertTrue(Character.isAlphabetic('\u6775'))
    assertTrue(Character.isAlphabetic('\u7434'))
    assertTrue(Character.isAlphabetic('\u83FB'))
    assertTrue(Character.isAlphabetic('\u8761'))
    assertTrue(Character.isAlphabetic('\u8993'))
    assertTrue(Character.isAlphabetic('\u947A'))
    assertTrue(Character.isAlphabetic('\u98AB'))
    assertTrue(Character.isAlphabetic('\u98DA'))
    assertTrue(Character.isAlphabetic('\u9B44'))
    assertTrue(Character.isAlphabetic('\uADFF'))
    assertTrue(Character.isAlphabetic('\uC091'))
    assertTrue(Character.isAlphabetic('\uC43F'))
    assertTrue(Character.isAlphabetic('\uCB5D'))
    assertTrue(Character.isAlphabetic(133889))
    assertTrue(Character.isAlphabetic(134427))
    assertTrue(Character.isAlphabetic(134471))
    assertTrue(Character.isAlphabetic(138909))
    assertTrue(Character.isAlphabetic(139164))
    assertTrue(Character.isAlphabetic(140493))
    assertTrue(Character.isAlphabetic(148737))
    assertTrue(Character.isAlphabetic(149345))
    assertTrue(Character.isAlphabetic(151435))
    assertTrue(Character.isAlphabetic(156857))
    assertTrue(Character.isAlphabetic(158440))
    assertTrue(Character.isAlphabetic(159937))
    assertTrue(Character.isAlphabetic(159952))
    assertTrue(Character.isAlphabetic(163859))
    assertTrue(Character.isAlphabetic(166872))
    assertTrue(Character.isAlphabetic(167076))
    assertTrue(Character.isAlphabetic(168670))
    assertTrue(Character.isAlphabetic(170390))
    assertTrue(Character.isAlphabetic(170999))
    assertTrue(Character.isAlphabetic(172036))
    assertTrue(Character.isAlphabetic(173135))
    assertTrue(Character.isAlphabetic(176898))

    // 50 randomly chosen characters that produce false
    assertFalse(Character.isAlphabetic(1002047))
    assertFalse(Character.isAlphabetic(1009593))
    assertFalse(Character.isAlphabetic(1042564))
    assertFalse(Character.isAlphabetic(1052587))
    assertFalse(Character.isAlphabetic(1061824))
    assertFalse(Character.isAlphabetic(1077156))
    assertFalse(Character.isAlphabetic(1077935))
    assertFalse(Character.isAlphabetic(108164))
    assertFalse(Character.isAlphabetic(117071))
    assertFalse(Character.isAlphabetic(59053))
    assertFalse(Character.isAlphabetic(235975))
    assertFalse(Character.isAlphabetic(256440))
    assertFalse(Character.isAlphabetic(291721))
    assertFalse(Character.isAlphabetic(313351))
    assertFalse(Character.isAlphabetic(333549))
    assertFalse(Character.isAlphabetic(353806))
    assertFalse(Character.isAlphabetic(390947))
    assertFalse(Character.isAlphabetic(400920))
    assertFalse(Character.isAlphabetic(403305))
    assertFalse(Character.isAlphabetic(417636))
    assertFalse(Character.isAlphabetic(419085))
    assertFalse(Character.isAlphabetic(443247))
    assertFalse(Character.isAlphabetic(468248))
    assertFalse(Character.isAlphabetic(485549))
    assertFalse(Character.isAlphabetic(491917))
    assertFalse(Character.isAlphabetic(511059))
    assertFalse(Character.isAlphabetic(530210))
    assertFalse(Character.isAlphabetic(569030))
    assertFalse(Character.isAlphabetic(595429))
    assertFalse(Character.isAlphabetic(607797))
    assertFalse(Character.isAlphabetic(654788))
    assertFalse(Character.isAlphabetic(660783))
    assertFalse(Character.isAlphabetic(715383))
    assertFalse(Character.isAlphabetic(752828))
    assertFalse(Character.isAlphabetic(778169))
    assertFalse(Character.isAlphabetic(781077))
    assertFalse(Character.isAlphabetic(796535))
    assertFalse(Character.isAlphabetic(819655))
    assertFalse(Character.isAlphabetic(850895))
    assertFalse(Character.isAlphabetic(866871))
    assertFalse(Character.isAlphabetic(885354))
    assertFalse(Character.isAlphabetic(908455))
    assertFalse(Character.isAlphabetic(908635))
    assertFalse(Character.isAlphabetic(924461))
    assertFalse(Character.isAlphabetic(930019))
    assertFalse(Character.isAlphabetic(948273))
    assertFalse(Character.isAlphabetic(974041))
    assertFalse(Character.isAlphabetic(977329))
    assertFalse(Character.isAlphabetic(1085154))
    assertFalse(Character.isAlphabetic(993967))
  }

  @Test def isIdeographic(): Unit = {
    // 50 randomly chosen characters that produce true
    assertTrue(Character.isIdeographic('\u388F'))
    assertTrue(Character.isIdeographic('\u4711'))
    assertTrue(Character.isIdeographic('\u527E'))
    assertTrue(Character.isIdeographic('\u5328'))
    assertTrue(Character.isIdeographic('\u5922'))
    assertTrue(Character.isIdeographic('\u5BA2'))
    assertTrue(Character.isIdeographic('\u5CAC'))
    assertTrue(Character.isIdeographic('\u65AF'))
    assertTrue(Character.isIdeographic('\u694C'))
    assertTrue(Character.isIdeographic('\u8068'))
    assertTrue(Character.isIdeographic('\u8C34'))
    assertTrue(Character.isIdeographic('\u8C9D'))
    assertTrue(Character.isIdeographic('\u8D3D'))
    assertTrue(Character.isIdeographic('\u9C62'))
    assertTrue(Character.isIdeographic(131994))
    assertTrue(Character.isIdeographic(132852))
    assertTrue(Character.isIdeographic(133501))
    assertTrue(Character.isIdeographic(133591))
    assertTrue(Character.isIdeographic(134246))
    assertTrue(Character.isIdeographic(134328))
    assertTrue(Character.isIdeographic(136431))
    assertTrue(Character.isIdeographic(139867))
    assertTrue(Character.isIdeographic(140528))
    assertTrue(Character.isIdeographic(141460))
    assertTrue(Character.isIdeographic(146741))
    assertTrue(Character.isIdeographic(146759))
    assertTrue(Character.isIdeographic(147539))
    assertTrue(Character.isIdeographic(148459))
    assertTrue(Character.isIdeographic(148689))
    assertTrue(Character.isIdeographic(153593))
    assertTrue(Character.isIdeographic(155694))
    assertTrue(Character.isIdeographic(155818))
    assertTrue(Character.isIdeographic(159961))
    assertTrue(Character.isIdeographic(163220))
    assertTrue(Character.isIdeographic(163464))
    assertTrue(Character.isIdeographic(164167))
    assertTrue(Character.isIdeographic(164197))
    assertTrue(Character.isIdeographic(165508))
    assertTrue(Character.isIdeographic(165973))
    assertTrue(Character.isIdeographic(167743))
    assertTrue(Character.isIdeographic(168585))
    assertTrue(Character.isIdeographic(168758))
    assertTrue(Character.isIdeographic(169731))
    assertTrue(Character.isIdeographic(170186))
    assertTrue(Character.isIdeographic(171240))
    assertTrue(Character.isIdeographic(171988))
    assertTrue(Character.isIdeographic(172886))
    assertTrue(Character.isIdeographic(174236))
    assertTrue(Character.isIdeographic(177495))
    assertTrue(Character.isIdeographic(178011))

    // 50 randomly chosen characters that produce false
    assertFalse(Character.isIdeographic('\uFB45'))
    assertFalse(Character.isIdeographic(1005864))
    assertFalse(Character.isIdeographic(1006626))
    assertFalse(Character.isIdeographic(1009910))
    assertFalse(Character.isIdeographic(1032559))
    assertFalse(Character.isIdeographic(1040837))
    assertFalse(Character.isIdeographic(1070571))
    assertFalse(Character.isIdeographic(107607))
    assertFalse(Character.isIdeographic(1084694))
    assertFalse(Character.isIdeographic(1098896))
    assertFalse(Character.isIdeographic(121214))
    assertFalse(Character.isIdeographic(193874))
    assertFalse(Character.isIdeographic(208650))
    assertFalse(Character.isIdeographic(253670))
    assertFalse(Character.isIdeographic(266437))
    assertFalse(Character.isIdeographic(268828))
    assertFalse(Character.isIdeographic(269494))
    assertFalse(Character.isIdeographic(278691))
    assertFalse(Character.isIdeographic(282114))
    assertFalse(Character.isIdeographic(294021))
    assertFalse(Character.isIdeographic(334194))
    assertFalse(Character.isIdeographic(351339))
    assertFalse(Character.isIdeographic(356942))
    assertFalse(Character.isIdeographic(388239))
    assertFalse(Character.isIdeographic(398495))
    assertFalse(Character.isIdeographic(424210))
    assertFalse(Character.isIdeographic(437688))
    assertFalse(Character.isIdeographic(454763))
    assertFalse(Character.isIdeographic(499908))
    assertFalse(Character.isIdeographic(543025))
    assertFalse(Character.isIdeographic(544352))
    assertFalse(Character.isIdeographic(552973))
    assertFalse(Character.isIdeographic(557901))
    assertFalse(Character.isIdeographic(570614))
    assertFalse(Character.isIdeographic(607804))
    assertFalse(Character.isIdeographic(639906))
    assertFalse(Character.isIdeographic(659980))
    assertFalse(Character.isIdeographic(668239))
    assertFalse(Character.isIdeographic(711022))
    assertFalse(Character.isIdeographic(765532))
    assertFalse(Character.isIdeographic(776989))
    assertFalse(Character.isIdeographic(777331))
    assertFalse(Character.isIdeographic(812822))
    assertFalse(Character.isIdeographic(815221))
    assertFalse(Character.isIdeographic(828259))
    assertFalse(Character.isIdeographic(82920))
    assertFalse(Character.isIdeographic(869335))
    assertFalse(Character.isIdeographic(912462))
    assertFalse(Character.isIdeographic(958559))
    assertFalse(Character.isIdeographic(999076))
  }

  @Test def isSpaceChar(): Unit = {
    assertTrue(Character.isSpaceChar('\u0020'))
    assertTrue(Character.isSpaceChar('\u00A0'))
    assertTrue(Character.isSpaceChar('\u1680'))
    assertTrue(Character.isSpaceChar('\u2000'))
    assertTrue(Character.isSpaceChar('\u2001'))
    assertTrue(Character.isSpaceChar('\u2002'))
    assertTrue(Character.isSpaceChar('\u2004'))
    assertTrue(Character.isSpaceChar('\u2005'))
    assertTrue(Character.isSpaceChar('\u2006'))
    assertTrue(Character.isSpaceChar('\u2007'))
    assertTrue(Character.isSpaceChar('\u2008'))
    assertTrue(Character.isSpaceChar('\u2009'))
    assertTrue(Character.isSpaceChar('\u200A'))
    assertTrue(Character.isSpaceChar('\u2028'))
    assertTrue(Character.isSpaceChar('\u2029'))
    assertTrue(Character.isSpaceChar('\u202F'))
    assertTrue(Character.isSpaceChar('\u205F'))
    assertTrue(Character.isSpaceChar('\u3000'))

    // 100 randomly chosen characters that produce false
    assertFalse(Character.isSpaceChar('\u0057'))
    assertFalse(Character.isSpaceChar('\u0290'))
    assertFalse(Character.isSpaceChar('\u0374'))
    assertFalse(Character.isSpaceChar('\u04EA'))
    assertFalse(Character.isSpaceChar('\u0845'))
    assertFalse(Character.isSpaceChar('\u0882'))
    assertFalse(Character.isSpaceChar('\u0956'))
    assertFalse(Character.isSpaceChar('\u0979'))
    assertFalse(Character.isSpaceChar('\u0A78'))
    assertFalse(Character.isSpaceChar('\u0D6B'))
    assertFalse(Character.isSpaceChar('\u122F'))
    assertFalse(Character.isSpaceChar('\u137B'))
    assertFalse(Character.isSpaceChar('\u1690'))
    assertFalse(Character.isSpaceChar('\u1875'))
    assertFalse(Character.isSpaceChar('\u1B96'))
    assertFalse(Character.isSpaceChar('\u2253'))
    assertFalse(Character.isSpaceChar('\u2587'))
    assertFalse(Character.isSpaceChar('\u2770'))
    assertFalse(Character.isSpaceChar('\u2975'))
    assertFalse(Character.isSpaceChar('\u304F'))
    assertFalse(Character.isSpaceChar('\u3567'))
    assertFalse(Character.isSpaceChar('\u356A'))
    assertFalse(Character.isSpaceChar('\u35D0'))
    assertFalse(Character.isSpaceChar('\u3625'))
    assertFalse(Character.isSpaceChar('\u389E'))
    assertFalse(Character.isSpaceChar('\u395F'))
    assertFalse(Character.isSpaceChar('\u3A62'))
    assertFalse(Character.isSpaceChar('\u3BBF'))
    assertFalse(Character.isSpaceChar('\u3BF8'))
    assertFalse(Character.isSpaceChar('\u445D'))
    assertFalse(Character.isSpaceChar('\u45C1'))
    assertFalse(Character.isSpaceChar('\u4B46'))
    assertFalse(Character.isSpaceChar('\u4FBF'))
    assertFalse(Character.isSpaceChar('\u54B6'))
    assertFalse(Character.isSpaceChar('\u5A2C'))
    assertFalse(Character.isSpaceChar('\u5A90'))
    assertFalse(Character.isSpaceChar('\u5D28'))
    assertFalse(Character.isSpaceChar('\u5D62'))
    assertFalse(Character.isSpaceChar('\u5ED9'))
    assertFalse(Character.isSpaceChar('\u5FA1'))
    assertFalse(Character.isSpaceChar('\u6242'))
    assertFalse(Character.isSpaceChar('\u6437'))
    assertFalse(Character.isSpaceChar('\u673D'))
    assertFalse(Character.isSpaceChar('\u6AC1'))
    assertFalse(Character.isSpaceChar('\u6CA1'))
    assertFalse(Character.isSpaceChar('\u6D31'))
    assertFalse(Character.isSpaceChar('\u6F7C'))
    assertFalse(Character.isSpaceChar('\u70B3'))
    assertFalse(Character.isSpaceChar('\u74F9'))
    assertFalse(Character.isSpaceChar('\u759E'))
    assertFalse(Character.isSpaceChar('\u76A0'))
    assertFalse(Character.isSpaceChar('\u7792'))
    assertFalse(Character.isSpaceChar('\u814C'))
    assertFalse(Character.isSpaceChar('\u8209'))
    assertFalse(Character.isSpaceChar('\u8451'))
    assertFalse(Character.isSpaceChar('\u86DE'))
    assertFalse(Character.isSpaceChar('\u8878'))
    assertFalse(Character.isSpaceChar('\u8BB5'))
    assertFalse(Character.isSpaceChar('\u8D35'))
    assertFalse(Character.isSpaceChar('\u903B'))
    assertFalse(Character.isSpaceChar('\u907C'))
    assertFalse(Character.isSpaceChar('\u9151'))
    assertFalse(Character.isSpaceChar('\u9188'))
    assertFalse(Character.isSpaceChar('\u9775'))
    assertFalse(Character.isSpaceChar('\u9896'))
    assertFalse(Character.isSpaceChar('\u9C86'))
    assertFalse(Character.isSpaceChar('\u9CB2'))
    assertFalse(Character.isSpaceChar('\uA026'))
    assertFalse(Character.isSpaceChar('\uA34B'))
    assertFalse(Character.isSpaceChar('\uA5B2'))
    assertFalse(Character.isSpaceChar('\uAD41'))
    assertFalse(Character.isSpaceChar('\uAD56'))
    assertFalse(Character.isSpaceChar('\uAE7B'))
    assertFalse(Character.isSpaceChar('\uAFB1'))
    assertFalse(Character.isSpaceChar('\uB1DE'))
    assertFalse(Character.isSpaceChar('\uBC4C'))
    assertFalse(Character.isSpaceChar('\uBE62'))
    assertFalse(Character.isSpaceChar('\uC041'))
    assertFalse(Character.isSpaceChar('\uC24F'))
    assertFalse(Character.isSpaceChar('\uC867'))
    assertFalse(Character.isSpaceChar('\uC8C1'))
    assertFalse(Character.isSpaceChar('\uCA16'))
    assertFalse(Character.isSpaceChar('\uCDDF'))
    assertFalse(Character.isSpaceChar('\uCF2C'))
    assertFalse(Character.isSpaceChar('\uD08D'))
    assertFalse(Character.isSpaceChar('\uD200'))
    assertFalse(Character.isSpaceChar('\uD599'))
    assertFalse(Character.isSpaceChar('\uD7F0'))
    assertFalse(Character.isSpaceChar('\uDBA1'))
    assertFalse(Character.isSpaceChar('\uDBF8'))
    assertFalse(Character.isSpaceChar('\uE066'))
    assertFalse(Character.isSpaceChar('\uE260'))
    assertFalse(Character.isSpaceChar('\uE776'))
    assertFalse(Character.isSpaceChar('\uEA9E'))
    assertFalse(Character.isSpaceChar('\uED5A'))
    assertFalse(Character.isSpaceChar('\uF016'))
    assertFalse(Character.isSpaceChar('\uF2E0'))
    assertFalse(Character.isSpaceChar('\uFA9E'))
    assertFalse(Character.isSpaceChar('\uFB90'))
    assertFalse(Character.isSpaceChar('\uFD88'))
  }

  @Test def isWhitespace(): Unit = {
    // All whitespaces
    assertTrue(Character.isWhitespace('\u0009'))
    assertTrue(Character.isWhitespace('\u000a'))
    assertTrue(Character.isWhitespace('\u000b'))
    assertTrue(Character.isWhitespace('\u000c'))
    assertTrue(Character.isWhitespace('\u000d'))
    assertTrue(Character.isWhitespace('\u001c'))
    assertTrue(Character.isWhitespace('\u001d'))
    assertTrue(Character.isWhitespace('\u001e'))
    assertTrue(Character.isWhitespace('\u001f'))
    assertTrue(Character.isWhitespace('\u0020'))
    assertTrue(Character.isWhitespace('\u1680'))
    assertTrue(Character.isWhitespace('\u2000'))
    assertTrue(Character.isWhitespace('\u2001'))
    assertTrue(Character.isWhitespace('\u2002'))
    assertTrue(Character.isWhitespace('\u2003'))
    assertTrue(Character.isWhitespace('\u2004'))
    assertTrue(Character.isWhitespace('\u2005'))
    assertTrue(Character.isWhitespace('\u2006'))
    assertTrue(Character.isWhitespace('\u2008'))
    assertTrue(Character.isWhitespace('\u2009'))
    assertTrue(Character.isWhitespace('\u200a'))
    assertTrue(Character.isWhitespace('\u2028'))
    assertTrue(Character.isWhitespace('\u2029'))
    assertTrue(Character.isWhitespace('\u205f'))
    assertTrue(Character.isWhitespace('\u3000'))

    assertFalse(Character.isWhitespace('\u200b'))

    // 100 randomly chosen characters that produce false
    assertFalse(Character.isWhitespace(2515))
    assertFalse(Character.isWhitespace(231))
    assertFalse(Character.isWhitespace(63339))
    assertFalse(Character.isWhitespace(60302))
    assertFalse(Character.isWhitespace(56219))
    assertFalse(Character.isWhitespace(9884))
    assertFalse(Character.isWhitespace(23278))
    assertFalse(Character.isWhitespace(16711))
    assertFalse(Character.isWhitespace(29993))
    assertFalse(Character.isWhitespace(29089))
    assertFalse(Character.isWhitespace(42825))
    assertFalse(Character.isWhitespace(57735))
    assertFalse(Character.isWhitespace(21132))
    assertFalse(Character.isWhitespace(40727))
    assertFalse(Character.isWhitespace(21416))
    assertFalse(Character.isWhitespace(8420))
    assertFalse(Character.isWhitespace(46449))
    assertFalse(Character.isWhitespace(56988))
    assertFalse(Character.isWhitespace(60900))
    assertFalse(Character.isWhitespace(6835))
    assertFalse(Character.isWhitespace(17138))
    assertFalse(Character.isWhitespace(50841))
    assertFalse(Character.isWhitespace(1933))
    assertFalse(Character.isWhitespace(32264))
    assertFalse(Character.isWhitespace(56802))
    assertFalse(Character.isWhitespace(10621))
    assertFalse(Character.isWhitespace(63150))
    assertFalse(Character.isWhitespace(60479))
    assertFalse(Character.isWhitespace(2806))
    assertFalse(Character.isWhitespace(56525))
    assertFalse(Character.isWhitespace(51781))
    assertFalse(Character.isWhitespace(7818))
    assertFalse(Character.isWhitespace(12656))
    assertFalse(Character.isWhitespace(47124))
    assertFalse(Character.isWhitespace(52352))
    assertFalse(Character.isWhitespace(54740))
    assertFalse(Character.isWhitespace(27760))
    assertFalse(Character.isWhitespace(57895))
    assertFalse(Character.isWhitespace(11017))
    assertFalse(Character.isWhitespace(28645))
    assertFalse(Character.isWhitespace(46716))
    assertFalse(Character.isWhitespace(6558))
    assertFalse(Character.isWhitespace(8801))
    assertFalse(Character.isWhitespace(16394))
    assertFalse(Character.isWhitespace(44981))
    assertFalse(Character.isWhitespace(29442))
    assertFalse(Character.isWhitespace(1051))
    assertFalse(Character.isWhitespace(21028))
    assertFalse(Character.isWhitespace(21429))
    assertFalse(Character.isWhitespace(24582))
    assertFalse(Character.isWhitespace(3156))
    assertFalse(Character.isWhitespace(391299))
    assertFalse(Character.isWhitespace(555557))
    assertFalse(Character.isWhitespace(104630))
    assertFalse(Character.isWhitespace(311698))
    assertFalse(Character.isWhitespace(775850))
    assertFalse(Character.isWhitespace(472119))
    assertFalse(Character.isWhitespace(927519))
    assertFalse(Character.isWhitespace(345120))
    assertFalse(Character.isWhitespace(372337))
    assertFalse(Character.isWhitespace(415654))
    assertFalse(Character.isWhitespace(1003356))
    assertFalse(Character.isWhitespace(663703))
    assertFalse(Character.isWhitespace(675812))
    assertFalse(Character.isWhitespace(982076))
    assertFalse(Character.isWhitespace(727003))
    assertFalse(Character.isWhitespace(328478))
    assertFalse(Character.isWhitespace(386525))
    assertFalse(Character.isWhitespace(427552))
    assertFalse(Character.isWhitespace(1018963))
    assertFalse(Character.isWhitespace(594357))
    assertFalse(Character.isWhitespace(514150))
    assertFalse(Character.isWhitespace(112164))
    assertFalse(Character.isWhitespace(241410))
    assertFalse(Character.isWhitespace(961291))
    assertFalse(Character.isWhitespace(1098178))
    assertFalse(Character.isWhitespace(911919))
    assertFalse(Character.isWhitespace(867492))
    assertFalse(Character.isWhitespace(233623))
    assertFalse(Character.isWhitespace(641480))
    assertFalse(Character.isWhitespace(536446))
    assertFalse(Character.isWhitespace(557516))
    assertFalse(Character.isWhitespace(511075))
    assertFalse(Character.isWhitespace(195888))
    assertFalse(Character.isWhitespace(808876))
    assertFalse(Character.isWhitespace(841221))
    assertFalse(Character.isWhitespace(90617))
    assertFalse(Character.isWhitespace(872412))
    assertFalse(Character.isWhitespace(307770))
    assertFalse(Character.isWhitespace(307998))
    assertFalse(Character.isWhitespace(373461))
    assertFalse(Character.isWhitespace(149861))
    assertFalse(Character.isWhitespace(747431))
    assertFalse(Character.isWhitespace(992898))
    assertFalse(Character.isWhitespace(1083539))
    assertFalse(Character.isWhitespace(112853))
    assertFalse(Character.isWhitespace(897133))
    assertFalse(Character.isWhitespace(129841))
    assertFalse(Character.isWhitespace(801648))
    assertFalse(Character.isWhitespace(718253))
    assertFalse(Character.isWhitespace(934430))
    assertFalse(Character.isWhitespace(942329))
  }

  @Test def isTitleCase(): Unit = {
    // all tilteCases
    assertTrue(Character.isTitleCase('\u01C5'))
    assertTrue(Character.isTitleCase('\u01C8'))
    assertTrue(Character.isTitleCase('\u01CB'))
    assertTrue(Character.isTitleCase('\u01F2'))
    assertTrue(Character.isTitleCase('\u1F88'))
    assertTrue(Character.isTitleCase('\u1F89'))
    assertTrue(Character.isTitleCase('\u1F8A'))
    assertTrue(Character.isTitleCase('\u1F8B'))
    assertTrue(Character.isTitleCase('\u1F8C'))
    assertTrue(Character.isTitleCase('\u1F8D'))
    assertTrue(Character.isTitleCase('\u1F8E'))
    assertTrue(Character.isTitleCase('\u1F8F'))
    assertTrue(Character.isTitleCase('\u1F98'))
    assertTrue(Character.isTitleCase('\u1F99'))
    assertTrue(Character.isTitleCase('\u1F9A'))
    assertTrue(Character.isTitleCase('\u1F9B'))
    assertTrue(Character.isTitleCase('\u1F9C'))
    assertTrue(Character.isTitleCase('\u1F9D'))
    assertTrue(Character.isTitleCase('\u1F9E'))
    assertTrue(Character.isTitleCase('\u1F9F'))
    assertTrue(Character.isTitleCase('\u1FA8'))
    assertTrue(Character.isTitleCase('\u1FA9'))
    assertTrue(Character.isTitleCase('\u1FAA'))
    assertTrue(Character.isTitleCase('\u1FAB'))
    assertTrue(Character.isTitleCase('\u1FAC'))
    assertTrue(Character.isTitleCase('\u1FAD'))
    assertTrue(Character.isTitleCase('\u1FAE'))
    assertTrue(Character.isTitleCase('\u1FAF'))
    assertTrue(Character.isTitleCase('\u1FBC'))
    assertTrue(Character.isTitleCase('\u1FCC'))
    assertTrue(Character.isTitleCase('\u1FFC'))

    // 100 randomly chosen characters that produce false
    assertFalse(Character.isTitleCase('\u043A'))
    assertFalse(Character.isTitleCase('\u09F9'))
    assertFalse(Character.isTitleCase('\u0BD0'))
    assertFalse(Character.isTitleCase('\u0DF3'))
    assertFalse(Character.isTitleCase('\u1290'))
    assertFalse(Character.isTitleCase('\u1A35'))
    assertFalse(Character.isTitleCase('\u1B86'))
    assertFalse(Character.isTitleCase('\u1DB3'))
    assertFalse(Character.isTitleCase('\u228B'))
    assertFalse(Character.isTitleCase('\u22D1'))
    assertFalse(Character.isTitleCase('\u231A'))
    assertFalse(Character.isTitleCase('\u2A69'))
    assertFalse(Character.isTitleCase('\u2AD3'))
    assertFalse(Character.isTitleCase('\u2B75'))
    assertFalse(Character.isTitleCase('\u2FDC'))
    assertFalse(Character.isTitleCase('\u323F'))
    assertFalse(Character.isTitleCase('\u3354'))
    assertFalse(Character.isTitleCase('\u35AA'))
    assertFalse(Character.isTitleCase('\u3E32'))
    assertFalse(Character.isTitleCase('\u3EF6'))
    assertFalse(Character.isTitleCase('\u4251'))
    assertFalse(Character.isTitleCase('\u4860'))
    assertFalse(Character.isTitleCase('\u4CF7'))
    assertFalse(Character.isTitleCase('\u50C7'))
    assertFalse(Character.isTitleCase('\u531E'))
    assertFalse(Character.isTitleCase('\u54E9'))
    assertFalse(Character.isTitleCase('\u57F4'))
    assertFalse(Character.isTitleCase('\u5870'))
    assertFalse(Character.isTitleCase('\u58BF'))
    assertFalse(Character.isTitleCase('\u599B'))
    assertFalse(Character.isTitleCase('\u5D43'))
    assertFalse(Character.isTitleCase('\u62DC'))
    assertFalse(Character.isTitleCase('\u6BCA'))
    assertFalse(Character.isTitleCase('\u6DB8'))
    assertFalse(Character.isTitleCase('\u711E'))
    assertFalse(Character.isTitleCase('\u75E4'))
    assertFalse(Character.isTitleCase('\u7926'))
    assertFalse(Character.isTitleCase('\u7BBD'))
    assertFalse(Character.isTitleCase('\u7C46'))
    assertFalse(Character.isTitleCase('\u7CA5'))
    assertFalse(Character.isTitleCase('\u7E88'))
    assertFalse(Character.isTitleCase('\u7FA9'))
    assertFalse(Character.isTitleCase('\u817A'))
    assertFalse(Character.isTitleCase('\u82BD'))
    assertFalse(Character.isTitleCase('\u82F0'))
    assertFalse(Character.isTitleCase('\u8435'))
    assertFalse(Character.isTitleCase('\u85A5'))
    assertFalse(Character.isTitleCase('\u9080'))
    assertFalse(Character.isTitleCase('\u93BF'))
    assertFalse(Character.isTitleCase('\u9A88'))
    assertFalse(Character.isTitleCase('\u9AF9'))
    assertFalse(Character.isTitleCase('\uA18C'))
    assertFalse(Character.isTitleCase('\uA5C4'))
    assertFalse(Character.isTitleCase('\uA91B'))
    assertFalse(Character.isTitleCase('\uAD45'))
    assertFalse(Character.isTitleCase('\uAE88'))
    assertFalse(Character.isTitleCase('\uB06F'))
    assertFalse(Character.isTitleCase('\uB2E0'))
    assertFalse(Character.isTitleCase('\uB427'))
    assertFalse(Character.isTitleCase('\uB512'))
    assertFalse(Character.isTitleCase('\uB59C'))
    assertFalse(Character.isTitleCase('\uB79B'))
    assertFalse(Character.isTitleCase('\uB911'))
    assertFalse(Character.isTitleCase('\uB995'))
    assertFalse(Character.isTitleCase('\uBA25'))
    assertFalse(Character.isTitleCase('\uBFED'))
    assertFalse(Character.isTitleCase('\uC0E1'))
    assertFalse(Character.isTitleCase('\uC5CC'))
    assertFalse(Character.isTitleCase('\uC80B'))
    assertFalse(Character.isTitleCase('\uC823'))
    assertFalse(Character.isTitleCase('\uCAE2'))
    assertFalse(Character.isTitleCase('\uCC2E'))
    assertFalse(Character.isTitleCase('\uCD96'))
    assertFalse(Character.isTitleCase('\uD025'))
    assertFalse(Character.isTitleCase('\uD270'))
    assertFalse(Character.isTitleCase('\uD3B2'))
    assertFalse(Character.isTitleCase('\uD679'))
    assertFalse(Character.isTitleCase('\uD822'))
    assertFalse(Character.isTitleCase('\uDA58'))
    assertFalse(Character.isTitleCase('\uE1FE'))
    assertFalse(Character.isTitleCase('\uE247'))
    assertFalse(Character.isTitleCase('\uE41E'))
    assertFalse(Character.isTitleCase('\uE518'))
    assertFalse(Character.isTitleCase('\uE6F1'))
    assertFalse(Character.isTitleCase('\uE7E8'))
    assertFalse(Character.isTitleCase('\uE89F'))
    assertFalse(Character.isTitleCase('\uEA41'))
    assertFalse(Character.isTitleCase('\uEA6D'))
    assertFalse(Character.isTitleCase('\uEAB6'))
    assertFalse(Character.isTitleCase('\uEBD4'))
    assertFalse(Character.isTitleCase('\uEF15'))
    assertFalse(Character.isTitleCase('\uF506'))
    assertFalse(Character.isTitleCase('\uF519'))
    assertFalse(Character.isTitleCase('\uF53F'))
    assertFalse(Character.isTitleCase('\uF5EC'))
    assertFalse(Character.isTitleCase('\uF62F'))
    assertFalse(Character.isTitleCase('\uFAD4'))
    assertFalse(Character.isTitleCase('\uFDBC'))
    assertFalse(Character.isTitleCase('\u0029'))
    assertFalse(Character.isTitleCase('\u0070'))
  }

  @Test def isMirrored(): Unit = {
    // 50 randomly chosen characters that produce true
    assertTrue(Character.isMirrored('\u208D'))
    assertTrue(Character.isMirrored('\u208E'))
    assertTrue(Character.isMirrored('\u2140'))
    assertTrue(Character.isMirrored('\u2201'))
    assertTrue(Character.isMirrored('\u2204'))
    assertTrue(Character.isMirrored('\u221A'))
    assertTrue(Character.isMirrored('\u221B'))
    assertTrue(Character.isMirrored('\u221D'))
    assertTrue(Character.isMirrored('\u222D'))
    assertTrue(Character.isMirrored('\u2232'))
    assertTrue(Character.isMirrored('\u2252'))
    assertTrue(Character.isMirrored('\u2254'))
    assertTrue(Character.isMirrored('\u226A'))
    assertTrue(Character.isMirrored('\u2271'))
    assertTrue(Character.isMirrored('\u227A'))
    assertTrue(Character.isMirrored('\u2281'))
    assertTrue(Character.isMirrored('\u2284'))
    assertTrue(Character.isMirrored('\u2286'))
    assertTrue(Character.isMirrored('\u22A2'))
    assertTrue(Character.isMirrored('\u22D0'))
    assertTrue(Character.isMirrored('\u22DA'))
    assertTrue(Character.isMirrored('\u22F1'))
    assertTrue(Character.isMirrored('\u22F2'))
    assertTrue(Character.isMirrored('\u22F9'))
    assertTrue(Character.isMirrored('\u27C3'))
    assertTrue(Character.isMirrored('\u27CD'))
    assertTrue(Character.isMirrored('\u27EA'))
    assertTrue(Character.isMirrored('\u2988'))
    assertTrue(Character.isMirrored('\u298D'))
    assertTrue(Character.isMirrored('\u299C'))
    assertTrue(Character.isMirrored('\u29A5'))
    assertTrue(Character.isMirrored('\u29D1'))
    assertTrue(Character.isMirrored('\u29D9'))
    assertTrue(Character.isMirrored('\u29E4'))
    assertTrue(Character.isMirrored('\u29F5'))
    assertTrue(Character.isMirrored('\u2A1C'))
    assertTrue(Character.isMirrored('\u2A89'))
    assertTrue(Character.isMirrored('\u2A94'))
    assertTrue(Character.isMirrored('\u2AAB'))
    assertTrue(Character.isMirrored('\u2ABE'))
    assertTrue(Character.isMirrored('\u2AF9'))
    assertTrue(Character.isMirrored('\u2AFD'))
    assertTrue(Character.isMirrored('\u2E02'))
    assertTrue(Character.isMirrored('\u2E23'))
    assertTrue(Character.isMirrored('\u2E24'))
    assertTrue(Character.isMirrored('\uFE5C'))
    assertTrue(Character.isMirrored('\uFE64'))

    // 50 randomly chosen characters that produce false
    assertFalse(Character.isMirrored('\u0688'))
    assertFalse(Character.isMirrored('\u9319'))
    assertFalse(Character.isMirrored('\uFAF5'))
    assertFalse(Character.isMirrored(1030807))
    assertFalse(Character.isMirrored(1032997))
    assertFalse(Character.isMirrored(1073298))
    assertFalse(Character.isMirrored(1087805))
    assertFalse(Character.isMirrored(1090056))
    assertFalse(Character.isMirrored(1106453))
    assertFalse(Character.isMirrored(112391))
    assertFalse(Character.isMirrored(114041))
    assertFalse(Character.isMirrored(135408))
    assertFalse(Character.isMirrored(139769))
    assertFalse(Character.isMirrored(141057))
    assertFalse(Character.isMirrored(141146))
    assertFalse(Character.isMirrored(214605))
    assertFalse(Character.isMirrored(245103))
    assertFalse(Character.isMirrored(245550))
    assertFalse(Character.isMirrored(265448))
    assertFalse(Character.isMirrored(289372))
    assertFalse(Character.isMirrored(290850))
    assertFalse(Character.isMirrored(314422))
    assertFalse(Character.isMirrored(360533))
    assertFalse(Character.isMirrored(361613))
    assertFalse(Character.isMirrored(399166))
    assertFalse(Character.isMirrored(418562))
    assertFalse(Character.isMirrored(441100))
    assertFalse(Character.isMirrored(466467))
    assertFalse(Character.isMirrored(478964))
    assertFalse(Character.isMirrored(528293))
    assertFalse(Character.isMirrored(533681))
    assertFalse(Character.isMirrored(557036))
    assertFalse(Character.isMirrored(599773))
    assertFalse(Character.isMirrored(666210))
    assertFalse(Character.isMirrored(67175))
    assertFalse(Character.isMirrored(673481))
    assertFalse(Character.isMirrored(714304))
    assertFalse(Character.isMirrored(725335))
    assertFalse(Character.isMirrored(746217))
    assertFalse(Character.isMirrored(74649))
    assertFalse(Character.isMirrored(777837))
    assertFalse(Character.isMirrored(808419))
    assertFalse(Character.isMirrored(829990))
    assertFalse(Character.isMirrored(83437))
    assertFalse(Character.isMirrored(907564))
    assertFalse(Character.isMirrored(918918))
    assertFalse(Character.isMirrored(921306))
    assertFalse(Character.isMirrored(925902))
    assertFalse(Character.isMirrored(927621))
    assertFalse(Character.isMirrored(962242))
  }

  @Test def isDefined(): Unit = {
    // 100 randomly chosen characters that produce true
    assertTrue(Character.isDefined('\u00BB'))
    assertTrue(Character.isDefined('\u0191'))
    assertTrue(Character.isDefined('\u03D6'))
    assertTrue(Character.isDefined('\u04A1'))
    assertTrue(Character.isDefined('\u04BD'))
    assertTrue(Character.isDefined('\u07AC'))
    assertTrue(Character.isDefined('\u0BED'))
    assertTrue(Character.isDefined('\u0D21'))
    assertTrue(Character.isDefined('\u0D6B'))
    assertTrue(Character.isDefined('\u106D'))
    assertTrue(Character.isDefined('\u1982'))
    assertTrue(Character.isDefined('\u1A46'))
    assertTrue(Character.isDefined('\u1CD5'))
    assertTrue(Character.isDefined('\u1E87'))
    assertTrue(Character.isDefined('\u206C'))
    assertTrue(Character.isDefined('\u20BC'))
    assertTrue(Character.isDefined('\u20BD'))
    assertTrue(Character.isDefined('\u20BE'))
    assertTrue(Character.isDefined('\u20BF'))
    assertTrue(Character.isDefined('\u22F6'))
    assertTrue(Character.isDefined('\u2325'))
    assertTrue(Character.isDefined('\u277C'))
    assertTrue(Character.isDefined('\u2E0D'))
    assertTrue(Character.isDefined('\u2E27'))
    assertTrue(Character.isDefined('\u2FA4'))
    assertTrue(Character.isDefined('\u3163'))
    assertTrue(Character.isDefined('\u32D7'))
    assertTrue(Character.isDefined('\u32FF'))
    assertTrue(Character.isDefined('\u3384'))
    assertTrue(Character.isDefined('\u33D3'))
    assertTrue(Character.isDefined('\u35D9'))
    assertTrue(Character.isDefined('\u36F2'))
    assertTrue(Character.isDefined('\u385B'))
    assertTrue(Character.isDefined('\u398D'))
    assertTrue(Character.isDefined('\u3A81'))
    assertTrue(Character.isDefined('\u3D57'))
    assertTrue(Character.isDefined('\u3DD4'))
    assertTrue(Character.isDefined('\u415C'))
    assertTrue(Character.isDefined('\u4233'))
    assertTrue(Character.isDefined('\u4A87'))
    assertTrue(Character.isDefined('\u50C7'))
    assertTrue(Character.isDefined('\u51A2'))
    assertTrue(Character.isDefined('\u525B'))
    assertTrue(Character.isDefined('\u62D0'))
    assertTrue(Character.isDefined('\u6405'))
    assertTrue(Character.isDefined('\u67D2'))
    assertTrue(Character.isDefined('\u6ED4'))
    assertTrue(Character.isDefined('\u6F13'))
    assertTrue(Character.isDefined('\u7366'))
    assertTrue(Character.isDefined('\u7661'))
    assertTrue(Character.isDefined('\u76FB'))
    assertTrue(Character.isDefined('\u77D7'))
    assertTrue(Character.isDefined('\u7A26'))
    assertTrue(Character.isDefined('\u7E41'))
    assertTrue(Character.isDefined('\u7E9E'))
    assertTrue(Character.isDefined('\u7F5E'))
    assertTrue(Character.isDefined('\u8336'))
    assertTrue(Character.isDefined('\u842D'))
    assertTrue(Character.isDefined('\u8488'))
    assertTrue(Character.isDefined('\u88C9'))
    assertTrue(Character.isDefined('\u8A81'))
    assertTrue(Character.isDefined('\u8D21'))
    assertTrue(Character.isDefined('\u8EA0'))
    assertTrue(Character.isDefined('\u921B'))
    assertTrue(Character.isDefined('\u94EC'))
    assertTrue(Character.isDefined('\u972C'))
    assertTrue(Character.isDefined('\u98D4'))
    assertTrue(Character.isDefined('\u993D'))
    assertTrue(Character.isDefined('\u9B13'))
    assertTrue(Character.isDefined('\u9B6F'))
    assertTrue(Character.isDefined('\uA278'))
    assertTrue(Character.isDefined('\uA2F9'))
    assertTrue(Character.isDefined('\uA618'))
    assertTrue(Character.isDefined('\uA789'))
    assertTrue(Character.isDefined('\uAA1C'))
    assertTrue(Character.isDefined('\uAEF8'))
    assertTrue(Character.isDefined('\uAF74'))
    assertTrue(Character.isDefined('\uB170'))
    assertTrue(Character.isDefined('\uB508'))
    assertTrue(Character.isDefined('\uB820'))
    assertTrue(Character.isDefined('\uB8E3'))
    assertTrue(Character.isDefined('\uBC34'))
    assertTrue(Character.isDefined('\uC501'))
    assertTrue(Character.isDefined('\uCABF'))
    assertTrue(Character.isDefined('\uCC21'))
    assertTrue(Character.isDefined('\uCC27'))
    assertTrue(Character.isDefined('\uCC46'))
    assertTrue(Character.isDefined('\uCC7F'))
    assertTrue(Character.isDefined('\uCD32'))
    assertTrue(Character.isDefined('\uCFE0'))
    assertTrue(Character.isDefined('\uD08F'))
    assertTrue(Character.isDefined('\uD4AF'))
    assertTrue(Character.isDefined('\uD522'))
    assertTrue(Character.isDefined('\uD9F4'))
    assertTrue(Character.isDefined('\uE5A7'))
    assertTrue(Character.isDefined('\uEB96'))
    assertTrue(Character.isDefined('\uEEC0'))
    assertTrue(Character.isDefined('\uEF30'))
    assertTrue(Character.isDefined('\uEFD9'))
    assertTrue(Character.isDefined('\uF4E8'))
    assertTrue(Character.isDefined('\uF547'))
    assertTrue(Character.isDefined('\uFA7D'))
    assertTrue(Character.isDefined('\uFD39'))
    assertTrue(Character.isDefined('\uFF3A'))

    /* 100 randomly chosen characters that produce false, generated on JDK 11,
     * minus those that became true later.
     */
    assertFalse(Character.isDefined('\u13FE'))
    assertFalse(Character.isDefined('\u0AF7'))
    assertFalse(Character.isDefined('\u0DFB'))
    assertFalse(Character.isDefined('\u1FDC'))
    assertFalse(Character.isDefined('\u243A'))
    assertFalse(Character.isDefined('\u0EEB'))
    assertFalse(Character.isDefined('\u09D6'))
    assertFalse(Character.isDefined('\u2D7C'))
    assertFalse(Character.isDefined('\u2DC7'))
    assertFalse(Character.isDefined('\u0A3A'))
    assertFalse(Character.isDefined('\u0E3D'))
    assertFalse(Character.isDefined('\u1775'))
    assertFalse(Character.isDefined('\u0A54'))
    assertFalse(Character.isDefined('\u175F'))
    assertFalse(Character.isDefined('\uFDC8'))
    assertFalse(Character.isDefined('\u1C3A'))
    assertFalse(Character.isDefined('\u0E00'))
    assertFalse(Character.isDefined('\u20F9'))
    assertFalse(Character.isDefined('\u2D2E'))
    assertFalse(Character.isDefined('\u2E71'))
    assertFalse(Character.isDefined('\uFBD0'))
    assertFalse(Character.isDefined('\u0B5A'))
    assertFalse(Character.isDefined('\uA7E5'))
    assertFalse(Character.isDefined('\u0BD1'))
    assertFalse(Character.isDefined('\u0E3D'))
    assertFalse(Character.isDefined('\u0BBC'))
    assertFalse(Character.isDefined('\u0CDB'))
    assertFalse(Character.isDefined('\u20F3'))
    assertFalse(Character.isDefined('\u05F6'))
    assertFalse(Character.isDefined('\u0D99'))
    assertFalse(Character.isDefined('\u242D'))
    assertFalse(Character.isDefined('\u3130'))
    assertFalse(Character.isDefined('\u1AF5'))
    assertFalse(Character.isDefined('\u0557'))
    assertFalse(Character.isDefined('\uAA5A'))
    assertFalse(Character.isDefined('\u1AE7'))
    assertFalse(Character.isDefined('\u1AEB'))
    assertFalse(Character.isDefined('\u2D9B'))
    assertFalse(Character.isDefined('\u07BF'))
    assertFalse(Character.isDefined('\uA8CD'))
    assertFalse(Character.isDefined('\u09D8'))
    assertFalse(Character.isDefined('\u0378'))
    assertFalse(Character.isDefined('\u135C'))
    assertFalse(Character.isDefined('\u20CC'))
    assertFalse(Character.isDefined('\u09BA'))
    assertFalse(Character.isDefined('\u171C'))
    assertFalse(Character.isDefined('\uFFD9'))
    assertFalse(Character.isDefined('\u243E'))
    assertFalse(Character.isDefined('\u0FF8'))
    assertFalse(Character.isDefined('\u07FC'))
    assertFalse(Character.isDefined('\u16FF'))
    assertFalse(Character.isDefined('\u1CFB'))
    assertFalse(Character.isDefined('\u1311'))
    assertFalse(Character.isDefined('\uA7ED'))
    assertFalse(Character.isDefined('\u0B78'))
    assertFalse(Character.isDefined('\u13FF'))
    assertFalse(Character.isDefined('\u1AD4'))
    assertFalse(Character.isDefined('\uAAF8'))
    assertFalse(Character.isDefined('\u1978'))
    assertFalse(Character.isDefined('\u17EA'))
    assertFalse(Character.isDefined('\u0DCC'))
    assertFalse(Character.isDefined('\uA639'))
    assertFalse(Character.isDefined('\uFAF9'))
    assertFalse(Character.isDefined('\u20FB'))
    assertFalse(Character.isDefined('\u2458'))
    assertFalse(Character.isDefined('\u1758'))
    assertFalse(Character.isDefined('\uA63A'))
    assertFalse(Character.isDefined('\u0EEA'))
    assertFalse(Character.isDefined('\uFDD3'))
    assertFalse(Character.isDefined('\u0E3B'))
    assertFalse(Character.isDefined('\uFB45'))
    assertFalse(Character.isDefined('\u1AEF'))
    assertFalse(Character.isDefined('\uA7E7'))
    assertFalse(Character.isDefined('\uFFD8'))
    assertFalse(Character.isDefined('\u187C'))
    assertFalse(Character.isDefined('\u1C8F'))
    assertFalse(Character.isDefined('\u038B'))
    assertFalse(Character.isDefined('\u0B49'))
    assertFalse(Character.isDefined('\u0E5F'))
    assertFalse(Character.isDefined('\u16FF'))
    assertFalse(Character.isDefined('\u0A44'))
    assertFalse(Character.isDefined('\u0C5B'))
    assertFalse(Character.isDefined('\u2DBF'))
    assertFalse(Character.isDefined('\u128F'))
  }

  @Test def getType(): Unit = {
    // 300 first characters
    assertEquals(15, Character.getType(0))
    assertEquals(15, Character.getType(1))
    assertEquals(15, Character.getType(2))
    assertEquals(15, Character.getType(3))
    assertEquals(15, Character.getType(4))
    assertEquals(15, Character.getType(5))
    assertEquals(15, Character.getType(6))
    assertEquals(15, Character.getType(7))
    assertEquals(15, Character.getType(8))
    assertEquals(15, Character.getType(9))
    assertEquals(15, Character.getType(10))
    assertEquals(15, Character.getType(11))
    assertEquals(15, Character.getType(12))
    assertEquals(15, Character.getType(13))
    assertEquals(15, Character.getType(14))
    assertEquals(15, Character.getType(15))
    assertEquals(15, Character.getType(16))
    assertEquals(15, Character.getType(17))
    assertEquals(15, Character.getType(18))
    assertEquals(15, Character.getType(19))
    assertEquals(15, Character.getType(20))
    assertEquals(15, Character.getType(21))
    assertEquals(15, Character.getType(22))
    assertEquals(15, Character.getType(23))
    assertEquals(15, Character.getType(24))
    assertEquals(15, Character.getType(25))
    assertEquals(15, Character.getType(26))
    assertEquals(15, Character.getType(27))
    assertEquals(15, Character.getType(28))
    assertEquals(15, Character.getType(29))
    assertEquals(15, Character.getType(30))
    assertEquals(15, Character.getType(31))
    assertEquals(12, Character.getType(32))
    assertEquals(24, Character.getType(33))
    assertEquals(24, Character.getType(34))
    assertEquals(24, Character.getType(35))
    assertEquals(26, Character.getType(36))
    assertEquals(24, Character.getType(37))
    assertEquals(24, Character.getType(38))
    assertEquals(24, Character.getType(39))
    assertEquals(21, Character.getType(40))
    assertEquals(22, Character.getType(41))
    assertEquals(24, Character.getType(42))
    assertEquals(25, Character.getType(43))
    assertEquals(24, Character.getType(44))
    assertEquals(20, Character.getType(45))
    assertEquals(24, Character.getType(46))
    assertEquals(24, Character.getType(47))
    assertEquals(9, Character.getType(48))
    assertEquals(9, Character.getType(49))
    assertEquals(9, Character.getType(50))
    assertEquals(9, Character.getType(51))
    assertEquals(9, Character.getType(52))
    assertEquals(9, Character.getType(53))
    assertEquals(9, Character.getType(54))
    assertEquals(9, Character.getType(55))
    assertEquals(9, Character.getType(56))
    assertEquals(9, Character.getType(57))
    assertEquals(24, Character.getType(58))
    assertEquals(24, Character.getType(59))
    assertEquals(25, Character.getType(60))
    assertEquals(25, Character.getType(61))
    assertEquals(25, Character.getType(62))
    assertEquals(24, Character.getType(63))
    assertEquals(24, Character.getType(64))
    assertEquals(1, Character.getType(65))
    assertEquals(1, Character.getType(66))
    assertEquals(1, Character.getType(67))
    assertEquals(1, Character.getType(68))
    assertEquals(1, Character.getType(69))
    assertEquals(1, Character.getType(70))
    assertEquals(1, Character.getType(71))
    assertEquals(1, Character.getType(72))
    assertEquals(1, Character.getType(73))
    assertEquals(1, Character.getType(74))
    assertEquals(1, Character.getType(75))
    assertEquals(1, Character.getType(76))
    assertEquals(1, Character.getType(77))
    assertEquals(1, Character.getType(78))
    assertEquals(1, Character.getType(79))
    assertEquals(1, Character.getType(80))
    assertEquals(1, Character.getType(81))
    assertEquals(1, Character.getType(82))
    assertEquals(1, Character.getType(83))
    assertEquals(1, Character.getType(84))
    assertEquals(1, Character.getType(85))
    assertEquals(1, Character.getType(86))
    assertEquals(1, Character.getType(87))
    assertEquals(1, Character.getType(88))
    assertEquals(1, Character.getType(89))
    assertEquals(1, Character.getType(90))
    assertEquals(21, Character.getType(91))
    assertEquals(24, Character.getType(92))
    assertEquals(22, Character.getType(93))
    assertEquals(27, Character.getType(94))
    assertEquals(23, Character.getType(95))
    assertEquals(27, Character.getType(96))
    assertEquals(2, Character.getType(97))
    assertEquals(2, Character.getType(98))
    assertEquals(2, Character.getType(99))
    assertEquals(2, Character.getType(100))
    assertEquals(2, Character.getType(101))
    assertEquals(2, Character.getType(102))
    assertEquals(2, Character.getType(103))
    assertEquals(2, Character.getType(104))
    assertEquals(2, Character.getType(105))
    assertEquals(2, Character.getType(106))
    assertEquals(2, Character.getType(107))
    assertEquals(2, Character.getType(108))
    assertEquals(2, Character.getType(109))
    assertEquals(2, Character.getType(110))
    assertEquals(2, Character.getType(111))
    assertEquals(2, Character.getType(112))
    assertEquals(2, Character.getType(113))
    assertEquals(2, Character.getType(114))
    assertEquals(2, Character.getType(115))
    assertEquals(2, Character.getType(116))
    assertEquals(2, Character.getType(117))
    assertEquals(2, Character.getType(118))
    assertEquals(2, Character.getType(119))
    assertEquals(2, Character.getType(120))
    assertEquals(2, Character.getType(121))
    assertEquals(2, Character.getType(122))
    assertEquals(21, Character.getType(123))
    assertEquals(25, Character.getType(124))
    assertEquals(22, Character.getType(125))
    assertEquals(25, Character.getType(126))
    assertEquals(15, Character.getType(127))
    assertEquals(15, Character.getType(128))
    assertEquals(15, Character.getType(129))
    assertEquals(15, Character.getType(130))
    assertEquals(15, Character.getType(131))
    assertEquals(15, Character.getType(132))
    assertEquals(15, Character.getType(133))
    assertEquals(15, Character.getType(134))
    assertEquals(15, Character.getType(135))
    assertEquals(15, Character.getType(136))
    assertEquals(15, Character.getType(137))
    assertEquals(15, Character.getType(138))
    assertEquals(15, Character.getType(139))
    assertEquals(15, Character.getType(140))
    assertEquals(15, Character.getType(141))
    assertEquals(15, Character.getType(142))
    assertEquals(15, Character.getType(143))
    assertEquals(15, Character.getType(144))
    assertEquals(15, Character.getType(145))
    assertEquals(15, Character.getType(146))
    assertEquals(15, Character.getType(147))
    assertEquals(15, Character.getType(148))
    assertEquals(15, Character.getType(149))
    assertEquals(15, Character.getType(150))
    assertEquals(15, Character.getType(151))
    assertEquals(15, Character.getType(152))
    assertEquals(15, Character.getType(153))
    assertEquals(15, Character.getType(154))
    assertEquals(15, Character.getType(155))
    assertEquals(15, Character.getType(156))
    assertEquals(15, Character.getType(157))
    assertEquals(15, Character.getType(158))
    assertEquals(15, Character.getType(159))
    assertEquals(12, Character.getType(160))
    assertEquals(24, Character.getType(161))
    assertEquals(26, Character.getType(162))
    assertEquals(26, Character.getType(163))
    assertEquals(26, Character.getType(164))
    assertEquals(26, Character.getType(165))
    assertEquals(28, Character.getType(166))
    assertEquals(24, Character.getType(167))
    assertEquals(27, Character.getType(168))
    assertEquals(28, Character.getType(169))
    assertEquals(5, Character.getType(170))
    assertEquals(29, Character.getType(171))
    assertEquals(25, Character.getType(172))
    assertEquals(16, Character.getType(173))
    assertEquals(28, Character.getType(174))
    assertEquals(27, Character.getType(175))
    assertEquals(28, Character.getType(176))
    assertEquals(25, Character.getType(177))
    assertEquals(11, Character.getType(178))
    assertEquals(11, Character.getType(179))
    assertEquals(27, Character.getType(180))
    assertEquals(2, Character.getType(181))
    assertEquals(24, Character.getType(182))
    assertEquals(24, Character.getType(183))
    assertEquals(27, Character.getType(184))
    assertEquals(11, Character.getType(185))
    assertEquals(5, Character.getType(186))
    assertEquals(30, Character.getType(187))
    assertEquals(11, Character.getType(188))
    assertEquals(11, Character.getType(189))
    assertEquals(11, Character.getType(190))
    assertEquals(24, Character.getType(191))
    assertEquals(1, Character.getType(192))
    assertEquals(1, Character.getType(193))
    assertEquals(1, Character.getType(194))
    assertEquals(1, Character.getType(195))
    assertEquals(1, Character.getType(196))
    assertEquals(1, Character.getType(197))
    assertEquals(1, Character.getType(198))
    assertEquals(1, Character.getType(199))
    assertEquals(1, Character.getType(200))
    assertEquals(1, Character.getType(201))
    assertEquals(1, Character.getType(202))
    assertEquals(1, Character.getType(203))
    assertEquals(1, Character.getType(204))
    assertEquals(1, Character.getType(205))
    assertEquals(1, Character.getType(206))
    assertEquals(1, Character.getType(207))
    assertEquals(1, Character.getType(208))
    assertEquals(1, Character.getType(209))
    assertEquals(1, Character.getType(210))
    assertEquals(1, Character.getType(211))
    assertEquals(1, Character.getType(212))
    assertEquals(1, Character.getType(213))
    assertEquals(1, Character.getType(214))
    assertEquals(25, Character.getType(215))
    assertEquals(1, Character.getType(216))
    assertEquals(1, Character.getType(217))
    assertEquals(1, Character.getType(218))
    assertEquals(1, Character.getType(219))
    assertEquals(1, Character.getType(220))
    assertEquals(1, Character.getType(221))
    assertEquals(1, Character.getType(222))
    assertEquals(2, Character.getType(223))
    assertEquals(2, Character.getType(224))
    assertEquals(2, Character.getType(225))
    assertEquals(2, Character.getType(226))
    assertEquals(2, Character.getType(227))
    assertEquals(2, Character.getType(228))
    assertEquals(2, Character.getType(229))
    assertEquals(2, Character.getType(230))
    assertEquals(2, Character.getType(231))
    assertEquals(2, Character.getType(232))
    assertEquals(2, Character.getType(233))
    assertEquals(2, Character.getType(234))
    assertEquals(2, Character.getType(235))
    assertEquals(2, Character.getType(236))
    assertEquals(2, Character.getType(237))
    assertEquals(2, Character.getType(238))
    assertEquals(2, Character.getType(239))
    assertEquals(2, Character.getType(240))
    assertEquals(2, Character.getType(241))
    assertEquals(2, Character.getType(242))
    assertEquals(2, Character.getType(243))
    assertEquals(2, Character.getType(244))
    assertEquals(2, Character.getType(245))
    assertEquals(2, Character.getType(246))
    assertEquals(25, Character.getType(247))
    assertEquals(2, Character.getType(248))
    assertEquals(2, Character.getType(249))
    assertEquals(2, Character.getType(250))
    assertEquals(2, Character.getType(251))
    assertEquals(2, Character.getType(252))
    assertEquals(2, Character.getType(253))
    assertEquals(2, Character.getType(254))
    assertEquals(2, Character.getType(255))
    assertEquals(1, Character.getType(256))
    assertEquals(2, Character.getType(257))
    assertEquals(1, Character.getType(258))
    assertEquals(2, Character.getType(259))
    assertEquals(1, Character.getType(260))
    assertEquals(2, Character.getType(261))
    assertEquals(1, Character.getType(262))
    assertEquals(2, Character.getType(263))
    assertEquals(1, Character.getType(264))
    assertEquals(2, Character.getType(265))
    assertEquals(1, Character.getType(266))
    assertEquals(2, Character.getType(267))
    assertEquals(1, Character.getType(268))
    assertEquals(2, Character.getType(269))
    assertEquals(1, Character.getType(270))
    assertEquals(2, Character.getType(271))
    assertEquals(1, Character.getType(272))
    assertEquals(2, Character.getType(273))
    assertEquals(1, Character.getType(274))
    assertEquals(2, Character.getType(275))
    assertEquals(1, Character.getType(276))
    assertEquals(2, Character.getType(277))
    assertEquals(1, Character.getType(278))
    assertEquals(2, Character.getType(279))
    assertEquals(1, Character.getType(280))
    assertEquals(2, Character.getType(281))
    assertEquals(1, Character.getType(282))
    assertEquals(2, Character.getType(283))
    assertEquals(1, Character.getType(284))
    assertEquals(2, Character.getType(285))
    assertEquals(1, Character.getType(286))
    assertEquals(2, Character.getType(287))
    assertEquals(1, Character.getType(288))
    assertEquals(2, Character.getType(289))
    assertEquals(1, Character.getType(290))
    assertEquals(2, Character.getType(291))
    assertEquals(1, Character.getType(292))
    assertEquals(2, Character.getType(293))
    assertEquals(1, Character.getType(294))
    assertEquals(2, Character.getType(295))
    assertEquals(1, Character.getType(296))
    assertEquals(2, Character.getType(297))
    assertEquals(1, Character.getType(298))
    assertEquals(2, Character.getType(299))

    // Randomly chosen assigned characters (i.e., whose types are not 0)
    assertEquals(5, Character.getType(2581))
    assertEquals(28, Character.getType(9237))
    assertEquals(25, Character.getType(10583))
    assertEquals(5, Character.getType(14526))
    assertEquals(5, Character.getType(16468))
    assertEquals(5, Character.getType(18538))
    assertEquals(5, Character.getType(21037))
    assertEquals(5, Character.getType(21880))
    assertEquals(5, Character.getType(26084))
    assertEquals(5, Character.getType(29876))
    assertEquals(5, Character.getType(31553))
    assertEquals(5, Character.getType(33192))
    assertEquals(5, Character.getType(34508))
    assertEquals(5, Character.getType(38994))
    assertEquals(5, Character.getType(41384))
    assertEquals(5, Character.getType(41661))
    assertEquals(5, Character.getType(45859))
    assertEquals(5, Character.getType(46867))
    assertEquals(5, Character.getType(48796))
    assertEquals(5, Character.getType(50755))
    assertEquals(5, Character.getType(54077))
    assertEquals(19, Character.getType(55534))
    assertEquals(18, Character.getType(57625))
    assertEquals(18, Character.getType(59186))
    assertEquals(5, Character.getType(64622))
    assertEquals(5, Character.getType(78228))
    assertEquals(1, Character.getType(120024))
    assertEquals(5, Character.getType(131952))
    assertEquals(5, Character.getType(133129))
    assertEquals(5, Character.getType(135815))
    assertEquals(5, Character.getType(136969))
    assertEquals(5, Character.getType(138445))
    assertEquals(5, Character.getType(147888))
    assertEquals(5, Character.getType(152546))
    assertEquals(5, Character.getType(158177))
    assertEquals(5, Character.getType(163037))
    assertEquals(5, Character.getType(165823))
    assertEquals(5, Character.getType(169103))
    assertEquals(5, Character.getType(171304))
    assertEquals(5, Character.getType(172648))
    assertEquals(18, Character.getType(985623))
    assertEquals(18, Character.getType(989149))
    assertEquals(18, Character.getType(997988))
    assertEquals(18, Character.getType(1000132))
    assertEquals(18, Character.getType(1002215))
    assertEquals(18, Character.getType(1002917))
    assertEquals(18, Character.getType(1004580))
    assertEquals(18, Character.getType(1006595))
    assertEquals(18, Character.getType(1007725))
    assertEquals(18, Character.getType(1010481))
    assertEquals(18, Character.getType(1016838))
    assertEquals(18, Character.getType(1016895))
    assertEquals(18, Character.getType(1018160))
    assertEquals(18, Character.getType(1020126))
    assertEquals(18, Character.getType(1026576))
    assertEquals(18, Character.getType(1027582))
    assertEquals(18, Character.getType(1036147))
    assertEquals(18, Character.getType(1036943))
    assertEquals(18, Character.getType(1040859))
    assertEquals(18, Character.getType(1041209))
    assertEquals(18, Character.getType(1044637))
    assertEquals(18, Character.getType(1045872))
    assertEquals(18, Character.getType(1045977))
    assertEquals(18, Character.getType(1052466))
    assertEquals(18, Character.getType(1052541))
    assertEquals(18, Character.getType(1052557))
    assertEquals(18, Character.getType(1052647))
    assertEquals(18, Character.getType(1060162))
    assertEquals(18, Character.getType(1065690))
    assertEquals(18, Character.getType(1078626))
    assertEquals(18, Character.getType(1080764))
    assertEquals(18, Character.getType(1080771))
    assertEquals(18, Character.getType(1082986))
    assertEquals(18, Character.getType(1083091))
    assertEquals(18, Character.getType(1086585))
    assertEquals(18, Character.getType(1087471))
    assertEquals(18, Character.getType(1094259))
    assertEquals(18, Character.getType(1097626))
    assertEquals(18, Character.getType(1101125))
    assertEquals(18, Character.getType(1103470))
    assertEquals(18, Character.getType(1106648))
    assertEquals(18, Character.getType(1107521))
    assertEquals(18, Character.getType(1112476))
  }

  @Test def isJavaLetter(): Unit = {
    // 50 randomly chosen characters that produce true
    assertTrue(Character.isJavaLetter('\u1115'))
    assertTrue(Character.isJavaLetter('\u1895'))
    assertTrue(Character.isJavaLetter('\u1E89'))
    assertTrue(Character.isJavaLetter('\u1ECF'))
    assertTrue(Character.isJavaLetter('\u3C72'))
    assertTrue(Character.isJavaLetter('\u3D16'))
    assertTrue(Character.isJavaLetter('\u426E'))
    assertTrue(Character.isJavaLetter('\u436D'))
    assertTrue(Character.isJavaLetter('\u4723'))
    assertTrue(Character.isJavaLetter('\u500F'))
    assertTrue(Character.isJavaLetter('\u52D3'))
    assertTrue(Character.isJavaLetter('\u55DA'))
    assertTrue(Character.isJavaLetter('\u571D'))
    assertTrue(Character.isJavaLetter('\u5CE6'))
    assertTrue(Character.isJavaLetter('\u5F2A'))
    assertTrue(Character.isJavaLetter('\u5F3B'))
    assertTrue(Character.isJavaLetter('\u60ED'))
    assertTrue(Character.isJavaLetter('\u611A'))
    assertTrue(Character.isJavaLetter('\u626A'))
    assertTrue(Character.isJavaLetter('\u6539'))
    assertTrue(Character.isJavaLetter('\u6609'))
    assertTrue(Character.isJavaLetter('\u0698'))
    assertTrue(Character.isJavaLetter('\u6FF6'))
    assertTrue(Character.isJavaLetter('\u7332'))
    assertTrue(Character.isJavaLetter('\u7347'))
    assertTrue(Character.isJavaLetter('\u7431'))
    assertTrue(Character.isJavaLetter('\u7F68'))
    assertTrue(Character.isJavaLetter('\u7FB9'))
    assertTrue(Character.isJavaLetter('\u7FC0'))
    assertTrue(Character.isJavaLetter('\u8603'))
    assertTrue(Character.isJavaLetter('\u8707'))
    assertTrue(Character.isJavaLetter('\u8871'))
    assertTrue(Character.isJavaLetter('\u8B1B'))
    assertTrue(Character.isJavaLetter('\u8E1B'))
    assertTrue(Character.isJavaLetter('\u9176'))
    assertTrue(Character.isJavaLetter('\u99D5'))
    assertTrue(Character.isJavaLetter('\u9D5E'))
    assertTrue(Character.isJavaLetter('\u9D62'))
    assertTrue(Character.isJavaLetter('\uA0E2'))
    assertTrue(Character.isJavaLetter('\uA61E'))
    assertTrue(Character.isJavaLetter('\uA6C3'))
    assertTrue(Character.isJavaLetter('\uAFAD'))
    assertTrue(Character.isJavaLetter('\uBB42'))
    assertTrue(Character.isJavaLetter('\uC4B5'))
    assertTrue(Character.isJavaLetter('\uC4DA'))
    assertTrue(Character.isJavaLetter('\uD301'))
    assertTrue(Character.isJavaLetter('\uD3E5'))
    assertTrue(Character.isJavaLetter('\u0EAB'))
    assertTrue(Character.isJavaLetter('\uF9C0'))
    assertTrue(Character.isJavaLetter('\uFD8F'))

    /* 50 randomly chosen characters that produce false,
     * minus those that became true in JDK 11 or later.
     */
    assertFalse(Character.isJavaLetter('\u19CF'))
    assertFalse(Character.isJavaLetter('\u23D7'))
    assertFalse(Character.isJavaLetter('\u26A9'))
    assertFalse(Character.isJavaLetter('\u2775'))
    assertFalse(Character.isJavaLetter('\u2832'))
    assertFalse(Character.isJavaLetter('\u2885'))
    assertFalse(Character.isJavaLetter('\u2930'))
    assertFalse(Character.isJavaLetter('\u2B8F'))
    assertFalse(Character.isJavaLetter('\u2CF4'))
    assertFalse(Character.isJavaLetter('\u2F70'))
    assertFalse(Character.isJavaLetter('\u0037'))
    assertFalse(Character.isJavaLetter('\u073F'))
    assertFalse(Character.isJavaLetter('\u0969'))
    assertFalse(Character.isJavaLetter('\uA9E5'))
    assertFalse(Character.isJavaLetter('\uAAC1'))
    assertFalse(Character.isJavaLetter('\uAAF7'))
    assertFalse(Character.isJavaLetter('\u0B82'))
    assertFalse(Character.isJavaLetter('\uDA08'))
    assertFalse(Character.isJavaLetter('\uDA8D'))
    assertFalse(Character.isJavaLetter('\uDBC8'))
    assertFalse(Character.isJavaLetter('\uDBF7'))
    assertFalse(Character.isJavaLetter('\uDD3C'))
    assertFalse(Character.isJavaLetter('\uDDD1'))
    assertFalse(Character.isJavaLetter('\uDEC2'))
    assertFalse(Character.isJavaLetter('\uE068'))
    assertFalse(Character.isJavaLetter('\uE0A3'))
    assertFalse(Character.isJavaLetter('\uE210'))
    assertFalse(Character.isJavaLetter('\uE414'))
    assertFalse(Character.isJavaLetter('\u0E4C'))
    assertFalse(Character.isJavaLetter('\uE4FD'))
    assertFalse(Character.isJavaLetter('\uE5F7'))
    assertFalse(Character.isJavaLetter('\uE611'))
    assertFalse(Character.isJavaLetter('\u0E6A'))
    assertFalse(Character.isJavaLetter('\uE704'))
    assertFalse(Character.isJavaLetter('\uE71F'))
    assertFalse(Character.isJavaLetter('\uE7DB'))
    assertFalse(Character.isJavaLetter('\uEA9F'))
    assertFalse(Character.isJavaLetter('\uEAAB'))
    assertFalse(Character.isJavaLetter('\uEBF1'))
    assertFalse(Character.isJavaLetter('\uED24'))
    assertFalse(Character.isJavaLetter('\uED93'))
    assertFalse(Character.isJavaLetter('\uEDE1'))
    assertFalse(Character.isJavaLetter('\uEF40'))
    assertFalse(Character.isJavaLetter('\uF312'))
    assertFalse(Character.isJavaLetter('\uF6B9'))
    assertFalse(Character.isJavaLetter('\uF80F'))
    assertFalse(Character.isJavaLetter('\uFD3E'))
    assertFalse(Character.isJavaLetter('\u0FF9'))
  }

  @Test def isJavaLetterOrDigit(): Unit = {
    // 50 randomly chosen characters that produce true
    assertTrue(Character.isJavaLetterOrDigit('\u0B42'))
    assertTrue(Character.isJavaLetterOrDigit('\u0C35'))
    assertTrue(Character.isJavaLetterOrDigit('\u1DBD'))
    assertTrue(Character.isJavaLetterOrDigit('\u1FE5'))
    assertTrue(Character.isJavaLetterOrDigit('\u3572'))
    assertTrue(Character.isJavaLetterOrDigit('\u3FBC'))
    assertTrue(Character.isJavaLetterOrDigit('\u46E6'))
    assertTrue(Character.isJavaLetterOrDigit('\u47C1'))
    assertTrue(Character.isJavaLetterOrDigit('\u495B'))
    assertTrue(Character.isJavaLetterOrDigit('\u49D3'))
    assertTrue(Character.isJavaLetterOrDigit('\u4E38'))
    assertTrue(Character.isJavaLetterOrDigit('\u5513'))
    assertTrue(Character.isJavaLetterOrDigit('\u57CD'))
    assertTrue(Character.isJavaLetterOrDigit('\u5884'))
    assertTrue(Character.isJavaLetterOrDigit('\u5A74'))
    assertTrue(Character.isJavaLetterOrDigit('\u5CEB'))
    assertTrue(Character.isJavaLetterOrDigit('\u5D4B'))
    assertTrue(Character.isJavaLetterOrDigit('\u5F0B'))
    assertTrue(Character.isJavaLetterOrDigit('\u6889'))
    assertTrue(Character.isJavaLetterOrDigit('\u69D0'))
    assertTrue(Character.isJavaLetterOrDigit('\u6A40'))
    assertTrue(Character.isJavaLetterOrDigit('\u6C9E'))
    assertTrue(Character.isJavaLetterOrDigit('\u7237'))
    assertTrue(Character.isJavaLetterOrDigit('\u7289'))
    assertTrue(Character.isJavaLetterOrDigit('\u7313'))
    assertTrue(Character.isJavaLetterOrDigit('\u7626'))
    assertTrue(Character.isJavaLetterOrDigit('\u7C1C'))
    assertTrue(Character.isJavaLetterOrDigit('\u7DC9'))
    assertTrue(Character.isJavaLetterOrDigit('\u8093'))
    assertTrue(Character.isJavaLetterOrDigit('\u8573'))
    assertTrue(Character.isJavaLetterOrDigit('\u884E'))
    assertTrue(Character.isJavaLetterOrDigit('\u90AE'))
    assertTrue(Character.isJavaLetterOrDigit('\u94E8'))
    assertTrue(Character.isJavaLetterOrDigit('\u956A'))
    assertTrue(Character.isJavaLetterOrDigit('\u9912'))
    assertTrue(Character.isJavaLetterOrDigit('\u9E2A'))
    assertTrue(Character.isJavaLetterOrDigit('\uACE6'))
    assertTrue(Character.isJavaLetterOrDigit('\uB0E7'))
    assertTrue(Character.isJavaLetterOrDigit('\uB3E5'))
    assertTrue(Character.isJavaLetterOrDigit('\uB7CA'))
    assertTrue(Character.isJavaLetterOrDigit('\uBD2C'))
    assertTrue(Character.isJavaLetterOrDigit('\uBD3F'))
    assertTrue(Character.isJavaLetterOrDigit('\uC155'))
    assertTrue(Character.isJavaLetterOrDigit('\uC585'))
    assertTrue(Character.isJavaLetterOrDigit('\uC7FC'))
    assertTrue(Character.isJavaLetterOrDigit('\uD250'))
    assertTrue(Character.isJavaLetterOrDigit('\uD378'))
    assertTrue(Character.isJavaLetterOrDigit('\uD73B'))
    assertTrue(Character.isJavaLetterOrDigit('\uFDB1'))
    assertTrue(Character.isJavaLetterOrDigit('\uFEE5'))

    /* 50 randomly chosen characters that produce false,
     * minus those that became true in JDK 11 or later.
     */
    assertFalse(Character.isJavaLetterOrDigit('\u05FD'))
    assertFalse(Character.isJavaLetterOrDigit('\u0AD6'))
    assertFalse(Character.isJavaLetterOrDigit('\u0F3D'))
    assertFalse(Character.isJavaLetterOrDigit('\u0FCA'))
    assertFalse(Character.isJavaLetterOrDigit('\u17F5'))
    assertFalse(Character.isJavaLetterOrDigit('\u1A5F'))
    assertFalse(Character.isJavaLetterOrDigit('\u1F4E'))
    assertFalse(Character.isJavaLetterOrDigit('\u204C'))
    assertFalse(Character.isJavaLetterOrDigit('\u224A'))
    assertFalse(Character.isJavaLetterOrDigit('\u2296'))
    assertFalse(Character.isJavaLetterOrDigit('\u241C'))
    assertFalse(Character.isJavaLetterOrDigit('\u247C'))
    assertFalse(Character.isJavaLetterOrDigit('\u2490'))
    assertFalse(Character.isJavaLetterOrDigit('\u24B8'))
    assertFalse(Character.isJavaLetterOrDigit('\u253B'))
    assertFalse(Character.isJavaLetterOrDigit('\u2755'))
    assertFalse(Character.isJavaLetterOrDigit('\u2A18'))
    assertFalse(Character.isJavaLetterOrDigit('\u2A64'))
    assertFalse(Character.isJavaLetterOrDigit('\u2A91'))
    assertFalse(Character.isJavaLetterOrDigit('\u2F0F'))
    assertFalse(Character.isJavaLetterOrDigit('\uA4C1'))
    assertFalse(Character.isJavaLetterOrDigit('\uA4C3'))
    assertFalse(Character.isJavaLetterOrDigit('\uDA09'))
    assertFalse(Character.isJavaLetterOrDigit('\uDB3E'))
    assertFalse(Character.isJavaLetterOrDigit('\uDE30'))
    assertFalse(Character.isJavaLetterOrDigit('\uDFB4'))
    assertFalse(Character.isJavaLetterOrDigit('\uE116'))
    assertFalse(Character.isJavaLetterOrDigit('\uE20D'))
    assertFalse(Character.isJavaLetterOrDigit('\uE21D'))
    assertFalse(Character.isJavaLetterOrDigit('\uE341'))
    assertFalse(Character.isJavaLetterOrDigit('\uE4DC'))
    assertFalse(Character.isJavaLetterOrDigit('\uE6F7'))
    assertFalse(Character.isJavaLetterOrDigit('\uE6FB'))
    assertFalse(Character.isJavaLetterOrDigit('\uE785'))
    assertFalse(Character.isJavaLetterOrDigit('\uEA37'))
    assertFalse(Character.isJavaLetterOrDigit('\uEB1C'))
    assertFalse(Character.isJavaLetterOrDigit('\uEE47'))
    assertFalse(Character.isJavaLetterOrDigit('\uEEA6'))
    assertFalse(Character.isJavaLetterOrDigit('\uF27F'))
    assertFalse(Character.isJavaLetterOrDigit('\uF2FE'))
    assertFalse(Character.isJavaLetterOrDigit('\uF33F'))
    assertFalse(Character.isJavaLetterOrDigit('\uF3B1'))
    assertFalse(Character.isJavaLetterOrDigit('\uF5AD'))
    assertFalse(Character.isJavaLetterOrDigit('\uF6B1'))
    assertFalse(Character.isJavaLetterOrDigit('\uF84B'))
    assertFalse(Character.isJavaLetterOrDigit('\uFF03'))
  }

  @Test def isJavaIdentifierStart(): Unit = {
    // 50 randomly chosen characters that produce true
    assertTrue(Character.isJavaIdentifierStart('\u186C'))
    assertTrue(Character.isJavaIdentifierStart('\u1F75'))
    assertTrue(Character.isJavaIdentifierStart('\u347C'))
    assertTrue(Character.isJavaIdentifierStart('\u485F'))
    assertTrue(Character.isJavaIdentifierStart('\u54A6'))
    assertTrue(Character.isJavaIdentifierStart('\u58CF'))
    assertTrue(Character.isJavaIdentifierStart('\u65F6'))
    assertTrue(Character.isJavaIdentifierStart('\u6E34'))
    assertTrue(Character.isJavaIdentifierStart('\u7404'))
    assertTrue(Character.isJavaIdentifierStart('\u77BD'))
    assertTrue(Character.isJavaIdentifierStart('\u7977'))
    assertTrue(Character.isJavaIdentifierStart('\u83A9'))
    assertTrue(Character.isJavaIdentifierStart('\u844E'))
    assertTrue(Character.isJavaIdentifierStart('\u9D7D'))
    assertTrue(Character.isJavaIdentifierStart('\u9E7F'))
    assertTrue(Character.isJavaIdentifierStart('\uA734'))
    assertTrue(Character.isJavaIdentifierStart('\uAA81'))
    assertTrue(Character.isJavaIdentifierStart('\uACED'))
    assertTrue(Character.isJavaIdentifierStart('\uBD6B'))
    assertTrue(Character.isJavaIdentifierStart('\uBD89'))
    assertTrue(Character.isJavaIdentifierStart('\uC5E4'))
    assertTrue(Character.isJavaIdentifierStart('\uC7C7'))
    assertTrue(Character.isJavaIdentifierStart('\uD3C8'))
    assertTrue(Character.isJavaIdentifierStart('\uF972'))
    assertTrue(Character.isJavaIdentifierStart('\uFA9F'))
    assertTrue(Character.isJavaIdentifierStart('\uFDA6'))
    assertTrue(Character.isJavaIdentifierStart(132773))
    assertTrue(Character.isJavaIdentifierStart(136707))
    assertTrue(Character.isJavaIdentifierStart(140254))
    assertTrue(Character.isJavaIdentifierStart(141336))
    assertTrue(Character.isJavaIdentifierStart(147781))
    assertTrue(Character.isJavaIdentifierStart(148525))
    assertTrue(Character.isJavaIdentifierStart(149236))
    assertTrue(Character.isJavaIdentifierStart(151919))
    assertTrue(Character.isJavaIdentifierStart(153364))
    assertTrue(Character.isJavaIdentifierStart(153898))
    assertTrue(Character.isJavaIdentifierStart(157645))
    assertTrue(Character.isJavaIdentifierStart(158882))
    assertTrue(Character.isJavaIdentifierStart(161627))
    assertTrue(Character.isJavaIdentifierStart(164324))
    assertTrue(Character.isJavaIdentifierStart(168371))
    assertTrue(Character.isJavaIdentifierStart(169573))
    assertTrue(Character.isJavaIdentifierStart(171871))
    assertTrue(Character.isJavaIdentifierStart(172017))
    assertTrue(Character.isJavaIdentifierStart(172787))
    assertTrue(Character.isJavaIdentifierStart(173670))
    assertTrue(Character.isJavaIdentifierStart(175824))
    assertTrue(Character.isJavaIdentifierStart(74462))
    assertTrue(Character.isJavaIdentifierStart(78380))

    /* 50 randomly chosen characters that produce false,
     * minus those that became true in JDK 11 or later.
     */
    assertFalse(Character.isJavaIdentifierStart('\uE8A1'))
    assertFalse(Character.isJavaIdentifierStart('\uEBB6'))
    assertFalse(Character.isJavaIdentifierStart(1006625))
    assertFalse(Character.isJavaIdentifierStart(1038096))
    assertFalse(Character.isJavaIdentifierStart(1080963))
    assertFalse(Character.isJavaIdentifierStart(1098681))
    assertFalse(Character.isJavaIdentifierStart(1101181))
    assertFalse(Character.isJavaIdentifierStart(212082))
    assertFalse(Character.isJavaIdentifierStart(219401))
    assertFalse(Character.isJavaIdentifierStart(223687))
    assertFalse(Character.isJavaIdentifierStart(230929))
    assertFalse(Character.isJavaIdentifierStart(238776))
    assertFalse(Character.isJavaIdentifierStart(275525))
    assertFalse(Character.isJavaIdentifierStart(297540))
    assertFalse(Character.isJavaIdentifierStart(310684))
    assertFalse(Character.isJavaIdentifierStart(323571))
    assertFalse(Character.isJavaIdentifierStart(340895))
    assertFalse(Character.isJavaIdentifierStart(355972))
    assertFalse(Character.isJavaIdentifierStart(382586))
    assertFalse(Character.isJavaIdentifierStart(385597))
    assertFalse(Character.isJavaIdentifierStart(438811))
    assertFalse(Character.isJavaIdentifierStart(442235))
    assertFalse(Character.isJavaIdentifierStart(446274))
    assertFalse(Character.isJavaIdentifierStart(454075))
    assertFalse(Character.isJavaIdentifierStart(464326))
    assertFalse(Character.isJavaIdentifierStart(469494))
    assertFalse(Character.isJavaIdentifierStart(473583))
    assertFalse(Character.isJavaIdentifierStart(481617))
    assertFalse(Character.isJavaIdentifierStart(492310))
    assertFalse(Character.isJavaIdentifierStart(502865))
    assertFalse(Character.isJavaIdentifierStart(505834))
    assertFalse(Character.isJavaIdentifierStart(521762))
    assertFalse(Character.isJavaIdentifierStart(531102))
    assertFalse(Character.isJavaIdentifierStart(531557))
    assertFalse(Character.isJavaIdentifierStart(573907))
    assertFalse(Character.isJavaIdentifierStart(590651))
    assertFalse(Character.isJavaIdentifierStart(602729))
    assertFalse(Character.isJavaIdentifierStart(607107))
    assertFalse(Character.isJavaIdentifierStart(657499))
    assertFalse(Character.isJavaIdentifierStart(684199))
    assertFalse(Character.isJavaIdentifierStart(727324))
    assertFalse(Character.isJavaIdentifierStart(757790))
    assertFalse(Character.isJavaIdentifierStart(783173))
    assertFalse(Character.isJavaIdentifierStart(805578))
    assertFalse(Character.isJavaIdentifierStart(834136))
    assertFalse(Character.isJavaIdentifierStart(875116))
    assertFalse(Character.isJavaIdentifierStart(955913))
    assertFalse(Character.isJavaIdentifierStart(958246))
  }

  @Test def reverseBytes(): Unit =
    assertEquals('\u3412', Character.reverseBytes('\u1234'))
}
