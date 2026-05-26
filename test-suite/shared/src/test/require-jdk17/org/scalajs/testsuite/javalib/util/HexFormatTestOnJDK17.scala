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

package org.scalajs.testsuite.javalib.util

import java.io.{IOException, UncheckedIOException}
import java.nio.CharBuffer
import java.util.{Arrays, HexFormat}
import java.{lang => jl}

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows.{assertThrows, assertThrowsNPEIfCompliant}

class HexFormatTestOnJDK17 {

  @Test def factoriesWithersAccessorsAndObjectMethods(): Unit = {
    val default = HexFormat.of()
    assertEquals("", default.delimiter())
    assertEquals("", default.prefix())
    assertEquals("", default.suffix())
    assertFalse(default.isUpperCase())

    val custom =
      HexFormat
        .ofDelimiter(", ")
        .withPrefix("#")
        .withSuffix(";")
        .withUpperCase()
    assertEquals(", ", custom.delimiter())
    assertEquals("#", custom.prefix())
    assertEquals(";", custom.suffix())
    assertTrue(custom.isUpperCase())

    val same = HexFormat
      .of()
      .withDelimiter(", ")
      .withPrefix("#")
      .withSuffix(";")
      .withUpperCase()
    assertEquals(custom, same)
    assertEquals(custom.hashCode(), same.hashCode())
    assertNotEquals(custom, custom.withLowerCase())
    assertNotEquals(custom, "not a HexFormat")

    val description = custom.toString()
    assertTrue(description.contains("uppercase: true"))
    assertTrue(description.contains("delimiter: \", \""))
    assertTrue(description.contains("prefix: \"#\""))
    assertTrue(description.contains("suffix: \";\""))

    assertThrowsNPEIfCompliant(HexFormat.ofDelimiter(null))
    assertThrowsNPEIfCompliant(default.withDelimiter(null))
    assertThrowsNPEIfCompliant(default.withPrefix(null))
    assertThrowsNPEIfCompliant(default.withSuffix(null))
  }

  @Test def formatHexStrings(): Unit = {
    val bytes = Array[Byte](0, 1, 2, 10, 15, 16, 127, -128, -1)

    assertEquals("0001020a0f107f80ff", HexFormat.of().formatHex(bytes))
    assertEquals("01020a", HexFormat.of().formatHex(bytes, 1, 4))
    assertEquals("", HexFormat.of().formatHex(bytes, 2, 2))

    val fingerprint = HexFormat.ofDelimiter(":").withUpperCase()
    assertEquals("00:01:02:0A:0F:10:7F:80:FF", fingerprint.formatHex(bytes))

    val custom = HexFormat.ofDelimiter(", ").withPrefix("#").withSuffix(";")
    assertEquals(
      "#00;, #01;, #02;, #0a;, #0f;, #10;, #7f;, #80;, #ff;",
      custom.formatHex(bytes)
    )

    assertThrows(
      classOf[IndexOutOfBoundsException],
      HexFormat.of().formatHex(bytes, -1, 1)
    )
    assertThrows(
      classOf[IndexOutOfBoundsException],
      HexFormat.of().formatHex(bytes, 0, bytes.length + 1)
    )
    // Null bytes triggers NPE only via bytes.length dereference; compliant only.
    assertThrowsNPEIfCompliant(
      HexFormat.of().formatHex(null.asInstanceOf[Array[Byte]])
    )
  }

  @Test def formatHexAppendable(): Unit = {
    val bytes = Array[Byte](0x0a.toByte, 0x0b.toByte, 0x0c.toByte)
    val format = HexFormat.ofDelimiter("|").withPrefix("<").withSuffix(">")
    val builder = new jl.StringBuilder()

    val returned = format.formatHex(builder, bytes, 1, 3)
    assertSame(builder, returned)
    assertEquals("<0b>|<0c>", builder.toString())

    val allBuilder = new jl.StringBuilder()
    assertSame(allBuilder, HexFormat.of().formatHex(allBuilder, bytes))
    assertEquals("0a0b0c", allBuilder.toString())

    // Null Appendable: explicit Objects.requireNonNull(out); NPE under
    // scala-js is compliant-mode only.
    assertThrowsNPEIfCompliant(
      HexFormat.of().formatHex(null.asInstanceOf[jl.Appendable], bytes)
    )
    // Null bytes triggers NPE only via bytes.length dereference; compliant only.
    assertThrowsNPEIfCompliant(
      HexFormat
        .of()
        .formatHex(new jl.StringBuilder(), null.asInstanceOf[Array[Byte]])
    )

    val failure = new IOException("boom")
    val thrown = assertThrows(
      classOf[UncheckedIOException],
      HexFormat.of().formatHex(new FailingAppendable(failure), bytes)
    )
    assertSame(failure, thrown.getCause())
  }

  @Test def parseHexStringsAndRanges(): Unit = {
    assertBytesEquals(Array[Byte](), HexFormat.of().parseHex(""))
    assertBytesEquals(
      Array[Byte](0, 1, 2, 10, 15, 16, 127, -128, -1),
      HexFormat.of().parseHex("0001020A0f107f80ff")
    )
    assertBytesEquals(
      Array[Byte](0, 1),
      HexFormat.of().parseHex("xx0001yy", 2, 6)
    )

    val custom = HexFormat.ofDelimiter(":").withPrefix("<").withSuffix(">")
    assertBytesEquals(Array[Byte](), custom.parseHex(""))
    assertBytesEquals(Array[Byte](10), custom.parseHex("<0A>"))
    assertBytesEquals(Array[Byte](10, 11), custom.parseHex("<0a>:<0B>"))
    assertBytesEquals(
      Array[Byte](10, 11),
      custom.parseHex(new jl.StringBuilder("__<0a>:<0B>__"), 2, 11)
    )
    assertBytesEquals(
      Array[Byte](10, 11),
      custom.parseHex(CharBuffer.wrap("__<0a>:<0B>__"), 2, 11)
    )

    assertThrows(
      classOf[IllegalArgumentException],
      HexFormat.of().parseHex("0")
    )
    assertThrows(classOf[IllegalArgumentException], custom.parseHex("<0a><0b>"))
    assertThrows(classOf[IllegalArgumentException], custom.parseHex("<0g>"))
    assertThrows(
      classOf[IllegalArgumentException],
      HexFormat.ofDelimiter("a").parseHex("1a2a3a")
    )
    assertThrows(
      classOf[IndexOutOfBoundsException],
      HexFormat.of().parseHex("00", 1, 0)
    )

    val upperPrefix = HexFormat.of().withPrefix("0X")
    assertBytesEquals(Array[Byte](10), upperPrefix.parseHex("0X0a"))
    assertThrows(
      classOf[IllegalArgumentException],
      upperPrefix.parseHex("0x0a")
    )
  }

  @Test def parseHexCharArrayRange(): Unit = {
    val format = HexFormat.ofDelimiter(":").withPrefix("<").withSuffix(">")
    val chars = "__<0a>:<0B>__".toCharArray()
    assertBytesEquals(Array[Byte](10, 11), format.parseHex(chars, 2, 11))

    assertThrows(
      classOf[IndexOutOfBoundsException],
      format.parseHex(chars, -1, 11)
    )
    assertThrows(
      classOf[IllegalArgumentException],
      format.parseHex("<0a><0b>".toCharArray(), 0, 8)
    )
    // Null chars triggers NPE only via chars.length dereference; compliant only.
    assertThrowsNPEIfCompliant(
      HexFormat.of().parseHex(null.asInstanceOf[Array[Char]], 0, 0)
    )
  }

  @Test def hexDigitFormattingMethods(): Unit = {
    val lower = HexFormat.of()
    val upper = lower.withUpperCase()

    assertEquals('a', lower.toLowHexDigit(0x7a))
    assertEquals('7', lower.toHighHexDigit(0x7a))
    assertEquals('A', upper.toLowHexDigit(0x7a))
    assertEquals('F', upper.toHighHexDigit(0xff))

    val builder = new jl.StringBuilder()
    assertSame(builder, upper.toHexDigits(builder, 0xab.toByte))
    assertEquals("AB", builder.toString())

    assertEquals("ff", lower.toHexDigits(0xff.toByte))
    assertEquals("abcd", lower.toHexDigits(0xabcd.toChar))
    assertEquals("ffff", lower.toHexDigits((-1).toShort))
    assertEquals("89abcdef", lower.toHexDigits(0x89abcdef))
    assertEquals("0123456789abcdef", lower.toHexDigits(0x0123456789abcdefL))
    assertEquals(
      "23456789abcdef0",
      lower.toHexDigits(0x123456789abcdef0L, 15)
    )
    assertEquals("", lower.toHexDigits(0x123456789abcdef0L, 0))
    assertEquals("F0", upper.toHexDigits(0x123456789abcdef0L, 2))

    assertThrows(classOf[IllegalArgumentException], lower.toHexDigits(0L, -1))
    assertThrows(classOf[IllegalArgumentException], lower.toHexDigits(0L, 17))
    assertThrowsNPEIfCompliant(
      lower.toHexDigits(null.asInstanceOf[jl.Appendable], 0.toByte)
    )

    val failure = new IOException("boom")
    val thrown = assertThrows(
      classOf[UncheckedIOException],
      lower.toHexDigits(new FailingAppendable(failure), 0.toByte)
    )
    assertSame(failure, thrown.getCause())
  }

  @Test def hexDigitParsingMethods(): Unit = {
    for (ch <- '0' to '9') {
      assertTrue(HexFormat.isHexDigit(ch))
      assertEquals(ch - '0', HexFormat.fromHexDigit(ch))
    }
    for (ch <- 'a' to 'f') {
      assertTrue(HexFormat.isHexDigit(ch))
      assertEquals(ch - 'a' + 10, HexFormat.fromHexDigit(ch))
    }
    for (ch <- 'A' to 'F') {
      assertTrue(HexFormat.isHexDigit(ch))
      assertEquals(ch - 'A' + 10, HexFormat.fromHexDigit(ch))
    }

    assertFalse(HexFormat.isHexDigit(-1))
    assertFalse(HexFormat.isHexDigit('g'))
    assertFalse(HexFormat.isHexDigit(0x10000))
    assertThrows(classOf[NumberFormatException], HexFormat.fromHexDigit(-1))
    assertThrows(classOf[NumberFormatException], HexFormat.fromHexDigit('g'))
    assertThrows(
      classOf[NumberFormatException],
      HexFormat.fromHexDigit(0x10000)
    )

    assertEquals(0, HexFormat.fromHexDigits(""))
    assertEquals(0x1234abcd, HexFormat.fromHexDigits("xx1234abcdyy", 2, 10))
    assertEquals(-1, HexFormat.fromHexDigits("ffffffff"))
    assertEquals(0L, HexFormat.fromHexDigitsToLong(""))
    assertEquals(
      0x123456789abcdef0L,
      HexFormat.fromHexDigitsToLong("xx123456789abcdef0yy", 2, 18)
    )
    assertEquals(-1L, HexFormat.fromHexDigitsToLong("ffffffffffffffff"))

    assertThrows(
      classOf[IllegalArgumentException],
      HexFormat.fromHexDigits("100000000")
    )
    assertThrows(
      classOf[IllegalArgumentException],
      HexFormat.fromHexDigitsToLong("10000000000000000")
    )
    assertThrows(classOf[NumberFormatException], HexFormat.fromHexDigits("g"))
    assertThrows(
      classOf[IndexOutOfBoundsException],
      HexFormat.fromHexDigits("00", -1, 2)
    )
    assertThrows(
      classOf[IndexOutOfBoundsException],
      HexFormat.fromHexDigitsToLong("00", 0, 3)
    )
    // Null CharSequence triggers NPE only via string.length() dereference; compliant only.
    assertThrowsNPEIfCompliant(
      HexFormat.fromHexDigits(null.asInstanceOf[jl.CharSequence])
    )
  }

  @Test def roundTripDirectionalCoverage(): Unit = {
    val bytes = (0 until 256).map(_.toByte).toArray
    val formats = Seq(
      HexFormat.of(),
      HexFormat.of().withUpperCase(),
      HexFormat.ofDelimiter(":"),
      HexFormat.ofDelimiter(":").withUpperCase(),
      HexFormat.of().withPrefix("0x").withSuffix(";"),
      HexFormat
        .ofDelimiter(", ")
        .withPrefix("#")
        .withSuffix(";")
        .withUpperCase()
    )

    for (format <- formats)
      assertBytesEquals(bytes, format.parseHex(format.formatHex(bytes)))
  }

  private def assertBytesEquals(
      expected: Array[Byte],
      actual: Array[Byte]
  ): Unit =
    assertTrue(
      "expected: " + Arrays.toString(expected) +
        ", actual: " + Arrays.toString(actual),
      Arrays.equals(expected, actual)
    )

  private final class FailingAppendable(failure: IOException)
      extends jl.Appendable {
    def append(c: Char): jl.Appendable =
      throw failure

    def append(csq: jl.CharSequence): jl.Appendable =
      throw failure

    def append(csq: jl.CharSequence, start: Int, end: Int): jl.Appendable =
      throw failure
  }
}
