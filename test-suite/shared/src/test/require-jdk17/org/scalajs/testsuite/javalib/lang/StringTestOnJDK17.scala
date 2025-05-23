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

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows.assertThrows

class StringTestOnJDK17 {

  // indent and transform are available since JDK 12 but we're not testing them separately

  @Test def indent(): Unit = {
    assertEquals("", "".indent(1))
    assertEquals("", "".indent(0))
    assertEquals("", "".indent(-1))
    assertEquals(" \n", "\n".indent(1))
    assertEquals("\n", "\n".indent(0))
    assertEquals("\n", "\n".indent(-1))

    // indent adds the extra new line due to JDK normalization requirements
    assertEquals("  abc\n", "abc".indent(2))
    assertEquals(" abc\n", "abc".indent(1))
    assertEquals("abc\n", "abc".indent(0))
    assertEquals("abc\n", "abc".indent(-1))
    assertEquals("abc\n", "abc".indent(-2))
    assertEquals("     a\n       b\n", "a\n  b\n".indent(5))
    assertEquals("a\n  b\n", "a\n  b\n".indent(0))
    assertEquals("a\nb\n", "a\n  b\n".indent(-5))
    assertEquals("      \n", "      ".indent(0))
    assertEquals("            \n", "      ".indent(6))
    assertEquals("\n", "      ".indent(-6))
    assertEquals(" \n", "   ".indent(-2))
    assertEquals("  \n", "        ".indent(-6))

    assertEquals("  a\n  \n  c\n", "a\n\nc".indent(2))
    assertEquals("  abc\n  def\n", "abc\ndef".indent(2))
    assertEquals("  abc\n  def\n  \n  \n  \n  a\n", "abc\ndef\n\n\n\na".indent(2))

    assertEquals(" \n  \n", "\n \n".indent(1))
    assertEquals("  \n  \n  \n", " \n \n ".indent(1))
    assertEquals(" \n \n \n \n", "\n\n\n\n".indent(1))
    assertEquals(" 0\n A\n B\n C\n D\n", "0\r\nA\r\nB\r\nC\r\nD".indent(1))
    assertEquals(" 0\n A\n B\n C\n D\n", "0\rA\rB\rC\rD".indent(1))

    assertEquals("  \n  \n  \n", "\r\r\n\n".indent(2))
    assertEquals("  \n  \n  \n  \n", "\r\r\r\r".indent(2))
    assertEquals("  \n  \n", "\r\n\r\n".indent(2))
    assertEquals("\n\n\n", "\r\n\n\n".indent(-1))
    assertEquals("\n\n\n", "\r\n\n\n".indent(0))

    // non-U+0020 WS
    assertEquals("  \u2028 \u2029 \u2004 \u200a \u3000 \n", "\u2028 \u2029 \u2004 \u200A \u3000 ".indent(2))
    assertEquals("\u2029 \u2004 \u200A \u3000 \n", "\u2028 \u2029 \u2004 \u200A \u3000 ".indent(-2))
    assertEquals("\u2028 \u2029 \u2004 \u200A \u3000 \n", "\u2028 \u2029 \u2004 \u200A \u3000 ".indent(0))

  }

  @Test def transform(): Unit = {
    assertEquals("", "".transform(x => x))
    assertEquals("abcabc", "abc".transform(_ * 2))
    assertEquals("bar", "foo".transform(_ => "bar"))
  }

  @Test def stripIndent(): Unit = {

    // single line indents
    assertEquals("", "".stripIndent())
    assertEquals("", " ".stripIndent())
    assertEquals("-", "-".stripIndent())
    assertEquals("-", " -".stripIndent())
    assertEquals("-", "   -".stripIndent())
    assertEquals("-", "   -   ".stripIndent())
    assertEquals("", "  ".stripIndent())

    // new line normalization
    assertEquals("\n", "\n".stripIndent())
    assertEquals("\n", " \n".stripIndent())
    assertEquals("\n", " \n ".stripIndent())
    assertEquals("\n\n", "\n\n".stripIndent())
    assertEquals("\n\n\n", "\n\n\n".stripIndent())
    assertEquals("\n\n", "\n  \n".stripIndent())
    assertEquals("  A\n  B\n\n", "  A\n  B\r \n".stripIndent())
    assertEquals("  A\n  B\n", "  A\n  B\r\n".stripIndent())
    assertEquals("  A\n  B\n", "  A\n  B\n".stripIndent())
    assertEquals("A\nB", "  A\n  B".stripIndent())
    assertEquals("\n\n", "\n  \n    ".stripIndent())
    assertEquals("\n  A\n  B\n", "  \n  A\n  B  \n".stripIndent())
    assertEquals("\nA\nB", "  \nA  \nB".stripIndent())
    assertEquals("A\nA\nB", "A  \nA  \nB".stripIndent())
    assertEquals("A\nA\nA\nA", "  A\n  A\n  A\n  A".stripIndent())
    assertEquals("A\nA\nA\nA", "  A\n  A\n  A\n  A ".stripIndent())
    assertEquals("__\nABC\n Ac\nA", "  __  \n  ABC  \n   Ac\n  A  ".stripIndent())

    // variable indents
    assertEquals("A\n B\n  C\n   D\n    E\n", "A\n B\n  C\n   D\n    E\n     ".stripIndent())
    assertEquals("    A\n   B\n  C\n   D\n    E\n", "    A\n   B\n  C\n   D\n    E\n".stripIndent())
    assertEquals("    A\nB\n\n", "    A\nB\n  \n".stripIndent())
    assertEquals("  A\n    B\n  C\n", "  A\n    B\n  C\n".stripIndent())

    // variable indents (no trailing new line)
    assertEquals("A\n B\n  C\n   D\n    E", "A\n B\n  C\n   D\n    E".stripIndent())
    assertEquals("  A\n B\nC\n D\n  E", "    A\n   B\n  C\n   D\n    E".stripIndent())
    assertEquals("    A\nB", "    A\nB".stripIndent())
    assertEquals("A\n  B\nC", "  A\n    B\n  C".stripIndent())

    // alternative WS and tabs
    assertEquals(
        "A\n\u2028B\n\u2028C\n\u2028\u2028D\n\u2028\u2028\u2028E",
        "A\n\u2028B\n\u2028C\u2028\n\u2028\u2028D\u2028\n\u2028\u2028\u2028E \u2028".stripIndent())
    assertEquals(
        "\u2028 A\n B\nC\n\n  E",
        "\u2029 \u2028 A\n   B\n\u3000 C \u2028\n \t\n \u2004  E".stripIndent())
    assertEquals("    A\nB", "    A\t\nB".stripIndent())
    assertEquals("\tA\n  B\nC", "\t\tA\t\n   B\n\tC".stripIndent())
    assertEquals("A\n B\nC", "\tA\n\t B\t\n\tC".stripIndent())

    // leading/trailing WS
    assertEquals("A\nB\n", " A\n B\n ".stripIndent())
    assertEquals("A\nB\n", " A\n B\n  ".stripIndent())
    assertEquals("  A\n  B\n", "  A\n  B\n".stripIndent())
    assertEquals(" A\n B\n", "  A\n  B\n ".stripIndent())
    assertEquals("A\nB\n", "  A\n  B\n  ".stripIndent())
    assertEquals("A\nB\n", "  A\n  B\n   ".stripIndent())
    assertEquals(" A\n B\n", "   A\n   B\n  ".stripIndent())

    assertEquals("\n", "    \n".stripIndent())
    assertEquals("\n", "   \n".stripIndent())
    assertEquals("\n", "  \n ".stripIndent())
    assertEquals("\n", " \n  ".stripIndent())
    assertEquals("\n", "\n   ".stripIndent())
    assertEquals("\n", " \n".stripIndent())
    assertEquals("\n", "  \n ".stripIndent())
    assertEquals("\n", "   \n  ".stripIndent())
    assertEquals("\n", "    \n   ".stripIndent())
    assertEquals("\n", "  \n".stripIndent())
    assertEquals("\n", "  \n ".stripIndent())
    assertEquals("\n", "  \n  ".stripIndent())
    assertEquals("\n", "  \n   ".stripIndent())
  }

  @Test def translateEscapes(): Unit = {

    // bad escapes
    assertThrows(classOf[IllegalArgumentException], "\\u2022".translateEscapes())
    assertThrows(classOf[IllegalArgumentException], """\z""".translateEscapes())
    assertThrows(classOf[IllegalArgumentException], """\_""".translateEscapes())
    assertThrows(classOf[IllegalArgumentException], """\999""".translateEscapes())
    assertThrows(classOf[IllegalArgumentException], """\""".translateEscapes())
    assertThrows(classOf[IllegalArgumentException], """\ """.translateEscapes())
    assertThrows(classOf[IllegalArgumentException], """ \""".translateEscapes())
    assertThrows(classOf[IllegalArgumentException], """\_\""".translateEscapes())
    assertThrows(classOf[IllegalArgumentException], """\n\""".translateEscapes())
    assertThrows(classOf[IllegalArgumentException], """foo\""".translateEscapes())

    def oct(s: String): Char = Integer.parseInt(s, 8).toChar

    // octals
    assertEquals(s"${oct("333")}", """\333""".translateEscapes())
    assertEquals(s"${oct("12")}", """\12""".translateEscapes())
    assertEquals(s"${oct("77")}", """\77""".translateEscapes())
    assertEquals(s"${oct("42")}", """\42""".translateEscapes())
    assertEquals(s"${oct("0")}", """\0""".translateEscapes())
    assertEquals(s"${oct("00")}", """\00""".translateEscapes())
    assertEquals(s"${oct("000")}", """\000""".translateEscapes())
    assertEquals(s" ${oct("333")}_${oct("333")} ", """ \333_\333 """.translateEscapes())
    assertEquals(s" ${oct("12")}_${oct("12")} ", """ \12_\12 """.translateEscapes())
    assertEquals(s" ${oct("77")}_${oct("77")} ", """ \77_\77 """.translateEscapes())
    assertEquals(s" ${oct("42")}_${oct("42")} ", """ \42_\42 """.translateEscapes())
    assertEquals(s" ${oct("0")}_${oct("0")} ", """ \0_\0 """.translateEscapes())
    assertEquals(s" ${oct("00")}_${oct("00")} ", """ \00_\00 """.translateEscapes())
    assertEquals(s" ${oct("000")}_${oct("000")} ", """ \000_\000 """.translateEscapes())
    assertEquals(s"\t${oct("12")}${oct("34")}${oct("56")}${oct("7")} 89", """\t\12\34\56\7 89""".translateEscapes())
    assertEquals(s" ${oct("111")}1 ", """ \1111 """.translateEscapes())
    assertEquals(s" ${oct("54")}11 ", """ \5411 """.translateEscapes())
    assertEquals(s" ${oct("1")}92 ", """ \192 """.translateEscapes())
    assertEquals(s" ${oct("12")}81 ", """ \1281 """.translateEscapes())

    // don't discard CR/LF if not preceded by \
    assertEquals("\r", "\r".translateEscapes())
    assertEquals("\n", "\n".translateEscapes())
    assertEquals("\r\n", "\r\n".translateEscapes())
    assertEquals(" \r \n ", " \r \n ".translateEscapes())
    assertEquals(" \r\n ", " \r\n ".translateEscapes())

    // do discard otherwise
    assertEquals("", "\\\n".translateEscapes())
    assertEquals("", "\\\r".translateEscapes())
    assertEquals("", "\\\r\n".translateEscapes())
    assertEquals("", "\\\n\\\n".translateEscapes())
    assertEquals("", "\\\r\\\n".translateEscapes())
    assertEquals(" ", "\\\n \\\n".translateEscapes())
    assertEquals("  ", " \\\n\\\n ".translateEscapes())
    assertEquals("   ", "   \\\n".translateEscapes())

    // expected should look syntactically equivalent to actual but in normal quotes
    assertEquals("", """""".translateEscapes())
    assertEquals(" ", """ """.translateEscapes())
    assertEquals("\u2022", """•""".translateEscapes())
    assertEquals("\t\n", """\t\n""".translateEscapes())
    assertEquals("\r\n", """\r\n""".translateEscapes())
    assertEquals("\n\n", """\n\n""".translateEscapes())
    assertEquals("\n\n\n\n0\n\n\n\n0\n\n\n\naaaa\n\n\\", """\n\n\n\n0\n\n\n\n0\n\n\n\naaaa\n\n\\""".translateEscapes())
    assertEquals("a\nb\nc\nd\ne\nf\t", """a\nb\nc\nd\ne\nf\t""".translateEscapes())
    assertEquals("\na", """\na""".translateEscapes())
    assertEquals("\na\n", """\na\n""".translateEscapes())
    assertEquals("a\n", """a\n""".translateEscapes())
    assertEquals("a\nb", """a\nb""".translateEscapes())
    assertEquals("a\nb\n", """a\nb\n""".translateEscapes())
    assertEquals("abcd", """abcd""".translateEscapes())
    assertEquals("\"\' \r\f\n\t\b\\\"\' \r\f\n\t\b\"", """\"\'\s\r\f\n\t\b\\\"\'\s\r\f\n\t\b\"""".translateEscapes())
    assertEquals("\\\\", """\\\\""".translateEscapes())
    assertEquals("\\abcd", """\\abcd""".translateEscapes())
    assertEquals("abcd\\", """abcd\\""".translateEscapes())
    assertEquals("\\abcd\\", """\\abcd\\""".translateEscapes())
    assertEquals("\\\\\\", """\\\\\\""".translateEscapes())
  }

}
