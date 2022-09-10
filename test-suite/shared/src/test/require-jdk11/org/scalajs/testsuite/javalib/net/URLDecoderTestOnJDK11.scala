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

package org.scalajs.testsuite.javalib.net

import org.junit.Test
import org.junit.Assert._

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets._
import java.net.URLDecoder

class URLDecoderTestOnJDK11 {
  import URLDecoderTest._

  @Test
  def decodeCharsetCharset(): Unit = {
    def test(encoded: String, expected: String, enc: Charset = UTF_8): Unit =
      assertEquals(expected, URLDecoder.decode(encoded, enc))

    def illegalArgumentOrReplacement(encoded: String, enc: Charset = UTF_8): Unit =
      testIllegalArgumentOrReplacementGeneric(encoded, URLDecoder.decode(_, enc))

    // empty string
    test("", "")

    // '+' -> ' '
    test("a+b+c", "a b c")

    // single byte codepoint
    test("a%20b%20c", "a b c")

    // multi byte codepoint
    test("a%c3%9fc", "aßc")

    // consecutive characters
    test("a%20%20c", "a  c")

    // illegal codepoints
    illegalArgumentOrReplacement("a%b%c")
    illegalArgumentOrReplacement("%-1")
    illegalArgumentOrReplacement("%20%8")
    illegalArgumentOrReplacement("%c3%28")

    // other charsets
    test("a%20%A3%20c", "a £ c", enc = ISO_8859_1)
    test("a%20b%20c", "a b c", enc = US_ASCII)
    test("a%00%20b%00%20c", "a b c", enc = UTF_16BE)
    test("a%20%00b%20%00c", "a b c", enc = UTF_16LE)
    test("a%fe%ff%00%20b%fe%ff%00%20c", "a b c", enc = UTF_16)
  }
}
