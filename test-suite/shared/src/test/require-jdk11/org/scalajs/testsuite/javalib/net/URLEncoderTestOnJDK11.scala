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
import java.net.URLEncoder

class URLEncoderTestOnJDK11 {
  @Test
  def encodeCharsetCharset(): Unit = {
    def test(unencoded: String, expected: String, enc: Charset = UTF_8): Unit =
      assertEquals(expected, URLEncoder.encode(unencoded, enc))

    // empty string
    test("", "")

    // . - * _ remain the same, as well as ASCII letters and digits
    test("afz-AFZ-069-.*_", "afz-AFZ-069-.*_")

    // ' ' -> '+'
    test("a b c", "a+b+c")

    // single byte codepoint
    test("a@b#c", "a%40b%23c")

    // multi byte codepoint, include surrogate pairs
    test("aßcd\uD834\uDD1Eé", "a%C3%9Fcd%F0%9D%84%9E%C3%A9") // 𝄞 U+1D11E MUSICAL SYMBOL G CLEF

    // consecutive characters
    test("aß#c", "a%C3%9F%23c")

    // other charsets
    test("a-£-#-é-ß-c", "a-%C2%A3-%23-%C3%A9-%C3%9F-c", enc = UTF_8)
    test("a-£-#-é-ß-c", "a-%A3-%23-%E9-%DF-c", enc = ISO_8859_1)
    test("a-£-#-é-ß-c", "a-%3F-%23-%3F-%3F-c", enc = US_ASCII)
    test("a-£-#-é-ß-c", "a-%00%A3-%00%23-%00%E9-%00%DF-c", enc = UTF_16BE)
    test("a-£-#-é-ß-c", "a-%A3%00-%23%00-%E9%00-%DF%00-c", enc = UTF_16LE)

    /* Do not test with UTF_16 becauses it introduces BOM's in the middle of
     * the encoded string, which is nonsensical.
     */
  }
}
