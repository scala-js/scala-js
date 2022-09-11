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

import org.scalajs.testsuite.utils.AssertThrows.assertThrows

import java.net.URLEncoder
import java.io.UnsupportedEncodingException

class URLEncoderTest {
  private final val utf8 = "utf-8"

  @Test
  def encodeNoCharset(): Unit = {
    def test(unencoded: String, expected: String): Unit =
      assertEquals(expected, URLEncoder.encode(unencoded))

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
  }

  @Test
  def encodeStringCharset(): Unit = {
    def test(unencoded: String, expected: String, enc: String = utf8): Unit =
      assertEquals(expected, URLEncoder.encode(unencoded, enc))

    def unsupportedEncoding(unencoded: String, enc: String = utf8): Unit = {
      val exception = classOf[UnsupportedEncodingException]
      assertThrows(exception, URLEncoder.encode(unencoded, enc))
    }

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

    // invalid encoding
    unsupportedEncoding("2a%20b%20c", enc = "dummy")

    /* Throw even if the charset is not needed.
     * Unlike for URLDecoder.decode, this is really specified, and even JDK 8
     * behaves like that.
     */
    unsupportedEncoding("abc", enc = "dummy")
    unsupportedEncoding("a+b+c", enc = "dummy")

    // other charsets
    test("a-£-#-é-ß-c", "a-%C2%A3-%23-%C3%A9-%C3%9F-c", enc = "utf-8")
    test("a-£-#-é-ß-c", "a-%A3-%23-%E9-%DF-c", enc = "iso-8859-1")
    test("a-£-#-é-ß-c", "a-%3F-%23-%3F-%3F-c", enc = "us-ascii")
    test("a-£-#-é-ß-c", "a-%00%A3-%00%23-%00%E9-%00%DF-c", enc = "utf-16be")
    test("a-£-#-é-ß-c", "a-%A3%00-%23%00-%E9%00-%DF%00-c", enc = "utf-16le")

    /* Do not test with utf-16 becauses it introduces BOM's in the middle of
     * the encoded string, which is nonsensical.
     */
  }
}
