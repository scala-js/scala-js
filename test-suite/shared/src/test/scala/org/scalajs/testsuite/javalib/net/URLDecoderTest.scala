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
import org.scalajs.testsuite.utils.Platform._

import java.net.URLDecoder
import java.io.UnsupportedEncodingException

class URLDecoderTest {

  private final val utf8 = "utf-8"
  private final val ReplacementChar = '\uFFFD'

  @Test
  def decodeTest(): Unit = {
    def test(encoded: String, expected: String, enc: String = utf8): Unit =
      assertEquals(URLDecoder.decode(encoded, enc), expected)

    def illegalArgumentOrReplacement(encoded: String, enc: String = utf8): Unit = {
      val thrown = {
        try {
          val res = URLDecoder.decode(encoded, enc)

          /* It is valid to return the Unicode replacement character (U+FFFD)
           * when encountering an invalid codepoint.
           */
          res.contains(ReplacementChar)
        } catch {
          case _: IllegalArgumentException => true
        }
      }

      assertTrue(thrown)
    }

    def unsupportedEncoding(encoded: String, enc: String = utf8): Unit = {
      val exception = classOf[UnsupportedEncodingException]
      assertThrows(exception, URLDecoder.decode(encoded, enc))
    }

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

    // invalid encoding
    unsupportedEncoding("2a%20b%20c", enc = "dummy")

    /* Throw even if the charset is not needed.
     * Despite what the documentation says, `decode` eagerly throws when the
     * charset is not supported. This behavior started in JDK 10, when they
     * added the overload of `decode` taking a `Charset`. With that addition,
     * maintaining the lazy behavior would probably have required duplicating
     * the implementation. So in all likelihood, it will not be coming back.
     * It is still throwing as of JDK 17.
     */
    if (!executingInJVMOnLowerThanJDK10) {
      unsupportedEncoding("abc", enc = "dummy")
      unsupportedEncoding("a+b+c", enc = "dummy")
    }

    // other charsets
    test("a%20%A3%20c", "a £ c", enc = "iso-8859-1")
    test("a%20b%20c", "a b c", enc = "us-ascii")
    test("a%00%20b%00%20c", "a b c", enc = "utf-16be")
    test("a%20%00b%20%00c", "a b c", enc = "utf-16le")
    test("a%fe%ff%00%20b%fe%ff%00%20c", "a b c", enc = "utf-16")
  }
}
