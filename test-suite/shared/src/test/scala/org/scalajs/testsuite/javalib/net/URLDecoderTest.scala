package org.scalajs.testsuite.javalib.net

import org.junit.Test
import org.junit.Assert._
import org.scalajs.testsuite.utils.AssertThrows._

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

    // doesn't throw when charset is not needed
    test("a+b+c", "a b c", enc = "dummy")

    // other charsets
    test("a%20%A3%20c", "a £ c", enc = "iso-8859-1")
    test("a%20b%20c", "a b c", enc = "us-ascii")
    test("a%00%20b%00%20c", "a b c", enc = "utf-16be")
    test("a%20%00b%20%00c", "a b c", enc = "utf-16le")
    test("a%fe%ff%00%20b%fe%ff%00%20c", "a b c", enc = "utf-16")
  }
}
