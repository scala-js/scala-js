package org.scalajs.testsuite.niocharset

import scala.language.implicitConversions

import scala.scalajs.niocharset.StandardCharsets._

import java.nio.charset._

import org.junit.Test
import org.junit.Assert._

class CharsetJSTest {

  @Test def defaultCharset(): Unit = {
    assertSame(UTF_8, Charset.defaultCharset())
  }

  @Test def forName(): Unit = {
    assertSame(ISO_8859_1, Charset.forName("ISO-8859-1"))
    assertSame(ISO_8859_1, Charset.forName("Iso8859-1"))
    assertSame(ISO_8859_1, Charset.forName("iso_8859_1"))
    assertSame(ISO_8859_1, Charset.forName("LaTin1"))
    assertSame(ISO_8859_1, Charset.forName("l1"))

    assertSame(US_ASCII, Charset.forName("US-ASCII"))
    assertSame(US_ASCII, Charset.forName("Default"))

    assertSame(UTF_8, Charset.forName("UTF-8"))
    assertSame(UTF_8, Charset.forName("utf-8"))
    assertSame(UTF_8, Charset.forName("UtF8"))
    assertSame(UTF_8, Charset.forName("UTF-8"))

    assertSame(UTF_16BE, Charset.forName("UTF-16BE"))
    assertSame(UTF_16BE, Charset.forName("Utf_16BE"))
    assertSame(UTF_16BE, Charset.forName("UnicodeBigUnmarked"))

    assertSame(UTF_16LE, Charset.forName("UTF-16le"))
    assertSame(UTF_16LE, Charset.forName("Utf_16le"))
    assertSame(UTF_16LE, Charset.forName("UnicodeLittleUnmarked"))

    assertSame(UTF_16, Charset.forName("UTF-16"))
    assertSame(UTF_16, Charset.forName("Utf_16"))
    assertSame(UTF_16, Charset.forName("unicode"))
    assertSame(UTF_16, Charset.forName("UnicodeBig"))
  }
}
