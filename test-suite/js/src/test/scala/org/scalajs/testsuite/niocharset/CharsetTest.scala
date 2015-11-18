/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.niocharset

import scala.language.implicitConversions

import java.nio._
import java.nio.charset._

import scala.scalajs.js
import scala.scalajs.niocharset.StandardCharsets._

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._

class CharsetTest {

  @Test def defaultCharset(): Unit = {
    assertSame(Charset.defaultCharset(), UTF_8)
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
    assertSame(UTF_8, Charset.forName("UTF_8"))
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

    expectThrows(classOf[UnsupportedCharsetException],
        Charset.forName("this-charset-does-not-exist"))
  }

  @Test def isSupported(): Unit = {
    assertTrue(Charset.isSupported("ISO-8859-1"))
    assertTrue(Charset.isSupported("US-ASCII"))
    assertTrue(Charset.isSupported("Default"))
    assertTrue(Charset.isSupported("utf-8"))
    assertTrue(Charset.isSupported("UnicodeBigUnmarked"))
    assertTrue(Charset.isSupported("Utf_16le"))
    assertTrue(Charset.isSupported("UTF-16"))
    assertTrue(Charset.isSupported("unicode"))

    assertFalse(Charset.isSupported("this-charset-does-not-exist"))

  }
}
