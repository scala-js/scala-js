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

package org.scalajs.testsuite.niocharset

import java.nio.charset._
import java.nio.charset.StandardCharsets._

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.javalib.util.TrivialImmutableCollection
import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform.executingInJVM

class CharsetTest {
  def javaSet[A](elems: A*): java.util.Set[A] =
    new java.util.HashSet(TrivialImmutableCollection(elems: _*))

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

    // Issue #2040
    expectThrows(classOf[UnsupportedCharsetException], Charset.forName("UTF_8"))

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

  @Test def aliases(): Unit = {
    assertEquals(Charset.forName("UTF-8").aliases(), javaSet("UTF8", "unicode-1-1-utf-8"))
    assertEquals(Charset.forName("UTF-16").aliases(),
        javaSet("UTF_16", "unicode", "utf16", "UnicodeBig"))
    assertEquals(Charset.forName("UTF-16BE").aliases(),
        javaSet("X-UTF-16BE", "UTF_16BE", "ISO-10646-UCS-2", "UnicodeBigUnmarked"))
    assertEquals(Charset.forName("UTF-16LE").aliases(),
        javaSet("UnicodeLittleUnmarked", "UTF_16LE", "X-UTF-16LE"))
    assertEquals(Charset.forName("US-ASCII").aliases(),
        javaSet("ANSI_X3.4-1968", "cp367", "csASCII", "iso-ir-6", "ASCII", "iso_646.irv:1983",
            "ANSI_X3.4-1986", "ascii7", "default", "ISO_646.irv:1991", "ISO646-US", "IBM367", "646",
            "us"))
    assertEquals(Charset.forName("ISO-8859-1").aliases(),
        javaSet("819", "ISO8859-1", "l1", "ISO_8859-1:1987", "ISO_8859-1", "8859_1", "iso-ir-100",
            "latin1", "cp819", "ISO8859_1", "IBM819", "ISO_8859_1", "IBM-819", "csISOLatin1"))
  }
}
