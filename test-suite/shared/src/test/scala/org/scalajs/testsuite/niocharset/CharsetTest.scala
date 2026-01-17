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

import scala.annotation.tailrec

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.javalib.util.TrivialImmutableCollection
import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform._

class CharsetTest {
  def javaSet[A](elems: A*): java.util.Set[A] =
    new java.util.HashSet(TrivialImmutableCollection(elems: _*))

  /* "default" was removed as alias of US_ASCII in JDK 18 through JEP 400.
   * See https://openjdk.org/jeps/400#The-legacy-default-charset
   * TODO Maybe we should remove it as well in our implementation.
   */
  lazy val isDefaultSupported: Boolean =
    !executingInJVM || executingInJVMOnLowerThanJDK(18)

  @Test def defaultCharset(): Unit =
    assertSame(UTF_8, Charset.defaultCharset())

  @Test def forName(): Unit = {
    assertSame(ISO_8859_1, Charset.forName("ISO-8859-1"))
    assertSame(ISO_8859_1, Charset.forName("Iso8859-1"))
    assertSame(ISO_8859_1, Charset.forName("iso_8859_1"))
    assertSame(ISO_8859_1, Charset.forName("LaTin1"))
    assertSame(ISO_8859_1, Charset.forName("l1"))

    assertSame(US_ASCII, Charset.forName("US-ASCII"))

    if (isDefaultSupported)
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
    assertThrows(classOf[UnsupportedCharsetException], Charset.forName("UTF_8"))

    assertThrows(classOf[UnsupportedCharsetException],
        Charset.forName("this-charset-does-not-exist"))
  }

  @Test def isSupported(): Unit = {
    assertTrue(Charset.isSupported("ISO-8859-1"))
    assertTrue(Charset.isSupported("US-ASCII"))
    assertTrue(Charset.isSupported("utf-8"))
    assertTrue(Charset.isSupported("UnicodeBigUnmarked"))
    assertTrue(Charset.isSupported("Utf_16le"))
    assertTrue(Charset.isSupported("UTF-16"))
    assertTrue(Charset.isSupported("unicode"))

    assertEquals(isDefaultSupported, Charset.isSupported("Default"))

    assertFalse(Charset.isSupported("this-charset-does-not-exist"))
  }

  @Test def aliases(): Unit = {
    assertEquals(Charset.forName("UTF-8").aliases(),
        javaSet("UTF8", "unicode-1-1-utf-8"))
    assertEquals(Charset.forName("UTF-16").aliases(),
        javaSet("UTF_16", "unicode", "utf16", "UnicodeBig"))
    assertEquals(Charset.forName("UTF-16BE").aliases(),
        javaSet("X-UTF-16BE", "UTF_16BE", "ISO-10646-UCS-2",
            "UnicodeBigUnmarked"))
    assertEquals(Charset.forName("UTF-16LE").aliases(),
        javaSet("UnicodeLittleUnmarked", "UTF_16LE", "X-UTF-16LE"))

    val expectedUSAsciiAliases = javaSet("ANSI_X3.4-1968", "cp367", "csASCII",
        "iso-ir-6", "ASCII", "iso_646.irv:1983", "ANSI_X3.4-1986", "ascii7",
        "ISO_646.irv:1991", "ISO646-US", "IBM367", "646", "us")
    if (isDefaultSupported)
      expectedUSAsciiAliases.add("default")
    assertEquals(Charset.forName("US-ASCII").aliases(), expectedUSAsciiAliases)

    assertEquals(Charset.forName("ISO-8859-1").aliases(),
        javaSet("819", "ISO8859-1", "l1", "ISO_8859-1:1987", "ISO_8859-1", "8859_1",
            "iso-ir-100", "latin1", "cp819", "ISO8859_1", "IBM819", "ISO_8859_1",
            "IBM-819", "csISOLatin1"))
  }

  @Test def availableCharsets(): Unit = {
    val c = Charset.availableCharsets()

    /* - Check available charsets with case insensitive canonical name
     * - Check aliases are *not* present
     */

    assertSame(ISO_8859_1, c.get("IsO-8859-1"))
    assertNull(c.get("Iso8859-1"))
    assertNull(c.get("iso_8859_1"))
    assertNull(c.get("LaTin1"))
    assertNull(c.get("l1"))

    assertSame(US_ASCII, c.get("us-ASCII"))
    assertNull(c.get("Default"))

    assertSame(UTF_8, c.get("UTF-8"))
    assertNull(c.get("UtF8"))

    assertSame(UTF_16BE, c.get("UtF-16BE"))
    assertNull(c.get("Utf_16BE"))
    assertNull(c.get("UnicodeBigUnmarked"))

    assertSame(UTF_16LE, c.get("UtF-16le"))
    assertNull(c.get("Utf_16le"))
    assertNull(c.get("UnicodeLittleUnmarked"))

    assertSame(UTF_16, c.get("UtF-16"))
    assertNull(c.get("Utf_16"))
    assertNull(c.get("unicode"))
    assertNull(c.get("UnicodeBig"))

    // Check unavailable charsets & modification

    assertNull(c.get("this-charset-does-not-exist"))
    assertThrows(classOf[UnsupportedOperationException], c.put("my-charset", US_ASCII))

    // Check iteration: On the JVM we only assert the subsequence.

    val iter = c.entrySet().iterator()

    for (expect <- List(ISO_8859_1, US_ASCII, UTF_16, UTF_16BE, UTF_16LE, UTF_8)) {
      @tailrec
      def assertNext(): Unit = {
        assertTrue(iter.hasNext())
        val e = iter.next()
        if (!executingInJVM || (e.getValue() eq expect)) {
          assertSame(expect, e.getValue())
          assertEquals(expect.name, e.getKey())
        } else {
          assertNext()
        }
      }

      assertNext()
    }

    if (!executingInJVM)
      assertFalse(iter.hasNext())
  }
}
