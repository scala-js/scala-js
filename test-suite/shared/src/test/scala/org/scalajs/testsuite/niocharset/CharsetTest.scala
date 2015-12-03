/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.niocharset

import java.nio.charset._

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform.executingInJVM

class CharsetTest {

  @Test def defaultCharset(): Unit = {
    assertSame("UTF-8", Charset.defaultCharset().name())
  }

  @Test def forName(): Unit = {
    assertEquals("ISO-8859-1", Charset.forName("ISO-8859-1").name())
    assertEquals("ISO-8859-1", Charset.forName("Iso8859-1").name())
    assertEquals("ISO-8859-1", Charset.forName("iso_8859_1").name())
    assertEquals("ISO-8859-1", Charset.forName("LaTin1").name())
    assertEquals("ISO-8859-1", Charset.forName("l1").name())

    assertEquals("US-ASCII", Charset.forName("US-ASCII").name())
    assertEquals("US-ASCII", Charset.forName("Default").name())

    assertEquals("UTF-8", Charset.forName("UTF-8").name())
    assertEquals("UTF-8", Charset.forName("utf-8").name())
    assertEquals("UTF-8", Charset.forName("UtF8").name())

    assertEquals("UTF-16BE", Charset.forName("UTF-16BE").name())
    assertEquals("UTF-16BE", Charset.forName("Utf_16BE").name())
    assertEquals("UTF-16BE", Charset.forName("UnicodeBigUnmarked").name())

    assertEquals("UTF-16LE", Charset.forName("UTF-16le").name())
    assertEquals("UTF-16LE", Charset.forName("Utf_16le").name())
    assertEquals("UTF-16LE", Charset.forName("UnicodeLittleUnmarked").name())

    assertEquals("UTF-16", Charset.forName("UTF-16").name())
    assertEquals("UTF-16", Charset.forName("Utf_16").name())
    assertEquals("UTF-16", Charset.forName("unicode").name())
    assertEquals("UTF-16", Charset.forName("UnicodeBig").name())

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
}
