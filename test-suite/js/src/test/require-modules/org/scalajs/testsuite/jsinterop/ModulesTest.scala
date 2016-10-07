/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2016, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.jsinterop

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.junit.Assert._
import org.junit.Test

/* This is currently hard-coded for Node.js modules in particular.
 * We are importing built-in Node.js modules, because we do not have any
 * infrastructure to load non-built-in modules. In the future, we should use
 * our own user-defined ES6 modules written in JavaScript.
 */
class ModulesTest {
  import ModulesTest._

  @Test def testImportModuleItself(): Unit = {
    val qs = QueryString
    assertTrue(qs.isInstanceOf[js.Object])

    val dict = js.Dictionary("foo" -> "bar", "baz" -> "qux")

    assertEquals("foo=bar&baz=qux", qs.stringify(dict))
    assertEquals("foo:bar;baz:qux", qs.stringify(dict, ";", ":"))

    /* Potentially, this could be "optimized" by importing `stringify` as a
     * global symbol if we are emitting ES2015 modules.
     */
    assertEquals("foo=bar&baz=qux", QueryString.stringify(dict))
    assertEquals("foo:bar;baz:qux", QueryString.stringify(dict, ";", ":"))
  }

  @Test def testImportLegacyModuleItselfAsDefault(): Unit = {
    val qs = QueryStringAsDefault
    assertTrue(qs.isInstanceOf[js.Object])

    val dict = js.Dictionary("foo" -> "bar", "baz" -> "qux")

    assertEquals("foo=bar&baz=qux", qs.stringify(dict))
    assertEquals("foo:bar;baz:qux", qs.stringify(dict, ";", ":"))

    /* Potentially, this could be "optimized" by importing `stringify` as a
     * global symbol if we are emitting ES2015 modules.
     */
    assertEquals("foo=bar&baz=qux", QueryStringAsDefault.stringify(dict))
    assertEquals("foo:bar;baz:qux", QueryStringAsDefault.stringify(dict, ";", ":"))
  }

  @Test def testImportObjectInModule(): Unit = {
    assertTrue((Buffer: Any).isInstanceOf[js.Object])
    assertFalse(Buffer.isBuffer(5))
  }

  @Test def testImportClassInModule(): Unit = {
    val b = new Buffer(5)
    for (i <- 0 until 5)
      b(i) = (i * i).toShort

    for (i <- 0 until 5)
      assertEquals(i * i, b(i).toInt)
  }

  @Test def testImportIntegrated(): Unit = {
    val b = new Buffer(js.Array[Short](0xe3, 0x81, 0x93, 0xe3, 0x82, 0x93, 0xe3,
        0x81, 0xab, 0xe3, 0x81, 0xa1, 0xe3, 0x81, 0xaf))
    val decoder = new StringDecoder()
    assertTrue(Buffer.isBuffer(b))
    assertFalse(Buffer.isBuffer(decoder))
    assertEquals("こんにちは", decoder.write(b))
    assertEquals("", decoder.end())
  }

}

object ModulesTest {
  @js.native
  @JSImport("querystring", JSImport.Namespace)
  object QueryString extends js.Object {
    def stringify(obj: js.Dictionary[String], sep: String = "&",
        eq: String = "="): String = js.native
  }

  @js.native
  @JSImport("querystring", JSImport.Default)
  object QueryStringAsDefault extends js.Object {
    def stringify(obj: js.Dictionary[String], sep: String = "&",
        eq: String = "="): String = js.native
  }

  @js.native
  @JSImport("string_decoder", "StringDecoder")
  class StringDecoder(encoding: String = "utf8") extends js.Object {
    def write(buffer: Buffer): String = js.native
    def end(buffer: Buffer): String = js.native
    def end(): String = js.native
  }

  /* To stay compatible with Node.js 4.2.1, we describe and use deprecated
   * APIs.
   */
  @js.native
  @JSImport("buffer", "Buffer")
  class Buffer private () extends js.typedarray.Uint8Array(0) {
    def this(size: Int) = this()
    def this(array: js.Array[Short]) = this()
  }

  @js.native
  @JSImport("buffer", "Buffer")
  object Buffer extends js.Object {
    def isBuffer(x: Any): Boolean = js.native
  }
}
