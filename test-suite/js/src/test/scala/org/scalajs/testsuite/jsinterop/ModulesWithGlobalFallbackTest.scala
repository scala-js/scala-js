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

package org.scalajs.testsuite.jsinterop

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.typedarray._

import java.nio.{ByteBuffer, CharBuffer}

import org.junit.Assert._
import org.junit.Assume._
import org.junit.BeforeClass
import org.junit.Test

import org.scalajs.testsuite.utils.Platform._

/* !!! This is mostly copy-pasted from `ModulesTest.scala` in
 * `src/test/require-modules/`. This is the version with global fallbacks.
 */
class ModulesWithGlobalFallbackTest {
  import ModulesWithGlobalFallbackTest._

  @Test def testImportModuleItself(): Unit = {
    val qs = QueryString
    assertEquals("object", js.typeOf(qs))

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
    assertEquals("object", js.typeOf(qs))

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
    val b = Buffer.alloc(5)
    for (i <- 0 until 5)
      b(i) = (i * i).toShort

    for (i <- 0 until 5)
      assertEquals(i * i, b(i).toInt)
  }

  @Test def testImportIntegrated(): Unit = {
    val b = Buffer.from(js.Array[Short](0xe3, 0x81, 0x93, 0xe3, 0x82, 0x93,
        0xe3, 0x81, 0xab, 0xe3, 0x81, 0xa1, 0xe3, 0x81, 0xaf))
    val decoder = new StringDecoder()
    assertTrue(Buffer.isBuffer(b))
    assertFalse(Buffer.isBuffer(decoder))
    assertEquals("こんにちは", decoder.write(b))
    assertEquals("", decoder.end())
  }

}

object ModulesWithGlobalFallbackTest {
  private object QueryStringFallbackImpl extends js.Object {
    def stringify(obj: js.Dictionary[String], sep: String = "&",
        eq: String = "="): String = {
      var result = ""
      for ((key, value) <- obj) {
        if (result != "")
          result += sep
        result += key + eq + value
      }
      result
    }
  }

  private class StringDecoderFallbackImpl(charsetName: String = "utf8")
      extends js.Object {
    import java.nio.charset._

    private val charset = Charset.forName(charsetName)
    private val decoder = charset.newDecoder()

    private def writeInternal(buffer: Uint8Array,
        endOfInput: Boolean): String = {
      val in = TypedArrayBuffer.wrap(buffer.buffer, buffer.byteOffset,
          buffer.byteLength)

      // +2 so that a pending incomplete character has some space
      val out = CharBuffer.allocate(
          Math.ceil(decoder.maxCharsPerByte().toDouble * in.remaining()).toInt + 2)

      val result = decoder.decode(in, out, endOfInput)
      if (!result.isUnderflow())
        result.throwException()

      if (endOfInput) {
        val flushResult = decoder.flush(out)
        if (!flushResult.isUnderflow())
          flushResult.throwException()
      }

      out.flip()
      out.toString()
    }

    def write(buffer: Uint8Array): String =
      writeInternal(buffer, endOfInput = false)

    def end(buffer: Uint8Array): String =
      writeInternal(buffer, endOfInput = true)

    def end(): String =
      writeInternal(new Uint8Array(0), endOfInput = true)
  }

  object BufferStaticFallbackImpl extends js.Object {
    def alloc(size: Int): Any = new Uint8Array(size)
    def from(array: js.Array[Short]): Any = new Uint8Array(array)

    def isBuffer(x: Any): Boolean = x.isInstanceOf[Uint8Array]
  }

  @BeforeClass
  def beforeClass(): Unit = {
    assumeTrue("Assuming that Typed Arrays are supported",
        areTypedArraysSupported)

    if (isNoModule) {
      js.Dynamic.global.ModulesWithGlobalFallbackTest_QueryString =
        QueryStringFallbackImpl
      js.Dynamic.global.ModulesWithGlobalFallbackTest_StringDecoder =
        js.constructorOf[StringDecoderFallbackImpl]
      js.Dynamic.global.ModulesWithGlobalFallbackTest_Buffer =
        js.constructorOf[Uint8Array]
      js.Dynamic.global.ModulesWithGlobalFallbackTest_BufferStatic =
        BufferStaticFallbackImpl
    }
  }

  @js.native
  @JSImport("querystring", JSImport.Namespace,
      globalFallback = "ModulesWithGlobalFallbackTest_QueryString")
  object QueryString extends js.Object {
    def stringify(obj: js.Dictionary[String], sep: String = "&",
        eq: String = "="): String = js.native
  }

  @js.native
  @JSImport("querystring", JSImport.Default,
      globalFallback = "ModulesWithGlobalFallbackTest_QueryString")
  object QueryStringAsDefault extends js.Object {
    def stringify(obj: js.Dictionary[String], sep: String = "&",
        eq: String = "="): String = js.native
  }

  @js.native
  @JSImport("string_decoder", "StringDecoder",
      globalFallback = "ModulesWithGlobalFallbackTest_StringDecoder")
  class StringDecoder(encoding: String = "utf8") extends js.Object {
    def write(buffer: Buffer): String = js.native
    def end(buffer: Buffer): String = js.native
    def end(): String = js.native
  }

  @js.native
  @JSImport("buffer", "Buffer",
      globalFallback = "ModulesWithGlobalFallbackTest_Buffer")
  class Buffer private[this] () extends js.typedarray.Uint8Array(0)

  // This API requires Node.js >= v5.10.0
  @js.native
  @JSImport("buffer", "Buffer",
      globalFallback = "ModulesWithGlobalFallbackTest_BufferStatic")
  object Buffer extends js.Object {
    def alloc(size: Int): Buffer = js.native
    def from(array: js.Array[Short]): Buffer = js.native

    def isBuffer(x: Any): Boolean = js.native
  }
}
