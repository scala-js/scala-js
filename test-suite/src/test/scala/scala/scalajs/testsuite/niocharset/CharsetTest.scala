/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.testsuite.niocharset

import scala.language.implicitConversions

import java.nio._
import java.nio.charset._

import scala.scalajs.js
import scala.scalajs.niocharset.StandardCharsets._

import org.scalajs.jasminetest.JasmineTest

object CharsetTest extends JasmineTest {
  implicit def charsetAsJSAny(charset: Charset): js.Any =
    charset.asInstanceOf[js.Any]

  describe("java.nio.charset.Charset") {
    it("defaultCharset") {
      expect(Charset.defaultCharset()).toBe(UTF_8)
    }

    it("forName") {
      expect(Charset.forName("ISO-8859-1")).toBe(ISO_8859_1)
      expect(Charset.forName("Iso8859-1")).toBe(ISO_8859_1)
      expect(Charset.forName("iso_8859_1")).toBe(ISO_8859_1)
      expect(Charset.forName("LaTin1")).toBe(ISO_8859_1)
      expect(Charset.forName("l1")).toBe(ISO_8859_1)

      expect(Charset.forName("US-ASCII")).toBe(US_ASCII)
      expect(Charset.forName("Default")).toBe(US_ASCII)

      expect(Charset.forName("UTF-8")).toBe(UTF_8)
      expect(Charset.forName("utf-8")).toBe(UTF_8)
      expect(Charset.forName("UtF8")).toBe(UTF_8)
      expect(Charset.forName("UTF_8")).toBe(UTF_8)
      expect(Charset.forName("UTF-8")).toBe(UTF_8)

      expect(Charset.forName("UTF-16BE")).toBe(UTF_16BE)
      expect(Charset.forName("Utf_16BE")).toBe(UTF_16BE)
      expect(Charset.forName("UnicodeBigUnmarked")).toBe(UTF_16BE)

      expect(Charset.forName("UTF-16le")).toBe(UTF_16LE)
      expect(Charset.forName("Utf_16le")).toBe(UTF_16LE)
      expect(Charset.forName("UnicodeLittleUnmarked")).toBe(UTF_16LE)

      expect(Charset.forName("UTF-16")).toBe(UTF_16)
      expect(Charset.forName("Utf_16")).toBe(UTF_16)
      expect(Charset.forName("unicode")).toBe(UTF_16)
      expect(Charset.forName("UnicodeBig")).toBe(UTF_16)

      expect(() => Charset.forName("this-charset-does-not-exist")).toThrow
    }

    it("isSupported") {
      expect(Charset.isSupported("ISO-8859-1")).toBeTruthy
      expect(Charset.isSupported("US-ASCII")).toBeTruthy
      expect(Charset.isSupported("Default")).toBeTruthy
      expect(Charset.isSupported("utf-8")).toBeTruthy
      expect(Charset.isSupported("UnicodeBigUnmarked")).toBeTruthy
      expect(Charset.isSupported("Utf_16le")).toBeTruthy
      expect(Charset.isSupported("UTF-16")).toBeTruthy
      expect(Charset.isSupported("unicode")).toBeTruthy

      expect(Charset.isSupported("this-charset-does-not-exist")).toBeFalsy
    }
  }
}
