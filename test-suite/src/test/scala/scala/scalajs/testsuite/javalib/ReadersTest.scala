/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.testsuite.javalib

import scala.annotation.tailrec

import java.io._

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

import org.scalajs.jasminetest.JasmineTest

/** Tests for our implementation of java.io._ reader classes */
object ReadersTest extends JasmineTest {

  describe("java.io.StringReader") {
    val str = "asdf"
    def newReader = new StringReader(str)

    it("should provide read()") {
      val r = newReader

      for (c <- str) {
        expect(r.read().toChar).toEqual(c)
      }

      expect(r.read()).toEqual(-1)
    }

    it("should provide read(buf: Array[Char], off: Int, len: Int)") {
      val r = newReader
      val buf = new Array[Char](10)

      expect(r.read(buf, 2, 8)).toBe(4)
      expect(buf.map(_.toInt).toJSArray).toEqual(
        js.Array[Int](0,0,'a','s','d','f',0,0,0,0))
    }

    it("should provide read(java.nio.CharBuffer)") {
      val r = newReader
      val buf0 = java.nio.CharBuffer.allocate(25)
      buf0.position(3)
      val buf = buf0.slice()
      buf.position(4)
      buf.limit(14)

      expect(r.read(buf)).toBe(4)
      expect(buf.position()).toBe(8)
      buf.flip()
      expect(buf.toString().map(_.toInt).toJSArray).toEqual(
          js.Array[Int](0, 0, 0, 0, 'a', 's', 'd', 'f'))
    }

    it("should provide ready") {
      val r = newReader

      for (c <- str) {
        expect(r.ready()).toBeTruthy
        expect(r.read().toChar).toEqual(c)
      }

      expect(r.ready()).toBeFalsy
      expect(r.read()).toEqual(-1)
    }

    it("should provide mark/reset") {
      val r = newReader
      r.mark(str.length)

      for (c <- str) {
        expect(r.read().toChar).toEqual(c)
      }
      expect(r.read()).toEqual(-1)

      r.reset()

      for (c <- str) {
        expect(r.read().toChar).toEqual(c)
      }
      expect(r.read()).toEqual(-1)
    }

    it("should provide skip") {
      val r = newReader

      expect(r.read()).toEqual('a')
      expect(r.skip(2L).toInt).toBe(2)

      expect(r.read()).toEqual('f')
      expect(r.read()).toEqual(-1)
    }

    it("should provide close") {
      val r = newReader

      r.close()
      expect(() => r.read()).toThrow
    }

    it("should support marking") {
      expect(newReader.markSupported).toBeTruthy
    }
  }

  describe("java.io.BufferedReader") {
    val str = "line1\nline2\r\n\nline4\rline5"
    def newReader = new BufferedReader(new StringReader(str), 3)

    it("should provide read()") {
      val r = newReader

      for (c <- str) {
        expect(r.read().toChar).toEqual(c)
      }
      expect(r.read()).toEqual(-1)
    }

    it("should provide read(cbuf)") {
      var read = 0
      val r = newReader
      val buf = new Array[Char](15)

      // twice to force filling internal buffer
      for (_ <- 0 to 1) {
        val len = r.read(buf)
        expect(len).toBeGreaterThan(0)

        for (i <- 0 until len)
          expect(buf(i)).toEqual(str.charAt(i+read))

        read += len
      }
    }

    it("should provide read(cbuf, off, len)") {
      var read = 0
      val r = newReader
      val buf = new Array[Char](15)

      // twice to force filling internal buffer
      for (_ <- 0 to 1) {
        val len = r.read(buf, 1, 10)
        expect(len).toBeGreaterThan(0)
        expect(len).toBeLessThan(11)

        for (i <- 0 until len)
          expect(buf(i+1)).toEqual(str.charAt(i+read))

        read += len
      }
    }

    it("should provide mark/reset") {
      val r = newReader
      expect(r.read()).toEqual('l')

      // force moving and resizing buffer
      r.mark(10)

      for (i <- 0 until 10) {
        expect(r.read()).toEqual(str.charAt(i+1))
      }

      r.reset()

      for (i <- 1 until str.length) {
        expect(r.read()).toEqual(str.charAt(i))
      }
    }

    it("should provide readLine") {
      val r = newReader

      expect(r.readLine()).toEqual("line1")
      expect(r.readLine()).toEqual("line2")
      expect(r.readLine()).toEqual("")
      expect(r.readLine()).toEqual("line4")
      expect(r.readLine()).toEqual("line5")
      expect(r.readLine()).toEqual(null)
    }

    it("should readLine on an empty stream") {
      val r = new BufferedReader(new StringReader(""))

      expect(r.readLine()).toEqual(null)
    }

    it("should readline with empty lines only") {
      val r = new BufferedReader(new StringReader("\n\r\n\r\r\n"), 1)

      for (_ <- 1 to 4)
        expect(r.readLine()).toEqual("")

      expect(r.readLine()).toEqual(null)
    }

    it("should support marking") {
      expect(newReader.markSupported).toBeTruthy
    }
  }

  describe("java.io.InputStreamReader") {

    it("should read UTF8") {

      val buf = Array[Byte](72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100,
          46, -29, -127, -109, -29, -126, -109, -29, -127, -85, -29, -127, -95,
          -29, -127, -81, -26, -105, -91, -26, -100, -84, -24, -86, -98, -29,
          -126, -110, -24, -86, -83, -29, -126, -127, -29, -127, -66, -29, -127,
          -103, -29, -127, -117, -29, -128, -126)

      val r = new InputStreamReader(new ByteArrayInputStream(buf))

      def expectRead(str: String) = {
        val buf = new Array[Char](str.length)
        @tailrec
        def readAll(readSoFar: Int): Int = {
          if (readSoFar == buf.length) readSoFar
          else {
            val newlyRead = r.read(buf, readSoFar, buf.length - readSoFar)
            if (newlyRead == -1) readSoFar
            else readAll(readSoFar + newlyRead)
          }
        }
        expect(readAll(0)).toBe(str.length)
        expect(new String(buf)).toEqual(str)
      }

      expectRead("Hello World.")
      expectRead("こんにちは")
      expectRead("日本語を読めますか。")
      expect(r.read()).toBe(-1)

    }

  }

}
