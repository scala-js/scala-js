/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package javalib

import scala.scalajs.js
import scala.scalajs.test.JasmineTest

object StringBufferTest extends JasmineTest {

  describe("java.lang.StringBuffer") {

    def newBuf = new java.lang.StringBuffer

    it("should respond to `append`") {
      expect(newBuf.append("asdf").toString).toEqual("asdf")
      expect(newBuf.append(null: AnyRef).toString).toEqual("null")
      expect(newBuf.append(null: String).toString).toEqual("null")
      expect(newBuf.append(null: CharSequence,0,2).toString).toEqual("nu")
      expect(newBuf.append(js.undefined).toString).toEqual("undefined")
      expect(newBuf.append(true).toString).toEqual("true")
      expect(newBuf.append('a').toString).toEqual("a")
      expect(newBuf.append(Array('a','b','c','d')).toString).toEqual("abcd")
      expect(newBuf.append(Array('a','b','c','d'), 1, 2).toString).toEqual("bc")
      expect(newBuf.append(4.toByte).toString).toEqual("4")
      expect(newBuf.append(304.toShort).toString).toEqual("304")
      expect(newBuf.append(100000).toString).toEqual("100000")
      expect(newBuf.append(2.5f).toString).toEqual("2.5")
      expect(newBuf.append(3.5).toString).toEqual("3.5")
    }

    it("should respond to `setCharAt`") {
      val buf = newBuf
      buf.append("foobar")

      buf.setCharAt(2, 'x')
      expect(buf.toString).toEqual("foxbar")

      buf.setCharAt(5, 'h')
      expect(buf.toString).toEqual("foxbah")

      expect(() => buf.setCharAt(-1, 'h')).toThrow
      expect(() => buf.setCharAt(6,  'h')).toThrow
    }

    it("should properly setLength") {
      val buf = newBuf
      buf.append("foobar")

      expect(() => buf.setLength(-3)).toThrow

      expect({ buf.setLength(3); buf.toString }).toEqual("foo")
      expect({ buf.setLength(6); buf.toString }).toEqual("foo\u0000\u0000\u0000")
    }

  }

  describe("java.lang.StringBuilder") {

    def newBuf = new java.lang.StringBuilder

    it("should respond to `append`") {
      expect(newBuf.append("asdf").toString).toEqual("asdf")
      expect(newBuf.append(null: AnyRef).toString).toEqual("null")
      expect(newBuf.append(null: String).toString).toEqual("null")
      expect(newBuf.append(null: CharSequence,0,2).toString).toEqual("nu")
      expect(newBuf.append(js.undefined).toString).toEqual("undefined")
      expect(newBuf.append(true).toString).toEqual("true")
      expect(newBuf.append('a').toString).toEqual("a")
      expect(newBuf.append(Array('a','b','c','d')).toString).toEqual("abcd")
      expect(newBuf.append(Array('a','b','c','d'), 1, 2).toString).toEqual("bc")
      expect(newBuf.append(4.toByte).toString).toEqual("4")
      expect(newBuf.append(304.toShort).toString).toEqual("304")
      expect(newBuf.append(100000).toString).toEqual("100000")
      expect(newBuf.append(2.5f).toString).toEqual("2.5")
      expect(newBuf.append(3.5).toString).toEqual("3.5")
    }

    it("should allow string interpolation to survive `null` and `undefined`") {
      expect(s"${null}").toEqual("null")
      expect(s"${js.undefined}").toEqual("undefined")
    }

    it("should respond to `setCharAt`") {
      val buf = newBuf
      buf.append("foobar")

      buf.setCharAt(2, 'x')
      expect(buf.toString).toEqual("foxbar")

      buf.setCharAt(5, 'h')
      expect(buf.toString).toEqual("foxbah")

      expect(() => buf.setCharAt(-1, 'h')).toThrow
      expect(() => buf.setCharAt(6,  'h')).toThrow
    }

    it("should properly setLength") {
      val buf = newBuf
      buf.append("foobar")

      expect(() => buf.setLength(-3)).toThrow

      expect({ buf.setLength(3); buf.toString }).toEqual("foo")
      expect({ buf.setLength(6); buf.toString }).toEqual("foo\u0000\u0000\u0000")
    }

  }
}
