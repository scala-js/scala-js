/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib

import scala.reflect.{classTag, ClassTag}
import scala.scalajs.js
import org.scalajs.jasminetest.JasmineTest

object StringBufferTest extends JasmineTest {

  def shouldThrow[T: ClassTag](fn: => Unit): Unit = {
    try {
      fn
      expect("exception").toBe("thrown")
    } catch {
      case e: T =>
      case x: Throwable => expect(x.toString).toBe(classTag[T].runtimeClass.getSimpleName)
    }
  }

  describe("java.lang.StringBuffer") {

    def newBuf: java.lang.StringBuffer =
      new java.lang.StringBuffer

    def initBuf(str: String): java.lang.StringBuffer =
      new java.lang.StringBuffer(str)

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

    it("should respond to `insert`") {
      expect(newBuf.insert(0, "asdf").toString).toEqual("asdf")
      expect(newBuf.insert(0, null: AnyRef).toString).toEqual("null")
      expect(newBuf.insert(0, null: String).toString).toEqual("null")
      expect(newBuf.insert(0, null: CharSequence,0,2).toString).toEqual("nu")
      expect(newBuf.insert(0, js.undefined).toString).toEqual("undefined")
      expect(newBuf.insert(0, true).toString).toEqual("true")
      expect(newBuf.insert(0, 'a').toString).toEqual("a")
      expect(newBuf.insert(0, Array('a','b','c','d')).toString).toEqual("abcd")
      expect(newBuf.insert(0, Array('a','b','c','d'), 1, 2).toString).toEqual("bc")
      expect(newBuf.insert(0, 4.toByte).toString).toEqual("4")
      expect(newBuf.insert(0, 304.toShort).toString).toEqual("304")
      expect(newBuf.insert(0, 100000).toString).toEqual("100000")
      expect(newBuf.insert(0, 2.5f).toString).toEqual("2.5")
      expect(newBuf.insert(0, 3.5).toString).toEqual("3.5")

      expect(initBuf("adef").insert(1, "bc")).toEqual("abcdef")
      expect(initBuf("abcd").insert(4, "ef")).toEqual("abcdef")
      expect(initBuf("adef").insert(1, Array('b','c'))).toEqual("abcdef")
      expect(initBuf("adef").insert(1, initBuf("bc"))).toEqual("abcdef")
      expect(initBuf("abef").insert(2, Array('a','b','c','d','e'), 2, 2)).toEqual("abcdef")
      expect(initBuf("abef").insert(2, initBuf("abcde"), 2, 4)).toEqual("abcdef")

      shouldThrow[StringIndexOutOfBoundsException](initBuf("abcd").insert(5, "whatever"))
      shouldThrow[StringIndexOutOfBoundsException](initBuf("abcd").insert(-1, "whatever"))
    }

    it("should respond to `deleteCharAt`") {
      expect(initBuf("0123").deleteCharAt(1).toString).toEqual("023")
      expect(initBuf("0123").deleteCharAt(0).toString).toEqual("123")
      expect(initBuf("0123").deleteCharAt(3).toString).toEqual("012")
      shouldThrow[StringIndexOutOfBoundsException](initBuf("0123").deleteCharAt(-1))
      shouldThrow[StringIndexOutOfBoundsException](initBuf("0123").deleteCharAt(4))
    }

    it("should respond to `replace`") {
      expect(initBuf("0123").replace(1,3,"bc").toString).toEqual("0bc3")
      expect(initBuf("0123").replace(0,4,"abcd").toString).toEqual("abcd")
      expect(initBuf("0123").replace(0,10,"abcd").toString).toEqual("abcd")
      expect(initBuf("0123").replace(3,10,"defg").toString).toEqual("012defg")
      expect(initBuf("0123").replace(0,1,"xxxx").toString).toEqual("xxxx123")
      expect(initBuf("0123").replace(1,1,"xxxx").toString).toEqual("0xxxx123")

      shouldThrow[StringIndexOutOfBoundsException](initBuf("0123").replace(-1,3,"x"))
      shouldThrow[StringIndexOutOfBoundsException](initBuf("0123").replace(4,5,"x"))
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

    it("should respond to `appendCodePoint`") {
      val buf = newBuf
      buf.appendCodePoint(0x61)
      expect(buf.toString).toEqual("a")
      buf.appendCodePoint(0x10000)
      expect(buf.toString).toEqual("a\uD800\uDC00")
      buf.append("fixture")
      buf.appendCodePoint(0x00010FFFF)
      expect(buf.toString).toEqual("a\uD800\uDC00fixture\uDBFF\uDFFF")
    }

  }

  describe("java.lang.StringBuilder") {

    def newBuilder: java.lang.StringBuilder =
      new java.lang.StringBuilder

    def initBuilder(str: String): java.lang.StringBuilder =
      new java.lang.StringBuilder(str)

    it("should respond to `append`") {
      expect(newBuilder.append("asdf").toString).toEqual("asdf")
      expect(newBuilder.append(null: AnyRef).toString).toEqual("null")
      expect(newBuilder.append(null: String).toString).toEqual("null")
      expect(newBuilder.append(null: CharSequence,0,2).toString).toEqual("nu")
      expect(newBuilder.append(js.undefined).toString).toEqual("undefined")
      expect(newBuilder.append(true).toString).toEqual("true")
      expect(newBuilder.append('a').toString).toEqual("a")
      expect(newBuilder.append(Array('a','b','c','d')).toString).toEqual("abcd")
      expect(newBuilder.append(Array('a','b','c','d'), 1, 2).toString).toEqual("bc")
      expect(newBuilder.append(4.toByte).toString).toEqual("4")
      expect(newBuilder.append(304.toShort).toString).toEqual("304")
      expect(newBuilder.append(100000).toString).toEqual("100000")
      expect(newBuilder.append(2.5f).toString).toEqual("2.5")
      expect(newBuilder.append(3.5).toString).toEqual("3.5")
    }

    it("should respond to `insert`") {
      expect(newBuilder.insert(0, "asdf").toString).toEqual("asdf")
      expect(newBuilder.insert(0, null: AnyRef).toString).toEqual("null")
      expect(newBuilder.insert(0, null: String).toString).toEqual("null")
      expect(newBuilder.insert(0, null: CharSequence,0,2).toString).toEqual("nu")
      expect(newBuilder.insert(0, js.undefined).toString).toEqual("undefined")
      expect(newBuilder.insert(0, true).toString).toEqual("true")
      expect(newBuilder.insert(0, 'a').toString).toEqual("a")
      expect(newBuilder.insert(0, Array('a','b','c','d')).toString).toEqual("abcd")
      expect(newBuilder.insert(0, Array('a','b','c','d'), 1, 2).toString).toEqual("bc")
      expect(newBuilder.insert(0, 4.toByte).toString).toEqual("4")
      expect(newBuilder.insert(0, 304.toShort).toString).toEqual("304")
      expect(newBuilder.insert(0, 100000).toString).toEqual("100000")
      expect(newBuilder.insert(0, 2.5f).toString).toEqual("2.5")
      expect(newBuilder.insert(0, 3.5).toString).toEqual("3.5")

      expect(initBuilder("adef").insert(1, "bc")).toEqual("abcdef")
      expect(initBuilder("abcd").insert(4, "ef")).toEqual("abcdef")
      expect(initBuilder("adef").insert(1, Array('b','c'))).toEqual("abcdef")
      expect(initBuilder("adef").insert(1, initBuilder("bc"))).toEqual("abcdef")
      expect(initBuilder("abef").insert(2, Array('a','b','c','d','e'), 2, 2)).toEqual("abcdef")
      expect(initBuilder("abef").insert(2, initBuilder("abcde"), 2, 4)).toEqual("abcdef")

      shouldThrow[StringIndexOutOfBoundsException](initBuilder("abcd").insert(5, "whatever"))
      shouldThrow[StringIndexOutOfBoundsException](initBuilder("abcd").insert(-1, "whatever"))
    }

    it("should allow string interpolation to survive `null` and `undefined`") {
      expect(s"${null}").toEqual("null")
      expect(s"${js.undefined}").toEqual("undefined")
    }

    it("should respond to `deleteCharAt`") {
      expect(initBuilder("0123").deleteCharAt(1).toString).toEqual("023")
      expect(initBuilder("0123").deleteCharAt(0).toString).toEqual("123")
      expect(initBuilder("0123").deleteCharAt(3).toString).toEqual("012")
      shouldThrow[StringIndexOutOfBoundsException](initBuilder("0123").deleteCharAt(-1))
      shouldThrow[StringIndexOutOfBoundsException](initBuilder("0123").deleteCharAt(4))
    }

    it("should respond to `replace`") {
      expect(initBuilder("0123").replace(1,3,"bc").toString).toEqual("0bc3")
      expect(initBuilder("0123").replace(0,4,"abcd").toString).toEqual("abcd")
      expect(initBuilder("0123").replace(0,10,"abcd").toString).toEqual("abcd")
      expect(initBuilder("0123").replace(3,10,"defg").toString).toEqual("012defg")
      expect(initBuilder("0123").replace(0,1,"xxxx").toString).toEqual("xxxx123")
      expect(initBuilder("0123").replace(1,1,"xxxx").toString).toEqual("0xxxx123")

      shouldThrow[StringIndexOutOfBoundsException](initBuilder("0123").replace(-1,3,"x"))
      shouldThrow[StringIndexOutOfBoundsException](initBuilder("0123").replace(4,5,"x"))
    }

    it("should respond to `setCharAt`") {
      val b = newBuilder
      b.append("foobar")

      b.setCharAt(2, 'x')
      expect(b.toString).toEqual("foxbar")

      b.setCharAt(5, 'h')
      expect(b.toString).toEqual("foxbah")

      expect(() => b.setCharAt(-1, 'h')).toThrow
      expect(() => b.setCharAt(6,  'h')).toThrow
    }

    it("should properly setLength") {
      val b = newBuilder
      b.append("foobar")

      expect(() => b.setLength(-3)).toThrow

      expect({ b.setLength(3); b.toString }).toEqual("foo")
      expect({ b.setLength(6); b.toString }).toEqual("foo\u0000\u0000\u0000")
    }

    it("should respond to `appendCodePoint`") {
      val b = newBuilder
      b.appendCodePoint(0x61)
      expect(b.toString).toEqual("a")
      b.appendCodePoint(0x10000)
      expect(b.toString).toEqual("a\uD800\uDC00")
      b.append("fixture")
      b.appendCodePoint(0x00010FFFF)
      expect(b.toString).toEqual("a\uD800\uDC00fixture\uDBFF\uDFFF")
    }
  }
}
