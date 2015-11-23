/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.typedarray

import scala.language.implicitConversions

import java.io.InputStream

import scala.scalajs.js
import scala.scalajs.js.typedarray._

import org.scalajs.jasminetest.JasmineTest

/** Tests for our implementation of java.io._ stream classes */
object ArrayBufferInputStreamTest extends JasmineTest {

  implicit def traversable2array[T](a: TraversableOnce[T]): js.Array[T] = {
    import js.JSConverters._
    a.toJSArray
  }

  implicit def array2array[T](a: Array[T]): js.Array[T] = {
    import js.JSConverters._
    a.toJSArray
  }

  when("typedarray").
  describe("scala.scalajs.js.typedarray.ArrayBufferInputStream") {
    byteArrayInputStreamLikeTests(seq => new ArrayBufferInputStream(
        new Int8Array(seq).buffer))
  }

  when("typedarray").
  describe("scala.scalajs.js.typedarray.ArrayBufferInputStream - with offset") {
    byteArrayInputStreamLikeTests { seq =>
      val off = 100
      val data = new Int8Array(seq.size + off)
      data.set(seq, off)

      new ArrayBufferInputStream(data.buffer, off, seq.size)
    }
  }

  def byteArrayInputStreamLikeTests(mkStream: Seq[Int] => InputStream): Unit = {
    val length = 50
    def newStream = mkStream(1 to length)

    it("should provide `read()`") {
      val stream = newStream

      for (i <- 1 to length)
        expect(stream.read()).toBe(i)

      for (_ <- 1 to 5)
        expect(stream.read()).toBe(-1)
    }

    it("should provide `read(buf)`") {
      val stream = newStream
      val buf = new Array[Byte](10)

      expect(stream.read(buf)).toBe(10)
      expect(buf).toEqual(1 to 10)

      expect(stream.skip(35)).toBe(35)

      expect(stream.read(buf)).toBe(5)
      expect(buf).toEqual((46 to 50) ++ (6 to 10))

      expect(stream.read(buf)).toBe(-1)
      expect(stream.read()).toBe(-1)
    }

    it("should provide full-argument `read`") {
      val stream = newStream
      val buf = new Array[Byte](20)

      expect(stream.read(buf, 10, 5)).toBe(5)
      expect(buf).toEqual(Seq.fill(10)(0) ++ (1 to 5) ++ Seq.fill(5)(0))

      expect(stream.read(buf, 0, 20)).toBe(20)
      expect(buf).toEqual(6 to 25)

      expect(stream.read(buf, 10, 0)).toBe(0)
      expect(buf).toEqual(6 to 25)

      expect(() => stream.read(buf, -1, 0)).toThrow
      expect(() => stream.read(buf, 0, -1)).toThrow
      expect(() => stream.read(buf, 100, 0)).toThrow
      expect(() => stream.read(buf, 10, 100)).toThrow
      expect(buf).toEqual(6 to 25)

      expect(stream.skip(20)).toBe(20)

      expect(stream.read(buf, 0, 10)).toBe(5)
      expect(buf).toEqual((46 to 50) ++ (11 to 25))

      expect(stream.read(buf, 0, 10)).toBe(-1)
      expect(stream.read(buf, 0,  0)).toBe(0)
      expect(buf).toEqual((46 to 50) ++ (11 to 25))
    }

    it("should provide `available`") {
      val stream = newStream

      def mySkip(n: Int) = for (_ <- 1 to n) expect(stream.read()).not.toBe(-1)
      def check(n: Int) = expect(stream.available).toBe(n)

      check(50)
      mySkip(5)
      check(45)
      expect(stream.skip(10)).toBe(10)
      check(35)
      mySkip(30)
      check(5)
      expect(stream.skip(20)).toBe(5)
      check(0)
    }

    it("should provide `skip`") {
      val stream = newStream

      expect(stream.skip(7)).toBe(7)

      for (i <- 8 to 32)
        expect(stream.read()).toBe(i)

      expect(stream.skip(0)).toBe(0)
      expect(stream.read()).toBe(33)
      expect(stream.skip(-4)).toBe(0)
      expect(stream.read()).toBe(34)

      expect(stream.skip(30)).toBe(16)
      expect(stream.skip(30)).toBe(0)
    }

    it("should return true from `markSupported`") {
      expect(newStream.markSupported).toBe(true)
    }

    it("should provide no-op `close`") {
      val stream = newStream

      for (i <- 1 to length) {
        stream.close()
        expect(stream.read()).toBe(i)
      }
    }

    it("should provide `mark`/`reset`") {
      val stream = newStream

      def read(range: Range) = for (i <- range) expect(stream.read()).toBe(i)

      read(1 to 10)
      stream.reset()  // mark must be 0 at creation
      read(1 to 5)
      stream.mark(length)
      read(6 to 22)
      stream.reset()
      read(6 to 20)
      stream.reset()
      read(6 to 25)
      stream.reset()
      expect(stream.skip(40)).toBe(40)
      stream.mark(length)
      read(46 to 50)
      stream.reset()
      read(46 to 50)
      stream.mark(length)
      expect(stream.read()).toBe(-1)
      stream.reset()
      expect(stream.read()).toBe(-1)
    }

    it("should return positive integers when calling read") {
      val stream = mkStream(Seq(-1, -2, -3))
      expect(stream.read()).toBe(255)
      expect(stream.read()).toBe(254)
      expect(stream.read()).toBe(253)
      expect(stream.read()).toBe(-1)
    }
  }
}
