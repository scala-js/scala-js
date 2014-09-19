/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.testsuite.javalib

import java.io._

import scala.language.implicitConversions

import scala.scalajs.js

import org.scalajs.jasminetest.JasmineTest

/** Tests for our implementation of java.io._ stream classes */
object StreamsTest extends JasmineTest with CommonStreamsTests {

  // Need to define this again, otherwise conversion on function
  // triggers for Seqs
  override implicit def traversable2array[T](
      a: TraversableOnce[T]): js.Array[T] = super.traversable2array(a)

  describe("java.io.InputStream") {

    class DummyInputStream(val length: Int) extends InputStream {
      private var i: Int = 0
      def read(): Int = if (i < length) { i += 1; i } else -1
    }

    it("should provide a default implementation of `read` to an array") {
      val stream = new DummyInputStream(200)

      val buf = new Array[Byte](50)

      // Should read first 50 bytes (next: 51)
      expect(stream.read(buf)).toBe(50)
      expect(buf).toEqual((1 to 50))

      // Should read another 20 (next: 71)
      expect(stream.read(buf, 10, 20)).toBe(20)
      expect(buf).toEqual((1 to 10) ++ (51 to 70) ++ (31 to 50))

      // Test some Exception conditions
      expect(() => stream.read(null, 0, 10)).toThrow
      expect(() => stream.read(buf, -1, 10)).toThrow
      expect(() => stream.read(buf, 0, -1)).toThrow
      expect(() => stream.read(buf, 10, 100)).toThrow
      // Buffer should be unmodified
      expect(buf).toEqual((1 to 10) ++ (51 to 70) ++ (31 to 50))

      // Should read nothing (next: 71)
      expect(stream.read(buf, 10, 0)).toBe(0)
      expect(buf).toEqual((1 to 10) ++ (51 to 70) ++ (31 to 50))

      // Skip 40 bytes (next: 111)
      expect(stream.skip(40)).toBe(40)

      // Read 50 bytes, should wrap (next: 161)
      expect(stream.read(buf)).toBe(50)
      expect(buf).toEqual((111 to 127) ++ (-128 to -96))

      // Read 45 bytes, should read 40 (next: EOF)
      expect(stream.read(buf, 5, 45)).toBe(40)
      expect(buf).toEqual((111 to 115) ++ (-95 to -56) ++ (-100 to -96))

      // Read 50 bytes, should read nothing
      expect(stream.read(buf)).toBe(-1)
      expect(stream.read(buf, 0, 0)).toBe(0)
      expect(buf).toEqual((111 to 115) ++ (-95 to -56) ++ (-100 to -96))
    }

    it("should provide a default implementation of `skip`") {
      val stream = new DummyInputStream(10)

      expect(stream.skip(5)).toBe(5)
      expect(stream.read()).toBe(6)
      expect(stream.skip(1)).toBe(1)
      expect(stream.read()).toBe(8)
      expect(stream.skip(-5)).toBe(0)
      expect(stream.read()).toBe(9)
      expect(stream.skip(0)).toBe(0)
      expect(stream.read()).toBe(10)
      expect(stream.skip(10)).toBe(0)
    }

  }

  describe("java.io.ByteArrayInputStream") {
    byteArrayInputStreamLikeTests(seq =>
      new ByteArrayInputStream(seq.map(_.toByte).toArray))
  }

  describe("java.io.ByteArrayOutputStream") {

    it("should support simple write(x: Int)") {
      val out = new ByteArrayOutputStream()

      for (i <- 0 to 9)
        out.write(i)

      expect(out.toByteArray).toEqual(js.Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
    }

    it("should support simple write(x: Array[Byte])") {
      val out = new ByteArrayOutputStream()
      val arr = Array[Byte](0, 1, 2, 3, 4, 5)

      out.write(arr, 1, 4)
      out.write(arr)

      expect(out.toByteArray).toEqual(js.Array(1, 2, 3, 4, 0, 1, 2, 3, 4, 5))
    }

    it("should support write(x: Array[Byte]) with buffer resize") {
      val out = new ByteArrayOutputStream(16)
      val arr = Array[Byte](0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

      out.write(arr)
      out.write(arr)

      expect(out.toByteArray).toEqual(arr ++ arr)
    }

    it("should support toString (with UTF8)") {
      val buf = Array[Byte](72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100,
          46, -29, -127, -109, -29, -126, -109, -29, -127, -85, -29, -127, -95,
          -29, -127, -81, -26, -105, -91, -26, -100, -84, -24, -86, -98, -29,
          -126, -110, -24, -86, -83, -29, -126, -127, -29, -127, -66, -29, -127,
          -103, -29, -127, -117, -29, -128, -126)

      val out = new ByteArrayOutputStream()
      out.write(buf)

      expect(out.toString).toEqual("Hello World.こんにちは日本語を読めますか。")
    }

    it("should support reset()") {
      val out = new ByteArrayOutputStream()
      for (i <- 0 to 9) out.write(i)
      out.reset()
      for (i <- 0 to 9) out.write(i)

      expect(out.toByteArray).toEqual(js.Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
    }

  }

}

/** tests also used by typedarray.ArrayBufferInputStreamTests */
trait CommonStreamsTests extends JasmineTest {

  implicit def traversable2array[T](a: TraversableOnce[T]): js.Array[T] = {
    import js.JSConverters._
    a.toJSArray
  }

  implicit def array2array[T](a: Array[T]): js.Array[T] = {
    import js.JSConverters._
    a.toJSArray
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
