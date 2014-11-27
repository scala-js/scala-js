/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.niobuffer

import java.nio._

import scala.scalajs.js
import js.JSConverters._

object CharBufferTest extends BaseBufferTest {
  trait Factory extends BaseFactory {
    type BufferType = CharBuffer

    def withContent(capacity: Int, content: Char*): CharBuffer =
      withContent(0, capacity, capacity, content: _*)

    def withContent(pos: Int, limit: Int, capacity: Int,
        content: Char*): CharBuffer = {
      val buf = allocBuffer(pos, limit, capacity)
      buf.put(content.toArray)
      buf.position(pos)
      buf
    }
  }

  def zeros(n: Int): String =
    "\u0000"*n

  def charRange(start: Int, end: Int): Array[Char] =
    (start until end).map(_.toChar).toArray

  def defineTests(factory: Factory): Unit = {
    import factory._

    commonTests(factory)

    it("absolute get()") {
      val buf = withContent(10, charRange(0, 10): _*)
      expect(buf.get(0)).toEqual(0)
      expect(buf.position()).toEqual(0)
      expect(buf.get(3)).toEqual(3)
      expect(buf.position()).toEqual(0)

      expect(() => buf.get(-1)).toThrow
      expect(() => buf.get(15)).toThrow

      buf.limit(4)
      expect(() => buf.get(5)).toThrow
    }


    if (!createsReadOnly) {
      it("absolute put()") {
        val buf = allocBuffer(10)
        buf.put(5, 42)
        expect(buf.position()).toEqual(0)
        buf.put(3, 2)
        expect(buf.get(3)).toEqual(2)
        expect(buf.get(5)).toEqual(42)
        expect(buf.get(7)).toEqual(0)

        expect(() => buf.put(-1, 2)).toThrow
        expect(() => buf.put(14, 9)).toThrow

        buf.limit(4)
        expect(() => buf.put(4, 1)).toThrow
      }
    } else {
      it("absolute put() - read-only") {
        val buf = allocBuffer(10)
        expect(() => buf.put(2, 1)).toThrow
        expect(buf.get(2)).toEqual(0)
        expect(buf.position()).toEqual(0)

        expect(() => buf.put(-2, 1)).toThrow
        expect(() => buf.put(12, 1)).toThrow
      }
    }

    it("relative get()") {
      val buf = withContent(10, charRange(0, 10): _*)
      expect(buf.get()).toEqual(0)
      expect(buf.position()).toEqual(1)
      buf.position(3)
      expect(buf.get()).toEqual(3)
      expect(buf.position()).toEqual(4)

      buf.limit(4)
      expect(() => buf.get()).toThrow
    }

    if (!createsReadOnly) {
      it("relative put()") {
        val buf = allocBuffer(10)
        buf.put(5.toChar)
        expect(buf.position()).toEqual(1)
        expect(buf.get(0)).toEqual(5)

        buf.position(3)
        buf.put(36.toChar)
        expect(buf.position()).toEqual(4)
        expect(buf.get(3)).toEqual(36)

        buf.position(10)
        expect(() => buf.put(3.toChar)).toThrow
      }
    } else {
      it("relative put() - read-only") {
        val buf = allocBuffer(10)
        expect(() => buf.put(5.toChar)).toThrow
        expect(buf.position()).toEqual(0)
        expect(buf.get(0)).toEqual(0)

        buf.position(10)
        expect(() => buf.put(3.toChar)).toThrow
      }
    }

    it("relative bulk get()") {
      val buf = withContent(10, charRange(0, 10): _*)
      val a = new Array[Char](4)
      buf.get(a)
      expect(a.map(_.toInt).toJSArray).toEqual(js.Array(0, 1, 2, 3))
      expect(buf.position()).toEqual(4)

      buf.position(6)
      buf.get(a, 1, 2)
      expect(a.map(_.toInt).toJSArray).toEqual(js.Array(0, 6, 7, 3))
      expect(buf.position()).toEqual(8)

      expect(() => buf.get(a)).toThrow
      expect(buf.position()).toEqual(8)
      expect(a.map(_.toInt).toJSArray).toEqual(js.Array(0, 6, 7, 3))
    }

    if (!createsReadOnly) {
      it("relative bulk put()") {
        val buf = allocBuffer(10)
        buf.put(Array[Char](6, 7, 12))
        expect((0 to 3).map(buf.get(_).toInt).toJSArray).toEqual(js.Array(6, 7, 12, 0))
        expect(buf.position()).toEqual(3)

        buf.position(2)
        buf.put(Array[Char](44, 55, 66, 77, 88), 2, 2)
        expect((0 to 4).map(buf.get(_).toInt).toJSArray).toEqual(js.Array(6, 7, 66, 77, 0))
        expect(buf.position()).toEqual(4)

        expect(() => buf.put(Array.fill[Char](10)(0))).toThrow
        expect(buf.position()).toEqual(4)
        expect((0 to 4).map(buf.get(_).toInt).toJSArray).toEqual(js.Array(6, 7, 66, 77, 0))
      }
    } else {
      it("relative bulk put() - read-only") {
        val buf = allocBuffer(10)
        expect(() => buf.put(Array[Char](6, 7, 12))).toThrow
        expect(buf.position()).toEqual(0)
        expect(buf.get(0)).toEqual(0)

        buf.position(8)
        expect(() => buf.put(Array[Char](6, 7, 12))).toThrow
        expect(buf.position()).toEqual(8)
        expect(buf.get(8)).toEqual(0)
      }
    }

    if (!createsReadOnly) {
      it("compact()") {
        val buf = withContent(10, charRange(0, 10): _*)
        buf.position(6)
        buf.mark()

        buf.compact()
        expect(buf.position()).toEqual(4)
        expect(buf.limit()).toEqual(10)
        expect(() => buf.reset()).toThrow

        for (i <- 0 until 4)
          expect(buf.get(i).toInt).toEqual(i + 6)
      }
    } else {
      it("compact() - read-only") {
        val buf = allocBuffer(10)
        expect(() => buf.compact()).toThrow
      }
    }

    it("slice()" + (if (createsReadOnly) " - read-only" else "")) {
      val buf1 = withContent(10, charRange(0, 10): _*)
      buf1.position(3)
      buf1.limit(7)
      buf1.mark()
      val buf2 = buf1.slice()
      expect(buf2.position()).toEqual(0)
      expect(buf2.limit()).toEqual(4)
      expect(buf2.capacity()).toEqual(4)
      expect(() => buf2.reset()).toThrow
      expect(buf2.get(1)).toEqual(4)

      buf2.position(2)
      expect(buf1.position()).toEqual(3)

      if (!createsReadOnly) {
        buf2.put(89.toChar)
        expect(buf1.get(5)).toEqual(89)
        expect(buf2.position()).toEqual(3)
        expect(buf1.position()).toEqual(3)
      }

      expect(() => buf2.limit(5)).toThrow
      expect(buf2.limit()).toEqual(4)

      buf2.limit(3)
      expect(buf1.limit()).toEqual(7)

      if (!createsReadOnly) {
        buf1.put(3, 23)
        expect(buf2.get(0)).toEqual(23)
      }
    }

    it("duplicate()" + (if (createsReadOnly) " - read-only" else "")) {
      val buf1 = withContent(10, charRange(0, 10): _*)
      buf1.position(3)
      buf1.limit(7)
      buf1.mark()
      val buf2 = buf1.duplicate()
      expect(buf2.position()).toEqual(3)
      expect(buf2.limit()).toEqual(7)
      expect(buf2.capacity()).toEqual(10)
      expect(buf2.get(4)).toEqual(4)

      buf2.position(4)
      expect(buf1.position()).toEqual(3)
      expect(buf2.position()).toEqual(4)

      buf2.reset()
      expect(buf2.position()).toEqual(3)
      buf2.position(4)

      if (!createsReadOnly) {
        buf2.put(89.toChar)
        expect(buf1.get(4)).toEqual(89)
        expect(buf2.position()).toEqual(5)
        expect(buf1.position()).toEqual(3)
      }

      buf2.limit(5)
      expect(buf1.limit()).toEqual(7)

      if (!createsReadOnly) {
        buf1.put(6, 23)
        buf2.limit(10)
        expect(buf2.get(6)).toEqual(23)
      }
    }
  }

  describe("Allocated CharBuffer") {
    defineTests(new Factory {
      def allocBuffer(capacity: Int): CharBuffer =
        CharBuffer.allocate(capacity)
    })
  }

  class WrappedCharBufferFactory extends Factory {
    def allocBuffer(capacity: Int): CharBuffer =
      CharBuffer.wrap(new Array[Char](capacity))

    override def allocBuffer(pos: Int, limit: Int, capacity: Int): CharBuffer =
      CharBuffer.wrap(new Array[Char](capacity), pos, limit-pos)

    override def withContent(pos: Int, limit: Int, capacity: Int,
        content: Char*): CharBuffer = {
      val after = capacity - (pos + content.size)
      CharBuffer.wrap(
          (Seq.fill(pos)(0.toChar) ++ content ++ Seq.fill(after)(0.toChar)).toArray,
          pos, limit-pos)
    }
  }

  describe("Wrapped CharBuffer") {
    defineTests(new WrappedCharBufferFactory)
  }

  describe("Read-only wrapped CharBuffer") {
    defineTests(new WrappedCharBufferFactory {
      override val createsReadOnly = true

      override def allocBuffer(capacity: Int): CharBuffer =
        super.allocBuffer(capacity).asReadOnlyBuffer()

      override def allocBuffer(pos: Int, limit: Int, capacity: Int): CharBuffer =
        super.allocBuffer(pos, limit, capacity).asReadOnlyBuffer()

      override def withContent(pos: Int, limit: Int, capacity: Int,
          content: Char*): CharBuffer =
        super.withContent(pos, limit, capacity, content: _*).asReadOnlyBuffer()
    })
  }

  describe("CharBuffer wrapping a CharSequence") {
    defineTests(new Factory {
      override val createsReadOnly = true

      def allocBuffer(capacity: Int): CharBuffer = {
        if (capacity < 0)
          throw new IllegalArgumentException
        CharBuffer.wrap(zeros(capacity))
      }

      override def allocBuffer(pos: Int, limit: Int, capacity: Int): CharBuffer = {
        if (capacity < 0)
          throw new IllegalArgumentException
        CharBuffer.wrap(zeros(capacity), pos, limit-pos)
      }

      override def withContent(pos: Int, limit: Int, capacity: Int,
          content: Char*): CharBuffer = {
        val after = capacity - (pos + content.size)
        CharBuffer.wrap(
            zeros(pos) + content.mkString + zeros(after),
            pos, limit-pos)
      }
    })
  }

  describe("Sliced CharBuffer") {
    defineTests(new Factory {
      def allocBuffer(capacity: Int): CharBuffer = {
        if (capacity < 0)
          throw new IllegalArgumentException
        val buf = CharBuffer.allocate(capacity+25)
        buf.position(17)
        buf.limit(17+capacity)
        buf.slice()
      }

      override def withContent(pos: Int, limit: Int, capacity: Int,
          content: Char*): CharBuffer = {
        if (!(0 <= pos && pos <= limit && limit <= capacity))
          throw new IllegalArgumentException
        val buf = CharBuffer.allocate(capacity+25)
        buf.position(9+pos)
        buf.put(content.toArray)
        buf.position(9)
        buf.limit(9+capacity)
        val buf2 = buf.slice()
        buf2.position(pos)
        buf2.limit(limit)
        buf2
      }
    })
  }

  describe("Sliced CharBuffer wrapping a CharSequence") {
    defineTests(new Factory {
      override val createsReadOnly = true

      def allocBuffer(capacity: Int): CharBuffer = {
        if (capacity < 0)
          throw new IllegalArgumentException
        val buf = CharBuffer.wrap(zeros(capacity+25))
        buf.position(17)
        buf.limit(17+capacity)
        buf.slice()
      }

      override def withContent(pos: Int, limit: Int, capacity: Int,
          content: Char*): CharBuffer = {
        if (!(0 <= pos && pos <= limit && limit <= capacity))
          throw new IllegalArgumentException
        val after = (25+capacity) - (9+pos+content.size)
        val buf = CharBuffer.wrap(zeros(9+pos) + content.mkString + zeros(after))
        buf.position(9)
        buf.limit(9+capacity)
        val buf2 = buf.slice()
        buf2.position(pos)
        buf2.limit(limit)
        buf2
      }
    })
  }
}
