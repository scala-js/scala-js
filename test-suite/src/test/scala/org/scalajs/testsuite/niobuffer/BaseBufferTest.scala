/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.niobuffer

import org.scalajs.jasminetest.JasmineTest

import java.nio._

import scala.scalajs.js
import js.JSConverters._

abstract class BaseBufferTest extends JasmineTest {

  type Factory <: BufferFactory

  def commonTests(factory: Factory): Unit = {
    import factory._

    it("allocate") {
      val buf = allocBuffer(10)
      expect(buf.position).toEqual(0)
      expect(buf.limit).toEqual(10)
      expect(buf.capacity).toEqual(10)

      expect(allocBuffer(0).capacity).toEqual(0)

      expect(() => allocBuffer(-1)).toThrow

      val buf2 = allocBuffer(1, 5, 9)
      expect(buf2.position()).toEqual(1)
      expect(buf2.limit()).toEqual(5)
      expect(buf2.capacity()).toEqual(9)
    }

    it("isReadOnly()") {
      val buf = allocBuffer(10)
      if (createsReadOnly)
        expect(buf.isReadOnly()).toBeTruthy
      else
        expect(buf.isReadOnly()).toBeFalsy
    }

    it("position") {
      val buf = allocBuffer(10)
      buf.position(3)
      expect(buf.position()).toEqual(3)
      buf.position(10)
      expect(buf.position()).toEqual(10)
      buf.position(0)
      expect(buf.position()).toEqual(0)

      expect(() => buf.position(-1)).toThrow
      expect(() => buf.position(11)).toThrow
      expect(buf.position()).toEqual(0)

      val buf2 = allocBuffer(1, 5, 9)
      expect(buf2.position()).toEqual(1)
      buf2.position(5)
      expect(buf2.position()).toEqual(5)
      expect(() => buf2.position(6)).toThrow
      expect(buf2.position()).toEqual(5)
    }

    it("limit") {
      val buf = allocBuffer(10)
      buf.position(3)
      buf.limit(7)
      expect(buf.limit()).toEqual(7)
      expect(buf.position()).toEqual(3)
      expect(() => buf.limit(11)).toThrow
      expect(buf.limit()).toEqual(7)
      expect(() => buf.limit(-1)).toThrow
      expect(buf.limit()).toEqual(7)
      expect(buf.position()).toEqual(3)

      buf.position(5)
      buf.limit(4)
      expect(buf.limit()).toEqual(4)
      expect(buf.position()).toEqual(4)
    }

    it("mark() and reset()") {
      val buf = allocBuffer(10)

      // Initially, the mark should not be set
      expect(() => buf.reset()).toThrow

      // Simple test
      buf.position(3)
      buf.mark()
      buf.position(8)
      buf.reset()
      expect(buf.position()).toEqual(3)

      // reset() should not have cleared the mark
      buf.position(5)
      buf.reset()
      expect(buf.position()).toEqual(3)

      // setting position() below the mark should clear the mark
      buf.position(2)
      expect(() => buf.reset()).toThrow
    }

    it("clear()") {
      val buf = allocBuffer(3, 6, 10)
      buf.mark()
      buf.position(4)

      buf.clear()
      expect(buf.position()).toEqual(0)
      expect(buf.limit()).toEqual(10) // the capacity
      expect(buf.capacity()).toEqual(10)
      expect(() => buf.reset()).toThrow
    }

    it("flip()") {
      val buf = allocBuffer(3, 6, 10)
      buf.mark()
      buf.position(4)

      buf.flip()
      expect(buf.position()).toEqual(0)
      expect(buf.limit()).toEqual(4) // old position
      expect(buf.capacity()).toEqual(10)
      expect(() => buf.reset()).toThrow
    }

    it("rewind()") {
      val buf = allocBuffer(3, 6, 10)
      buf.mark()
      buf.position(4)

      buf.rewind()
      expect(buf.position()).toEqual(0)
      expect(buf.limit()).toEqual(6) // unchanged
      expect(buf.capacity()).toEqual(10)
      expect(() => buf.reset()).toThrow
    }

    it("remaining() and hasRemaining()") {
      val buf = allocBuffer(3, 7, 10)
      expect(buf.remaining()).toEqual(7-3)
      expect(buf.hasRemaining()).toBeTruthy

      buf.position(6)
      expect(buf.remaining()).toEqual(7-6)
      expect(buf.hasRemaining()).toBeTruthy

      buf.limit(9)
      expect(buf.remaining()).toEqual(9-6)
      expect(buf.hasRemaining()).toBeTruthy

      buf.limit(2)
      expect(buf.remaining()).toEqual(0)
      expect(buf.hasRemaining()).toBeFalsy

      buf.position(0)
      expect(buf.remaining()).toEqual(2)
      expect(buf.hasRemaining()).toBeTruthy
    }

    it("absolute get()") {
      val buf = withContent(10, elemRange(0, 10): _*)
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
      val buf = withContent(10, elemRange(0, 10): _*)
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
        buf.put(5)
        expect(buf.position()).toEqual(1)
        expect(buf.get(0)).toEqual(5)

        buf.position(3)
        buf.put(36)
        expect(buf.position()).toEqual(4)
        expect(buf.get(3)).toEqual(36)

        buf.position(10)
        expect(() => buf.put(3)).toThrow
      }
    } else {
      it("relative put() - read-only") {
        val buf = allocBuffer(10)
        expect(() => buf.put(5)).toThrow
        expect(buf.position()).toEqual(0)
        expect(buf.get(0)).toEqual(0)

        buf.position(10)
        expect(() => buf.put(3)).toThrow
      }
    }

    it("relative bulk get()") {
      val buf = withContent(10, elemRange(0, 10): _*)
      val a = new Array[ElementType](4)
      buf.get(a)
      expect(a.map(elemToJSAny).toJSArray).toEqual(js.Array(0, 1, 2, 3))
      expect(buf.position()).toEqual(4)

      buf.position(6)
      buf.get(a, 1, 2)
      expect(a.map(elemToJSAny).toJSArray).toEqual(js.Array(0, 6, 7, 3))
      expect(buf.position()).toEqual(8)

      expect(() => buf.get(a)).toThrow
      expect(buf.position()).toEqual(8)
      expect(a.map(elemToJSAny).toJSArray).toEqual(js.Array(0, 6, 7, 3))
    }

    if (!createsReadOnly) {
      it("relative bulk put()") {
        val buf = allocBuffer(10)
        buf.put(Array[ElementType](6, 7, 12))
        expect((0 to 3).map(buf.get(_): js.Any).toJSArray).toEqual(js.Array(6, 7, 12, 0))
        expect(buf.position()).toEqual(3)

        buf.position(2)
        buf.put(Array[ElementType](44, 55, 66, 77, 88), 2, 2)
        expect((0 to 4).map(buf.get(_): js.Any).toJSArray).toEqual(js.Array(6, 7, 66, 77, 0))
        expect(buf.position()).toEqual(4)

        expect(() => buf.put(Array.fill[ElementType](10)(0))).toThrow
        expect(buf.position()).toEqual(4)
        expect((0 to 4).map(buf.get(_): js.Any).toJSArray).toEqual(js.Array(6, 7, 66, 77, 0))
      }
    } else {
      it("relative bulk put() - read-only") {
        val buf = allocBuffer(10)
        expect(() => buf.put(Array[ElementType](6, 7, 12))).toThrow
        expect(buf.position()).toEqual(0)
        expect(buf.get(0)).toEqual(0)

        buf.position(8)
        expect(() => buf.put(Array[ElementType](6, 7, 12))).toThrow
        expect(buf.position()).toEqual(8)
        expect(buf.get(8)).toEqual(0)
      }
    }

    if (!createsReadOnly) {
      it("compact()") {
        val buf = withContent(10, elemRange(0, 10): _*)
        buf.position(6)
        buf.mark()

        buf.compact()
        expect(buf.position()).toEqual(4)
        expect(buf.limit()).toEqual(10)
        expect(() => buf.reset()).toThrow

        for (i <- 0 until 4)
          expect(buf.get(i)).toEqual(i + 6)
      }
    } else {
      it("compact() - read-only") {
        val buf = allocBuffer(10)
        expect(() => buf.compact()).toThrow
      }
    }

    it("slice()" + (if (createsReadOnly) " - read-only" else "")) {
      val buf1 = withContent(10, elemRange(0, 10): _*)
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
        buf2.put(89)
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
      val buf1 = withContent(10, elemRange(0, 10): _*)
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
        buf2.put(89)
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
}
