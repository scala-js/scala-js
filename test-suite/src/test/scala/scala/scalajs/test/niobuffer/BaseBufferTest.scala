/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test.niobuffer

import scala.scalajs.test.JasmineTest

import java.nio._

abstract class BaseBufferTest extends JasmineTest {
  trait BaseFactory {
    type BufferType <: Buffer

    val createsReadOnly: Boolean = false

    def allocBuffer(capacity: Int): BufferType

    def allocBuffer(pos: Int, limit: Int, capacity: Int): BufferType = {
      val buf = allocBuffer(capacity)
      buf.limit(limit).position(pos)
      buf
    }
  }

  type Factory <: BaseFactory

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
  }
}
