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
  type Factory = BufferFactory.CharBufferFactory

  def zeros(n: Int): String =
    "\u0000"*n

  def defineTests(factory: Factory): Unit = {
    commonTests(factory)
  }

  class AllocCharBufferFactory extends Factory {
    def allocBuffer(capacity: Int): CharBuffer =
      CharBuffer.allocate(capacity)
  }

  class WrappedCharBufferFactory extends Factory with BufferFactory.WrappedBufferFactory {
    def baseWrap(array: Array[Char]): CharBuffer =
      CharBuffer.wrap(array)

    def baseWrap(array: Array[Char], offset: Int, length: Int): CharBuffer =
      CharBuffer.wrap(array, offset, length)
  }

  describe("Allocated CharBuffer") {
    defineTests(new AllocCharBufferFactory)
  }

  describe("Wrapped CharBuffer") {
    defineTests(new WrappedCharBufferFactory)
  }

  describe("Read-only wrapped CharBuffer") {
    defineTests(new WrappedCharBufferFactory with BufferFactory.ReadOnlyBufferFactory)
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
    defineTests(new AllocCharBufferFactory with BufferFactory.SlicedBufferFactory)
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
