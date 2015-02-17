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

object ByteBufferTest extends BaseBufferTest {
  type Factory = BufferFactory.ByteBufferFactory

  def defineTests(factory: Factory): Unit = {
    commonTests(factory)
  }

  class AllocByteBufferFactory extends Factory {
    def allocBuffer(capacity: Int): ByteBuffer =
      ByteBuffer.allocate(capacity)
  }

  class WrappedByteBufferFactory extends Factory with BufferFactory.WrappedBufferFactory {
    def baseWrap(array: Array[Byte]): ByteBuffer =
      ByteBuffer.wrap(array)

    def baseWrap(array: Array[Byte], offset: Int, length: Int): ByteBuffer =
      ByteBuffer.wrap(array, offset, length)
  }

  describe("Allocated ByteBuffer") {
    defineTests(new AllocByteBufferFactory)
  }

  describe("Wrapped ByteBuffer") {
    defineTests(new WrappedByteBufferFactory)
  }

  describe("Read-only wrapped ByteBuffer") {
    defineTests(new WrappedByteBufferFactory with BufferFactory.ReadOnlyBufferFactory)
  }

  describe("Sliced ByteBuffer") {
    defineTests(new AllocByteBufferFactory with BufferFactory.SlicedBufferFactory)
  }
}
