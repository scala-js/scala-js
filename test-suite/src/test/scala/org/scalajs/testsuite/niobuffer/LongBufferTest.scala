/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.niobuffer

import java.nio._

import scala.scalajs.js
import js.JSConverters._

object LongBufferTest extends BaseBufferTest {
  type Factory = BufferFactory.LongBufferFactory

  def defineTests(factory: Factory): Unit = {
    commonTests(factory)
  }

  class AllocLongBufferFactory extends Factory {
    def allocBuffer(capacity: Int): LongBuffer =
      LongBuffer.allocate(capacity)
  }

  class WrappedLongBufferFactory extends Factory with BufferFactory.WrappedBufferFactory {
    def baseWrap(array: Array[Long]): LongBuffer =
      LongBuffer.wrap(array)

    def baseWrap(array: Array[Long], offset: Int, length: Int): LongBuffer =
      LongBuffer.wrap(array, offset, length)
  }

  class ByteBufferLongViewFactory(
      byteBufferFactory: BufferFactory.ByteBufferFactory,
      order: ByteOrder)
      extends Factory with BufferFactory.ByteBufferViewFactory {
    require(!byteBufferFactory.createsReadOnly)

    def baseAllocBuffer(capacity: Int): LongBuffer =
      byteBufferFactory.allocBuffer(capacity * 8).order(order).asLongBuffer()
  }

  describe("Allocated LongBuffer") {
    defineTests(new AllocLongBufferFactory)
  }

  describe("Wrapped LongBuffer") {
    defineTests(new WrappedLongBufferFactory)
  }

  describe("Read-only wrapped LongBuffer") {
    defineTests(new WrappedLongBufferFactory with BufferFactory.ReadOnlyBufferFactory)
  }

  describe("Sliced LongBuffer") {
    defineTests(new AllocLongBufferFactory with BufferFactory.SlicedBufferFactory)
  }

  for ((description, byteBufferFactory) <- ByteBufferFactories.WriteableByteBufferFactories) {
    for (order <- Seq(ByteOrder.BIG_ENDIAN, ByteOrder.LITTLE_ENDIAN)) {
      describe("Long view of " + description + " - " + order) {
        defineTests(new ByteBufferLongViewFactory(byteBufferFactory, order))
      }

      describe("Read-only Long view of " + description + " - " + order) {
        defineTests(new ByteBufferLongViewFactory(byteBufferFactory, order)
            with BufferFactory.ReadOnlyBufferFactory)
      }
    }
  }
}
