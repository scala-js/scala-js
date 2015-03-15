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
import js.typedarray._
import js.JSConverters._

object ShortBufferTest extends BaseBufferTest {
  type Factory = BufferFactory.ShortBufferFactory

  def defineTests(factory: Factory): Unit = {
    commonTests(factory)
  }

  class AllocShortBufferFactory extends Factory {
    def allocBuffer(capacity: Int): ShortBuffer =
      ShortBuffer.allocate(capacity)
  }

  class WrappedShortBufferFactory extends Factory with BufferFactory.WrappedBufferFactory {
    def baseWrap(array: Array[Short]): ShortBuffer =
      ShortBuffer.wrap(array)

    def baseWrap(array: Array[Short], offset: Int, length: Int): ShortBuffer =
      ShortBuffer.wrap(array, offset, length)
  }

  class WrappedTypedArrayShortBufferFactory
      extends Factory with BufferFactory.WrappedTypedArrayBufferFactory {
    def baseWrap(array: Array[Short]): ShortBuffer =
      TypedArrayBuffer.wrap(new Int16Array(array.toJSArray))
  }

  class ByteBufferShortViewFactory(
      byteBufferFactory: BufferFactory.ByteBufferFactory,
      order: ByteOrder)
      extends Factory with BufferFactory.ByteBufferViewFactory {
    require(!byteBufferFactory.createsReadOnly)

    def baseAllocBuffer(capacity: Int): ShortBuffer =
      byteBufferFactory.allocBuffer(capacity * 2).order(order).asShortBuffer()
  }

  describe("Allocated ShortBuffer") {
    defineTests(new AllocShortBufferFactory)
  }

  describe("Wrapped ShortBuffer") {
    defineTests(new WrappedShortBufferFactory)
  }

  describe("Read-only wrapped ShortBuffer") {
    defineTests(new WrappedShortBufferFactory with BufferFactory.ReadOnlyBufferFactory)
  }

  when("typedarray").
  describe("Wrapped TypedArray ShortBuffer") {
    defineTests(new WrappedTypedArrayShortBufferFactory)
  }

  describe("Sliced ShortBuffer") {
    defineTests(new AllocShortBufferFactory with BufferFactory.SlicedBufferFactory)
  }

  for ((description, byteBufferFactory) <- ByteBufferFactories.WriteableByteBufferFactories) {
    for (order <- Seq(ByteOrder.BIG_ENDIAN, ByteOrder.LITTLE_ENDIAN)) {
      describe("Short view of " + description + " - " + order) {
        defineTests(new ByteBufferShortViewFactory(byteBufferFactory, order))
      }

      describe("Read-only Short view of " + description + " - " + order) {
        defineTests(new ByteBufferShortViewFactory(byteBufferFactory, order)
            with BufferFactory.ReadOnlyBufferFactory)
      }
    }
  }
}
