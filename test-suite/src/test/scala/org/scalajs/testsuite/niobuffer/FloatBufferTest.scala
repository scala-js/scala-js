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

object FloatBufferTest extends BaseBufferTest {
  type Factory = BufferFactory.FloatBufferFactory

  def defineTests(factory: Factory): Unit = {
    commonTests(factory)
  }

  class AllocFloatBufferFactory extends Factory {
    def allocBuffer(capacity: Int): FloatBuffer =
      FloatBuffer.allocate(capacity)
  }

  class WrappedFloatBufferFactory extends Factory with BufferFactory.WrappedBufferFactory {
    def baseWrap(array: Array[Float]): FloatBuffer =
      FloatBuffer.wrap(array)

    def baseWrap(array: Array[Float], offset: Int, length: Int): FloatBuffer =
      FloatBuffer.wrap(array, offset, length)
  }

  class WrappedTypedArrayFloatBufferFactory
      extends Factory with BufferFactory.WrappedTypedArrayBufferFactory {
    def baseWrap(array: Array[Float]): FloatBuffer =
      TypedArrayBuffer.wrap(new Float32Array(array.toJSArray))
  }

  class ByteBufferFloatViewFactory(
      byteBufferFactory: BufferFactory.ByteBufferFactory,
      order: ByteOrder)
      extends Factory with BufferFactory.ByteBufferViewFactory {
    require(!byteBufferFactory.createsReadOnly)

    def baseAllocBuffer(capacity: Int): FloatBuffer =
      byteBufferFactory.allocBuffer(capacity * 4).order(order).asFloatBuffer()
  }

  describe("Allocated FloatBuffer") {
    defineTests(new AllocFloatBufferFactory)
  }

  describe("Wrapped FloatBuffer") {
    defineTests(new WrappedFloatBufferFactory)
  }

  describe("Read-only wrapped FloatBuffer") {
    defineTests(new WrappedFloatBufferFactory with BufferFactory.ReadOnlyBufferFactory)
  }

  when("typedarray").
  describe("Wrapped TypedArray FloatBuffer") {
    defineTests(new WrappedTypedArrayFloatBufferFactory)
  }

  describe("Sliced FloatBuffer") {
    defineTests(new AllocFloatBufferFactory with BufferFactory.SlicedBufferFactory)
  }

  for ((description, byteBufferFactory) <- ByteBufferFactories.WriteableByteBufferFactories) {
    for (order <- Seq(ByteOrder.BIG_ENDIAN, ByteOrder.LITTLE_ENDIAN)) {
      describe("Float view of " + description + " - " + order) {
        defineTests(new ByteBufferFloatViewFactory(byteBufferFactory, order))
      }

      describe("Read-only Float view of " + description + " - " + order) {
        defineTests(new ByteBufferFloatViewFactory(byteBufferFactory, order)
            with BufferFactory.ReadOnlyBufferFactory)
      }
    }
  }
}
