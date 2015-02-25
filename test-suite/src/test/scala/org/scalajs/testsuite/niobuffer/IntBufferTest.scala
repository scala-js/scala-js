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

object IntBufferTest extends BaseBufferTest {
  type Factory = BufferFactory.IntBufferFactory

  def defineTests(factory: Factory): Unit = {
    commonTests(factory)
  }

  class AllocIntBufferFactory extends Factory {
    def allocBuffer(capacity: Int): IntBuffer =
      IntBuffer.allocate(capacity)
  }

  class WrappedIntBufferFactory extends Factory with BufferFactory.WrappedBufferFactory {
    def baseWrap(array: Array[Int]): IntBuffer =
      IntBuffer.wrap(array)

    def baseWrap(array: Array[Int], offset: Int, length: Int): IntBuffer =
      IntBuffer.wrap(array, offset, length)
  }

  class WrappedTypedArrayIntBufferFactory
      extends Factory with BufferFactory.WrappedTypedArrayBufferFactory {
    def baseWrap(array: Array[Int]): IntBuffer =
      TypedArrayBuffer.wrap(new Int32Array(array.toJSArray))
  }

  class ByteBufferIntViewFactory(
      byteBufferFactory: BufferFactory.ByteBufferFactory)
      extends Factory with BufferFactory.ByteBufferViewFactory {
    require(!byteBufferFactory.createsReadOnly)

    def baseAllocBuffer(capacity: Int): IntBuffer =
      byteBufferFactory.allocBuffer(capacity * 4).asIntBuffer()
  }

  describe("Allocated IntBuffer") {
    defineTests(new AllocIntBufferFactory)
  }

  describe("Wrapped IntBuffer") {
    defineTests(new WrappedIntBufferFactory)
  }

  describe("Read-only wrapped IntBuffer") {
    defineTests(new WrappedIntBufferFactory with BufferFactory.ReadOnlyBufferFactory)
  }

  when("typedarray").
  describe("Wrapped TypedArray IntBuffer") {
    defineTests(new WrappedTypedArrayIntBufferFactory)
  }

  describe("Sliced IntBuffer") {
    defineTests(new AllocIntBufferFactory with BufferFactory.SlicedBufferFactory)
  }

  for ((description, byteBufferFactory) <- ByteBufferFactories.WriteableByteBufferFactories) {
    describe("Int view of " + description) {
      defineTests(new ByteBufferIntViewFactory(byteBufferFactory))
    }

    describe("Read-only Int view of " + description) {
      defineTests(new ByteBufferIntViewFactory(byteBufferFactory)
          with BufferFactory.ReadOnlyBufferFactory)
    }
  }
}
