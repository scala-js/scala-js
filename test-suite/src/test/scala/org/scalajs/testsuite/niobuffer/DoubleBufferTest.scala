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

object DoubleBufferTest extends BaseBufferTest {
  type Factory = BufferFactory.DoubleBufferFactory

  def defineTests(factory: Factory): Unit = {
    commonTests(factory)
  }

  class AllocDoubleBufferFactory extends Factory {
    def allocBuffer(capacity: Int): DoubleBuffer =
      DoubleBuffer.allocate(capacity)
  }

  class WrappedDoubleBufferFactory extends Factory with BufferFactory.WrappedBufferFactory {
    def baseWrap(array: Array[Double]): DoubleBuffer =
      DoubleBuffer.wrap(array)

    def baseWrap(array: Array[Double], offset: Int, length: Int): DoubleBuffer =
      DoubleBuffer.wrap(array, offset, length)
  }

  class WrappedTypedArrayDoubleBufferFactory
      extends Factory with BufferFactory.WrappedTypedArrayBufferFactory {
    def baseWrap(array: Array[Double]): DoubleBuffer =
      TypedArrayBuffer.wrap(new Float64Array(array.toJSArray))
  }

  class ByteBufferDoubleViewFactory(
      byteBufferFactory: BufferFactory.ByteBufferFactory)
      extends Factory with BufferFactory.ByteBufferViewFactory {
    require(!byteBufferFactory.createsReadOnly)

    def baseAllocBuffer(capacity: Int): DoubleBuffer =
      byteBufferFactory.allocBuffer(capacity * 8).asDoubleBuffer()
  }

  describe("Allocated DoubleBuffer") {
    defineTests(new AllocDoubleBufferFactory)
  }

  describe("Wrapped DoubleBuffer") {
    defineTests(new WrappedDoubleBufferFactory)
  }

  describe("Read-only wrapped DoubleBuffer") {
    defineTests(new WrappedDoubleBufferFactory with BufferFactory.ReadOnlyBufferFactory)
  }

  when("typedarray").
  describe("Wrapped TypedArray DoubleBuffer") {
    defineTests(new WrappedTypedArrayDoubleBufferFactory)
  }

  describe("Sliced DoubleBuffer") {
    defineTests(new AllocDoubleBufferFactory with BufferFactory.SlicedBufferFactory)
  }

  for ((description, byteBufferFactory) <- ByteBufferFactories.WriteableByteBufferFactories) {
    describe("Double view of " + description) {
      defineTests(new ByteBufferDoubleViewFactory(byteBufferFactory))
    }

    describe("Read-only Double view of " + description) {
      defineTests(new ByteBufferDoubleViewFactory(byteBufferFactory)
          with BufferFactory.ReadOnlyBufferFactory)
    }
  }
}
