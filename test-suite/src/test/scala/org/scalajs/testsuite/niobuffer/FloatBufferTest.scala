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

  describe("Allocated FloatBuffer") {
    defineTests(new AllocFloatBufferFactory)
  }

  describe("Wrapped FloatBuffer") {
    defineTests(new WrappedFloatBufferFactory)
  }

  describe("Read-only wrapped FloatBuffer") {
    defineTests(new WrappedFloatBufferFactory with BufferFactory.ReadOnlyBufferFactory)
  }

  describe("Sliced FloatBuffer") {
    defineTests(new AllocFloatBufferFactory with BufferFactory.SlicedBufferFactory)
  }
}
