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

  describe("Allocated IntBuffer") {
    defineTests(new AllocIntBufferFactory)
  }

  describe("Wrapped IntBuffer") {
    defineTests(new WrappedIntBufferFactory)
  }

  describe("Read-only wrapped IntBuffer") {
    defineTests(new WrappedIntBufferFactory with BufferFactory.ReadOnlyBufferFactory)
  }

  describe("Sliced IntBuffer") {
    defineTests(new AllocIntBufferFactory with BufferFactory.SlicedBufferFactory)
  }
}
