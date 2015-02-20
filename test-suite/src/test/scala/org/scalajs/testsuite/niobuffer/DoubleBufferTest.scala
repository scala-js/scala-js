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

  describe("Allocated DoubleBuffer") {
    defineTests(new AllocDoubleBufferFactory)
  }

  describe("Wrapped DoubleBuffer") {
    defineTests(new WrappedDoubleBufferFactory)
  }

  describe("Read-only wrapped DoubleBuffer") {
    defineTests(new WrappedDoubleBufferFactory with BufferFactory.ReadOnlyBufferFactory)
  }

  describe("Sliced DoubleBuffer") {
    defineTests(new AllocDoubleBufferFactory with BufferFactory.SlicedBufferFactory)
  }
}
