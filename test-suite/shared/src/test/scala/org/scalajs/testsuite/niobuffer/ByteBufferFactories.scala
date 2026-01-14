/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.testsuite.niobuffer

import java.nio._

object ByteBufferFactories {
  import BufferFactory._

  class AllocByteBufferFactory extends ByteBufferFactory {
    def allocBuffer(capacity: Int): ByteBuffer =
      ByteBuffer.allocate(capacity)
  }

  class WrappedByteBufferFactory extends ByteBufferFactory with WrappedBufferFactory {
    def baseWrap(array: Array[Byte]): ByteBuffer =
      ByteBuffer.wrap(array)

    def baseWrap(array: Array[Byte], offset: Int, length: Int): ByteBuffer =
      ByteBuffer.wrap(array, offset, length)
  }

  class AllocDirectByteBufferFactory extends ByteBufferFactory {
    def allocBuffer(capacity: Int): ByteBuffer =
      ByteBuffer.allocateDirect(capacity)
  }

  class ReadOnlyWrappedByteBufferFactory extends WrappedByteBufferFactory with ReadOnlyBufferFactory

  class SlicedAllocByteBufferFactory extends AllocByteBufferFactory with SlicedBufferFactory

  class SlicedAllocDirectByteBufferFactory
      extends AllocDirectByteBufferFactory with SlicedBufferFactory
}
