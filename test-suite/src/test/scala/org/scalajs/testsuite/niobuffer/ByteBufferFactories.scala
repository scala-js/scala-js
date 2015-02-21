package org.scalajs.testsuite.niobuffer

import java.nio._

object ByteBufferFactories {
  import BufferFactory._

  class AllocByteBufferFactory extends ByteBufferFactory {
    def allocBuffer(capacity: Int): ByteBuffer =
      ByteBuffer.allocate(capacity)
  }

  class WrappedByteBufferFactory extends ByteBufferFactory
                                    with WrappedBufferFactory {
    def baseWrap(array: Array[Byte]): ByteBuffer =
      ByteBuffer.wrap(array)

    def baseWrap(array: Array[Byte], offset: Int, length: Int): ByteBuffer =
      ByteBuffer.wrap(array, offset, length)
  }

  class ReadOnlyWrappedByteBufferFactory
      extends WrappedByteBufferFactory with ReadOnlyBufferFactory

  class SlicedAllocByteBufferFactory
      extends AllocByteBufferFactory with SlicedBufferFactory

  val WriteableByteBufferFactories = Seq(
      "Allocated ByteBuffer" -> new AllocByteBufferFactory,
      "Wrapped ByteBuffer"   -> new WrappedByteBufferFactory,
      "Sliced ByteBuffer"    -> new SlicedAllocByteBufferFactory
  )

  val AllByteBufferFactories = Seq(
      "Allocated ByteBuffer"         -> new AllocByteBufferFactory,
      "Wrapped ByteBuffer"           -> new WrappedByteBufferFactory,
      "Read-only wrapped ByteBuffer" -> new ReadOnlyWrappedByteBufferFactory,
      "Sliced ByteBuffer"            -> new SlicedAllocByteBufferFactory
  )
}
