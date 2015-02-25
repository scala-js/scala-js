package org.scalajs.testsuite.niobuffer

import scala.scalajs.js.JSConverters._
import scala.scalajs.js.typedarray._

import org.scalajs.jasminetest.TestSuiteContext

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

  class AllocDirectByteBufferFactory extends ByteBufferFactory {
    def allocBuffer(capacity: Int): ByteBuffer =
      ByteBuffer.allocateDirect(capacity)
  }

  class WrappedTypedArrayByteBufferFactory
      extends ByteBufferFactory with WrappedTypedArrayBufferFactory {
    def baseWrap(array: Array[Byte]): ByteBuffer = {
      val buf = TypedArrayBuffer.wrap(new Int8Array(array.toJSArray))
      buf.order(ByteOrder.BIG_ENDIAN)
      buf
    }
  }

  class ReadOnlyWrappedByteBufferFactory
      extends WrappedByteBufferFactory with ReadOnlyBufferFactory

  class SlicedAllocByteBufferFactory
      extends AllocByteBufferFactory with SlicedBufferFactory

  class SlicedAllocDirectByteBufferFactory
      extends AllocDirectByteBufferFactory with SlicedBufferFactory

  val AllByteBufferFactories = Seq(
      "Allocated ByteBuffer"         -> new AllocByteBufferFactory,
      "Wrapped ByteBuffer"           -> new WrappedByteBufferFactory,
      "Read-only wrapped ByteBuffer" -> new ReadOnlyWrappedByteBufferFactory,
      "Sliced ByteBuffer"            -> new SlicedAllocByteBufferFactory
  ) ++ {
    if (TestSuiteContext.hasTag("typedarray")) {
      Seq(
          "Allocated direct ByteBuffer"   -> new AllocDirectByteBufferFactory,
          "Sliced direct ByteBuffer"      -> new SlicedAllocDirectByteBufferFactory,
          "Wrapped TypedArray ByteBuffer" -> new WrappedTypedArrayByteBufferFactory
      )
    } else Nil
  }

  val WriteableByteBufferFactories =
    AllByteBufferFactories.filter(!_._2.createsReadOnly)
}
