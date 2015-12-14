package org.scalajs.testsuite.niobuffer

import scala.scalajs.js.JSConverters._
import scala.scalajs.js.typedarray._

import java.nio._

object ByteBufferJSFactories {
  import BufferFactory.{ByteBufferFactory, WrappedTypedArrayBufferFactory}

  class WrappedTypedArrayByteBufferFactory
      extends ByteBufferFactory with WrappedTypedArrayBufferFactory {
    def baseWrap(array: Array[Byte]): ByteBuffer = {
      val buf = TypedArrayBuffer.wrap(new Int8Array(array.toJSArray))
      buf.order(ByteOrder.BIG_ENDIAN)
      buf
    }
  }
}
