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

import org.scalajs.testsuite.niobuffer.BufferFactory.ShortBufferFactory

import scala.scalajs.js
import js.typedarray._
import js.JSConverters._

import org.scalajs.testsuite.niobuffer.ByteBufferFactories._
import org.scalajs.testsuite.niobuffer.ByteBufferJSFactories._

class WrappedTypedArrayShortBufferJSTest extends ShortBufferTest {

  val factory: ShortBufferFactory = new WrappedTypedArrayShortBufferJSFactory

  class WrappedTypedArrayShortBufferJSFactory
      extends Factory with BufferFactory.WrappedTypedArrayBufferFactory {
    def baseWrap(array: Array[Short]): ShortBuffer =
      TypedArrayBuffer.wrap(new Int16Array(array.toJSArray))
  }
}

// Short views of byte buffers

class ShortViewOfAllocDirectByteBufferBigEndianJSTest
    extends ShortViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ShortViewOfSlicedAllocDirectByteBufferBigEndianJSTest
    extends ShortViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ShortViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends ShortViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ShortViewOfAllocDirectByteBufferLittleEndianJSTest
    extends ShortViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ShortViewOfSlicedAllocDirectByteBufferLittleEndianJSTest
    extends ShortViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ShortViewOfWrappedTypedArrayByteBufferLittleEndianJSTest
    extends ShortViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

// Read only Short views of byte buffers

class ReadOnlyShortViewOfAllocDirectByteBufferBigEndianJSTest
    extends ReadOnlyShortViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyShortViewOfSlicedAllocDirectByteBufferBigEndianJSTest
    extends ReadOnlyShortViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyShortViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends ReadOnlyShortViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyShortViewOfAllocDirectByteBufferLittleEndianJSTest
    extends ReadOnlyShortViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ReadOnlyShortViewOfSlicedAllocDirectByteBufferLittleEndianJSTest
    extends ReadOnlyShortViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ReadOnlyShortViewOfWrappedTypedArrayByteBufferLittleEndianJSTest
    extends ReadOnlyShortViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)
