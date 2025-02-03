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

import org.scalajs.testsuite.niobuffer.BufferFactory.FloatBufferFactory

import scala.scalajs.js
import js.typedarray._
import js.JSConverters._

import org.scalajs.testsuite.niobuffer.ByteBufferFactories._
import org.scalajs.testsuite.niobuffer.ByteBufferJSFactories._

class WrappedTypedArrayFloatBufferJSTest extends FloatBufferTest {

  val factory: FloatBufferFactory = new WrappedTypedArrayFloatBufferJSFactory

  class WrappedTypedArrayFloatBufferJSFactory
      extends Factory with BufferFactory.WrappedTypedArrayBufferFactory {
    def baseWrap(array: Array[Float]): FloatBuffer =
      TypedArrayBuffer.wrap(new Float32Array(array.toJSArray))
  }
}

// Float views of byte buffers

class FloatViewOfAllocDirectByteBufferBigEndianJSTest
    extends FloatViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

class FloatViewOfSlicedAllocDirectByteBufferBigEndianJSTest
    extends FloatViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

class FloatViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends FloatViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

class FloatViewOfAllocDirectByteBufferLittleEndianJSTest
    extends FloatViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class FloatViewOfSlicedAllocDirectByteBufferLittleEndianJSTest
    extends FloatViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class FloatViewOfWrappedTypedArrayByteBufferLittleEndianJSTest
    extends FloatViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

// Read only Float views of byte buffers

class ReadOnlyFloatViewOfAllocDirectByteBufferBigEndianJSTest
    extends ReadOnlyFloatViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyFloatViewOfSlicedAllocDirectByteBufferBigEndianJSTest
    extends ReadOnlyFloatViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyFloatViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends ReadOnlyFloatViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyFloatViewOfAllocDirectByteBufferLittleEndianJSTest
    extends ReadOnlyFloatViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ReadOnlyFloatViewOfSlicedAllocDirectByteBufferLittleEndianJSTest
    extends ReadOnlyFloatViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ReadOnlyFloatViewOfWrappedTypedArrayByteBufferLittleEndianJSTest
    extends ReadOnlyFloatViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)
