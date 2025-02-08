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

import org.scalajs.testsuite.niobuffer.BufferFactory.IntBufferFactory

import scala.scalajs.js
import js.typedarray._
import js.JSConverters._

import org.scalajs.testsuite.niobuffer.ByteBufferFactories._
import org.scalajs.testsuite.niobuffer.ByteBufferJSFactories._

class WrappedTypedArrayIntBufferJSTest extends IntBufferTest {

  val factory: IntBufferFactory = new WrappedTypedArrayIntBufferJSFactory

  class WrappedTypedArrayIntBufferJSFactory
      extends Factory with BufferFactory.WrappedTypedArrayBufferFactory {
    def baseWrap(array: Array[Int]): IntBuffer =
      TypedArrayBuffer.wrap(new Int32Array(array.toJSArray))
  }
}

// Int views of byte buffers

class IntViewOfAllocDirectByteBufferBigEndianJSTest
    extends IntViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

class IntViewOfSlicedAllocDirectByteBufferBigEndianJSTest
    extends IntViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

class IntViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends IntViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

class IntViewOfAllocDirectByteBufferLittleEndianJSTest
    extends IntViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class IntViewOfSlicedAllocDirectByteBufferLittleEndianJSTest
    extends IntViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class IntViewOfWrappedTypedArrayByteBufferLittleEndianJSTest
    extends IntViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

// Read only Int views of byte buffers

class ReadOnlyIntViewOfAllocDirectByteBufferBigEndianJSTest
    extends ReadOnlyIntViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyIntViewOfSlicedAllocDirectByteBufferBigEndianJSTest
    extends ReadOnlyIntViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyIntViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends ReadOnlyIntViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyIntViewOfAllocDirectByteBufferLittleEndianJSTest
    extends ReadOnlyIntViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ReadOnlyIntViewOfSlicedAllocDirectByteBufferLittleEndianJSTest
    extends ReadOnlyIntViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ReadOnlyIntViewOfWrappedTypedArrayByteBufferLittleEndianJSTest
    extends ReadOnlyIntViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)
