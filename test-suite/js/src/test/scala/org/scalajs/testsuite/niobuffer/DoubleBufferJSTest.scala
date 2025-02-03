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

import org.scalajs.testsuite.niobuffer.BufferFactory.DoubleBufferFactory

import scala.scalajs.js
import js.typedarray._
import js.JSConverters._

import org.scalajs.testsuite.niobuffer.ByteBufferFactories._
import org.scalajs.testsuite.niobuffer.ByteBufferJSFactories._

class WrappedTypedArrayDoubleBufferJSTest extends DoubleBufferTest {

  val factory: DoubleBufferFactory = new WrappedTypedArrayDoubleBufferJSFactory

  class WrappedTypedArrayDoubleBufferJSFactory
      extends Factory with BufferFactory.WrappedTypedArrayBufferFactory {
    def baseWrap(array: Array[Double]): DoubleBuffer =
      TypedArrayBuffer.wrap(new Float64Array(array.toJSArray))
  }
}

// Double views of byte buffers

class DoubleViewOfAllocDirectByteBufferBigEndianJSTest
    extends DoubleViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

class DoubleViewOfSlicedAllocDirectByteBufferBigEndianJSTest
    extends DoubleViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

class DoubleViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends DoubleViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

class DoubleViewOfAllocDirectByteBufferLittleEndianJSTest
    extends DoubleViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class DoubleViewOfSlicedAllocDirectByteBufferLittleEndianJSTest
    extends DoubleViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class DoubleViewOfWrappedTypedArrayByteBufferLittleEndianJSTest
    extends DoubleViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

// Read only Double views of byte buffers

class ReadOnlyDoubleViewOfAllocDirectByteBufferBigEndianJSTest
    extends ReadOnlyDoubleViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyDoubleViewOfSlicedAllocDirectByteBufferBigEndianJSTest
    extends ReadOnlyDoubleViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyDoubleViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends ReadOnlyDoubleViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyDoubleViewOfAllocDirectByteBufferLittleEndianJSTest
    extends ReadOnlyDoubleViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ReadOnlyDoubleViewOfSlicedAllocDirectByteBufferLittleEndianJSTest
    extends ReadOnlyDoubleViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ReadOnlyDoubleViewOfWrappedTypedArrayByteBufferLittleEndianJSTest
    extends ReadOnlyDoubleViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)
