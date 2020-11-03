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

import org.scalajs.testsuite.niobuffer.ByteBufferFactories._

abstract class FloatBufferTest extends BaseBufferTest {
  type Factory = BufferFactory.FloatBufferFactory

  class AllocFloatBufferFactory extends Factory {
    def allocBuffer(capacity: Int): FloatBuffer =
      FloatBuffer.allocate(capacity)
  }

  class WrappedFloatBufferFactory extends Factory with BufferFactory.WrappedBufferFactory {
    def baseWrap(array: Array[Float]): FloatBuffer =
      FloatBuffer.wrap(array)

    def baseWrap(array: Array[Float], offset: Int, length: Int): FloatBuffer =
      FloatBuffer.wrap(array, offset, length)
  }

  class ByteBufferFloatViewFactory(byteBufferFactory: BufferFactory.ByteBufferFactory,
      order: ByteOrder)
      extends Factory with BufferFactory.ByteBufferViewFactory {
    require(!byteBufferFactory.createsReadOnly)

    def baseAllocBuffer(capacity: Int): FloatBuffer =
      byteBufferFactory.allocBuffer(capacity * 4).order(order).asFloatBuffer()
  }

}

class AllocFloatBufferTest extends FloatBufferTest {
  val factory: Factory = new AllocFloatBufferFactory
}

class WrappedFloatBufferTest extends FloatBufferTest {
  val factory: Factory = new WrappedFloatBufferFactory
}

class WrappedFloatReadOnlyBufferTest extends FloatBufferTest {
  val factory: Factory =
    new WrappedFloatBufferFactory with BufferFactory.ReadOnlyBufferFactory
}

class AllocFloatSlicedBufferTest extends FloatBufferTest {
  val factory: Factory =
    new AllocFloatBufferFactory with BufferFactory.SlicedBufferFactory
}

// Float views of byte buffers

abstract class FloatViewOfByteBufferTest(byteBufferFactory: BufferFactory.ByteBufferFactory,
    order: ByteOrder)
    extends FloatBufferTest {

  val factory: BufferFactory.FloatBufferFactory =
    new ByteBufferFloatViewFactory(byteBufferFactory, order)
}

class FloatViewOfAllocByteBufferBigEndianTest
    extends FloatViewOfByteBufferTest(new AllocByteBufferFactory, ByteOrder.BIG_ENDIAN)

class FloatViewOfWrappedByteBufferBigEndianTest
    extends FloatViewOfByteBufferTest(new WrappedByteBufferFactory, ByteOrder.BIG_ENDIAN)

class FloatViewOfSlicedAllocByteBufferBigEndianTest
    extends FloatViewOfByteBufferTest(new SlicedAllocByteBufferFactory, ByteOrder.BIG_ENDIAN)

class FloatViewOfAllocByteBufferLittleEndianTest
    extends FloatViewOfByteBufferTest(new AllocByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class FloatViewOfWrappedByteBufferLittleEndianTest
    extends FloatViewOfByteBufferTest(new WrappedByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class FloatViewOfSlicedAllocByteBufferLittleEndianTest
    extends FloatViewOfByteBufferTest(new SlicedAllocByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

// Read only Float views of byte buffers

abstract class ReadOnlyFloatViewOfByteBufferTest(byteBufferFactory: BufferFactory.ByteBufferFactory,
    order: ByteOrder)
    extends FloatBufferTest {

  val factory: BufferFactory.FloatBufferFactory = {
    new ByteBufferFloatViewFactory(byteBufferFactory, order)
      with BufferFactory.ReadOnlyBufferFactory
  }
}

class ReadOnlyFloatViewOfAllocByteBufferBigEndianTest
    extends ReadOnlyFloatViewOfByteBufferTest(new AllocByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyFloatViewOfWrappedByteBufferBigEndianTest
    extends ReadOnlyFloatViewOfByteBufferTest(new WrappedByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyFloatViewOfSlicedAllocByteBufferBigEndianTest
    extends ReadOnlyFloatViewOfByteBufferTest(new SlicedAllocByteBufferFactory,
        ByteOrder.BIG_ENDIAN)

class ReadOnlyFloatViewOfAllocByteBufferLittleEndianTest
    extends ReadOnlyFloatViewOfByteBufferTest(new AllocByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ReadOnlyFloatViewOfWrappedByteBufferLittleEndianTest
    extends ReadOnlyFloatViewOfByteBufferTest(new WrappedByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ReadOnlyFloatViewOfSlicedAllocByteBufferLittleEndianTest
    extends ReadOnlyFloatViewOfByteBufferTest(new SlicedAllocByteBufferFactory,
        ByteOrder.LITTLE_ENDIAN)
