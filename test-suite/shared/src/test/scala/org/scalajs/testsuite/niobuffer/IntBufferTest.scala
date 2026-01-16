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

abstract class IntBufferTest extends BaseBufferTest {
  type Factory = BufferFactory.IntBufferFactory

  class AllocIntBufferFactory extends Factory {
    def allocBuffer(capacity: Int): IntBuffer =
      IntBuffer.allocate(capacity)
  }

  class WrappedIntBufferFactory extends Factory with BufferFactory.WrappedBufferFactory {
    def baseWrap(array: Array[Int]): IntBuffer =
      IntBuffer.wrap(array)

    def baseWrap(array: Array[Int], offset: Int, length: Int): IntBuffer =
      IntBuffer.wrap(array, offset, length)
  }

  class ByteBufferIntViewFactory(
      byteBufferFactory: BufferFactory.ByteBufferFactory,
      order: ByteOrder)
      extends Factory with BufferFactory.ByteBufferViewFactory {
    require(!byteBufferFactory.createsReadOnly)

    def baseAllocBuffer(capacity: Int): IntBuffer =
      byteBufferFactory.allocBuffer(capacity * 4).order(order).asIntBuffer()
  }

}

class AllocIntBufferTest extends IntBufferTest {
  val factory: Factory = new AllocIntBufferFactory
}

class WrappedIntBufferTest extends IntBufferTest {
  val factory: Factory = new WrappedIntBufferFactory
}

class WrappedIntReadOnlyBufferTest extends IntBufferTest {
  val factory: Factory =
    new WrappedIntBufferFactory with BufferFactory.ReadOnlyBufferFactory
}

class AllocIntSlicedBufferTest extends IntBufferTest {
  val factory: Factory =
    new AllocIntBufferFactory with BufferFactory.SlicedBufferFactory
}

// Int views of byte buffers

abstract class IntViewOfByteBufferTest(
    byteBufferFactory: BufferFactory.ByteBufferFactory, order: ByteOrder)
    extends IntBufferTest {

  val factory: BufferFactory.IntBufferFactory =
    new ByteBufferIntViewFactory(byteBufferFactory, order)
}

class IntViewOfAllocByteBufferBigEndianTest
    extends IntViewOfByteBufferTest(new AllocByteBufferFactory, ByteOrder.BIG_ENDIAN)

class IntViewOfWrappedByteBufferBigEndianTest
    extends IntViewOfByteBufferTest(new WrappedByteBufferFactory, ByteOrder.BIG_ENDIAN)

class IntViewOfSlicedAllocByteBufferBigEndianTest
    extends IntViewOfByteBufferTest(new SlicedAllocByteBufferFactory, ByteOrder.BIG_ENDIAN)

class IntViewOfAllocByteBufferLittleEndianTest
    extends IntViewOfByteBufferTest(new AllocByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class IntViewOfWrappedByteBufferLittleEndianTest
    extends IntViewOfByteBufferTest(new WrappedByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class IntViewOfSlicedAllocByteBufferLittleEndianTest
    extends IntViewOfByteBufferTest(new SlicedAllocByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

// Read only Int views of byte buffers

abstract class ReadOnlyIntViewOfByteBufferTest(
    byteBufferFactory: BufferFactory.ByteBufferFactory, order: ByteOrder)
    extends IntBufferTest {

  val factory: BufferFactory.IntBufferFactory = {
    new ByteBufferIntViewFactory(byteBufferFactory, order)
      with BufferFactory.ReadOnlyBufferFactory
  }
}

class ReadOnlyIntViewOfAllocByteBufferBigEndianTest
    extends ReadOnlyIntViewOfByteBufferTest(new AllocByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyIntViewOfWrappedByteBufferBigEndianTest
    extends ReadOnlyIntViewOfByteBufferTest(new WrappedByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyIntViewOfSlicedAllocByteBufferBigEndianTest
    extends ReadOnlyIntViewOfByteBufferTest(new SlicedAllocByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyIntViewOfAllocByteBufferFactoryEndianTest
    extends ReadOnlyIntViewOfByteBufferTest(new AllocByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ReadOnlyIntViewOfWrappedByteBufferLittleEndianTest
    extends ReadOnlyIntViewOfByteBufferTest(new WrappedByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ReadOnlyIntViewOfSlicedAllocByteBufferLittleEndianTest
    extends ReadOnlyIntViewOfByteBufferTest(
        new SlicedAllocByteBufferFactory, ByteOrder.LITTLE_ENDIAN)
