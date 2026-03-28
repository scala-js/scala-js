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

abstract class DoubleBufferTest extends BaseBufferTest {
  type Factory = BufferFactory.DoubleBufferFactory

  class AllocDoubleBufferFactory extends Factory {
    def allocBuffer(capacity: Int): DoubleBuffer =
      DoubleBuffer.allocate(capacity)
  }

  class WrappedDoubleBufferFactory extends Factory with BufferFactory.WrappedBufferFactory {
    def baseWrap(array: Array[Double]): DoubleBuffer =
      DoubleBuffer.wrap(array)

    def baseWrap(array: Array[Double], offset: Int, length: Int): DoubleBuffer =
      DoubleBuffer.wrap(array, offset, length)
  }

  class ByteBufferDoubleViewFactory(
      protected val byteBufferFactory: BufferFactory.ByteBufferFactory,
      order: ByteOrder)
      extends Factory with BufferFactory.ByteBufferViewFactory {

    def baseAllocBuffer(capacity: Int): DoubleBuffer =
      byteBufferFactory.allocBuffer(capacity * 8).order(order).asDoubleBuffer()
  }

}

class AllocDoubleBufferTest extends DoubleBufferTest {
  val factory: Factory = new AllocDoubleBufferFactory
}

class WrappedDoubleBufferTest extends DoubleBufferTest {
  val factory: Factory = new WrappedDoubleBufferFactory
}

class WrappedDoubleReadOnlyBufferTest extends DoubleBufferTest {
  val factory: Factory =
    new WrappedDoubleBufferFactory with BufferFactory.ReadOnlyBufferFactory
}

class AllocDoubleSlicedBufferTest extends DoubleBufferTest {
  val factory: Factory =
    new AllocDoubleBufferFactory with BufferFactory.SlicedBufferFactory
}

// Double views of byte buffers

abstract class DoubleViewOfByteBufferTest(
    byteBufferFactory: BufferFactory.ByteBufferFactory, order: ByteOrder)
    extends DoubleBufferTest {

  val factory: BufferFactory.DoubleBufferFactory =
    new ByteBufferDoubleViewFactory(byteBufferFactory, order)
}

class DoubleViewOfAllocByteBufferBigEndianTest
    extends DoubleViewOfByteBufferTest(new AllocByteBufferFactory, ByteOrder.BIG_ENDIAN)

class DoubleViewOfWrappedByteBufferBigEndianTest
    extends DoubleViewOfByteBufferTest(new WrappedByteBufferFactory, ByteOrder.BIG_ENDIAN)

class DoubleViewOfSlicedAllocByteBufferBigEndianTest
    extends DoubleViewOfByteBufferTest(new SlicedAllocByteBufferFactory, ByteOrder.BIG_ENDIAN)

class DoubleViewOfAllocByteBufferLittleEndianTest
    extends DoubleViewOfByteBufferTest(new AllocByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class DoubleViewOfWrappedByteBufferLittleEndianTest
    extends DoubleViewOfByteBufferTest(new WrappedByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class DoubleViewOfSlicedAllocByteBufferLittleEndianTest
    extends DoubleViewOfByteBufferTest(new SlicedAllocByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

// Read only Double views of byte buffers

abstract class ReadOnlyDoubleViewOfByteBufferTest(
    byteBufferFactory: BufferFactory.ByteBufferFactory, order: ByteOrder)
    extends DoubleBufferTest {

  val factory: BufferFactory.DoubleBufferFactory = {
    new ByteBufferDoubleViewFactory(byteBufferFactory, order)
      with BufferFactory.ReadOnlyBufferFactory
  }
}

class ReadOnlyDoubleViewOfAllocByteBufferBigEndianTest
    extends ReadOnlyDoubleViewOfByteBufferTest(new AllocByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyDoubleViewOfWrappedByteBufferBigEndianTest
    extends ReadOnlyDoubleViewOfByteBufferTest(new WrappedByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyDoubleViewOfSlicedAllocByteBufferBigEndianTest
    extends ReadOnlyDoubleViewOfByteBufferTest(
        new SlicedAllocByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyDoubleViewOfAllocByteBufferLittleEndianTest
    extends ReadOnlyDoubleViewOfByteBufferTest(new AllocByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ReadOnlyDoubleViewOfWrappedByteBufferLittleEndianTest
    extends ReadOnlyDoubleViewOfByteBufferTest(new WrappedByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ReadOnlyDoubleViewOfSlicedAllocByteBufferLittleEndianTest
    extends ReadOnlyDoubleViewOfByteBufferTest(
        new SlicedAllocByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

// Double views of direct byte buffers

class DoubleViewOfAllocDirectByteBufferBigEndianTest
    extends DoubleViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

class DoubleViewOfSlicedAllocDirectByteBufferBigEndianTest
    extends DoubleViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

class DoubleViewOfAllocDirectByteBufferLittleEndianTest
    extends DoubleViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class DoubleViewOfSlicedAllocDirectByteBufferLittleEndianTest extends DoubleViewOfByteBufferTest(
        new SlicedAllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

// Read only Double views of direct byte buffers

class ReadOnlyDoubleViewOfAllocDirectByteBufferBigEndianTest
    extends ReadOnlyDoubleViewOfByteBufferTest(
        new AllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyDoubleViewOfSlicedAllocDirectByteBufferBigEndianTest
    extends ReadOnlyDoubleViewOfByteBufferTest(
        new SlicedAllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyDoubleViewOfAllocDirectByteBufferLittleEndianTest
    extends ReadOnlyDoubleViewOfByteBufferTest(
        new AllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ReadOnlyDoubleViewOfSlicedAllocDirectByteBufferLittleEndianTest
    extends ReadOnlyDoubleViewOfByteBufferTest(
        new SlicedAllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)
