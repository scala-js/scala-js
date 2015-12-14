/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.niobuffer

import java.nio._

import org.scalajs.testsuite.niobuffer.ByteBufferFactories._

abstract class LongBufferTest extends BaseBufferTest {
  type Factory = BufferFactory.LongBufferFactory

  class AllocLongBufferFactory extends Factory {
    def allocBuffer(capacity: Int): LongBuffer =
      LongBuffer.allocate(capacity)
  }

  class WrappedLongBufferFactory extends Factory with BufferFactory.WrappedBufferFactory {
    def baseWrap(array: Array[Long]): LongBuffer =
      LongBuffer.wrap(array)

    def baseWrap(array: Array[Long], offset: Int, length: Int): LongBuffer =
      LongBuffer.wrap(array, offset, length)
  }

  class ByteBufferLongViewFactory(
      byteBufferFactory: BufferFactory.ByteBufferFactory,
      order: ByteOrder)
      extends Factory with BufferFactory.ByteBufferViewFactory {
    require(!byteBufferFactory.createsReadOnly)

    def baseAllocBuffer(capacity: Int): LongBuffer =
      byteBufferFactory.allocBuffer(capacity * 8).order(order).asLongBuffer()
  }

}

class AllocLongBufferTest extends LongBufferTest {
  val factory: Factory = new AllocLongBufferFactory
}

class WrappedLongBufferTest extends LongBufferTest {
  val factory: Factory = new WrappedLongBufferFactory
}

class WrappedLongReadOnlyBufferTest extends LongBufferTest {
  val factory: Factory =
    new WrappedLongBufferFactory with BufferFactory.ReadOnlyBufferFactory
}

class AllocLongSlicedBufferTest extends LongBufferTest {
  val factory: Factory =
    new AllocLongBufferFactory with BufferFactory.SlicedBufferFactory
}

// Long views of byte buffers

abstract class LongViewOfByteBufferTest(
    byteBufferFactory: BufferFactory.ByteBufferFactory, order: ByteOrder)
    extends LongBufferTest {

  val factory: BufferFactory.LongBufferFactory =
    new ByteBufferLongViewFactory(byteBufferFactory, order)
}

class LongViewOfAllocByteBufferBigEndianTest
    extends LongViewOfByteBufferTest(new AllocByteBufferFactory, ByteOrder.BIG_ENDIAN)

class LongViewOfWrappedByteBufferBigEndianTest
    extends LongViewOfByteBufferTest(new WrappedByteBufferFactory, ByteOrder.BIG_ENDIAN)

class LongViewOfSlicedAllocByteBufferBigEndianTest
    extends LongViewOfByteBufferTest(new SlicedAllocByteBufferFactory, ByteOrder.BIG_ENDIAN)

class LongViewOfAllocByteBufferLittleEndianTest
    extends LongViewOfByteBufferTest(new AllocByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class LongViewOfWrappedByteBufferLittleEndianTest
    extends LongViewOfByteBufferTest(new WrappedByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class LongViewOfSlicedAllocByteBufferLittleEndianTest
    extends LongViewOfByteBufferTest(new SlicedAllocByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

// Read only Long views of byte buffers

abstract class ReadOnlyLongViewOfByteBufferTest(
    byteBufferFactory: BufferFactory.ByteBufferFactory, order: ByteOrder)
    extends LongBufferTest {

  val factory: BufferFactory.LongBufferFactory = {
    new ByteBufferLongViewFactory(byteBufferFactory, order)
        with BufferFactory.ReadOnlyBufferFactory
  }
}

class ReadOnlyLongViewOfAllocByteBufferBigEndianTest
    extends ReadOnlyLongViewOfByteBufferTest(new AllocByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyLongViewOfWrappedByteBufferBigEndianTest
    extends ReadOnlyLongViewOfByteBufferTest(new WrappedByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyLongViewOfSlicedAllocByteBufferBigEndianTest
    extends ReadOnlyLongViewOfByteBufferTest(new SlicedAllocByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyLongViewOfAllocByteBufferLittleEndianTest
    extends ReadOnlyLongViewOfByteBufferTest(new AllocByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ReadOnlyLongViewOfWrappedByteBufferLittleEndianTest
    extends ReadOnlyLongViewOfByteBufferTest(new WrappedByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ReadOnlyLongViewOfSlicedAllocByteBufferLittleEndianTest
    extends ReadOnlyLongViewOfByteBufferTest(new SlicedAllocByteBufferFactory, ByteOrder.LITTLE_ENDIAN)