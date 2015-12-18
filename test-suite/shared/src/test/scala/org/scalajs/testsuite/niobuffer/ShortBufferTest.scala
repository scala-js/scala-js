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

abstract class ShortBufferTest extends BaseBufferTest {
  type Factory = BufferFactory.ShortBufferFactory

  class AllocShortBufferFactory extends Factory {
    def allocBuffer(capacity: Int): ShortBuffer =
      ShortBuffer.allocate(capacity)
  }

  class WrappedShortBufferFactory extends Factory with BufferFactory.WrappedBufferFactory {
    def baseWrap(array: Array[Short]): ShortBuffer =
      ShortBuffer.wrap(array)

    def baseWrap(array: Array[Short], offset: Int, length: Int): ShortBuffer =
      ShortBuffer.wrap(array, offset, length)
  }

  class ByteBufferShortViewFactory(
      byteBufferFactory: BufferFactory.ByteBufferFactory,
      order: ByteOrder)
      extends Factory with BufferFactory.ByteBufferViewFactory {
    require(!byteBufferFactory.createsReadOnly)

    def baseAllocBuffer(capacity: Int): ShortBuffer =
      byteBufferFactory.allocBuffer(capacity * 2).order(order).asShortBuffer()
  }

}

class AllocShortBufferTest extends ShortBufferTest {
  val factory: Factory = new AllocShortBufferFactory
}

class WrappedShortBufferTest extends ShortBufferTest {
  val factory: Factory = new WrappedShortBufferFactory
}

class WrappedShortReadOnlyBufferTest extends ShortBufferTest {
  val factory: Factory =
    new WrappedShortBufferFactory with BufferFactory.ReadOnlyBufferFactory
}

class AllocShortSlicedBufferTest extends ShortBufferTest {
  val factory: Factory =
    new AllocShortBufferFactory with BufferFactory.SlicedBufferFactory
}

// Short views of byte buffers

abstract class ShortViewOfByteBufferTest(
    byteBufferFactory: BufferFactory.ByteBufferFactory, order: ByteOrder)
    extends ShortBufferTest {

  val factory: BufferFactory.ShortBufferFactory =
    new ByteBufferShortViewFactory(byteBufferFactory, order)
}

class ShortViewOfAllocByteBufferBigEndianTest
    extends ShortViewOfByteBufferTest(new AllocByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ShortViewOfWrappedByteBufferBigEndianTest
    extends ShortViewOfByteBufferTest(new WrappedByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ShortViewOfSlicedAllocByteBufferBigEndianTest
    extends ShortViewOfByteBufferTest(new SlicedAllocByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ShortViewOfAllocByteBufferLittleEndianTest
    extends ShortViewOfByteBufferTest(new AllocByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ShortViewOfWrappedByteBufferLittleEndianTest
    extends ShortViewOfByteBufferTest(new WrappedByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ShortViewOfSlicedAllocByteBufferLittleEndianTest
    extends ShortViewOfByteBufferTest(new SlicedAllocByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

// Read only Short views of byte buffers

abstract class ReadOnlyShortViewOfByteBufferTest(
    byteBufferFactory: BufferFactory.ByteBufferFactory, order: ByteOrder)
    extends ShortBufferTest {

  val factory: BufferFactory.ShortBufferFactory = {
    new ByteBufferShortViewFactory(byteBufferFactory, order)
        with BufferFactory.ReadOnlyBufferFactory
  }
}

class ReadOnlyShortViewOfAllocByteBufferBigEndianTest
    extends ReadOnlyShortViewOfByteBufferTest(new AllocByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyShortViewOfWrappedByteBufferBigEndianTest
    extends ReadOnlyShortViewOfByteBufferTest(new WrappedByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyShortViewOfSlicedAllocByteBufferBigEndianTest
    extends ReadOnlyShortViewOfByteBufferTest(new SlicedAllocByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyShortViewOfAllocByteBufferLittleEndianTest
    extends ReadOnlyShortViewOfByteBufferTest(new AllocByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ReadOnlyShortViewOfWrappedByteBufferLittleEndianTest
    extends ReadOnlyShortViewOfByteBufferTest(new WrappedByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ReadOnlyShortViewOfSlicedAllocByteBufferLittleEndianTest
    extends ReadOnlyShortViewOfByteBufferTest(new SlicedAllocByteBufferFactory, ByteOrder.LITTLE_ENDIAN)