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
      byteBufferFactory: BufferFactory.ByteBufferFactory,
      order: ByteOrder)
      extends Factory with BufferFactory.ByteBufferViewFactory {
    require(!byteBufferFactory.createsReadOnly)

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
    extends ReadOnlyDoubleViewOfByteBufferTest(new SlicedAllocByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyDoubleViewOfAllocByteBufferLittleEndianTest
    extends ReadOnlyDoubleViewOfByteBufferTest(new AllocByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ReadOnlyDoubleViewOfWrappedByteBufferLittleEndianTest
    extends ReadOnlyDoubleViewOfByteBufferTest(new WrappedByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ReadOnlyDoubleViewOfSlicedAllocByteBufferLittleEndianTest
    extends ReadOnlyDoubleViewOfByteBufferTest(new SlicedAllocByteBufferFactory, ByteOrder.LITTLE_ENDIAN)
