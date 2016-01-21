/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.niobuffer

import java.nio._

import org.scalajs.testsuite.niobuffer.BufferFactory.CharBufferFactory
import org.scalajs.testsuite.niobuffer.ByteBufferFactories._

abstract class CharBufferTest extends BaseBufferTest {
  type Factory = BufferFactory.CharBufferFactory

  def zeros(n: Int): String =
    "\u0000"*n

  class AllocCharBufferFactory extends Factory {
    def allocBuffer(capacity: Int): CharBuffer =
      CharBuffer.allocate(capacity)
  }

  class WrappedCharBufferFactory extends Factory with BufferFactory.WrappedBufferFactory {
    def baseWrap(array: Array[Char]): CharBuffer =
      CharBuffer.wrap(array)

    def baseWrap(array: Array[Char], offset: Int, length: Int): CharBuffer =
      CharBuffer.wrap(array, offset, length)
  }

  class ByteBufferCharViewFactory(
      byteBufferFactory: BufferFactory.ByteBufferFactory,
      order: ByteOrder)
      extends Factory with BufferFactory.ByteBufferViewFactory {
    require(!byteBufferFactory.createsReadOnly)

    def baseAllocBuffer(capacity: Int): CharBuffer =
      byteBufferFactory.allocBuffer(capacity * 2).order(order).asCharBuffer()
  }
}

class AllocCharBufferTest extends CharBufferTest {
  val factory: CharBufferFactory = new AllocCharBufferFactory
}

class WrappedCharBufferTest extends CharBufferTest {
  val factory: CharBufferFactory = new WrappedCharBufferFactory
}

class WrappedCharReadOnlyBufferTest extends CharBufferTest {
  val factory: CharBufferFactory =
    new WrappedCharBufferFactory with BufferFactory.ReadOnlyBufferFactory
}

class AllocCharSlicedBufferTest extends CharBufferTest {
  val factory: CharBufferFactory =
    new AllocCharBufferFactory with BufferFactory.SlicedBufferFactory
}

class CharBufferWrappingACharSequenceTest extends CharBufferTest {
  
  val factory: CharBufferFactory = new CharBufferWrappingACharSequenceFactory

  class CharBufferWrappingACharSequenceFactory extends Factory {
    override val createsReadOnly = true

    def allocBuffer(capacity: Int): CharBuffer = {
      if (capacity < 0)
        throw new IllegalArgumentException
      CharBuffer.wrap(zeros(capacity))
    }

    override def allocBuffer(pos: Int, limit: Int, capacity: Int): CharBuffer = {
      if (capacity < 0)
        throw new IllegalArgumentException
      CharBuffer.wrap(zeros(capacity), pos, limit)
    }

    override def withContent(pos: Int, limit: Int, capacity: Int,
        content: Char*): CharBuffer = {
      val after = capacity - (pos + content.size)
      CharBuffer.wrap(
        zeros(pos) + content.mkString + zeros(after),
        pos, limit)
    }
  }
}

class SlicedCharBufferWrappingACharSequenceTest extends CharBufferTest {

  val factory: CharBufferFactory = new SlicedCharBufferWrappingACharSequenceFactory

  class SlicedCharBufferWrappingACharSequenceFactory extends Factory {
    override val createsReadOnly = true

    def allocBuffer(capacity: Int): CharBuffer = {
      if (capacity < 0)
        throw new IllegalArgumentException
      val buf = CharBuffer.wrap(zeros(capacity+25))
      buf.position(17)
      buf.limit(17+capacity)
      buf.slice()
    }

    override def withContent(pos: Int, limit: Int, capacity: Int,
        content: Char*): CharBuffer = {
      if (!(0 <= pos && pos <= limit && limit <= capacity))
        throw new IllegalArgumentException
      val after = (25+capacity) - (9+pos+content.size)
      val buf = CharBuffer.wrap(zeros(9+pos) + content.mkString + zeros(after))
      buf.position(9)
      buf.limit(9+capacity)
      val buf2 = buf.slice()
      buf2.position(pos)
      buf2.limit(limit)
      buf2
    }
  }
}

// Char views of byte buffers

abstract class CharViewOfByteBufferTest(byteBufferFactory: BufferFactory.ByteBufferFactory,
    order: ByteOrder) extends CharBufferTest {
  val factory: CharBufferFactory =
    new ByteBufferCharViewFactory(byteBufferFactory, order)
}

class CharViewOfAllocByteBufferBigEndianTest
    extends CharViewOfByteBufferTest(new AllocByteBufferFactory, ByteOrder.BIG_ENDIAN)

class CharViewOfWrappedByteBufferBigEndianTest
    extends CharViewOfByteBufferTest(new WrappedByteBufferFactory, ByteOrder.BIG_ENDIAN)

class CharViewOfSlicedAllocByteBufferBigEndianTest
    extends CharViewOfByteBufferTest(new SlicedAllocByteBufferFactory, ByteOrder.BIG_ENDIAN)

class CharViewOfAllocByteBufferLittleEndianTest
    extends CharViewOfByteBufferTest(new AllocByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class CharViewOfWrappedByteBufferLittleEndianTest
    extends CharViewOfByteBufferTest(new WrappedByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class CharViewOfSlicedAllocByteBufferLittleEndianTest
    extends CharViewOfByteBufferTest(new SlicedAllocByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

// Read only Char views of byte buffers

abstract class ReadOnlyCharViewOfByteBufferTest(byteBufferFactory: BufferFactory.ByteBufferFactory,
    order: ByteOrder) extends CharBufferTest {
  val factory: CharBufferFactory =
    new ByteBufferCharViewFactory(byteBufferFactory, order) with BufferFactory.ReadOnlyBufferFactory

}

class ReadOnlyCharViewOfAllocByteBufferBigEndianTest
    extends ReadOnlyCharViewOfByteBufferTest(new AllocByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyCharViewOfWrappedByteBufferBigEndianTest
    extends ReadOnlyCharViewOfByteBufferTest(new WrappedByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyCharViewOfSlicedAllocByteBufferBigEndianTest
    extends ReadOnlyCharViewOfByteBufferTest(new SlicedAllocByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyCharViewOfAllocByteBufferLittleEndianTest
    extends ReadOnlyCharViewOfByteBufferTest(new AllocByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ReadOnlyCharViewOfWrappedByteBufferLittleEndianTest
    extends ReadOnlyCharViewOfByteBufferTest(new WrappedByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ReadOnlyCharViewOfSlicedAllocByteBufferLittleEndianTest
    extends ReadOnlyCharViewOfByteBufferTest(new SlicedAllocByteBufferFactory, ByteOrder.LITTLE_ENDIAN)
