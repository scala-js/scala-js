/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.niobuffer

import java.nio._

import org.scalajs.testsuite.niobuffer.BufferFactory.ShortBufferFactory

import scala.scalajs.js
import js.typedarray._
import js.JSConverters._

import org.scalajs.testsuite.niobuffer.ByteBufferFactories._
import org.scalajs.testsuite.niobuffer.ByteBufferJSFactories._

object WrappedTypedArrayShortBufferJSTest extends SupportsTypedArrays

class WrappedTypedArrayShortBufferJSTest extends ShortBufferTest {

  val factory: ShortBufferFactory = new WrappedTypedArrayShortBufferJSFactory

  class WrappedTypedArrayShortBufferJSFactory
      extends Factory with BufferFactory.WrappedTypedArrayBufferFactory {
    def baseWrap(array: Array[Short]): ShortBuffer =
      TypedArrayBuffer.wrap(new Int16Array(array.toJSArray))
  }
}

// Short views of byte buffers

object ShortViewOfAllocDirectByteBufferBigEndianJSTest extends SupportsTypedArrays

class ShortViewOfAllocDirectByteBufferBigEndianJSTest
    extends ShortViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

object ShortViewOfSlicedAllocDirectByteBufferBigEndianJSTest extends SupportsTypedArrays

class ShortViewOfSlicedAllocDirectByteBufferBigEndianJSTest
    extends ShortViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

object ShortViewOfWrappedTypedArrayByteBufferBigEndianJSTest extends SupportsTypedArrays

class ShortViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends ShortViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

object ShortViewOfAllocDirectByteBufferLittleEndianJSTest extends SupportsTypedArrays

class ShortViewOfAllocDirectByteBufferLittleEndianJSTest
    extends ShortViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

object ShortViewOfSlicedAllocDirectByteBufferLittleEndianJSTest extends SupportsTypedArrays

class ShortViewOfSlicedAllocDirectByteBufferLittleEndianJSTest
    extends ShortViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

object ShortViewOfWrappedTypedArrayByteBufferLittleEndianJSTest extends SupportsTypedArrays

class ShortViewOfWrappedTypedArrayByteBufferLittleEndianJSTest
    extends ShortViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

// Read only Short views of byte buffers

object ReadOnlyShortViewOfAllocDirectByteBufferBigEndianJSTest extends SupportsTypedArrays

class ReadOnlyShortViewOfAllocDirectByteBufferBigEndianJSTest
    extends ReadOnlyShortViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

object ReadOnlyShortViewOfSlicedAllocDirectByteBufferBigEndianJSTest extends SupportsTypedArrays

class ReadOnlyShortViewOfSlicedAllocDirectByteBufferBigEndianJSTest
    extends ReadOnlyShortViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

object ReadOnlyShortViewOfWrappedTypedArrayByteBufferBigEndianJSTest extends SupportsTypedArrays

class ReadOnlyShortViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends ReadOnlyShortViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

object ReadOnlyShortViewOfAllocDirectByteBufferLittleEndianJSTest extends SupportsTypedArrays

class ReadOnlyShortViewOfAllocDirectByteBufferLittleEndianJSTest
    extends ReadOnlyShortViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

object ReadOnlyShortViewOfSlicedAllocDirectByteBufferLittleEndianJSTest extends SupportsTypedArrays

class ReadOnlyShortViewOfSlicedAllocDirectByteBufferLittleEndianJSTest
    extends ReadOnlyShortViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

object ReadOnlyShortViewOfWrappedTypedArrayByteBufferLittleEndianJSTest extends SupportsTypedArrays

class ReadOnlyShortViewOfWrappedTypedArrayByteBufferLittleEndianJSTest
    extends ReadOnlyShortViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)
