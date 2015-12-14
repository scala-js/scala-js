/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.niobuffer

import java.nio._

import org.scalajs.testsuite.niobuffer.BufferFactory.DoubleBufferFactory

import scala.scalajs.js
import js.typedarray._
import js.JSConverters._

import org.scalajs.testsuite.niobuffer.ByteBufferFactories._
import org.scalajs.testsuite.niobuffer.ByteBufferJSFactories._

object WrappedTypedArrayDoubleBufferJSTest extends SupportsTypedArrays

class WrappedTypedArrayDoubleBufferJSTest extends DoubleBufferTest {

  val factory: DoubleBufferFactory = new WrappedTypedArrayDoubleBufferJSFactory

  class WrappedTypedArrayDoubleBufferJSFactory
      extends Factory with BufferFactory.WrappedTypedArrayBufferFactory {
    def baseWrap(array: Array[Double]): DoubleBuffer =
      TypedArrayBuffer.wrap(new Float64Array(array.toJSArray))
  }
}

// Double views of byte buffers

object DoubleViewOfAllocDirectByteBufferBigEndianJSTest extends SupportsTypedArrays

class DoubleViewOfAllocDirectByteBufferBigEndianJSTest
    extends DoubleViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

object DoubleViewOfSlicedAllocDirectByteBufferBigEndianJSTest extends SupportsTypedArrays

class DoubleViewOfSlicedAllocDirectByteBufferBigEndianJSTest
    extends DoubleViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

object DoubleViewOfWrappedTypedArrayByteBufferBigEndianJSTest extends SupportsTypedArrays

class DoubleViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends DoubleViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

object DoubleViewOfAllocDirectByteBufferLittleEndianJSTest extends SupportsTypedArrays

class DoubleViewOfAllocDirectByteBufferLittleEndianJSTest
    extends DoubleViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

object DoubleViewOfSlicedAllocDirectByteBufferLittleEndianJSTest extends SupportsTypedArrays

class DoubleViewOfSlicedAllocDirectByteBufferLittleEndianJSTest
    extends DoubleViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

object DoubleViewOfWrappedTypedArrayByteBufferLittleEndianJSTest extends SupportsTypedArrays

class DoubleViewOfWrappedTypedArrayByteBufferLittleEndianJSTest
    extends DoubleViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

// Read only Double views of byte buffers

object ReadOnlyDoubleViewOfAllocDirectByteBufferBigEndianJSTest extends SupportsTypedArrays

class ReadOnlyDoubleViewOfAllocDirectByteBufferBigEndianJSTest
    extends ReadOnlyDoubleViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

object ReadOnlyDoubleViewOfSlicedAllocDirectByteBufferBigEndianJSTest extends SupportsTypedArrays

class ReadOnlyDoubleViewOfSlicedAllocDirectByteBufferBigEndianJSTest
    extends ReadOnlyDoubleViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

object ReadOnlyDoubleViewOfWrappedTypedArrayByteBufferBigEndianJSTest extends SupportsTypedArrays

class ReadOnlyDoubleViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends ReadOnlyDoubleViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

object ReadOnlyDoubleViewOfAllocDirectByteBufferLittleEndianJSTest extends SupportsTypedArrays

class ReadOnlyDoubleViewOfAllocDirectByteBufferLittleEndianJSTest
    extends ReadOnlyDoubleViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

object ReadOnlyDoubleViewOfSlicedAllocDirectByteBufferLittleEndianJSTest extends SupportsTypedArrays

class ReadOnlyDoubleViewOfSlicedAllocDirectByteBufferLittleEndianJSTest
    extends ReadOnlyDoubleViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

object ReadOnlyDoubleViewOfWrappedTypedArrayByteBufferLittleEndianJSTest extends SupportsTypedArrays

class ReadOnlyDoubleViewOfWrappedTypedArrayByteBufferLittleEndianJSTest
    extends ReadOnlyDoubleViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)
