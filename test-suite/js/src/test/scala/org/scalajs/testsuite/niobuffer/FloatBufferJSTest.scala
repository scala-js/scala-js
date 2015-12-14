/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.niobuffer

import java.nio._

import org.scalajs.testsuite.niobuffer.BufferFactory.FloatBufferFactory

import scala.scalajs.js
import js.typedarray._
import js.JSConverters._

import org.scalajs.testsuite.niobuffer.ByteBufferFactories._
import org.scalajs.testsuite.niobuffer.ByteBufferJSFactories._

object WrappedTypedArrayFloatBufferJSTest extends SupportsTypedArrays

class WrappedTypedArrayFloatBufferJSTest extends FloatBufferTest {

  val factory: FloatBufferFactory = new WrappedTypedArrayFloatBufferJSFactory

  class WrappedTypedArrayFloatBufferJSFactory
      extends Factory with BufferFactory.WrappedTypedArrayBufferFactory {
    def baseWrap(array: Array[Float]): FloatBuffer =
      TypedArrayBuffer.wrap(new Float32Array(array.toJSArray))
  }
}

// Float views of byte buffers

object FloatViewOfAllocDirectByteBufferBigEndianJSTest extends SupportsTypedArrays

class FloatViewOfAllocDirectByteBufferBigEndianJSTest
    extends FloatViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

object FloatViewOfSlicedAllocDirectByteBufferBigEndianJSTest extends SupportsTypedArrays

class FloatViewOfSlicedAllocDirectByteBufferBigEndianJSTest
    extends FloatViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

object FloatViewOfWrappedTypedArrayByteBufferBigEndianJSTest extends SupportsTypedArrays

class FloatViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends FloatViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

object FloatViewOfAllocDirectByteBufferLittleEndianJSTest extends SupportsTypedArrays

class FloatViewOfAllocDirectByteBufferLittleEndianJSTest
    extends FloatViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

object FloatViewOfSlicedAllocDirectByteBufferLittleEndianJSTest extends SupportsTypedArrays

class FloatViewOfSlicedAllocDirectByteBufferLittleEndianJSTest
    extends FloatViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

object FloatViewOfWrappedTypedArrayByteBufferLittleEndianJSTest extends SupportsTypedArrays

class FloatViewOfWrappedTypedArrayByteBufferLittleEndianJSTest
    extends FloatViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

// Read only Float views of byte buffers

object ReadOnlyFloatViewOfAllocDirectByteBufferBigEndianJSTest extends SupportsTypedArrays

class ReadOnlyFloatViewOfAllocDirectByteBufferBigEndianJSTest
    extends ReadOnlyFloatViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

object ReadOnlyFloatViewOfSlicedAllocDirectByteBufferBigEndianJSTest extends SupportsTypedArrays

class ReadOnlyFloatViewOfSlicedAllocDirectByteBufferBigEndianJSTest
    extends ReadOnlyFloatViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

object ReadOnlyFloatViewOfWrappedTypedArrayByteBufferBigEndianJSTest extends SupportsTypedArrays

class ReadOnlyFloatViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends ReadOnlyFloatViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

object ReadOnlyFloatViewOfAllocDirectByteBufferLittleEndianJSTest extends SupportsTypedArrays

class ReadOnlyFloatViewOfAllocDirectByteBufferLittleEndianJSTest
    extends ReadOnlyFloatViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

object ReadOnlyFloatViewOfSlicedAllocDirectByteBufferLittleEndianJSTest extends SupportsTypedArrays

class ReadOnlyFloatViewOfSlicedAllocDirectByteBufferLittleEndianJSTest
    extends ReadOnlyFloatViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

object ReadOnlyFloatViewOfWrappedTypedArrayByteBufferLittleEndianJSTest extends SupportsTypedArrays

class ReadOnlyFloatViewOfWrappedTypedArrayByteBufferLittleEndianJSTest
    extends ReadOnlyFloatViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)
