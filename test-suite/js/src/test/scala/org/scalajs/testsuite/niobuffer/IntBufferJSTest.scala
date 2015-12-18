/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.niobuffer

import java.nio._

import org.scalajs.testsuite.niobuffer.BufferFactory.IntBufferFactory

import scala.scalajs.js
import js.typedarray._
import js.JSConverters._

import org.scalajs.testsuite.niobuffer.ByteBufferFactories._
import org.scalajs.testsuite.niobuffer.ByteBufferJSFactories._

object WrappedTypedArrayIntBufferJSTest extends SupportsTypedArrays

class WrappedTypedArrayIntBufferJSTest extends IntBufferTest {

  val factory: IntBufferFactory = new WrappedTypedArrayIntBufferJSFactory

  class WrappedTypedArrayIntBufferJSFactory
      extends Factory with BufferFactory.WrappedTypedArrayBufferFactory {
    def baseWrap(array: Array[Int]): IntBuffer =
      TypedArrayBuffer.wrap(new Int32Array(array.toJSArray))
  }
}

// Int views of byte buffers

object IntViewOfAllocDirectByteBufferBigEndianJSTest extends SupportsTypedArrays

class IntViewOfAllocDirectByteBufferBigEndianJSTest
    extends IntViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

object IntViewOfSlicedAllocDirectByteBufferBigEndianJSTest extends SupportsTypedArrays

class IntViewOfSlicedAllocDirectByteBufferBigEndianJSTest
    extends IntViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

object IntViewOfWrappedTypedArrayByteBufferBigEndianJSTest extends SupportsTypedArrays

class IntViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends IntViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

object IntViewOfAllocDirectByteBufferLittleEndianJSTest extends SupportsTypedArrays

class IntViewOfAllocDirectByteBufferLittleEndianJSTest
    extends IntViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

object IntViewOfSlicedAllocDirectByteBufferLittleEndianJSTest extends SupportsTypedArrays

class IntViewOfSlicedAllocDirectByteBufferLittleEndianJSTest
    extends IntViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

object IntViewOfWrappedTypedArrayByteBufferLittleEndianJSTest extends SupportsTypedArrays

class IntViewOfWrappedTypedArrayByteBufferLittleEndianJSTest
    extends IntViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

// Read only Int views of byte buffers

object ReadOnlyIntViewOfAllocDirectByteBufferBigEndianJSTest extends SupportsTypedArrays

class ReadOnlyIntViewOfAllocDirectByteBufferBigEndianJSTest
    extends ReadOnlyIntViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

object ReadOnlyIntViewOfSlicedAllocDirectByteBufferBigEndianJSTest extends SupportsTypedArrays

class ReadOnlyIntViewOfSlicedAllocDirectByteBufferBigEndianJSTest
    extends ReadOnlyIntViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

object ReadOnlyIntViewOfWrappedTypedArrayByteBufferBigEndianJSTest extends SupportsTypedArrays

class ReadOnlyIntViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends ReadOnlyIntViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

object ReadOnlyIntViewOfAllocDirectByteBufferLittleEndianJSTest extends SupportsTypedArrays

class ReadOnlyIntViewOfAllocDirectByteBufferLittleEndianJSTest
    extends ReadOnlyIntViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

object ReadOnlyIntViewOfSlicedAllocDirectByteBufferLittleEndianJSTest extends SupportsTypedArrays

class ReadOnlyIntViewOfSlicedAllocDirectByteBufferLittleEndianJSTest
    extends ReadOnlyIntViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

object ReadOnlyIntViewOfWrappedTypedArrayByteBufferLittleEndianJSTest extends SupportsTypedArrays

class ReadOnlyIntViewOfWrappedTypedArrayByteBufferLittleEndianJSTest
    extends ReadOnlyIntViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)
