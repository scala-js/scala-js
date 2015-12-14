/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.niobuffer

import java.nio._

import org.scalajs.testsuite.niobuffer.BufferFactory.CharBufferFactory

import scala.scalajs.js
import js.typedarray._
import js.JSConverters._

import org.scalajs.testsuite.niobuffer.ByteBufferFactories._
import org.scalajs.testsuite.niobuffer.ByteBufferJSFactories._

object WrappedTypedArrayCharBufferJSTest extends SupportsTypedArrays

class WrappedTypedArrayCharBufferJSTest extends CharBufferTest {

  val factory: CharBufferFactory = new WrappedTypedArrayCharBufferJSFactory

  class WrappedTypedArrayCharBufferJSFactory
      extends Factory with BufferFactory.WrappedTypedArrayBufferFactory {
    def baseWrap(array: Array[Char]): CharBuffer =
      TypedArrayBuffer.wrap(new Uint16Array(array.map(_.toInt).toJSArray))
  }
}

// Char views of byte buffers

object CharViewOfAllocDirectByteBufferBigEndianJSTest extends SupportsTypedArrays

class CharViewOfAllocDirectByteBufferBigEndianJSTest
    extends CharViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

object CharViewOfSlicedAllocDirectByteBufferBigEndianJSTest extends SupportsTypedArrays

class CharViewOfSlicedAllocDirectByteBufferBigEndianJSTest
    extends CharViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

object CharViewOfWrappedTypedArrayByteBufferBigEndianJSTest extends SupportsTypedArrays

class CharViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends CharViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

object CharViewOfAllocDirectByteBufferLittleEndianJSTest extends SupportsTypedArrays

class CharViewOfAllocDirectByteBufferLittleEndianJSTest
    extends CharViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

object CharViewOfSlicedAllocDirectByteBufferLittleEndianJSTest extends SupportsTypedArrays

class CharViewOfSlicedAllocDirectByteBufferLittleEndianJSTest
    extends CharViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

object CharViewOfWrappedTypedArrayByteBufferLittleEndianJSTest extends SupportsTypedArrays

class CharViewOfWrappedTypedArrayByteBufferLittleEndianJSTest
    extends CharViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

// Read only Char views of byte buffers

object ReadOnlyCharViewOfAllocDirectByteBufferBigEndianJSTest extends SupportsTypedArrays

class ReadOnlyCharViewOfAllocDirectByteBufferBigEndianJSTest
    extends ReadOnlyCharViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

object ReadOnlyCharViewOfSlicedAllocDirectByteBufferBigEndianJSTest extends SupportsTypedArrays

class ReadOnlyCharViewOfSlicedAllocDirectByteBufferBigEndianJSTest
    extends ReadOnlyCharViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

object ReadOnlyCharViewOfWrappedTypedArrayByteBufferBigEndianJSTest extends SupportsTypedArrays

class ReadOnlyCharViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends ReadOnlyCharViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

object ReadOnlyCharViewOfAllocDirectByteBufferLittleEndianJSTest extends SupportsTypedArrays

class ReadOnlyCharViewOfAllocDirectByteBufferLittleEndianJSTest
    extends ReadOnlyCharViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

object ReadOnlyCharViewOfSlicedAllocDirectByteBufferLittleEndianJSTest extends SupportsTypedArrays

class ReadOnlyCharViewOfSlicedAllocDirectByteBufferLittleEndianJSTest
    extends ReadOnlyCharViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

object ReadOnlyCharViewOfWrappedTypedArrayByteBufferLittleEndianJSTest extends SupportsTypedArrays

class ReadOnlyCharViewOfWrappedTypedArrayByteBufferLittleEndianJSTest
    extends ReadOnlyCharViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)
