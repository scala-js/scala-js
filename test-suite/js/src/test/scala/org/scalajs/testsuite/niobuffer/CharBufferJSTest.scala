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

import org.scalajs.testsuite.niobuffer.BufferFactory.CharBufferFactory

import scala.scalajs.js
import js.typedarray._
import js.JSConverters._

import org.scalajs.testsuite.niobuffer.ByteBufferFactories._
import org.scalajs.testsuite.niobuffer.ByteBufferJSFactories._

class WrappedTypedArrayCharBufferJSTest extends CharBufferTest {

  val factory: CharBufferFactory = new WrappedTypedArrayCharBufferJSFactory

  class WrappedTypedArrayCharBufferJSFactory
      extends Factory with BufferFactory.WrappedTypedArrayBufferFactory {
    def baseWrap(array: Array[Char]): CharBuffer =
      TypedArrayBuffer.wrap(new Uint16Array(array.map(_.toInt).toJSArray))
  }
}

// Char views of byte buffers

class CharViewOfAllocDirectByteBufferBigEndianJSTest
    extends CharViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

class CharViewOfSlicedAllocDirectByteBufferBigEndianJSTest
    extends CharViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

class CharViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends CharViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

class CharViewOfAllocDirectByteBufferLittleEndianJSTest
    extends CharViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class CharViewOfSlicedAllocDirectByteBufferLittleEndianJSTest
    extends CharViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class CharViewOfWrappedTypedArrayByteBufferLittleEndianJSTest
    extends CharViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

// Read only Char views of byte buffers

class ReadOnlyCharViewOfAllocDirectByteBufferBigEndianJSTest
    extends ReadOnlyCharViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyCharViewOfSlicedAllocDirectByteBufferBigEndianJSTest
    extends ReadOnlyCharViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyCharViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends ReadOnlyCharViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyCharViewOfAllocDirectByteBufferLittleEndianJSTest
    extends ReadOnlyCharViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ReadOnlyCharViewOfSlicedAllocDirectByteBufferLittleEndianJSTest
    extends ReadOnlyCharViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ReadOnlyCharViewOfWrappedTypedArrayByteBufferLittleEndianJSTest
    extends ReadOnlyCharViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)
