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

object ShortViewOfWrappedTypedArrayByteBufferBigEndianJSTest extends SupportsTypedArrays

class ShortViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends ShortViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

object ShortViewOfWrappedTypedArrayByteBufferLittleEndianJSTest extends SupportsTypedArrays

class ShortViewOfWrappedTypedArrayByteBufferLittleEndianJSTest extends ShortViewOfByteBufferTest(
        new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

// Read only Short views of byte buffers

object ReadOnlyShortViewOfWrappedTypedArrayByteBufferBigEndianJSTest extends SupportsTypedArrays

class ReadOnlyShortViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends ReadOnlyShortViewOfByteBufferTest(
        new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

object ReadOnlyShortViewOfWrappedTypedArrayByteBufferLittleEndianJSTest extends SupportsTypedArrays

class ReadOnlyShortViewOfWrappedTypedArrayByteBufferLittleEndianJSTest
    extends ReadOnlyShortViewOfByteBufferTest(
        new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)
