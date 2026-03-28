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

object DoubleViewOfWrappedTypedArrayByteBufferBigEndianJSTest extends SupportsTypedArrays

class DoubleViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends DoubleViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

object DoubleViewOfWrappedTypedArrayByteBufferLittleEndianJSTest extends SupportsTypedArrays

class DoubleViewOfWrappedTypedArrayByteBufferLittleEndianJSTest extends DoubleViewOfByteBufferTest(
        new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

// Read only Double views of byte buffers

object ReadOnlyDoubleViewOfWrappedTypedArrayByteBufferBigEndianJSTest extends SupportsTypedArrays

class ReadOnlyDoubleViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends ReadOnlyDoubleViewOfByteBufferTest(
        new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

object ReadOnlyDoubleViewOfWrappedTypedArrayByteBufferLittleEndianJSTest extends SupportsTypedArrays

class ReadOnlyDoubleViewOfWrappedTypedArrayByteBufferLittleEndianJSTest
    extends ReadOnlyDoubleViewOfByteBufferTest(
        new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)
