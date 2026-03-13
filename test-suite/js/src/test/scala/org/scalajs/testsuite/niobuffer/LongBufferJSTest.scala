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

import org.scalajs.testsuite.niobuffer.ByteBufferFactories._
import org.scalajs.testsuite.niobuffer.ByteBufferJSFactories._

// Long views of byte buffers

object LongViewOfWrappedTypedArrayByteBufferBigEndianJSTest extends SupportsTypedArrays

class LongViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends LongViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

object LongViewOfWrappedTypedArrayByteBufferLittleEndianJSTest extends SupportsTypedArrays

class LongViewOfWrappedTypedArrayByteBufferLittleEndianJSTest
    extends LongViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

// Read only Long views of byte buffers

object ReadOnlyLongViewOfWrappedTypedArrayByteBufferBigEndianJSTest extends SupportsTypedArrays

class ReadOnlyLongViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends ReadOnlyLongViewOfByteBufferTest(
        new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

object ReadOnlyLongViewOfWrappedTypedArrayByteBufferLittleEndianJSTest extends SupportsTypedArrays

class ReadOnlyLongViewOfWrappedTypedArrayByteBufferLittleEndianJSTest
    extends ReadOnlyLongViewOfByteBufferTest(
        new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)
