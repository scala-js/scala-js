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

class LongViewOfAllocDirectByteBufferBigEndianJSTest
    extends LongViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

class LongViewOfSlicedAllocDirectByteBufferBigEndianJSTest
    extends LongViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

class LongViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends LongViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

class LongViewOfAllocDirectByteBufferLittleEndianJSTest
    extends LongViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class LongViewOfSlicedAllocDirectByteBufferLittleEndianJSTest
    extends LongViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class LongViewOfWrappedTypedArrayByteBufferLittleEndianJSTest
    extends LongViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

// Read only Long views of byte buffers

class ReadOnlyLongViewOfAllocDirectByteBufferBigEndianJSTest
    extends ReadOnlyLongViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyLongViewOfSlicedAllocDirectByteBufferBigEndianJSTest
    extends ReadOnlyLongViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyLongViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends ReadOnlyLongViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

class ReadOnlyLongViewOfAllocDirectByteBufferLittleEndianJSTest
    extends ReadOnlyLongViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ReadOnlyLongViewOfSlicedAllocDirectByteBufferLittleEndianJSTest
    extends ReadOnlyLongViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

class ReadOnlyLongViewOfWrappedTypedArrayByteBufferLittleEndianJSTest
    extends ReadOnlyLongViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)
