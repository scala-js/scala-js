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

object LongViewOfAllocDirectByteBufferBigEndianJSTest extends SupportsTypedArrays

class LongViewOfAllocDirectByteBufferBigEndianJSTest
    extends LongViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

object LongViewOfSlicedAllocDirectByteBufferBigEndianJSTest extends SupportsTypedArrays

class LongViewOfSlicedAllocDirectByteBufferBigEndianJSTest
    extends LongViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

object LongViewOfWrappedTypedArrayByteBufferBigEndianJSTest extends SupportsTypedArrays

class LongViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends LongViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

object LongViewOfAllocDirectByteBufferLittleEndianJSTest extends SupportsTypedArrays

class LongViewOfAllocDirectByteBufferLittleEndianJSTest
    extends LongViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

object LongViewOfSlicedAllocDirectByteBufferLittleEndianJSTest extends SupportsTypedArrays

class LongViewOfSlicedAllocDirectByteBufferLittleEndianJSTest
    extends LongViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

object LongViewOfWrappedTypedArrayByteBufferLittleEndianJSTest extends SupportsTypedArrays

class LongViewOfWrappedTypedArrayByteBufferLittleEndianJSTest
    extends LongViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

// Read only Long views of byte buffers

object ReadOnlyLongViewOfAllocDirectByteBufferBigEndianJSTest extends SupportsTypedArrays

class ReadOnlyLongViewOfAllocDirectByteBufferBigEndianJSTest
    extends ReadOnlyLongViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

object ReadOnlyLongViewOfSlicedAllocDirectByteBufferBigEndianJSTest extends SupportsTypedArrays

class ReadOnlyLongViewOfSlicedAllocDirectByteBufferBigEndianJSTest
    extends ReadOnlyLongViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.BIG_ENDIAN)

object ReadOnlyLongViewOfWrappedTypedArrayByteBufferBigEndianJSTest extends SupportsTypedArrays

class ReadOnlyLongViewOfWrappedTypedArrayByteBufferBigEndianJSTest
    extends ReadOnlyLongViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.BIG_ENDIAN)

object ReadOnlyLongViewOfAllocDirectByteBufferLittleEndianJSTest extends SupportsTypedArrays

class ReadOnlyLongViewOfAllocDirectByteBufferLittleEndianJSTest
    extends ReadOnlyLongViewOfByteBufferTest(new AllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

object ReadOnlyLongViewOfSlicedAllocDirectByteBufferLittleEndianJSTest extends SupportsTypedArrays

class ReadOnlyLongViewOfSlicedAllocDirectByteBufferLittleEndianJSTest
    extends ReadOnlyLongViewOfByteBufferTest(new SlicedAllocDirectByteBufferFactory, ByteOrder.LITTLE_ENDIAN)

object ReadOnlyLongViewOfWrappedTypedArrayByteBufferLittleEndianJSTest extends SupportsTypedArrays

class ReadOnlyLongViewOfWrappedTypedArrayByteBufferLittleEndianJSTest
    extends ReadOnlyLongViewOfByteBufferTest(new WrappedTypedArrayByteBufferFactory, ByteOrder.LITTLE_ENDIAN)
