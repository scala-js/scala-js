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

import org.scalajs.testsuite.niobuffer.BufferFactory.ByteBufferFactory

class AllocDirectByteBufferJSTest extends ByteBufferTest {
  val factory: ByteBufferFactory =
    new ByteBufferFactories.AllocDirectByteBufferFactory
}

class SlicedAllocDirectByteBufferJSTest extends ByteBufferTest {
  val factory: ByteBufferFactory =
    new ByteBufferFactories.SlicedAllocDirectByteBufferFactory
}

class WrappedTypedArrayByteBufferJSTest extends ByteBufferTest {
  val factory: ByteBufferFactory =
    new ByteBufferJSFactories.WrappedTypedArrayByteBufferFactory
}
