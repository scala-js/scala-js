/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.niobuffer

import org.scalajs.testsuite.niobuffer.BufferFactory.ByteBufferFactory

object AllocDirectByteBufferJSTest extends SupportsTypedArrays

class AllocDirectByteBufferJSTest extends ByteBufferTest {
  val factory: ByteBufferFactory =
    new ByteBufferFactories.AllocDirectByteBufferFactory
}

object SlicedAllocDirectByteBufferJSTest extends SupportsTypedArrays

class SlicedAllocDirectByteBufferJSTest extends ByteBufferTest {
  val factory: ByteBufferFactory =
    new ByteBufferFactories.SlicedAllocDirectByteBufferFactory
}

object WrappedTypedArrayByteBufferJSTest extends SupportsTypedArrays

class WrappedTypedArrayByteBufferJSTest extends ByteBufferTest {
  val factory: ByteBufferFactory =
    new ByteBufferJSFactories.WrappedTypedArrayByteBufferFactory
}
