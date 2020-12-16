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

package org.scalajs.testsuite.javalib.io

import org.junit.Test
import org.junit.Assert._

import java.io.Closeable

class AutoCloseableTest {
  import AutoCloseableTest._

  private def close(x: AutoCloseable): Unit = x.close()

  @Test
  def closeableExtendsAutoCloseable(): Unit = {
    val x = new SomeCloseable
    assertFalse(x.closed)
    close(x)
    assertTrue(x.closed)
  }

  @Test
  def byteArrayOutputStreamIsAnAutoCloseable_Issue3107(): Unit = {
    val x = new java.io.ByteArrayOutputStream
    close(x)
  }
}

object AutoCloseableTest {
  class SomeCloseable extends Closeable {
    var closed: Boolean = false

    def close(): Unit = closed = true
  }
}
