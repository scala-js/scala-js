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

import java.io._

/** A ByteArrayOutputStream that exposes various hooks for testing purposes. */
class MockByteArrayOutputStream extends ByteArrayOutputStream {
  private var _flushed: Boolean = true
  private var _closed: Boolean = false

  var throwing: Boolean = false

  def flushed: Boolean = _flushed
  def closed: Boolean = _closed

  private def maybeThrow(): Unit = {
    if (throwing)
      throw new IOException("MockByteArrayOutputStream throws")
  }

  private def writeOp[A](op: => A): A = {
    maybeThrow()
    _flushed = false
    op
  }

  override def flush(): Unit = {
    maybeThrow()
    super.flush()
    _flushed = true
  }

  override def close(): Unit = {
    maybeThrow()
    super.close()
    _closed = true
  }

  override def write(c: Int): Unit =
    writeOp(super.write(c))

  override def write(b: Array[Byte]): Unit =
    writeOp(super.write(b))

  override def write(b: Array[Byte], off: Int, len: Int): Unit =
    writeOp(super.write(b, off, len))
}
