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

package org.scalajs.linker.irio

import scala.concurrent._

import java.io._
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

final class WritableMemVirtualBinaryFile extends WritableVirtualBinaryFile {
  private var _content: Array[Byte] = _

  def content: Array[Byte] = _content

  def newChannel()(implicit ec: ExecutionContext): Future[WriteChannel] =
    Future.successful(new Channel)

  override def writeFull(buf: ByteBuffer)(implicit ec: ExecutionContext): Future[Unit] = {
    val c = new Array[Byte](buf.remaining())
    buf.get(c)
    _content = c
    Future.successful(())
  }

  private class Channel extends WriteChannel {
    private val out = new ByteArrayOutputStream

    def write(buf: ByteBuffer)(implicit ec: ExecutionContext): Future[Unit] = Future {
      val promise = Promise[Unit]()
      if (buf.hasArray()) {
        out.write(buf.array(), buf.arrayOffset() + buf.position(), buf.remaining())
        buf.position(buf.limit())
      } else {
        val c = new Array[Byte](buf.remaining())
        buf.get(c)
        out.write(c)
      }
    }

    def close()(implicit ec: ExecutionContext): Future[Unit] = {
      _content = out.toByteArray
      Future.successful(())
    }
  }
}
