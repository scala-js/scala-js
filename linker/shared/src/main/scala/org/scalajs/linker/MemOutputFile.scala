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

package org.scalajs.linker

import scala.concurrent._

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer

import org.scalajs.linker.interface.LinkerOutput
import org.scalajs.linker.interface.unstable.OutputFileImpl

sealed trait MemOutputFile extends LinkerOutput.File {
  /** Content that has been written to this [[MemOutputFile]].
   *
   *  @throws java.lang.IllegalStateException if nothing has been written yet.
   */
  def content: Array[Byte]
}

object MemOutputFile {
  def apply(): MemOutputFile = new MemFileImpl()

  private final class MemFileImpl extends OutputFileImpl with MemOutputFile {
    @volatile
    private var _content: Array[Byte] = _

    def content: Array[Byte] = {
      if (_content == null)
        throw new IllegalStateException("content hasn't been written yet")
      _content
    }

    def sibling(name: String): Nothing = ???

    def newChannel()(implicit ec: ExecutionContext): Future[OutputFileImpl.Channel] =
      Future.successful(new Channel)

    override def writeFull(buf: ByteBuffer)(implicit ec: ExecutionContext): Future[Unit] = {
      val c = new Array[Byte](buf.remaining())
      buf.get(c)
      _content = c
      Future.successful(())
    }

    private class Channel extends OutputFileImpl.Channel {
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
}
