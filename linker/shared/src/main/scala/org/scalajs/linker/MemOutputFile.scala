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

    def writeFull(buf: ByteBuffer)(implicit ec: ExecutionContext): Future[Unit] = {
      val c = new Array[Byte](buf.remaining())
      buf.get(c)
      _content = c
      Future.successful(())
    }
  }
}
