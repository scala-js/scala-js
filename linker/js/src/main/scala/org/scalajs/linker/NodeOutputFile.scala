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

import scala.scalajs.js
import scala.scalajs.js.typedarray.TypedArrayBufferOps._

import java.nio.ByteBuffer

import org.scalajs.linker.interface.LinkerOutput
import org.scalajs.linker.interface.unstable.OutputFileImpl

object NodeOutputFile {
  import NodeFS.cbFuture

  def apply(path: String): LinkerOutput.File = new NodeOutputFileImpl(path)

  private final class NodeOutputFileImpl(path: String) extends OutputFileImpl {
    def writeFull(buf: ByteBuffer)(implicit ec: ExecutionContext): Future[Unit] = {
      val data =
        if (buf.hasTypedArray()) buf.typedArray().subarray(buf.position(), buf.limit())
        else ByteBuffer.allocateDirect(buf.remaining()).put(buf).typedArray()

      cbFuture[Unit](NodeFS.writeFile(path, data, _))
        .map(_ => buf.position(buf.limit()))
    }
  }
}
