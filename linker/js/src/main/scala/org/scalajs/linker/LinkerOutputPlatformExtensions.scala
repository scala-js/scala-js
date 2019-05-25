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

import org.scalajs.linker.irio.NodeFS._
import org.scalajs.linker.standard.OutputFileImpl

abstract class LinkerOutputPlatformExtensions private[linker] () {
  def newNodeFile(path: String): LinkerOutput.File =
    new LinkerOutputPlatformExtensions.NodeOutputFileImpl(path)
}

private object LinkerOutputPlatformExtensions {
  private final class NodeOutputFileImpl(path: String) extends OutputFileImpl {
    def newChannel()(implicit ec: ExecutionContext): Future[OutputFileImpl.Channel] = {
      cbFuture[Int](FS.open(path, "w", _)).map(new NodeOutputChannel(_))
    }
  }

  private final class NodeOutputChannel(fd: Int) extends OutputFileImpl.Channel {
    def write(buf: ByteBuffer)(implicit ec: ExecutionContext): Future[Unit] = {
      val pos = buf.position()
      val write = {
        if (buf.hasTypedArray) {
          cbFuture[Int](FS.write(fd, buf.typedArray(), pos, buf.remaining(), (), _))
        } else {
          val ta = ByteBuffer.allocateDirect(buf.remaining()).put(buf).typedArray()
          cbFuture[Int](FS.write(fd, ta, 0, ta.length, js.undefined, _))
        }
      }

      write.map(bytesWritten => buf.position(pos + bytesWritten))
    }

    def close()(implicit ec: ExecutionContext): Future[Unit] =
      cbFuture[Unit](FS.close(fd, _))
  }
}
