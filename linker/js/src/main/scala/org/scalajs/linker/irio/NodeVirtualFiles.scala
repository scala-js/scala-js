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

import scala.annotation.tailrec

import scala.concurrent._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray._
import scala.scalajs.js.typedarray.TypedArrayBufferOps._

import java.io._
import java.net.URI
import java.nio.ByteBuffer

final class WritableNodeVirtualBinaryFile(path: String) extends WritableVirtualBinaryFile {
  import NodeFS._

  def newChannel()(implicit ec: ExecutionContext): Future[WriteChannel] = {
    cbFuture[Int](FS.open(path, "w", _)).map(new WritableNodeVirtualBinaryFile.Channel(_))
  }
}

private object WritableNodeVirtualBinaryFile {
  import NodeFS._

  private final class Channel(fd: Int) extends WriteChannel {
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
