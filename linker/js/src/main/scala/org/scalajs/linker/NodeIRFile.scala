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
import scala.scalajs.js.typedarray._
import scala.scalajs.js.typedarray.TypedArrayBufferOps._

import org.scalajs.linker.interface.IRFile
import org.scalajs.linker.interface.unstable.IRFileImpl

import java.io.EOFException
import java.nio._

import org.scalajs.ir

object NodeIRFile {
  import NodeFS._

  def apply(path: String)(implicit ec: ExecutionContext): Future[IRFile] = {
    cbFuture[Stats](stat(path, _)).map(
      stats => new NodeIRFileImpl(path, stats.mtime.toOption))
  }

  private[linker] def dateToVersion(optDate: Option[js.Date]): ir.Version = {
    optDate
      .map(_.getTime())
      // filter invalid dates and over / underflows.
      .filter(d => !d.isNaN && !d.isInfinity)
      .map(_.toLong)
      .fold(ir.Version.Unversioned)(ir.Version.fromLong(_))
  }

  private final class NodeIRFileImpl(path: String, version: Option[js.Date])
      extends IRFileImpl(path, dateToVersion(version)) {

    def entryPointsInfo(implicit ec: ExecutionContext): Future[ir.EntryPointsInfo] = {
      def loop(fd: Int, buf: ByteBuffer): Future[ir.EntryPointsInfo] = {
        val len = buf.remaining()
        val off = buf.position()

        cbFuture[Int](read(fd, buf.typedArray(), off, len, off, _)).map { bytesRead =>
          if (bytesRead <= 0)
            throw new EOFException

          buf.position(buf.position() + bytesRead)
          buf.flip()
          ir.Serializers.deserializeEntryPointsInfo(buf)
        }.recoverWith {
          case _: BufferUnderflowException =>
            // Reset to write again.
            buf.position(buf.limit())
            buf.limit(buf.capacity())

            val newBuf = if (buf.remaining() <= 0) {
              val newBuf = ByteBuffer.allocateDirect(buf.capacity() * 2)
              buf.flip()
              newBuf.put(buf)
              buf
            } else {
              buf
            }

            loop(fd, newBuf)
        }
      }

      val result = cbFuture[Int](open(path, "r", _)).flatMap { fd =>
        loop(fd, ByteBuffer.allocateDirect(1024))
          .finallyWith(cbFuture[Unit](close(fd, _)))
      }

      IRFileImpl.withPathExceptionContext(path, result)
    }

    def tree(implicit ec: ExecutionContext): Future[ir.Trees.ClassDef] = {
      val result = cbFuture[Uint8Array](readFile(path, _)).map { arr =>
        ir.Serializers.deserialize(TypedArrayBuffer.wrap(arr.buffer))
      }

      IRFileImpl.withPathExceptionContext(path, result)
    }
  }
}
