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

import java.io._
import java.nio._
import java.nio.channels._
import java.nio.file._
import java.nio.file.attribute._

import org.scalajs.ir
import org.scalajs.linker.standard.IRFileImpl

abstract class IRFilePlatformExtensions private[linker] () {
  def fromPath(path: Path)(implicit ec: ExecutionContext): Future[IRFile] = {
    Future(blocking(Files.getLastModifiedTime(path)))
      .map(new IRFilePlatformExtensions.PathIRFileImpl(path, _))
  }
}

private object IRFilePlatformExtensions {
  private[linker] final class PathIRFileImpl(path: Path, lastModified: FileTime)
      extends IRFileImpl(path.toString, Some(lastModified.toString)) {
    def entryPointsInfo(implicit ec: ExecutionContext): Future[ir.EntryPointsInfo] = {
      def loop(chan: AsynchronousFileChannel, buf: ByteBuffer): Future[ir.EntryPointsInfo] = {
        readAsync(chan, buf).map { _ =>
          buf.flip()
          ir.Serializers.deserializeEntryPointsInfo(buf)
        }.recoverWith {
          case _: BufferUnderflowException =>
            // Reset to write again.
            buf.position(buf.limit())
            buf.limit(buf.capacity())

            val newBuf = if (buf.remaining() <= 0) {
              val newBuf = ByteBuffer.allocate(buf.capacity() * 2)
              buf.flip()
              newBuf.put(buf)
              buf
            } else {
              buf
            }

            loop(chan, newBuf)
        }
      }

      withChannel(loop(_, ByteBuffer.allocate(1024)))
    }

    def tree(implicit ec: ExecutionContext): Future[ir.Trees.ClassDef] = {
      withChannel { chan =>
        val s = chan.size()
        if (s > Int.MaxValue) {
          throw new IOException(s"$path is too big ($s bytes)")
        } else {
          val buf = ByteBuffer.allocate(s.toInt)
          def read(): Future[Unit] = readAsync(chan, buf).flatMap { _ =>
            if (buf.hasRemaining()) read()
            else Future.successful(())
          }

          read().map { _ =>
            buf.flip()
            ir.Serializers.deserialize(buf)
          }
        }
      }
    }

    private def withChannel[T](body: AsynchronousFileChannel => Future[T])(
        implicit ec: ExecutionContext): Future[T] = {
      val result = Future(AsynchronousFileChannel.open(path)).flatMap { chan =>
        body(chan).finallyWith(Future(blocking(chan.close())))
      }

      IRFileImpl.withPathExceptionContext(path.toString, result)
    }
  }

  private def readAsync(chan: AsynchronousFileChannel, buf: ByteBuffer): Future[Unit] = {
    val promise = Promise[Unit]()
    chan.read(buf, buf.position(), promise, ReadCompletionHandler)
    promise.future
  }

  private object ReadCompletionHandler extends CompletionHandler[Integer, Promise[Unit]] {
    def completed(result: Integer, attachment: Promise[Unit]): Unit = {
      if (result <= 0)
        attachment.failure(new EOFException)
      else
        attachment.success(())
    }

    def failed(exc: Throwable, attachment: Promise[Unit]): Unit =
      attachment.failure(exc)
  }
}
