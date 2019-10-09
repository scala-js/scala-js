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

import java.nio._
import java.nio.channels._
import java.nio.file._

import java.io.IOException

import org.scalajs.linker.standard.OutputFileImpl

object PathOutputFile {
  def apply(path: Path): LinkerOutput.File = new PathOutputFileImpl(path)

  def atomic(path: Path): LinkerOutput.File = new AtomicPathOutputFileImpl(path)

  import OutputFileImpl.Channel

  private final class PathOutputFileImpl(path: Path) extends OutputFileImpl {
    def newChannel()(implicit ec: ExecutionContext): Future[Channel] =
      Future(blocking(new PathChannel(path)))
  }

  private final class AtomicPathOutputFileImpl(path: Path) extends OutputFileImpl {
    def newChannel()(implicit ec: ExecutionContext): Future[Channel] =
      Future(blocking(newAtomicChannel(path)))
  }

  private def newAtomicChannel(path: Path): Channel = {
    val tmpFile =
      Files.createTempFile(path.getParent(), ".tmp-" + path.getFileName(), ".tmp")

    try {
      val chan = new PathChannel(tmpFile)
      new AtomicChannel(path, tmpFile, chan)
    } catch {
      case t: Throwable =>
        Files.deleteIfExists(tmpFile)
        throw t
    }
  }

  private final class AtomicChannel(baseFile: Path, tmpFile: Path, chan: Channel) extends Channel {
    def write(buf: ByteBuffer)(implicit ec: ExecutionContext): Future[Unit] = chan.write(buf)

    def close()(implicit ec: ExecutionContext): Future[Unit] = {
      chan.close()
        .map(_ => blocking(move()))
        .finallyWith(Future(blocking(Files.deleteIfExists(tmpFile))))
    }

    private def move(): Unit = {
      try {
        // Try atomic move.
        Files.move(tmpFile, baseFile, StandardCopyOption.ATOMIC_MOVE)
      } catch {
        case _: IOException =>
          /* We need to catch all exceptions, because it is platform dependent:
           * - whether ATOMIC_MOVE overrides an existing file or not,
           * - it throws a FileAlreadyExistsException in this case.
           *
           * If the atomic move fails, we fall back to a normal copy & delete.
           */
          Files.copy(tmpFile, baseFile, StandardCopyOption.REPLACE_EXISTING)
      }
    }
  }

  private final class PathChannel(path: Path) extends Channel {
    import StandardOpenOption._

    private[this] var _pos = 0L

    private[this] val chan =
      AsynchronousFileChannel.open(path, WRITE, CREATE, TRUNCATE_EXISTING)

    def write(buf: ByteBuffer)(implicit ec: ExecutionContext): Future[Unit] = {
      val promise = Promise[Unit]()
      chan.write(buf, _pos, promise, Handler)
      promise.future
    }

    def close()(implicit ec: ExecutionContext): Future[Unit] =
      Future(blocking(chan.close()))

    private object Handler extends CompletionHandler[Integer, Promise[Unit]]{
      def completed(written: Integer, promise: Promise[Unit]): Unit = {
        _pos += written
        promise.success(())
      }

      def failed(exc: Throwable, promise: Promise[Unit]): Unit =
        promise.failure(exc)
    }
  }
}
