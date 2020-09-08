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

import org.scalajs.linker.interface.LinkerOutput
import org.scalajs.linker.interface.unstable.OutputFileImpl

object PathOutputFile {
  def apply(path: Path): LinkerOutput.File = new PathOutputFileImpl(path)

  def atomic(path: Path): LinkerOutput.File = new AtomicPathOutputFileImpl(path)

  private final class PathOutputFileImpl(path: Path) extends OutputFileImpl {
    def writeFull(buf: ByteBuffer)(implicit ec: ExecutionContext): Future[Unit] =
      writeFullImpl(path, buf)
  }

  private final class AtomicPathOutputFileImpl(path: Path) extends OutputFileImpl {
    def writeFull(buf: ByteBuffer)(implicit ec: ExecutionContext): Future[Unit] = {
      Future(blocking(Files.createTempFile(
          path.getParent(), ".tmp-" + path.getFileName(), ".tmp"))).flatMap { tmpFile =>
        writeFullImpl(tmpFile, buf)
          .flatMap(_ => Future(blocking(move(tmpFile, path))))
          .finallyWith(Future(blocking(Files.deleteIfExists(tmpFile))))
      }
    }
  }

  private def writeFullImpl(path: Path, buf: ByteBuffer)(
      implicit ec: ExecutionContext): Future[Unit] = {
    import StandardOpenOption._

    Future(blocking(AsynchronousFileChannel.open(
        path, WRITE, CREATE, TRUNCATE_EXISTING))).flatMap { chan =>
      writeToChannel(chan, buf)
        .finallyWith(Future(blocking(chan.close())))
    }
  }

  private def writeToChannel(chan: AsynchronousFileChannel, buf: ByteBuffer): Future[Unit] = {
    val promise = Promise[Unit]()

    var pos = 0

    def writeLoop(): Unit =
      chan.write(buf, pos, (), Handler)

    object Handler extends CompletionHandler[Integer, Unit]{
      def completed(written: Integer, unit: Unit): Unit = {
        pos += written
        if (buf.hasRemaining())
          writeLoop()
        else
          promise.success(())
      }

      def failed(exc: Throwable, unit: Unit): Unit =
        promise.failure(exc)
    }

    writeLoop()
    promise.future
  }

  private def move(from: Path, to: Path): Unit = {
    try {
      // Try atomic move.
      Files.move(from, to, StandardCopyOption.ATOMIC_MOVE)
    } catch {
      case _: IOException =>
        /* We need to catch all exceptions, because it is platform dependent:
         * - whether ATOMIC_MOVE overrides an existing file or not,
         * - it throws a FileAlreadyExistsException in this case.
         *
         * If the atomic move fails, we fall back to a normal copy & delete.
         */
        Files.copy(from, to, StandardCopyOption.REPLACE_EXISTING)
    }
  }
}
