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

import java.nio._
import java.nio.file._

import java.io.IOException

final class AtomicWritableFileVirtualBinaryFile(path: Path) extends WritableVirtualBinaryFile {
  def newChannel()(implicit ec: ExecutionContext): Future[WriteChannel] = {
    Future(newTmpFile()).flatMap { tmpFile =>
      Future(new WritableFileVirtualBinaryFile(tmpFile))
        .flatMap(_.newChannel())
        .map(new AtomicWritableFileVirtualBinaryFile.Channel(path, tmpFile, _))
        .recover {
          case t =>
            blocking(Files.deleteIfExists(tmpFile))
            throw t
        }
    }
  }

  private def newTmpFile(): Path =
    Files.createTempFile(path.getParent(), ".tmp-" + path.getFileName(), ".tmp")
}

private object AtomicWritableFileVirtualBinaryFile {
  private final class Channel(baseFile: Path, tmpFile: Path, chan: WriteChannel) extends WriteChannel {
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
}
