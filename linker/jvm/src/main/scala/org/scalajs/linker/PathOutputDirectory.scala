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
import java.nio.file.attribute.BasicFileAttributes

import java.util.EnumSet

import java.io.IOException

import org.scalajs.linker.interface.OutputDirectory
import org.scalajs.linker.interface.unstable.OutputDirectoryImpl

object PathOutputDirectory {
  def apply(directory: Path): OutputDirectory = {
    require(Files.isDirectory(directory))
    new Impl(directory)
  }

  private final class Impl(directory: Path) extends OutputDirectoryImpl {
    def writeFull(name: String, buf: ByteBuffer)(implicit ec: ExecutionContext): Future[Unit] = {
      val file = getPath(name)

      needsWrite(file, buf).flatMap { needsWrite =>
        if (!needsWrite)
          Future.successful(())
        else
          writeAtomic(file, buf)
      }
    }

    def listFiles()(implicit ec: ExecutionContext): Future[Iterable[String]] = Future {
      blocking {
        val builder = Iterable.newBuilder[String]

        /* Files.list(...) would be simpler, but it requires Java Streams which
         * are, at the time of this writing, not implemented in Scala.js.
         */
        val dirVisitor = new SimpleFileVisitor[Path] {
          override def visitFile(path: Path, attrs: BasicFileAttributes): FileVisitResult = {
            if (path != directory)
              builder += directory.relativize(path).toString()
            super.visitFile(path, attrs)
          }
        }

        Files.walkFileTree(directory, EnumSet.noneOf(classOf[FileVisitOption]), 1, dirVisitor)

        builder.result()
      }
    }

    def delete(name: String)(implicit ec: ExecutionContext): Future[Unit] =
      Future(blocking(Files.delete(getPath(name))))

    private def getPath(name: String) = directory.resolve(name)
  }

  private def writeAtomic(path: Path, buf: ByteBuffer)(
      implicit ec: ExecutionContext): Future[Unit] = {
    import StandardOpenOption._

    val tmpFileFuture = Future(blocking(
        Files.createTempFile(path.getParent(), ".tmp-" + path.getFileName(), ".tmp")))

    tmpFileFuture.flatMap { tmpFile =>
      val writeFuture = withChannel(tmpFile, WRITE, CREATE, TRUNCATE_EXISTING) { chan =>
        writeToChannel(chan, buf)
      }

      writeFuture
        .flatMap(_ => Future(blocking(move(tmpFile, path))))
        .finallyWith(Future(blocking(Files.deleteIfExists(tmpFile))))
    }
  }

  private def writeToChannel(chan: AsynchronousFileChannel, buf: ByteBuffer): Future[Unit] = {
    val promise = Promise[Unit]()

    var pos = 0

    def writeLoop(): Unit =
      chan.write(buf, pos, (), Handler)

    object Handler extends CompletionHandler[Integer, Unit] {
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

  private def withChannel[T](f: Path, openOptions: OpenOption*)(
      body: AsynchronousFileChannel => Future[T])(
      implicit ec: ExecutionContext): Future[T] = {
    val chanFuture =
      Future(blocking(AsynchronousFileChannel.open(f, openOptions: _*)))

    chanFuture.flatMap { chan =>
      body(chan).finallyWith(Future(blocking(chan.close())))
    }
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

  private def needsWrite(path: Path, buf: ByteBuffer)(
      implicit ec: ExecutionContext): Future[Boolean] = {
    Future(blocking(Files.exists(path))).flatMap { exists =>
      if (!exists) {
        Future.successful(true)
      } else {
        withChannel(path, StandardOpenOption.READ) { chan =>
          // slice buf to protect position / limit.
          fileDiffers(chan, buf.slice())
        }
      }
    }
  }

  private def fileDiffers(chan: AsynchronousFileChannel, cmpBuf: ByteBuffer): Future[Boolean] = {
    if (chan.size() != cmpBuf.remaining()) {
      Future.successful(true)
    } else {
      var pos = 0L

      val promise = Promise[Boolean]()
      val readBuf = ByteBuffer.allocate(Math.min(2048, cmpBuf.remaining()))

      def readNext(): Unit = {
        readBuf.clear()
        chan.read(readBuf, pos, null, Handler)
      }

      object Handler extends CompletionHandler[Integer, Null] {
        def completed(read: Integer, n: Null): Unit = {
          if (read == -1) {
            /* We have checked the file size beforehand. So if we get here,
             * there's no diff.
             */
            promise.success(false)
          } else {
            pos += read

            readBuf.flip()

            val tmpCmpBuf = cmpBuf.slice()
            tmpCmpBuf.limit(read)

            if (readBuf != tmpCmpBuf) {
              promise.success(true)
            } else {
              cmpBuf.position(cmpBuf.position() + read)
              readNext()
            }
          }
        }

        def failed(exc: Throwable, n: Null): Unit =
          promise.failure(exc)
      }

      readNext()
      promise.future
    }
  }
}
