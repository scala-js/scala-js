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

import java.security.MessageDigest

import java.util.{Arrays, EnumSet}

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

  private def writeAtomic(path: Path, buf: ByteBuffer)(implicit ec: ExecutionContext): Future[Unit] = {
    Future(blocking(Files.createTempFile(
        path.getParent(), ".tmp-" + path.getFileName(), ".tmp"))).flatMap { tmpFile =>
      writeToPath(tmpFile, buf)
        .flatMap(_ => Future(blocking(move(tmpFile, path))))
        .finallyWith(Future(blocking(Files.deleteIfExists(tmpFile))))
    }
  }

  private def writeToPath(path: Path, buf: ByteBuffer)(
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
        for (currentDigest <- readDigest(path)) yield {
          val writeDigest = MessageDigest.getInstance("SHA-1")
          writeDigest.update(buf.slice()) // slice to retain position.

          !Arrays.equals(currentDigest, writeDigest.digest())
        }
      }
    }
  }

  private def readDigest(file: Path): Future[Array[Byte]] = {
    var _pos = 0L
    val chan = AsynchronousFileChannel.open(file, StandardOpenOption.READ)

    val digest = MessageDigest.getInstance("SHA-1")
    val promise = Promise[Array[Byte]]()
    val buf = ByteBuffer.allocate(2048)

    def readNext(): Unit = {
      buf.clear()
      chan.read(buf, _pos, null, Handler)
    }

    object Handler extends CompletionHandler[Integer, Null] {
      def completed(read: Integer, n: Null): Unit = {
        if (read == -1) {
          promise.success(digest.digest())
        } else {
          _pos += read
          buf.flip()
          digest.update(buf)
          readNext()
        }
      }

      def failed(exc: Throwable, n: Null): Unit =
        promise.failure(exc)
    }

    readNext()
    promise.future
  }
}
