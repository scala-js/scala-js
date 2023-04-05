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
import java.nio.file.attribute.PosixFilePermission

import java.io.IOException
import java.util.EnumSet

import org.scalajs.linker.interface.OutputDirectory
import org.scalajs.linker.interface.unstable.OutputDirectoryImpl

object PathOutputDirectory {
  def apply(directory: Path): OutputDirectory = {
    require(Files.isDirectory(directory))
    new Impl(directory)
  }

  /* #4841 In the `CompletionHandler`s of our `AsynchronousFileChannel`s, we
   * use `promise.trySuccess` and `tryFailure` instead of `success` and
   * `failure`. This avoids `IllegalStateException` if there are double calls
   * to `CompletionHandler.{completed,failed}`. It should not happen, but we
   * observed it to happen on Windows anyway.
   */

  private final class Impl(directory: Path) extends OutputDirectoryImpl {
    def writeFull(name: String, buf: ByteBuffer)(implicit ec: ExecutionContext): Future[Unit] = {
      val file = getPath(name)

      needsWrite(file, buf).flatMap { needsWrite =>
        if (!needsWrite)
          Future.successful(())
        else
          writeAtomic(name, buf)
      }
    }

    override def writeFull(name: String, buf: ByteBuffer, skipContentCheck: Boolean)(
        implicit ec: ExecutionContext): Future[Unit] = {
      if (skipContentCheck)
        writeAtomic(name, buf)
      else
        writeFull(name, buf)
    }

    def readFull(name: String)(implicit ec: ExecutionContext): Future[ByteBuffer] =
      withChannel(getPath(name), StandardOpenOption.READ)(readFromChannel(_))

    def listFiles()(implicit ec: ExecutionContext): Future[List[String]] = Future {
      blocking {
        val builder = List.newBuilder[String]
        Files.list(directory).forEachOrdered { entry =>
          builder += directory.relativize(entry).toString()
        }
        builder.result()
      }
    }

    def delete(name: String)(implicit ec: ExecutionContext): Future[Unit] =
      Future(blocking(Files.delete(getPath(name))))

    private def getPath(name: String) = directory.resolve(name)

    private def writeAtomic(name: String, buf: ByteBuffer)(
        implicit ec: ExecutionContext): Future[Unit] = {
      import StandardOpenOption._
      import PosixFilePermission._

      val tmpFileFuture = Future {
        blocking {
          val tmpFile = Files.createTempFile(directory, ".tmp-" + name, ".tmp")

          /* Set file permissions for temporary file as in Linux it is created
           * with permissions 0600 which deviates from the standard 0644 used
           * in *.class and *.jar files.
           *
           * Uses setPosixFilePermissions instead of passing the permissions to
           * createTempFile to cover systems (if any exist) that uses POSIX
           * permissions, but can't set them atomically while creating a
           * temporary file.
           */
          val permissions =
            EnumSet.of(OWNER_READ, OWNER_WRITE, GROUP_READ, OTHERS_READ)

          try {
            Files.setPosixFilePermissions(tmpFile, permissions)
          } catch {
            case _: UnsupportedOperationException =>
          }

          tmpFile
        }
      }

      tmpFileFuture.flatMap { tmpFile =>
        val writeFuture = withChannel(tmpFile, WRITE, CREATE, TRUNCATE_EXISTING) { chan =>
          writeToChannel(chan, buf)
        }

        writeFuture
          .flatMap(_ => Future(blocking(move(tmpFile, getPath(name)))))
          .finallyWith(Future(blocking(Files.deleteIfExists(tmpFile))))
      }
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
          promise.trySuccess(())
      }

      def failed(exc: Throwable, unit: Unit): Unit =
        promise.tryFailure(exc)
    }

    writeLoop()
    promise.future
  }

  private def readFromChannel(chan: AsynchronousFileChannel): Future[ByteBuffer] = {
    val size = blocking(chan.size())

    if (size > Int.MaxValue)
      throw new IOException(s"file is too large ($size bytes)")

    val buf = ByteBuffer.allocate(size.toInt)

    val promise = Promise[ByteBuffer]()

    def readLoop(): Unit =
      chan.read(buf, buf.position(), (), Handler)

    object Handler extends CompletionHandler[Integer, Unit] {
      def completed(read: Integer, unit: Unit): Unit = {
        if (read == -1 || !buf.hasRemaining()) {
          buf.flip()
          promise.trySuccess(buf)
        } else {
          readLoop()
        }
      }

      def failed(exc: Throwable, unit: Unit): Unit =
        promise.tryFailure(exc)
    }

    readLoop()
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
        chan.read(readBuf, pos, (), Handler)
      }

      object Handler extends CompletionHandler[Integer, Unit] {
        def completed(read: Integer, unit: Unit): Unit = {
          if (read == -1) {
            /* We have checked the file size beforehand. So if we get here,
             * there's no diff.
             */
            promise.trySuccess(false)
          } else {
            pos += read

            readBuf.flip()

            val tmpCmpBuf = cmpBuf.slice()
            tmpCmpBuf.limit(read)

            if (readBuf != tmpCmpBuf) {
              promise.trySuccess(true)
            } else {
              cmpBuf.position(cmpBuf.position() + read)
              readNext()
            }
          }
        }

        def failed(exc: Throwable, unit: Unit): Unit =
          promise.tryFailure(exc)
      }

      readNext()
      promise.future
    }
  }
}
