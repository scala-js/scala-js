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

import java.io._
import java.nio._
import java.nio.channels._
import java.nio.file._

final class WritableFileVirtualBinaryFile(path: Path) extends WritableVirtualBinaryFile {
  def newChannel()(implicit ec: ExecutionContext): Future[WriteChannel] = {
    import StandardOpenOption._

    Future(blocking(AsynchronousFileChannel.open(path, WRITE, CREATE, TRUNCATE_EXISTING)))
      .map(new WritableFileVirtualBinaryFile.Channel(_))
  }
}

private object WritableFileVirtualBinaryFile {
  private final class Channel(chan: AsynchronousFileChannel) extends WriteChannel {
    private[this] var _pos = 0L

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
