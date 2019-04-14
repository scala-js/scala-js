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

import java.nio.ByteBuffer

/** A writable virtual binary file. */
trait WritableVirtualBinaryFile {
  def newChannel()(implicit ec: ExecutionContext): Future[WriteChannel]

  def writeFull(buf: ByteBuffer)(implicit ec: ExecutionContext): Future[Unit] = {
    newChannel().flatMap { chan =>
      def writeLoop(): Future[Unit] = {
        if (buf.hasRemaining()) chan.write(buf).flatMap(_ => writeLoop())
        else Future.successful(())
      }

      writeLoop().finallyWith(chan.close())
    }
  }
}

trait WriteChannel {
  def write(buf: ByteBuffer)(implicit ec: ExecutionContext): Future[Unit]
  def close()(implicit ec: ExecutionContext): Future[Unit]
}
