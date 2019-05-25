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

package org.scalajs.linker.standard

import scala.concurrent._

import java.nio.ByteBuffer

import org.scalajs.linker.LinkerOutput
import org.scalajs.linker.irio._

abstract class OutputFileImpl extends LinkerOutput.File {
  final private[linker] def impl: OutputFileImpl = this

  def newChannel()(implicit ec: ExecutionContext): Future[OutputFileImpl.Channel]

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

object OutputFileImpl {
  def fromOutputFile(f: LinkerOutput.File): OutputFileImpl = f.impl

  trait Channel {
    def write(buf: ByteBuffer)(implicit ec: ExecutionContext): Future[Unit]
    def close()(implicit ec: ExecutionContext): Future[Unit]
  }
}
