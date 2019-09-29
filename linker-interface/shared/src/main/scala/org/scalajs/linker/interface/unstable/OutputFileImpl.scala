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

package org.scalajs.linker.interface.unstable

import scala.concurrent._

import java.nio.ByteBuffer

import org.scalajs.linker.interface.LinkerOutput

abstract class OutputFileImpl extends LinkerOutput.File {
  final private[interface] def impl: OutputFileImpl = this

  def newChannel()(implicit ec: ExecutionContext): Future[OutputFileImpl.Channel]

  def writeFull(buf: ByteBuffer)(implicit ec: ExecutionContext): Future[Unit] = {
    newChannel().flatMap { chan =>
      def writeLoop(): Future[Unit] = {
        if (buf.hasRemaining()) chan.write(buf).flatMap(_ => writeLoop())
        else Future.successful(())
      }

      finallyWith(writeLoop(), chan.close())
    }
  }

  private def finallyWith(v: Future[Unit], f: => Future[Unit])(
      implicit ec: ExecutionContext): Future[Unit] = {
    v.map[Option[Throwable]](_ => None)
      .recover { case t => Some(t) }
      .flatMap {
        case None => f

        case Some(vt) =>
          f.transform(_ => throw vt, ft => { ft.addSuppressed(vt); ft })
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
