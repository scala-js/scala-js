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

import scala.collection.mutable
import scala.concurrent._

import java.io.{ByteArrayOutputStream, IOException}
import java.nio.ByteBuffer

import org.scalajs.linker.interface.OutputDirectory
import org.scalajs.linker.interface.unstable.OutputDirectoryImpl

sealed trait MemOutputDirectory extends OutputDirectory {
  def content(name: String): Option[Array[Byte]]
}

object MemOutputDirectory {
  def apply(): MemOutputDirectory = new Impl()

  private final class Impl
      extends OutputDirectoryImpl with MemOutputDirectory {
    private val _content: mutable.Map[String, Array[Byte]] = mutable.Map.empty

    def content(name: String): Option[Array[Byte]] = synchronized {
      _content.get(name)
    }

    def writeFull(name: String, buf: ByteBuffer)(
        implicit ec: ExecutionContext): Future[Unit] = {
      val c = new Array[Byte](buf.remaining())
      buf.get(c)
      _content(name) = c
      Future.successful(())
    }

    def listFiles()(implicit ec: ExecutionContext): Future[Iterable[String]] = synchronized {
      Future.successful(_content.keys)
    }

    def delete(name: String)(implicit ec: ExecutionContext): Future[Unit] = synchronized {
      if (_content.remove(name).isDefined)
        Future.successful(())
      else
        Future.failed(new IOException(s"file $name does not exist"))
    }
  }
}
