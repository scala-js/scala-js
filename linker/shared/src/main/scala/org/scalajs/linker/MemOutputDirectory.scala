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

/** OutputDirectory that simply writes to memory. */
sealed trait MemOutputDirectory extends OutputDirectory {
  /** Content of the file with `name` or `None` if the file was not written. */
  def content(name: String): Option[Array[Byte]]

  /** Names of files that are present (in no particular order). */
  def fileNames(): List[String]
}

object MemOutputDirectory {
  def apply(): MemOutputDirectory = new Impl()

  private final class Impl
      extends OutputDirectoryImpl with MemOutputDirectory {
    private val _content: mutable.Map[String, Array[Byte]] = mutable.Map.empty

    def content(name: String): Option[Array[Byte]] = synchronized {
      _content.get(name)
    }

    def fileNames(): List[String] = synchronized {
      _content.keys.toList
    }

    def writeFull(name: String, buf: ByteBuffer)(
        implicit ec: ExecutionContext): Future[Unit] = synchronized {
      val c = new Array[Byte](buf.remaining())
      buf.get(c)
      _content(name) = c
      Future.successful(())
    }

    def readFull(name: String)(
        implicit ec: ExecutionContext): Future[ByteBuffer] = {
      _content.get(name).fold[Future[ByteBuffer]] {
        fileNotFound(name)
      } { c =>
        Future.successful(ByteBuffer.wrap(c).asReadOnlyBuffer())
      }
    }

    def listFiles()(implicit ec: ExecutionContext): Future[List[String]] = synchronized {
      Future.successful(fileNames())
    }

    def delete(name: String)(implicit ec: ExecutionContext): Future[Unit] = synchronized {
      if (_content.remove(name).isDefined)
        Future.successful(())
      else
        fileNotFound(name)
    }

    private def fileNotFound(name: String): Future[Nothing] =
      Future.failed(new IOException(s"file $name does not exist"))
  }
}
