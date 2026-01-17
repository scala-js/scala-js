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

package org.scalajs.linker.interface

import scala.collection.mutable
import scala.concurrent._

import java.io.IOException
import java.nio.ByteBuffer

import org.scalajs.logging.Logger

import org.scalajs.linker.interface.unstable.OutputDirectoryImpl

/** A Scala.js linker, with its most abstract API.
 *
 *  A linker can take a sequence of virtual .sjsir files and a sequence of
 *  module initializers, link them together, and write the resulting JS file(s)
 *  to a directory.
 *
 *  Further, the linker returns a [[Report]] about the run.
 */
abstract class Linker private[interface] () {
  def link(irFiles: Seq[IRFile],
      moduleInitializers: Seq[ModuleInitializer],
      output: OutputDirectory, logger: Logger)(
      implicit ec: ExecutionContext): Future[Report]

  @deprecated("Use the overload taking an OutputDirectory instead", "1.3.0")
  final def link(irFiles: Seq[IRFile],
      moduleInitializers: Seq[ModuleInitializer],
      output: LinkerOutput, logger: Logger)(
      implicit ec: ExecutionContext): Future[Unit] = {

    val outDir = new Linker.MemOutputDirectory()

    link(irFiles, moduleInitializers, outDir, logger)
      .flatMap(ReportToLinkerOutputAdapter.convert(_, outDir, output))
      .recover {
        case e: ReportToLinkerOutputAdapter.UnsupportedLinkerOutputException =>
          throw new LinkingException(
              "The linker produced a result not supported by the pre v1.3.0 " +
              "legacy API. Call the overload taking an OutputDirectory " +
              "instead. " + e.getMessage(),
              e)
      }
  }
}

private object Linker {
  /* Basically a copy of MemOutputDirectory in `linker`, but we can't use is
   * here because we are in the interface which cannot depend on the linker.
   */
  private final class MemOutputDirectory extends OutputDirectoryImpl {
    private val content: mutable.Map[String, Array[Byte]] = mutable.Map.empty

    def writeFull(name: String, buf: ByteBuffer)(
        implicit ec: ExecutionContext): Future[Unit] = synchronized {
      val c = new Array[Byte](buf.remaining())
      buf.get(c)
      content(name) = c
      Future.successful(())
    }

    def readFull(name: String)(
        implicit ec: ExecutionContext): Future[ByteBuffer] = synchronized {
      Future.successful(ByteBuffer.wrap(content(name)))
    }

    def listFiles()(implicit ec: ExecutionContext): Future[List[String]] = synchronized {
      Future.successful(content.keys.toList)
    }

    def delete(name: String)(implicit ec: ExecutionContext): Future[Unit] = synchronized {
      if (content.remove(name).isDefined)
        Future.successful(())
      else
        Future.failed(new IOException(s"file $name does not exist"))
    }
  }
}
