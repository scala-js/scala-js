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
import java.net.URI
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import org.scalajs.logging.Logger

import org.scalajs.linker.interface.unstable.{OutputDirectoryImpl, OutputFileImpl}

/** A Scala.js linker, with its most abstract API.
 *
 *  A linker can take a sequence of virtual .sjsir files and a sequence of
 *  module initializers, link them together, and write the output to a writable
 *  .js file.
 */
abstract class Linker private[interface] () {
  def link(irFiles: Seq[IRFile],
      moduleInitializers: Seq[ModuleInitializer],
      output: OutputDirectory, logger: Logger)(
      implicit ec: ExecutionContext): Future[Report]

  final def link(irFiles: Seq[IRFile],
      moduleInitializers: Seq[ModuleInitializer],
      output: LinkerOutput, logger: Logger)(
      implicit ec: ExecutionContext): Future[Unit] = {
    val outDir = new Linker.MemOutputDirectory()

    link(irFiles, moduleInitializers, outDir, logger).flatMap { report =>
      val module = {
        if (report.publicModules.size != 1) {
          throw new LinkingException(
              "Linking returned more that one public module but the legacy " +
              "`link` method was called. Call the overload taking an " +
              s"OutputDirectory instead. Full report:\n$report")
        }
  
        report.publicModules.head
      }

      val expectedFiles = Set(module.jsFileName) ++ module.sourceMapName

      if (outDir.content.keySet != expectedFiles) {
          throw new LinkingException(
              "Linking produced more than a single JS file (and source map) but " +
              "the legacy `link` method was called. Call the overload taking " +
              "an OutputDirectory instead.")
      }

      val hasSourceMap =
        module.sourceMapName.isDefined && output.sourceMap.isDefined

      import StandardCharsets.UTF_8

      val jsFileWrite = {
        val content = new String(outDir.content(module.jsFileName), UTF_8)
        val patched = Linker.patchJSFileContent(content,
            output.sourceMapURI.filter(_ => hasSourceMap))

        OutputFileImpl.fromOutputFile(output.jsFile)
          .writeFull(ByteBuffer.wrap(patched.getBytes(UTF_8)))
      }

      val sourceMapWrite = for {
        sourceMapName <- module.sourceMapName
        sourceMapFile <- output.sourceMap
      } yield {
        val content = new String(outDir.content(sourceMapName), UTF_8)
        val patched = Linker.patchSourceMapContent(content, output.jsFileURI)

        OutputFileImpl.fromOutputFile(sourceMapFile)
          .writeFull(ByteBuffer.wrap(patched.getBytes(UTF_8)))
      }

      Future.sequence(List(jsFileWrite) ++ sourceMapWrite).map(_ => ())
    }
  }
}

private object Linker {
  private val sourceMapRe = """(?m)^//# sourceMappingURL=.*$""".r

  /* It is somewhat acceptable to parse the JSON field "file" with a Regex
   * because:
   *
   * - Source maps do not contain nested objects.
   * - The file URI should not contain '"', because URI.toASCIIString never
   *   returns them.
   *
   * So as a legacy mechanism, this is in the realm of the OK.
   */
  private val fileFieldRe = """(?m)([,{])\s*"file"\s*:\s*".*"\s*([,}])""".r

  private def patchJSFileContent(content: String,
      sourceMapURI: Option[URI]): String = {
    def fmt(u: URI) =
      s"//# sourceMappingURL=${u.toASCIIString}"

    var found = false
    val result = sourceMapRe.replaceAllIn(content, _ => {
      found = true
      // Replace the line with an empty line to not break the source map
      sourceMapURI.fold("")(fmt(_))
    })

    if (!found && sourceMapURI.isDefined) {
      result + fmt(sourceMapURI.get) + "\n"
    } else {
      result
    }
  }

  private def patchSourceMapContent(content: String,
      jsFileURI: Option[URI]): String = {
    fileFieldRe.replaceAllIn(content, m =>
      jsFileURI.fold {
        (if (m.group(1) == "{") "{" else "") + (if (m.group(2) == "}") "}" else "")
      } { uri =>
        // No need for quoting: toASCIIString never returns '"'
        m.group(1) + s""""file": "${uri.toASCIIString}"""" + m.group(2)
      }
    )
  }

  private final class MemOutputDirectory extends OutputDirectoryImpl {
    val content: mutable.Map[String, Array[Byte]] = mutable.Map.empty

    def writeFull(name: String, buf: ByteBuffer)(
        implicit ec: ExecutionContext): Future[Unit] = synchronized {
      val c = new Array[Byte](buf.remaining())
      buf.get(c)
      content(name) = c
      Future.successful(())
    }

    def listFiles()(implicit ec: ExecutionContext): Future[Iterable[String]] = synchronized {
      Future.successful(content.keys)
    }

    def delete(name: String)(implicit ec: ExecutionContext): Future[Unit] = synchronized {
      if (content.remove(name).isDefined)
        Future.successful(())
      else
        Future.failed(new IOException(s"file $name does not exist"))
    }
  }
}
