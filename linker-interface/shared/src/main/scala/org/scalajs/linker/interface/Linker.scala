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
    /* Backwards compatibility implementation for pre 1.3.0 link method.
     *
     * The major interface change in 1.3.0 is that the linker (and not the
     * caller) determines the set of files to be written. As a consequence, the
     * post 1.3.0 API does not offer as much control over cross-file references
     * (i.e. source map links): it is based on patterns rather than simply
     * asking the caller to verbatim provide the URI to reference in each file.
     *
     * To provide a backwards compatible interface, we do the following post-run
     * processing:
     *
     * - Match and copy the produced set of files (in the OutputDirectory) to
     *   the files provided by the caller (in LinkerOutput).
     * - Replace the pattern generated cross-file references with the ones
     *   provided by the caller. This is necessary as a post-processing step,
     *   because of the reduced flexibility of the 1.3.0 API: we cannot express
     *   all legacy requests in the new API.
     */

    val outDir = new Linker.MemOutputDirectory()

    link(irFiles, moduleInitializers, outDir, logger).flatMap { report =>
      val (jsFileContent, optSourceMapContent) =
        Linker.retrieveOutputFiles(report, outDir)

      val hasSourceMap =
        optSourceMapContent.isDefined && output.sourceMap.isDefined

      import StandardCharsets.UTF_8

      val jsFileWrite = {
        val content = new String(jsFileContent, UTF_8)
        val patched = Linker.patchJSFileContent(content,
            output.sourceMapURI.filter(_ => hasSourceMap))

        OutputFileImpl.fromOutputFile(output.jsFile)
          .writeFull(ByteBuffer.wrap(patched.getBytes(UTF_8)))
      }

      val sourceMapWrite = for {
        sourceMapContent <- optSourceMapContent
        sourceMapFile <- output.sourceMap
      } yield {
        val content = new String(sourceMapContent, UTF_8)
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
   * - The file URI should not contain '"', because URI.toASCIIString (which is
   *   used by the StandardLinker) never returns them.
   *
   * So as a legacy mechanism, this is OK-ish. It keeps us from having to build
   * the infrastructure to parse JSON cross platform.
   */
  private val fileFieldRe = """(?m)([,{])\s*"file"\s*:\s*".*"\s*([,}])""".r

  /** Retrieve the linker JS file and an optional source map */
  private def retrieveOutputFiles(report: Report,
      outDir: MemOutputDirectory): (Array[Byte], Option[Array[Byte]]) = {
    val module = {
      if (report.publicModules.size != 1) {
        throw new LinkingException(
            "Linking did not return exactly one public module, but the legacy " +
            "`link` method was called. Call the overload taking an " +
            s"OutputDirectory instead. Full report:\n$report")
      }

      report.publicModules.head
    }

    val expectedFiles = Set(module.jsFileName) ++ module.sourceMapName
    val foundFiles = outDir.content.keySet

    if (foundFiles != expectedFiles) {
      if (expectedFiles.subsetOf(foundFiles)) {
        throw new LinkingException(
            "Linking produced more than a single JS file (and source map) but " +
            "the legacy `link` method was called. Call the overload taking " +
            "an OutputDirectory instead. " +
            s"Expected files:\n$expectedFiles\nProduced files:\n$foundFiles")
      } else {
        throw new LinkingException(
            "Linking did not produce the files mentioned in the report. " +
            "This is a bug in the linker." +
            s"Expected files:\n$expectedFiles\nProduced files:\n$foundFiles")
      }
    }

    val jsFileContent = outDir.content(module.jsFileName)
    val sourceMapContent = module.sourceMapName.map(outDir.content(_))

    (jsFileContent, sourceMapContent)
  }

  /** Patches the JS file content to have the provided source map link (or none)
   *
   *  Looks for a line of the form `//# sourceMappingURL=.*` and replaces the
   *  URL with the provided `sourceMapURI`. In case `sourceMapURI` is None, the
   *  line is replaced with an empty line.
   */
  private def patchJSFileContent(content: String,
      sourceMapURI: Option[URI]): String = {

    val newLine =
      sourceMapURI.map(u => s"//# sourceMappingURL=${u.toASCIIString}")

    sourceMapRe.findFirstMatchIn(content).fold {
      content + newLine.fold("")("\n" + _ + "\n")
    } { reMatch =>
      val res = new StringBuilder

      res.append(reMatch.before)

      /* If there is no source map link, keep an empty line to not break a
       * potential (unlinked) source map
       */
      newLine.foreach(res.append(_))

      res.append(reMatch.after)

      res.toString()
    }
  }

  /** Patches the source map content to have the provided JS file link (or none).
   *
   *  Looks for a `"file":` key in the top-level source map JSON object and
   *  replaces it's value with `jsFileURI`. In case `jsFileURI` is None, it
   *  removes the key from the object.
   */
  private def patchSourceMapContent(content: String,
      jsFileURI: Option[URI]): String = {

    // No need for quoting: toASCIIString never returns '"'
    val newField =
      jsFileURI.map(u => s""""file": "${u.toASCIIString}"""")

    fileFieldRe.findFirstMatchIn(content).fold {
      newField.fold(content) { field =>
        val Array(pre, post) = content.split("\\{", 1)
        pre + field + "," + post
      }
    } { reMatch =>
      val res = new StringBuilder

      res.append(reMatch.before)

      newField match {
        case None =>
          if (reMatch.group(1) == "{")
            res.append('{')
          if (reMatch.group(2) == "}")
            res.append('}')
        case Some(field) =>
          res.append(reMatch.group(1))
          res.append(field)
          res.append(reMatch.group(2))
      }

      res.append(reMatch.after)

      res.toString()
    }
  }

  /* Basically a copy of MemOutputDirectory in `linker`, but we can't use is
   * here because we are in the interface which cannot depend on the linker.
   */
  private final class MemOutputDirectory extends OutputDirectoryImpl {
    val content: mutable.Map[String, Array[Byte]] = mutable.Map.empty

    def writeFull(name: String, buf: ByteBuffer)(
        implicit ec: ExecutionContext): Future[Unit] = synchronized {
      val c = new Array[Byte](buf.remaining())
      buf.get(c)
      content(name) = c
      Future.successful(())
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
