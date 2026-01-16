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

import scala.concurrent._

import java.net.URI
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8

import org.scalajs.logging.Logger

import org.scalajs.linker.interface.unstable.{OutputDirectoryImpl, OutputFileImpl}

/** Backwards compatibility implementation for pre 1.3.0 link method.
 *
 *  The major interface change in 1.3.0 is that the linker (and not the
 *  caller) determines the set of files to be written. As a consequence, the
 *  post 1.3.0 API does not offer as much control over cross-file references
 *  (i.e. source map links): it is based on patterns rather than simply
 *  asking the caller to verbatim provide the URI to reference in each file.
 *
 *  To provide a backwards compatible interface, we do the following post-run
 *  processing:
 *
 *  - Match and copy the produced set of files (in the OutputDirectory) to
 *    the files provided by the caller (in LinkerOutput).
 *  - Replace the pattern generated cross-file references with the ones
 *    provided by the caller. This is necessary as a post-processing step,
 *    because of the reduced flexibility of the 1.3.0 API: we cannot express
 *    all legacy requests in the new API.
  */
@deprecated("Part of legacy API.", "1.3.0")
object ReportToLinkerOutputAdapter {
  final class UnsupportedLinkerOutputException private[ReportToLinkerOutputAdapter] (
      message: String) extends IllegalArgumentException(message)

  def convert(report: Report, outputDirectory: OutputDirectory,
      legacyOutput: LinkerOutput)(
      implicit ec: ExecutionContext): Future[Unit] = {
    report.publicModules.toList match {
      case Nil =>
        writeEmptyOutput(legacyOutput)

      case List(module) =>
        retrieveOutputFiles(module, outputDirectory)
          .flatMap(writePatchedOutput(_, legacyOutput))

      case _ =>
        throw new UnsupportedLinkerOutputException(
            "Linking returned more than one public module. Full report:\n" +
            report)
    }
  }

  private def writeEmptyOutput(legacyOutput: LinkerOutput)(
      implicit ec: ExecutionContext): Future[Unit] = {
    legacyOutput.sourceMap.fold {
      writeString(legacyOutput.jsFile, "")
    } { sourceMapFile =>
      val smFields = List(
        "version" -> "3",
        "mappings" -> "\"\"",
        "sources" -> "[]",
        "names" -> "[]",
        "lineCount" -> "1"
      ) ++ legacyOutput.jsFileURI.map(uri => "file" -> s""""${uri.toASCIIString}"""")

      val jsContent =
        legacyOutput.sourceMapURI.fold("")(uri => s"//# sourceMappingURL=${uri.toASCIIString}\n")

      val smContent = smFields
        .map { case (n, v) => s""""$n": $v""" }
        .mkString("{\n", ",\n", "\n}")

      writeString(legacyOutput.jsFile, jsContent)
        .flatMap(_ => writeString(sourceMapFile, smContent))
    }
  }

  private def writePatchedOutput(output: (ByteBuffer, Option[ByteBuffer]),
      legacyOutput: LinkerOutput)(implicit ec: ExecutionContext): Future[Unit] = {
    val (jsFileContent, optSourceMapContent) = output

    val hasSourceMap =
      optSourceMapContent.isDefined && legacyOutput.sourceMap.isDefined

    val jsFileWrite = {
      val content = UTF_8.decode(jsFileContent).toString()
      val patched = patchJSFileContent(content,
          legacyOutput.sourceMapURI.filter(_ => hasSourceMap))
      writeString(legacyOutput.jsFile, patched)
    }

    val sourceMapWrite = for {
      sourceMapContent <- optSourceMapContent
      sourceMapFile <- legacyOutput.sourceMap
    } yield {
      val content = UTF_8.decode(sourceMapContent).toString()
      val patched = patchSourceMapContent(content, legacyOutput.jsFileURI)
      writeString(sourceMapFile, patched)
    }

    Future.sequence(List(jsFileWrite) ++ sourceMapWrite).map(_ => ())
  }

  /** Retrieve the linker JS file and an optional source map */
  private def retrieveOutputFiles(module: Report.Module,
      outputDirectory: OutputDirectory)(
      implicit ec: ExecutionContext): Future[(ByteBuffer, Option[ByteBuffer])] = {
    val outDirImpl = OutputDirectoryImpl.fromOutputDirectory(outputDirectory)

    val checkFiles = for {
      foundFilesList <- outDirImpl.listFiles()
    } yield {
      val foundFiles = foundFilesList.toSet
      val expectedFiles = Set(module.jsFileName) ++ module.sourceMapName

      if (foundFiles != expectedFiles) {
        if (expectedFiles.subsetOf(foundFiles)) {
          throw new UnsupportedLinkerOutputException(
              "Linking produced more than a single JS file (and source map). " +
              s"Expected files:\n$expectedFiles\nProduced files:\n$foundFiles")
        } else {
          throw new AssertionError(
              "Linking did not produce the files mentioned in the report. " +
              "This is a bug in the linker. " +
              s"Expected files:\n$expectedFiles\nProduced files:\n$foundFiles")
        }
      }
    }

    for {
      _ <- checkFiles
      jsFileContent <- outDirImpl.readFull(module.jsFileName)
      sourceMapContent <- Future.traverse(module.sourceMapName.toList)(outDirImpl.readFull(_))
    } yield {
      (jsFileContent, sourceMapContent.headOption)
    }
  }

  /* This regex would normally be written with the (?m) flag, but that would
   * require ES2018, so we work around it.
   */
  private val sourceMapRe = """(?:^|\n)(//# sourceMappingURL=[^\n]*)(?:\n|$)""".r

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

      res.append(reMatch.before(1))

      /* If there is no source map link, keep an empty line to not break a
       * potential (unlinked) source map
       */
      newLine.foreach(res.append(_))

      res.append(reMatch.after(1))

      res.toString()
    }
  }

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
  private val fileFieldRe = """([,{])\s*"file"\s*:\s*"[^"]*"\s*([,}])""".r

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
        content.split("\\{", 2) match {
          case Array(pre, post) =>
            pre + "{" + field + "," + post

          case _ =>
            throw new IllegalArgumentException(
                s"source map file does not seem to contain a JSON object: $content")
        }
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

  private def writeString(outputFile: LinkerOutput.File, content: String)(
      implicit ec: ExecutionContext): Future[Unit] = {
    OutputFileImpl.fromOutputFile(outputFile)
      .writeFull(ByteBuffer.wrap(content.getBytes(UTF_8)))
  }
}
