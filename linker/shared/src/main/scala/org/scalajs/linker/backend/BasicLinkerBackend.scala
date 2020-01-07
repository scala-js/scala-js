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

package org.scalajs.linker.backend

import scala.concurrent._

import java.io._
import java.net.URI
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import org.scalajs.logging.Logger

import org.scalajs.linker.interface.{IRFile, LinkerOutput}
import org.scalajs.linker.interface.unstable.OutputFileImpl
import org.scalajs.linker.standard._

import org.scalajs.linker.backend.emitter.Emitter
import org.scalajs.linker.backend.javascript.{Printers, SourceMapWriter}

/** The basic backend for the Scala.js linker.
 *
 *  Simply emits the JavaScript without applying any further optimizations.
 */
final class BasicLinkerBackend(config: LinkerBackendImpl.Config)
    extends LinkerBackendImpl(config) {

  private[this] val emitter = new Emitter(config.commonConfig)

  val symbolRequirements: SymbolRequirement = emitter.symbolRequirements

  override def injectedIRFiles: Seq[IRFile] = emitter.injectedIRFiles

  /** Emit the given [[standard.LinkingUnit LinkingUnit]] to the target output.
   *
   *  @param unit [[standard.LinkingUnit LinkingUnit]] to emit
   *  @param output File to write to
   */
  def emit(unit: LinkingUnit, output: LinkerOutput, logger: Logger)(
      implicit ec: ExecutionContext): Future[Unit] = {
    verifyUnit(unit)

    val emitterResult = logger.time("Emitter") {
      emitter.emit(unit, logger)
    }

    logger.timeFuture("BasicBackend: Write result") {
      output.sourceMap.filter(_ => config.sourceMap).fold {
        val code = withWriter { writer =>
          val printer = new Printers.JSTreePrinter(writer)
          writer.write(emitterResult.header)
          emitterResult.body.foreach(printer.printTopLevelTree _)
          writer.write(emitterResult.footer)
        }

        write(output.jsFile, code)
      } { sourceMapFile =>
        val sourceMapWriter = new SourceMapWriter(output.jsFileURI,
            config.relativizeSourceMapBase)

        val code = withWriter { writer =>
          val printer = new Printers.JSTreePrinterWithSourceMap(writer, sourceMapWriter)

          writer.write(emitterResult.header)
          for (_ <- 0 until emitterResult.header.count(_ == '\n'))
            sourceMapWriter.nextLine()

          emitterResult.body.foreach(printer.printTopLevelTree _)

          writer.write(emitterResult.footer)

          output.sourceMapURI.foreach { uri =>
            writer.write("//# sourceMappingURL=" + uri.toASCIIString() + "\n")
          }
        }

        val sourceMap = sourceMapWriter.result()

        write(output.jsFile, code)
          .flatMap(_ => write(sourceMapFile, sourceMap))
      }
    }
  }

  private def withWriter(body: Writer => Unit): ByteBuffer = {
    val byteStream = new ByteArrayOutputStream
    val out = new OutputStreamWriter(byteStream, StandardCharsets.UTF_8)
    body(out)
    out.close()
    ByteBuffer.wrap(byteStream.toByteArray())
  }

  private def write(file: LinkerOutput.File, buf: ByteBuffer)(
      implicit ec: ExecutionContext): Future[Unit] = {
    OutputFileImpl.fromOutputFile(file).writeFull(buf)
  }
}
