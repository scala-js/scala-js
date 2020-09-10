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

import java.io.Writer

import org.scalajs.logging.Logger

import org.scalajs.linker.interface.{IRFile, OutputDirectory, Report}
import org.scalajs.linker.interface.unstable.OutputPatternsImpl
import org.scalajs.linker.standard._
import org.scalajs.linker.standard.ModuleSet.ModuleID

import org.scalajs.linker.backend.emitter.Emitter
import org.scalajs.linker.backend.javascript.{Printers, SourceMapWriter}

/** The basic backend for the Scala.js linker.
 *
 *  Simply emits the JavaScript without applying any further optimizations.
 */
final class BasicLinkerBackend(config: LinkerBackendImpl.Config)
    extends LinkerBackendImpl(config) {

  private[this] val emitter = {
    val emitterConfig = Emitter.Config(config.commonConfig.coreSpec)
      .withInternalModulePattern(m => OutputPatternsImpl.moduleName(config.outputPatterns, m.id))

    new Emitter(emitterConfig)
  }

  val symbolRequirements: SymbolRequirement = emitter.symbolRequirements

  override def injectedIRFiles: Seq[IRFile] = emitter.injectedIRFiles

  /** Emit the given [[standard.ModuleSet ModuleSet]] to the target output.
   *
   *  @param moduleSet [[standard.ModuleSet ModuleSet]] to emit
   *  @param output File Directory write to
   */
  def emit(moduleSet: ModuleSet, output: OutputDirectory, logger: Logger)(
      implicit ec: ExecutionContext): Future[Report] = {
    verifyModuleSet(moduleSet)

    val emitterResult = logger.time("Emitter") {
      emitter.emit(moduleSet, logger)
    }

    val writer = new OutputWriter(output, config) {
      protected def writeModule(moduleID: ModuleID, jsFileWriter: Writer): Unit = {
        val printer = new Printers.JSTreePrinter(jsFileWriter)
        jsFileWriter.write(emitterResult.header)
        jsFileWriter.write("'use strict';\n")
        printer.printTopLevelTree(emitterResult.body(moduleID))
        jsFileWriter.write(emitterResult.footer)
      }

      protected def writeModule(moduleID: ModuleID, jsFileWriter: Writer,
          sourceMapWriter: Writer): Unit = {
        val jsFileURI = OutputPatternsImpl.jsFileURI(config.outputPatterns, moduleID.id)
        val sourceMapURI = OutputPatternsImpl.sourceMapURI(config.outputPatterns, moduleID.id)

        val smWriter = new SourceMapWriter(sourceMapWriter, jsFileURI,
            config.relativizeSourceMapBase)

        val printer = new Printers.JSTreePrinterWithSourceMap(jsFileWriter, smWriter)

        jsFileWriter.write(emitterResult.header)
        for (_ <- 0 until emitterResult.header.count(_ == '\n'))
          smWriter.nextLine()

        jsFileWriter.write("'use strict';\n")
        smWriter.nextLine()

        printer.printTopLevelTree(emitterResult.body(moduleID))

        jsFileWriter.write(emitterResult.footer)
        jsFileWriter.write("//# sourceMappingURL=" + sourceMapURI + "\n")

        smWriter.complete()
      }
    }

    logger.timeFuture("BasicBackend: Write result") {
      writer.write(moduleSet)
    }
  }
}
