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

import org.scalajs.logging.Logger

import org.scalajs.linker.interface.{IRFile, LinkerOutput}
import org.scalajs.linker.interface.unstable.OutputFileImpl
import org.scalajs.linker.standard._

import org.scalajs.linker.backend.emitter.Emitter
import org.scalajs.linker.backend.javascript.{JSLineBuilder, JSFileBuilder, JSFileBuilderWithSourceMap}

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

    logger.timeFuture("Emitter") {
      output.sourceMap.filter(_ => config.sourceMap).fold {
        // Without source map.
        val b = new JSFileBuilder
        emitter.emitAll(unit, b, logger)
        OutputFileImpl.fromOutputFile(output.jsFile).writeFull(b.complete())
      } { sourceMap =>
        // With source map.
        val b = new JSFileBuilderWithSourceMap(output.jsFileURI,
            output.sourceMapURI, config.relativizeSourceMapBase)
        emitter.emitAll(unit, b, logger)
        val (js, sm) = b.complete()

        OutputFileImpl.fromOutputFile(output.jsFile).writeFull(js)
          .flatMap(_ => OutputFileImpl.fromOutputFile(sourceMap).writeFull(sm))
      }
    }
  }
}
