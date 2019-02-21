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

import org.scalajs.linker.LinkerOutput
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

  /** Emit the given [[standard.LinkingUnit LinkingUnit]] to the target output.
   *
   *  @param unit [[standard.LinkingUnit LinkingUnit]] to emit
   *  @param output File to write to
   */
  def emit(unit: LinkingUnit, output: LinkerOutput, logger: Logger)(
      implicit ex: ExecutionContext): Future[Unit] = Future {
    verifyUnit(unit)

    val builder = newBuilder(output)
    try {
      logger.time("Emitter") {
        emitter.emitAll(unit, builder, logger)
      }
    } finally {
      builder.complete()
    }
  }

  private def newBuilder(output: LinkerOutput): JSLineBuilder = {
    if (config.sourceMap && output.sourceMap.isDefined) {
      new JSFileBuilderWithSourceMap(output, config.relativizeSourceMapBase)
    } else {
      new JSFileBuilder(output)
    }
  }
}
