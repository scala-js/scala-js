/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.linker.backend

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
  def emit(unit: LinkingUnit, output: LinkerOutput, logger: Logger): Unit = {
    verifyUnit(unit)

    val builder = newBuilder(output)
    try {
      logger.time("Emitter (write output)") {
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
