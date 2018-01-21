/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.linker.backend

import org.scalajs.logging.Logger
import org.scalajs.io.WritableVirtualJSFile

import org.scalajs.linker.standard._
import org.scalajs.linker.backend.emitter.Emitter

import org.scalajs.linker.backend.javascript.{
  JSFileBuilder, JSFileBuilderWithSourceMap
}

/** The basic backend for the Scala.js linker.
 *
 *  Simply emits the JavaScript without applying any further optimizations.
 */
final class BasicLinkerBackend(config: LinkerBackend.Config)
    extends LinkerBackend(config) {

  private[this] val emitter = new Emitter(config.commonConfig)

  val symbolRequirements: SymbolRequirement = emitter.symbolRequirements

  /** Emit the given [[standard.LinkingUnit LinkingUnit]] to the target output.
   *
   *  @param unit [[standard.LinkingUnit LinkingUnit]] to emit
   *  @param output File to write to
   */
  def emit(unit: LinkingUnit, output: WritableVirtualJSFile,
      logger: Logger): Unit = {
    verifyUnit(unit)

    val builder = newBuilder(output)

    try {
      logger.time("Emitter (write output)") {
        emitter.emitAll(unit, builder, logger)
      }

      builder.complete()
    } finally {
      builder.closeWriters()
    }
  }

  private def newBuilder(output: WritableVirtualJSFile): JSFileBuilder = {
    if (config.sourceMap) {
      new JSFileBuilderWithSourceMap(output.name, output.contentWriter,
          output.sourceMapWriter, config.relativizeSourceMapBase)
    } else {
      new JSFileBuilder(output.name, output.contentWriter)
    }
  }
}
