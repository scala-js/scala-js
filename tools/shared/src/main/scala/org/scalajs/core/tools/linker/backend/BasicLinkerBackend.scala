/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker.backend

import org.scalajs.core.tools.logging.Logger
import org.scalajs.core.tools.io.WritableVirtualJSFile
import org.scalajs.core.tools.sem.Semantics
import org.scalajs.core.tools.linker.LinkingUnit
import org.scalajs.core.tools.linker.analyzer.SymbolRequirement
import org.scalajs.core.tools.linker.backend.emitter.Emitter
import org.scalajs.core.tools.javascript.{JSFileBuilder, JSFileBuilderWithSourceMap}

/** The basic backend for the Scala.js linker.
 *
 *  Simply emits the JavaScript without applying any further optimizations.
 */
final class BasicLinkerBackend(
    semantics: Semantics,
    outputMode: OutputMode,
    moduleKind: ModuleKind,
    withSourceMap: Boolean,
    config: LinkerBackend.Config
) extends LinkerBackend(semantics, outputMode.esLevel, moduleKind,
    withSourceMap, config) {

  @deprecated("Use the overload with an explicit ModuleKind", "0.6.13")
  def this(semantics: Semantics, outputMode: OutputMode, withSourceMap: Boolean,
      config: LinkerBackend.Config) {
    this(semantics, outputMode, ModuleKind.NoModule, withSourceMap, config)
  }

  private[this] val emitter =
    new Emitter(semantics, outputMode, moduleKind)

  val symbolRequirements: SymbolRequirement = emitter.symbolRequirements

  /** Emit the given [[LinkingUnit]] to the target output
   *
   *  @param unit [[LinkingUnit]] to emit
   *  @param output File to write to
   */
  def emit(unit: LinkingUnit, output: WritableVirtualJSFile,
      logger: Logger): Unit = {
    verifyUnit(unit)

    val builder = newBuilder(output)

    try {
      logger.time("Emitter (write output)") {
        emitter.emitCustomHeader(config.customOutputWrapper._1, builder)
        emitter.emitAll(unit, builder, logger)
        emitter.emitCustomFooter(config.customOutputWrapper._2, builder)
      }

      builder.complete()
    } finally {
      builder.closeWriters()
    }
  }

  private def newBuilder(output: WritableVirtualJSFile): JSFileBuilder = {
    if (withSourceMap) {
      new JSFileBuilderWithSourceMap(output.name, output.contentWriter,
          output.sourceMapWriter, config.relativizeSourceMapBase)
    } else {
      new JSFileBuilder(output.name, output.contentWriter)
    }
  }
}
