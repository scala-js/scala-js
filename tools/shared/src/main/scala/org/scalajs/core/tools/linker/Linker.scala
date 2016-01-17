/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker

import org.scalajs.core.tools.logging.Logger
import org.scalajs.core.tools.io._

import org.scalajs.core.tools.sem.Semantics
import org.scalajs.core.tools.javascript.OutputMode
import org.scalajs.core.tools.linker.analyzer.SymbolRequirement
import org.scalajs.core.tools.linker.frontend.LinkerFrontend
import org.scalajs.core.tools.linker.frontend.optimizer.IncOptimizer
import org.scalajs.core.tools.linker.backend.{LinkerBackend, BasicLinkerBackend}

/** The Scala.js linker */
final class Linker(frontend: LinkerFrontend, backend: LinkerBackend)
    extends GenLinker {

  require(!backend.withSourceMap || frontend.withSourceMap,
      "Frontend must have source maps enabled if backend has them enabled")
  require(frontend.semantics == backend.semantics,
      "Frontend and backend must agree on semantics")
  require(frontend.esLevel == backend.outputMode.esLevel,
      "Frontend and backend must agree on ESLevel")

  val semantics: Semantics = frontend.semantics
  val outputMode: OutputMode = backend.outputMode

  private[this] var _valid = true

  def linkUnit(irFiles: Seq[VirtualScalaJSIRFile],
      symbolRequirements: SymbolRequirement, logger: Logger): LinkingUnit =
    guard(frontend.link(irFiles, symbolRequirements, logger))

  def link(irFiles: Seq[VirtualScalaJSIRFile],
      output: WritableVirtualJSFile, logger: Logger): Unit = {
    guard {
      val unit = frontend.link(irFiles, backend.symbolRequirements, logger)
      backend.emit(unit, output, logger)
    }
  }

  @inline
  private[this] def guard[T](body: => T): T = {
    if (!_valid) {
      throw new IllegalStateException(
          "Linker is invalid due to a previous exception in a component")
    }

    try {
      body
    } catch {
      case t: Throwable =>
        _valid = false
        throw t
    }
  }
}

object Linker extends LinkerPlatformExtensions
