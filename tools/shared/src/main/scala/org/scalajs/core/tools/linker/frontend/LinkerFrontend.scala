/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker.frontend

import org.scalajs.core.tools.logging.Logger
import org.scalajs.core.tools.io.VirtualScalaJSIRFile

import org.scalajs.core.tools.sem.Semantics
import org.scalajs.core.tools.javascript.ESLevel

import org.scalajs.core.tools.linker.LinkingUnit
import org.scalajs.core.tools.linker.analyzer.SymbolRequirement
import org.scalajs.core.tools.linker.frontend.optimizer.{GenIncOptimizer, IncOptimizer}

/** The frontend of the Scala.js linker. Produces a [[LinkingUnit]]
 *
 *  You probably want to use an instance of [[linker.Linker]], rather than this
 *  low-level class.
 *
 *  Attention: [[LinkerFrontend]] does not cache the IR input. It is advisable to do
 *  so, unless all IR is already in memory.
 */
final class LinkerFrontend(
    val semantics: Semantics,
    val esLevel: ESLevel,
    val withSourceMap: Boolean,
    config: LinkerFrontend.Config,
    optimizerFactory: Option[GenIncOptimizer.OptimizerFactory]) {

  private[this] val linker: BaseLinker =
    new BaseLinker(semantics, esLevel, withSourceMap)

  private[this] val optOptimizer: Option[GenIncOptimizer] =
    optimizerFactory.map(_(semantics, esLevel, withSourceMap))

  private[this] val refiner: Refiner = new Refiner

  /** Link and optionally optimize the given IR to a [[LinkingUnit]]. */
  def link(irFiles: Seq[VirtualScalaJSIRFile],
      symbolRequirements: SymbolRequirement, logger: Logger): LinkingUnit = {

    val preOptimizerRequirements = optOptimizer.fold(symbolRequirements) {
      optimizer => symbolRequirements ++ optimizer.symbolRequirements
    }

    val linkResult = logger.time("Basic Linking") {
      linker.linkInternal(irFiles, logger, preOptimizerRequirements,
          config.bypassLinkingErrors, config.checkIR)
    }

    optOptimizer.fold(linkResult) { optimizer =>
      if (linkResult.isComplete) {
        optimize(linkResult, symbolRequirements, optimizer, logger)
      } else {
        logger.warn("Not running the optimizer because there were linking errors.")
        linkResult
      }
    }
  }

  private def optimize(unit: LinkingUnit, symbolRequirements: SymbolRequirement,
      optimizer: GenIncOptimizer, logger: Logger): LinkingUnit = {
    val optimized = logger.time("Inc. optimizer") {
      optimizer.update(unit, logger)
    }

    logger.time("Refiner") {
      refiner.refine(optimized, symbolRequirements, logger)
    }
  }
}

object LinkerFrontend {
  /** Configurations relevant to the frontend */
  final class Config private (
      /** Whether to only warn if the linker has errors. */
      val bypassLinkingErrors: Boolean = false,
      /** If true, performs expensive checks of the IR for the used parts. */
      val checkIR: Boolean = false
  ) {
    @deprecated(
        "Bypassing linking errors will not be possible in the next major version.",
        "0.6.6")
    def withBypassLinkingErrors(bypassLinkingErrors: Boolean): Config =
      copy(bypassLinkingErrors = bypassLinkingErrors)

    // Non-deprecated version to call from the sbt plugin
    private[scalajs] def withBypassLinkingErrorsInternal(
        bypassLinkingErrors: Boolean): Config = {
      copy(bypassLinkingErrors = bypassLinkingErrors)
    }

    def withCheckIR(checkIR: Boolean): Config =
      copy(checkIR = checkIR)

    private def copy(
        bypassLinkingErrors: Boolean = bypassLinkingErrors,
        checkIR: Boolean = checkIR): Config = {
      new Config(bypassLinkingErrors, checkIR)
    }
  }

  object Config {
    def apply(): Config = new Config()
  }
}
