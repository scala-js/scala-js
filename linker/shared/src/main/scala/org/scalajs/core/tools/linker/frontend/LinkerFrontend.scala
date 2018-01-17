/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker.frontend

import org.scalajs.core.tools.logging.Logger

import org.scalajs.core.tools.linker._
import org.scalajs.core.tools.linker.standard._
import org.scalajs.core.tools.linker.analyzer.SymbolRequirement
import org.scalajs.core.tools.linker.frontend.optimizer.{GenIncOptimizer, IncOptimizer}
import org.scalajs.core.tools.linker.irio._

/** The frontend of the Scala.js linker. Produces a [[LinkingUnit]]
 *
 *  You probably want to use an instance of [[linker.Linker]], rather than this
 *  low-level class.
 *
 *  Attention: [[LinkerFrontend]] does not cache the IR input. It is advisable to do
 *  so, unless all IR is already in memory.
 */
final class LinkerFrontend private (config: LinkerFrontend.Config) {

  /** Core specification that this linker frontend implements. */
  val coreSpec = config.commonConfig.coreSpec

  private[this] val linker: BaseLinker =
    new BaseLinker(config.commonConfig)

  private[this] val optOptimizer: Option[GenIncOptimizer] =
    LinkerFrontendPlatform.createOptimizer(config)

  private[this] val refiner: Refiner = new Refiner(config.commonConfig)

  /** Link and optionally optimize the given IR to a [[LinkingUnit]]. */
  def link(irFiles: Seq[VirtualScalaJSIRFile],
      moduleInitializers: Seq[ModuleInitializer],
      symbolRequirements: SymbolRequirement, logger: Logger): LinkingUnit = {

    val preOptimizerRequirements = optOptimizer.fold(symbolRequirements) {
      optimizer => symbolRequirements ++ optimizer.symbolRequirements
    }

    val linkResult = logger.time("Basic Linking") {
      linker.link(irFiles, moduleInitializers, logger,
          preOptimizerRequirements, config.checkIR)
    }

    optOptimizer.fold(linkResult) { optimizer =>
      optimize(linkResult, symbolRequirements, optimizer, logger)
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
  def apply(config: Config): LinkerFrontend =
    new LinkerFrontend(config)

  /** Configurations relevant to the frontend */
  final class Config private (
      /** Common phase config. */
      val commonConfig: CommonPhaseConfig,
      /** If true, performs expensive checks of the IR for the used parts. */
      val checkIR: Boolean,
      /** Whether to use the Scala.js optimizer. */
      val optimizer: Boolean
  ) {
    private def this() = {
      this(
          commonConfig = CommonPhaseConfig(),
          checkIR = false,
          optimizer = true)
    }

    def withCommonConfig(commonConfig: CommonPhaseConfig): Config =
      copy(commonConfig = commonConfig)

    def withCheckIR(checkIR: Boolean): Config =
      copy(checkIR = checkIR)

    def withOptimizer(optimizer: Boolean): Config =
      copy(optimizer = optimizer)

    private def copy(
        commonConfig: CommonPhaseConfig = commonConfig,
        checkIR: Boolean = checkIR,
        optimizer: Boolean = optimizer): Config = {
      new Config(commonConfig, checkIR, optimizer)
    }
  }

  object Config {
    def apply(): Config = new Config()
  }
}