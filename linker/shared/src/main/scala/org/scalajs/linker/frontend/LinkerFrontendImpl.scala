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

package org.scalajs.linker.frontend

import scala.concurrent._

import org.scalajs.logging.Logger

import org.scalajs.linker.interface._
import org.scalajs.linker.standard._
import org.scalajs.linker.frontend.optimizer.IncOptimizer
import org.scalajs.linker.frontend.modulesplitter._

/** The frontend of the Scala.js linker.
 *
 *  Produces a [[standard.ModuleSet ModuleSet]].
 *
 *  You probably want to use an instance of [[interface.Linker]], rather than
 *  this low-level class.
 *
 *  Attention: [[LinkerFrontendImpl]] does not cache the IR input. It is
 *  advisable to do so, unless all IR is already in memory.
 */
final class LinkerFrontendImpl private (config: LinkerFrontendImpl.Config, backendRequirements: SymbolRequirement)
    extends LinkerFrontend {

  /** Core specification that this linker frontend implements. */
  val coreSpec = config.commonConfig.coreSpec

  private[this] val optOptimizer: Option[IncOptimizer] =
    LinkerFrontendImplPlatform.createOptimizer(config)

  private[this] val preOptimizerRequirements = optOptimizer.fold(backendRequirements) {
    optimizer => backendRequirements ++ optimizer.symbolRequirements
  }

  private[this] val linker: BaseLinker =
    new BaseLinker(config.commonConfig, config.checkIR, preOptimizerRequirements)

  private[this] val refiner: Refiner =
    new Refiner(config.commonConfig, config.checkIR, backendRequirements)

  private[this] val splitter: ModuleSplitter = config.moduleSplitStyle match {
    case ModuleSplitStyle.FewestModules             => ModuleSplitter.fewestModules()
    case ModuleSplitStyle.SmallestModules           => ModuleSplitter.smallestModules()
    case ModuleSplitStyle.SmallModulesFor(packages) => ModuleSplitter.smallModulesFor(packages)
  }

  /** Link and optionally optimize the given IR to a
   *  [[standard.ModuleSet ModuleSet]].
   */
  def link(irFiles: Seq[IRFile], moduleInitializers: Seq[ModuleInitializer], logger: Logger)(
      implicit ec: ExecutionContext): Future[ModuleSet] = {

    val linkResult = logger.timeFuture("Linker") {
      linker.link(irFiles, moduleInitializers, logger)
    }

    val optimizedResult = optOptimizer.fold(linkResult) { optimizer =>
      linkResult.flatMap(optimize(_, optimizer, logger))
    }

    optimizedResult.map { unit =>
      logger.time("Module Splitter") {
        splitter.split(unit, logger)
      }
    }
  }

  private def optimize(unit: LinkingUnit, optimizer: IncOptimizer, logger: Logger)(
      implicit ec: ExecutionContext): Future[LinkingUnit] = {
    val optimized = logger.time("Optimizer") {
      optimizer.update(unit, logger)
    }

    logger.timeFuture("Refiner") {
      refiner.refine(optimized, unit.moduleInitializers, logger)
    }
  }
}

object LinkerFrontendImpl {
  def apply(config: Config, backendRequirements: SymbolRequirement): LinkerFrontendImpl =
    new LinkerFrontendImpl(config, backendRequirements)

  /** Configurations relevant to the frontend */
  final class Config private (
      /** Common phase config. */
      val commonConfig: CommonPhaseConfig,
      /** How to split modules (if at all). */
      val moduleSplitStyle: ModuleSplitStyle,
      /** If true, performs expensive checks of the IR for the used parts. */
      val checkIR: Boolean,
      /** Whether to use the Scala.js optimizer. */
      val optimizer: Boolean
  ) {
    private def this() = {
      this(
          commonConfig = CommonPhaseConfig(),
          moduleSplitStyle = ModuleSplitStyle.FewestModules,
          checkIR = false,
          optimizer = true)
    }

    def withCommonConfig(commonConfig: CommonPhaseConfig): Config =
      copy(commonConfig = commonConfig)

    def withModuleSplitStyle(moduleSplitStyle: ModuleSplitStyle): Config =
      copy(moduleSplitStyle = moduleSplitStyle)

    def withCheckIR(checkIR: Boolean): Config =
      copy(checkIR = checkIR)

    def withOptimizer(optimizer: Boolean): Config =
      copy(optimizer = optimizer)

    private def copy(
        commonConfig: CommonPhaseConfig = commonConfig,
        moduleSplitStyle: ModuleSplitStyle = moduleSplitStyle,
        checkIR: Boolean = checkIR,
        optimizer: Boolean = optimizer): Config = {
      new Config(commonConfig, moduleSplitStyle, checkIR, optimizer)
    }
  }

  object Config {
    def apply(): Config = new Config()
  }
}
