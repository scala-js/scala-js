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

import org.scalajs.linker._
import org.scalajs.linker.standard._
import org.scalajs.linker.frontend.optimizer.{GenIncOptimizer, IncOptimizer}
import org.scalajs.linker.irio._

/** The frontend of the Scala.js linker.
 *
 *  Produces a [[standard.LinkingUnit LinkingUnit]].
 *
 *  You probably want to use an instance of [[linker.Linker]], rather than this
 *  low-level class.
 *
 *  Attention: [[LinkerFrontendImpl]] does not cache the IR input. It is
 *  advisable to do so, unless all IR is already in memory.
 */
final class LinkerFrontendImpl private (config: LinkerFrontendImpl.Config)
    extends LinkerFrontend {

  /** Core specification that this linker frontend implements. */
  val coreSpec = config.commonConfig.coreSpec

  private[this] val linker: BaseLinker =
    new BaseLinker(config.commonConfig)

  private[this] val optOptimizer: Option[GenIncOptimizer] =
    LinkerFrontendImplPlatform.createOptimizer(config)

  private[this] val refiner: Refiner = new Refiner(config.commonConfig)

  /** Link and optionally optimize the given IR to a
   *  [[standard.LinkingUnit LinkingUnit]].
   */
  def link(irFiles: Seq[IRFile],
      moduleInitializers: Seq[ModuleInitializer],
      symbolRequirements: SymbolRequirement, logger: Logger)(
      implicit ec: ExecutionContext): Future[LinkingUnit] = {

    val preOptimizerRequirements = optOptimizer.fold(symbolRequirements) {
      optimizer => symbolRequirements ++ optimizer.symbolRequirements
    }

    val linkResult = logger.timeFuture("Linker") {
      linker.link(irFiles, moduleInitializers, logger,
          preOptimizerRequirements, config.checkIR)
    }

    optOptimizer.fold(linkResult) { optimizer =>
      linkResult.flatMap(optimize(_, symbolRequirements, optimizer, logger))
    }
  }

  private def optimize(unit: LinkingUnit, symbolRequirements: SymbolRequirement,
      optimizer: GenIncOptimizer, logger: Logger)(
      implicit ec: ExecutionContext): Future[LinkingUnit] = {
    val optimized = logger.time("Optimizer") {
      optimizer.update(unit, logger)
    }

    logger.time("Refiner") {
      refiner.refine(optimized, symbolRequirements, logger)
    }
  }
}

object LinkerFrontendImpl {
  def apply(config: Config): LinkerFrontendImpl =
    new LinkerFrontendImpl(config)

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
