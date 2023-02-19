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

package org.scalajs.linker.testutils

import scala.concurrent._

import org.scalajs.ir.Trees.ClassDef

import org.scalajs.logging._

import org.scalajs.linker._
import org.scalajs.linker.analyzer._
import org.scalajs.linker.frontend.FileIRLoader
import org.scalajs.linker.interface._
import org.scalajs.linker.standard._

object LinkingUtils {
  def testLink(classDefs: Seq[ClassDef],
      moduleInitializers: List[ModuleInitializer],
      config: StandardConfig = StandardConfig(),
      output: OutputDirectory = MemOutputDirectory())(
      implicit ec: ExecutionContext): Future[Report] = {

    val linker = StandardImpl.linker(config.withCheckIR(true))
    val classDefsFiles = classDefs.map(MemClassDefIRFile(_))

    TestIRRepo.minilib.flatMap { stdLibFiles =>
      linker.link(stdLibFiles ++ classDefsFiles, moduleInitializers,
          output, new ScalaConsoleLogger(Level.Error))
    }
  }

  private final class StoreModuleSetLinkerBackend(originalBackend: LinkerBackend)
      extends LinkerBackend {

    @volatile
    private var _moduleSet: ModuleSet = _

    val coreSpec: CoreSpec = originalBackend.coreSpec

    val symbolRequirements: SymbolRequirement = originalBackend.symbolRequirements

    override def injectedIRFiles: Seq[IRFile] = originalBackend.injectedIRFiles

    def emit(moduleSet: ModuleSet, output: OutputDirectory, logger: Logger)(
        implicit ec: ExecutionContext): Future[Report] = {
      _moduleSet = moduleSet
      originalBackend.emit(moduleSet, output, logger)
    }

    def moduleSet: ModuleSet = {
      if (_moduleSet == null)
        throw new IllegalStateException("Cannot access moduleSet before emit is called")
      _moduleSet
    }
  }

  def linkToModuleSet(classDefs: Seq[ClassDef],
      moduleInitializers: List[ModuleInitializer],
      config: StandardConfig = StandardConfig(),
      stdlib: Future[Seq[IRFile]] = TestIRRepo.minilib)(
      implicit ec: ExecutionContext): Future[ModuleSet] = {

    val cfg = config.withCheckIR(true)
    val frontend = StandardLinkerFrontend(cfg)
    val backend = new StoreModuleSetLinkerBackend(StandardLinkerBackend(cfg))
    val linker = StandardLinkerImpl(frontend, backend)

    val classDefsFiles = classDefs.map(MemClassDefIRFile(_))
    val output = MemOutputDirectory()

    stdlib.flatMap { stdLibFiles =>
      linker.link(stdLibFiles ++ classDefsFiles, moduleInitializers,
          output, new ScalaConsoleLogger(Level.Error))
    }.map { _ =>
      backend.moduleSet
    }
  }

  private lazy val noSymbolRequirements: SymbolRequirement =
    SymbolRequirement.factory("linking utils").none()

  def computeAnalysis(classDefs: Seq[ClassDef],
      symbolRequirements: SymbolRequirement = noSymbolRequirements,
      moduleInitializers: Seq[ModuleInitializer] = Nil,
      config: StandardConfig = StandardConfig(),
      stdlib: Future[Seq[IRFile]] = TestIRRepo.minilib)(
      implicit ec: ExecutionContext): Future[Analysis] = {

    val classDefIRFiles = classDefs.map(MemClassDefIRFile(_))
    val injectedIRFiles = StandardLinkerBackend(config).injectedIRFiles

    val irLoader = new FileIRLoader
    val analyzer = new Analyzer(CommonPhaseConfig.fromStandardConfig(config),
        initial = true, checkIR = true, irLoader)
    val logger = new ScalaConsoleLogger(Level.Error)

    for {
      baseFiles <- stdlib
      _ <- irLoader.update(classDefIRFiles ++ baseFiles ++ injectedIRFiles)
      analysis <- analyzer.computeReachability(moduleInitializers, symbolRequirements, logger)
    } yield {
      analysis
    }
  }
}
