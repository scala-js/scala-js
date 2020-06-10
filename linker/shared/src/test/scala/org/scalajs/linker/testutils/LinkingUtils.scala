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
import org.scalajs.linker.interface._
import org.scalajs.linker.standard._

object LinkingUtils {

  def expectSuccess[A](link: Logger => Future[A])(
      implicit ec: ExecutionContext): Future[A] = {
    link(new ScalaConsoleLogger(Level.Error))
  }

  def expectFailure(link: Logger => Future[Unit])(
      implicit ec: ExecutionContext): Future[FailedLinkingResult] = {

    val logger = new TestLogger()

    // We cannot use `transform` because of 2.11.
    link(logger).failed.recoverWith {
      case _: NoSuchElementException =>
        Future.failed(new AssertionError("Linking did not fail"))
    }.map { exception =>
      exception match {
        case exception: LinkingException =>
          new FailedLinkingResult(exception, logger)
        case _ =>
          throw new AssertionError("Expected LinkingException but got " + exception)
      }
    }
  }

  def linkOnly(classDefs: Seq[ClassDef],
      moduleInitializers: List[ModuleInitializer],
      config: StandardConfig,
      stdlib: Future[Seq[IRFile]] = TestIRRepo.minilib)(
      implicit ec: ExecutionContext): Logger => Future[Unit] = { logger =>

    val linkerFrontend = StandardLinkerFrontend(config.withCheckIR(true))
    val noSymbolRequirements = SymbolRequirement.factory("linker test").none()

    withLibAndClassDefFiles(stdlib, classDefs) { irFiles =>
      linkerFrontend.link(irFiles, moduleInitializers, noSymbolRequirements,
          logger)
    }.map(_ => ())
  }

  private final class StoreLinkingUnitLinkerBackend(
      originalBackend: LinkerBackend)
      extends LinkerBackend {

    @volatile
    private var _linkingUnit: LinkingUnit = _

    val coreSpec: CoreSpec = originalBackend.coreSpec

    val symbolRequirements: SymbolRequirement = originalBackend.symbolRequirements

    override def injectedIRFiles: Seq[IRFile] = originalBackend.injectedIRFiles

    def emit(unit: LinkingUnit, output: LinkerOutput, logger: Logger)(
        implicit ec: ExecutionContext): Future[Unit] = {
      _linkingUnit = unit
      Future.successful(())
    }

    def linkingUnit: LinkingUnit = {
      if (_linkingUnit == null)
        throw new IllegalStateException("Cannot access linkingUnit before emit is called")
      _linkingUnit
    }
  }

  def linkToLinkingUnit(classDefs: Seq[ClassDef],
      moduleInitializers: List[ModuleInitializer],
      config: StandardConfig,
      stdlib: Future[Seq[IRFile]] = TestIRRepo.minilib)(
      implicit ec: ExecutionContext): Logger => Future[LinkingUnit] = { logger =>

    val configWithCheckIR = config.withCheckIR(true)
    val frontend = StandardLinkerFrontend(configWithCheckIR)
    val backend = new StoreLinkingUnitLinkerBackend(
        StandardLinkerBackend(configWithCheckIR))
    val linker = StandardLinkerImpl(frontend, backend)
    val output = LinkerOutput(MemOutputFile())

    withLibAndClassDefFiles(stdlib, classDefs) { irFiles =>
      linker.link(irFiles, moduleInitializers, output, logger)
    }.map { _ =>
      backend.linkingUnit
    }
  }

  def linkAndEmit(classDefs: Seq[ClassDef],
      moduleInitializers: List[ModuleInitializer],
      config: StandardConfig,
      stdlib: Future[Seq[IRFile]] = TestIRRepo.minilib)(
      implicit ec: ExecutionContext): Logger => Future[Unit] = { logger =>

    val linker = StandardImpl.linker(config.withCheckIR(true))
    val output = LinkerOutput(MemOutputFile())

    withLibAndClassDefFiles(stdlib, classDefs) { irFiles =>
      linker.link(irFiles, moduleInitializers, output, logger)
    }
  }

  private def withLibAndClassDefFiles[A](
      stdlib: Future[Seq[IRFile]], classDefs: Seq[ClassDef])(
      f: Seq[IRFile] => Future[A])(
      implicit ec: ExecutionContext): Future[A] = {
    stdlib.flatMap { stdLibFiles =>
      f(stdLibFiles ++ classDefs.map(MemClassDefIRFile(_)))
    }
  }

}
