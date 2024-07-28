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

import org.scalajs.ir.{EntryPointsInfo, Version}
import org.scalajs.ir.Names.ClassName
import org.scalajs.ir.Trees.ClassDef

import org.scalajs.logging._

import org.scalajs.linker.interface.ModuleInitializer
import org.scalajs.linker.standard._
import org.scalajs.linker.standard.ModuleSet.ModuleID
import org.scalajs.linker.analyzer._

/** Does a dead code elimination pass on a [[LinkingUnit]]. */
final class Refiner(config: CommonPhaseConfig, checkIR: Boolean) {
  import Refiner._

  private val irLoader = new ClassDefIRLoader
  private val analyzer =
    new Analyzer(config, initial = false, checkIR = checkIR, failOnError = true, irLoader)

  def refine(classDefs: Seq[(ClassDef, Version)],
      moduleInitializers: List[ModuleInitializer],
      symbolRequirements: SymbolRequirement, logger: Logger)(
      implicit ec: ExecutionContext): Future[LinkingUnit] = {

    irLoader.update(classDefs)

    val analysis = logger.timeFuture("Refiner: Compute reachability") {
      analyzer.computeReachability(moduleInitializers, symbolRequirements, logger)
    }

    for {
      analysis <- analysis
    } yield {
      val result = logger.time("Refiner: Assemble LinkedClasses") {
        val assembled = for {
          (classDef, version) <- classDefs
          if analysis.classInfos.contains(classDef.className)
        } yield {
          BaseLinker.linkClassDef(classDef, version,
              syntheticMethodDefs = Nil, analysis)
        }

        val (linkedClassDefs, linkedTopLevelExports) = assembled.unzip

        val globalInfo = new LinkedGlobalInfo(
          analysis.isClassSuperClassUsed
        )

        new LinkingUnit(config.coreSpec, linkedClassDefs.toList,
            linkedTopLevelExports.flatten.toList, moduleInitializers, globalInfo)
      }

      irLoader.cleanAfterRun()

      result
    }
  }
}

private object Refiner {
  private final class ClassDefIRLoader extends IRLoader {
    private var classesByName: Map[ClassName, ClassDef] = _

    def update(classDefs: Seq[(ClassDef, Version)]): Unit = {
      this.classesByName = classDefs.map(c => c._1.className -> c._1).toMap
    }

    def classesWithEntryPoints(): Iterable[ClassName] = {
      classesByName.values
        .withFilter(EntryPointsInfo.forClassDef(_).hasEntryPoint)
        .map(_.className)
    }

    def classExists(className: ClassName): Boolean =
      classesByName.contains(className)

    def irFileVersion(className: ClassName): Version =
      Version.Unversioned

    def loadClassDef(className: ClassName)(
        implicit ec: ExecutionContext): Future[ClassDef] = {
      Future.successful(classesByName(className))
    }

    def cleanAfterRun(): Unit = {
      classesByName = null
    }
  }
}
