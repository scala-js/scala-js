/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker.frontend

import org.scalajs.core.tools.sem._
import org.scalajs.core.tools.logging._

import org.scalajs.core.tools.linker._
import org.scalajs.core.tools.linker.analyzer._

import org.scalajs.core.ir.ClassKind

/** Does a dead code elimination pass on [[LinkedClass]]es */
final class Refiner {

  def refine(unit: LinkingUnit, symbolRequirements: SymbolRequirement,
      logger: Logger): LinkingUnit = {
    val analysis = logger.time("Refiner: Compute reachability") {
      val allSymbolRequirements = {
        symbolRequirements ++
        ModuleInitializer.toSymbolRequirement(unit.moduleInitializers)
      }
      Analyzer.computeReachability(unit.semantics, allSymbolRequirements,
          unit.infosInternal.values.toList, allowAddingSyntheticMethods = false)
    }

    /* There must not be linking errors at this point. If there are, it is a
     * bug in the optimizer.
     */
    if (analysis.errors.nonEmpty) {
      analysis.errors.foreach(Analysis.logError(_, logger, Level.Error))
      throw new AssertionError(
          "There were linking errors after the optimizer has run. " +
          "This is a bug, please report it. " +
          "You can work around the bug by disabling the optimizer. " +
          "In the sbt plugin, this can be done with " +
          "`scalaJSLinkerConfig ~= { _.withOptimizer(false) }`.")
    }

    logger.time("Refiner: Assemble LinkedClasses") {
      val linkedClassesByName =
        Map(unit.classDefs.map(c => c.encodedName -> c): _*)

      val linkedClassDefs = for {
        analyzerInfo <- analysis.classInfos.values
        if analyzerInfo.isNeededAtAll
      } yield {
        refineClassDef(linkedClassesByName(analyzerInfo.encodedName),
            analyzerInfo)
      }

      unit.updated(classDefs = linkedClassDefs.toList)
    }
  }

  private def refineClassDef(classDef: LinkedClass,
      info: Analysis.ClassInfo): LinkedClass = {

    val fields =
      if (info.isAnySubclassInstantiated) classDef.fields
      else Nil

    val staticMethods = classDef.staticMethods filter { m =>
      info.staticMethodInfos(m.info.encodedName).isReachable
    }

    val memberMethods = classDef.memberMethods filter { m =>
      info.methodInfos(m.info.encodedName).isReachable
    }

    val abstractMethods = classDef.abstractMethods filter { m =>
      info.methodInfos(m.info.encodedName).isReachable
    }

    val kind =
      if (info.isModuleAccessed) classDef.kind
      else classDef.kind.withoutModuleAccessor

    classDef.copy(
        kind = kind,
        fields = fields,
        staticMethods = staticMethods,
        memberMethods = memberMethods,
        abstractMethods = abstractMethods,
        hasInstances = info.isAnySubclassInstantiated,
        hasInstanceTests = info.areInstanceTestsUsed,
        hasRuntimeTypeInfo = info.isDataAccessed)
  }

}
