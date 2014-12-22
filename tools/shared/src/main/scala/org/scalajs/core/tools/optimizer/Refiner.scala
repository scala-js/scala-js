package org.scalajs.core.tools.optimizer

import org.scalajs.core.tools.sem._
import org.scalajs.core.tools.logging._

import org.scalajs.core.ir.ClassKind

/** Does a dead code elimination pass on [[LinkedClass]]es */
final class Refiner(semantics: Semantics) {

  def refine(unit: LinkingUnit, logger: Logger): LinkingUnit = {
    val analysis = logTime(logger, "Refiner: Compute reachability") {
      val analyzer = new Analyzer(semantics, reachOptimizerSymbols = false)
      analyzer.computeReachability(unit.infos.values.toList)
    }

    // Note: We ignore all errors of the analysis

    logTime(logger, "Refiner: Assemble LinkedClasses") {
      val linkedClassesByName =
        Map(unit.classDefs.map(c => c.encodedName -> c): _*)

      def optClassDef(analyzerInfo: Analysis.ClassInfo) = {
        val encodedName = analyzerInfo.encodedName

        def optDummyParent =
          if (!analyzerInfo.isAnySubclassInstantiated) None
          else Some(LinkedClass.dummyParent(encodedName, Some("dummy")))

        linkedClassesByName.get(encodedName).map {
          refineClassDef(_, analyzerInfo)
        }.orElse(optDummyParent)
      }

      val linkedClassDefs = for {
        classInfo <- analysis.classInfos.values
        if classInfo.isNeededAtAll
        linkedClassDef <- optClassDef(classInfo)
      } yield linkedClassDef

      unit.updated(classDefs = linkedClassDefs.toList,
          isComplete = analysis.allAvailable)
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

    val kind = {
      if (classDef.kind == ClassKind.ModuleClass && !info.isModuleAccessed)
        ClassKind.Class
      else
        classDef.kind
    }

    classDef.copy(fields = fields, staticMethods = staticMethods,
        memberMethods = memberMethods, abstractMethods = abstractMethods,
        kind = kind, hasInstances = info.isAnySubclassInstantiated,
        hasRuntimeTypeInfo = info.isDataAccessed)
  }

}
