package org.scalajs.core.tools.optimizer

import org.scalajs.core.tools.sem._
import org.scalajs.core.tools.javascript.OutputMode
import org.scalajs.core.tools.logging._

import org.scalajs.core.ir.ClassKind

/** Does a dead code elimination pass on [[LinkedClass]]es */
final class Refiner(semantics: Semantics, outputMode: OutputMode) {

  @deprecated("Use the overload with an explicit output mode", "0.6.3")
  def this(semantics: Semantics) =
    this(semantics, OutputMode.ECMAScript51Isolated)

  def refine(unit: LinkingUnit, logger: Logger): LinkingUnit = {
    val analysis = logTime(logger, "Refiner: Compute reachability") {
      val analyzer = new Analyzer(semantics, outputMode,
          reachOptimizerSymbols = false, initialLink = false)
      analyzer.computeReachability(unit.infos.values.toList)
    }

    /* There really should not be linking errors at this point. If there are,
     * it is most likely a bug in the optimizer. We should crash here, but we
     * used to silently ignore any errors before 0.6.6. So currently we only
     * warn, not to break compatibility.
     * TODO Issue errors when we can break backward compatibility.
     */
    analysis.errors.foreach(Analysis.logError(_, logger, Level.Warn))

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
