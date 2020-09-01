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

package org.scalajs.linker.frontend.modulesplitter

import scala.collection.mutable
import scala.collection.mutable.Builder

import org.scalajs.logging.Logger

import org.scalajs.ir.Names._

import org.scalajs.linker.frontend.LinkingUnit
import org.scalajs.linker.interface.ModuleInitializer
import org.scalajs.linker.interface.unstable.ModuleInitializerImpl
import org.scalajs.linker.standard._
import org.scalajs.linker.standard.ModuleSet.ModuleID

/** Splits Scala.js code into multiple modules. */
final class ModuleSplitter private (analyzer: ModuleAnalyzer) {
  import ModuleSplitter._
  import ModuleAnalyzer.DependencyInfo

  def split(unit: LinkingUnit, symbolRequirements: SymbolRequirement,
      logger: Logger): ModuleSet = {
    val dependencyInfo = logger.time("Module Splitter: Calculate Dependency Info") {
      calculateDependencyInfo(unit, symbolRequirements)
    }

    if (dependencyInfo.publicModuleDependencies.isEmpty) {
      /* If there are no public modules, we need no code whatsoever.
       * The analyzer cannot eliminate this degenerate case.
       * We have to do it here, otherwise we break the assumption, that all
       * non-abstract classes are reached through a static path.
       */
      new ModuleSet(unit.coreSpec, Nil, Nil)
    }

    val analysis = logger.time("Module Splitter: Analyze Modules") {
      analyzer.analyze(dependencyInfo)
    }

    logger.time("Module Splitter: Assemble Modules") {
      assembleModules(unit, analysis, dependencyInfo)
    }
  }

  private def calculateDependencyInfo(unit: LinkingUnit,
      symbolRequirements: SymbolRequirement): DependencyInfo = {
    def classDep(classDef: LinkedClass): Set[ClassName] = {
      val base = classDef.staticDependencies

      /* We use j.l.Object as representation of the core infrastructure.
       * As such, everything depends on j.l.Object and j.l.Object depends on all
       * symbol requirements.
       */
      if (classDef.className == ObjectClass)
        base ++ HijackedClasses ++ symbolRequirementDeps(symbolRequirements)
      else
        base + ObjectClass
    }

    val classDeps = unit.classDefs.map(c => c.className -> classDep(c)).toMap

    new DependencyInfo(classDeps, publicModuleDependencies(unit))
  }

  private def assembleModules(unit: LinkingUnit,
      analysis: ModuleAnalyzer.Analysis,
      dependencyInfo: DependencyInfo): ModuleSet = {

    // LinkedHashMap for stability.
    val builders = mutable.LinkedHashMap.empty[ModuleID, ModuleBuilder]
    val abstractClasses = List.newBuilder[LinkedClass]

    def getBuilder(moduleID: ModuleID): ModuleBuilder =
      builders.getOrElseUpdate(moduleID, new ModuleBuilder(moduleID))

    for (classDef <- unit.classDefs) {
      analysis.moduleForClass(classDef.className) match {
        case None =>
          assert(!classDef.hasAnyDefinitions,
              s"${classDef.fullName} was not put in a module but has definitions")
          abstractClasses += classDef

        case Some(moduleID) =>
          val builder = getBuilder(moduleID)
          builder.classDefs += classDef

          for (dep <- dependencyInfo.classDependencies(classDef.className)) {
            val dependencyModuleID = analysis.moduleForClass(dep).get
            if (dependencyModuleID != moduleID)
              builder.internalDependencies += dependencyModuleID
          }

          classDef.externalDependencies.foreach(builder.externalDependencies += _)
      }
    }

    // Output TopLevelExports and module initializers first for stability.

    for (tle <- unit.topLevelExports) {
      val builder = getBuilder(new ModuleID(tle.tree.moduleID))
      builder.topLevelExports += tle
      tle.externalDependencies.foreach(builder.externalDependencies += _)
    }

    for (mi <- unit.moduleInitializers) {
      val builder = getBuilder(new ModuleID(mi.moduleID))
      builder.initializers += mi.initializer
    }

    for {
      (moduleID, deps) <- dependencyInfo.publicModuleDependencies
    } {
      // Avoid getBuilder: All modules should be present. Otherwise it's a bug.
      val builder = builders(moduleID)
      for (dep <- deps) {
        val dependencyModuleID = analysis.moduleForClass(dep).get
        if (dependencyModuleID != moduleID)
          builder.internalDependencies += dependencyModuleID
      }
    }

    val modules = builders.values.map(_.result()).toList

    new ModuleSet(unit.coreSpec, modules, abstractClasses.result())
  }

  private def publicModuleDependencies(
      unit: LinkingUnit): Map[ModuleID, Set[ClassName]] = {
    /* All static initializers must be reached by all leaf modules.
     *
     * This is because we have lost the semantic information of *why* the things
     * in the static initializer are supposed to happen. In practical terms, at
     * the time of this writing, the Scala.js compiler emits static initializers
     * for two reasons:
     *
     * - Initialize fields that are exported to the top level:
     *   Needs to be executed for the class definition itself to be correct.
     * - Register a class with scala.scalajs.reflect.Reflect:
     *   Needs to be executed for reflective instantiation to work.
     *
     * PR #4146 and this comment thread provide additional context:
     * https://github.com/scala-js/scala-js/pull/4111#issuecomment-673590827
     */
    val classesWithStaticInits =
      unit.classDefs.filter(_.hasStaticInitializer).map(_.className)

    // What all modules depend on.
    val baseDeps = classesWithStaticInits.toSet + ObjectClass

    val result = mutable.Map.empty[ModuleID, Set[ClassName]]

    def add(moduleID: String, deps: Iterable[ClassName]) = {
      val id = new ModuleID(moduleID)
      val cur = result.getOrElse(id, baseDeps)
      result(id) = cur ++ deps
    }

    for (tle <- unit.topLevelExports)
      add(tle.tree.moduleID, tle.staticDependencies)

    for (mi <- unit.moduleInitializers) {
      val dep = mi.initializer match {
        case ModuleInitializerImpl.VoidMainMethod(className, _) =>
          className
        case ModuleInitializerImpl.MainMethodWithArgs(className, _, _) =>
          className
      }

      add(mi.moduleID, dep :: Nil)
    }

    result.toMap
  }
}

object ModuleSplitter {
  def minSplitter(): ModuleSplitter =
    new ModuleSplitter(new MinModuleAnalyzer())

  def maxSplitter(): ModuleSplitter =
    new ModuleSplitter(new MaxModuleAnalyzer())

  private def symbolRequirementDeps(requirement: SymbolRequirement): List[ClassName] = {
    import SymbolRequirement.Nodes._

    requirement match {
      case AccessModule(_, moduleName)        => List(moduleName)
      case InstantiateClass(_, className, _)  => List(className)
      case InstanceTests(_, className)        => List(className)
      case ClassData(_, className)            => List(className)
      case CallStaticMethod(_, className, _)  => List(className)

      case _: CallMethod | NoRequirement =>
        Nil

      case Optional(requirement) =>
        ??? // TODO: How do filter this?

      case Multiple(requirements) =>
        requirements.flatMap(symbolRequirementDeps(_))
    }
  }

  private class ModuleBuilder(id: ModuleID) {
    val internalDependencies: Builder[ModuleID, Set[ModuleID]] = Set.newBuilder
    val externalDependencies: Builder[String, Set[String]] = Set.newBuilder
    val classDefs: Builder[LinkedClass, List[LinkedClass]] = List.newBuilder
    val topLevelExports: Builder[LinkedTopLevelExport, List[LinkedTopLevelExport]] = List.newBuilder
    val initializers: Builder[ModuleInitializer.Initializer, List[ModuleInitializer.Initializer]] = List.newBuilder

    def result(): ModuleSet.Module = {
      val tles = topLevelExports.result()
      val inits = initializers.result()
      val public = tles.nonEmpty || inits.nonEmpty
      new ModuleSet.Module(id, internalDependencies.result(),
          externalDependencies.result(), public, classDefs.result(), tles, inits)
    }
  }
}
