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

import org.scalajs.ir.Names.ClassName

import org.scalajs.logging.Logger

import org.scalajs.linker.interface.{ModuleInitializer, ModuleKind}
import org.scalajs.linker.frontend.LinkingUnit
import org.scalajs.linker.standard._
import org.scalajs.linker.standard.ModuleSet.ModuleID

private[modulesplitter] final class MaxModuleSplitter extends ModuleSplitter {
  def split(unit: LinkingUnit, symbolRequirements: SymbolRequirement,
      logger: Logger): ModuleSet = {
    val moduleAnalyzer = new ModuleAnalyzer(unit, symbolRequirements)
    import moduleAnalyzer._

    if (allModules.size == 1) {
      val externalDeps =
        if (unit.coreSpec.moduleKind == ModuleKind.NoModule) Nil
        else allExternalDependencies().toList

      val id = allModules.head

      val module = new ModuleSet.Module(
          id = id,
          internalDeps = Set.empty,
          externalDeps = externalDeps,
          public = true,
          classDefs = unit.classDefs,
          topLevelExports = unit.topLevelExports,
          initializers = moduleInitializers(id)
      )

      new ModuleSet(unit.coreSpec, List(module), Nil)
    } else {
      require(unit.coreSpec.moduleKind != ModuleKind.NoModule)

      val groups = (new MaxModuleAnalyzer(moduleAnalyzer)).analyze()

      val publicModules = for {
        id <- allModules
      } yield {
        val internalDeps = groups.keys
          .filter(_.contains(id))
          .map(deriveModuleID(_))
          .toSet

        val classes = groups(Set(id))

        val externalDeps =
          externalDependencies(id) ++
          classes.flatMap(externalDependencies(_))

        new ModuleSet.Module(
          id = id,
          internalDeps = internalDeps,
          externalDeps = externalDeps.toList,
          public = true,
          classDefs = classes.map(linkedClassesByName(_)).toList,
          topLevelExports = topLevelExports(id),
          initializers = moduleInitializers(id)
        )
      }

      val internalModules = for {
        (mods, classes) <- groups
        if mods.size > 1
      } yield {
        val internalDeps = groups.keys
          .filter(_.subsetOf(mods))
          .map(deriveModuleID(_))
          .toSet

        val externalDeps = classes.flatMap(externalDependencies(_))
        val classDefs = classes.map(linkedClassesByName(_)).toList

        new ModuleSet.Module(
          id = deriveModuleID(mods),
          internalDeps = internalDeps,
          externalDeps = externalDeps.toList,
          public = false,
          classDefs = classDefs,
          topLevelExports = Nil,
          initializers = Nil
        )
      }

      val abstractClasses = groups.getOrElse(Set.empty, Nil)
        .map(linkedClassesByName(_))
        .toList

      val badAbstractClasses = abstractClasses.filter(_.hasAnyDefinitions)
      assert(badAbstractClasses.isEmpty, {
        val names = badAbstractClasses.map(_.fullName).mkString(", ")
        "Found abstract classes that contain code: $names"
      })

      new ModuleSet(unit.coreSpec, publicModules ++ internalModules, abstractClasses)
    }
  }

  private def deriveModuleID(names: Set[ModuleID]): ModuleID =
    if (names.size == 1) names.head
    else new ModuleID(names.map(_.id).toList.sorted.mkString("-"))
}

private class MaxModuleAnalyzer(moduleAnalyzer: ModuleAnalyzer) {
  import moduleAnalyzer._

  private[this] val allTags =
    mutable.Map.empty[ClassName, mutable.Set[ModuleID]]

  private[this] val toProcess = mutable.Set.empty[ClassName]

  def analyze(): Map[Set[ModuleID], Iterable[ClassName]] = {
    for {
      moduleID <- allModules
      dep <- staticDependencies(moduleID)
    } yield {
      tag(dep, moduleID)
    }

    process()

    allTags.groupBy(_._2.toSet).map(x => x._1 -> x._2.keys)
  }

  private def process(): Unit = {
    while (toProcess.nonEmpty) {
      val next = toProcess.head
      toProcess.remove(next)

      val deps = staticDependencies(next)
      val tags = allTags(next)

      for (d <- deps; t <- tags)
        tag(d, t)
    }
  }

  private def tag(className: ClassName, moduleName: ModuleID): Unit = {
    val changed = allTags.getOrElseUpdate(className, mutable.Set.empty).add(moduleName)
    if (changed)
      toProcess.add(className)
  }
}
