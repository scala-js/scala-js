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

import org.scalajs.ir.Names.ClassName
import org.scalajs.linker.standard.ModuleSet.ModuleID

/** Build fewest (largest) possible modules (without reaching unnecessary code).
 *
 *  Calculates a transitive closure over the dependency graph for each public
 *  module. After that, each class ends up with set of "tags": one "tag" for
 *  each public module it can be reached by. We then create a module for each
 *  distinct set of tags.
 */
private[modulesplitter] final class FewestModulesAnalyzer extends ModuleAnalyzer {
  import FewestModulesAnalyzer._

  def analyze(info: ModuleAnalyzer.DependencyInfo): ModuleAnalyzer.Analysis = {
    val hasDynDeps = info.classDependencies.exists(_._2.dynamicDependencies.nonEmpty)

    if (info.publicModuleDependencies.size == 1 && !hasDynDeps) {
      // Fast path.
      new SingleModuleAnalysis(info.publicModuleDependencies.head._1)
    } else {
      val modulesToAvoid = info.publicModuleDependencies.keys
      val moduleMap = new Tagger(info).tagAll(modulesToAvoid)

      new FullAnalysis(moduleMap)
    }
  }
}

private object FewestModulesAnalyzer {

  private final class SingleModuleAnalysis(moduleID: ModuleID)
      extends ModuleAnalyzer.Analysis {
    def moduleForClass(className: ClassName): Option[ModuleID] =
      Some(moduleID)
  }

  private final class FullAnalysis(map: scala.collection.Map[ClassName, ModuleID])
      extends ModuleAnalyzer.Analysis {
    def moduleForClass(className: ClassName): Option[ModuleID] =
      map.get(className)
  }
}
