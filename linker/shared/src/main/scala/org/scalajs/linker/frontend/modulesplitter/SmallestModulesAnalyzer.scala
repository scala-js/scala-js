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
import org.scalajs.linker.standard.ModuleSet.ModuleID

/** Build smallest possible modules.
 *
 *  Generates a module per class, except when there are circular dependencies.
 *
 *  In practice, this means it generates a module per strongly connected
 *  component of the (static) dependency graph.
 */
private[modulesplitter] final class SmallestModulesAnalyzer extends ModuleAnalyzer {
  def analyze(info: ModuleAnalyzer.DependencyInfo): ModuleAnalyzer.Analysis = {
    val run = new SmallestModulesAnalyzer.Run(info)
    run.analyze()
    run
  }
}

private[modulesplitter] object SmallestModulesAnalyzer {

  private final class Run(info: ModuleAnalyzer.DependencyInfo)
      extends StrongConnect(info) with ModuleAnalyzer.Analysis {

    private[this] val moduleIndexToID = mutable.Map.empty[Int, ModuleID]

    def moduleForClass(className: ClassName): Option[ModuleID] =
      moduleIndex(className).map(moduleIndexToID)

    protected def emitModule(moduleIndex: Int, classNames: List[ClassName]): Unit = {
      val repr = ModuleIDs.representativeClass(classNames)
      val id = ModuleIDs.forClassName(info.publicModuleDependencies.keySet, repr)
      moduleIndexToID(moduleIndex) = id
    }
  }
}
