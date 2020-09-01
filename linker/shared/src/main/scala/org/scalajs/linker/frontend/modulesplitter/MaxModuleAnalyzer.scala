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

/** Build fewest (largest) possible modules (without reaching unnecessary code).
 *
 *  Calculates a transitive closure over the dependency graph for each public
 *  module. After that, each class ends up with set of "tags": one "tag" for
 *  each public module it can be reached by. It then creates a module for each
 *  (distinct) set of tags.
 */
private[modulesplitter] final class MaxModuleAnalyzer extends ModuleAnalyzer {
  import MaxModuleAnalyzer._

  def analyze(info: ModuleAnalyzer.DependencyInfo): ModuleAnalyzer.Analysis = {
    if (info.publicModuleDependencies.size == 1) {
      new SingleModuleAnalysis(info.publicModuleDependencies.head._1)
    } else {
      val run = new Run(info)
      run.analyze()
      run
    }
  }
}

private object MaxModuleAnalyzer {

  private final class SingleModuleAnalysis(moduleID: ModuleID)
      extends ModuleAnalyzer.Analysis {
    def moduleForClass(className: ClassName): Option[ModuleID] =
      Some(moduleID)
  }

  private final class Run(infos: ModuleAnalyzer.DependencyInfo)
      extends ModuleAnalyzer.Analysis {

    private[this] val allTags =
      mutable.Map.empty[ClassName, mutable.Set[ModuleID]]

    private[this] val toProcess = mutable.Set.empty[ClassName]

    private[this] val classModule =
      mutable.Map.empty[ClassName, ModuleID]

    def moduleForClass(className: ClassName): Option[ModuleID] =
      classModule.get(className)

    def analyze(): Unit = {
      tagEntryPoints()
      processTags()
      buildModules()
    }

    private def tagEntryPoints(): Unit = {
      for {
        (moduleID, deps) <- infos.publicModuleDependencies
        className <- deps
      } {
        tag(className, moduleID :: Nil)
      }
    }

    private def processTags(): Unit = {
      /* We process tags in a pseudo breadth-first manner (pseudo because we can
       * process each node multiple times).
       *
       * Observe that the provenance of a tag does not matter when propagating
       * from any given node. Using the "BFS" strategy, we try to accumulate all
       * tags on a given node first and only then propagate forward. This is an
       * attempt to avoid traversing the same subtree multiple times.
       */

      while (toProcess.nonEmpty) {
        val next = toProcess.head
        toProcess.remove(next)

        val deps = infos.classDependencies(next)
        val tags = allTags(next)

        deps.foreach(tag(_, tags))
      }
    }

    private def tag(className: ClassName, tags: Iterable[ModuleID]): Unit = {
      val base = allTags.getOrElseUpdate(className, mutable.Set.empty)

      var changed = false

      for (t <- tags)
        changed ||= base.add(t)

      if (changed)
        toProcess.add(className)
    }

    private def buildModules(): Unit = {
      val ids = mutable.Map.empty[scala.collection.Set[ModuleID], ModuleID]

      def mkID(names: mutable.Set[ModuleID]) =
        new ModuleID(names.map(_.id).toList.sorted.mkString("-"))

      // TODO: Dedupe.

      for ((className, moduleIDs) <- allTags) {
        val id = ids.getOrElseUpdate(moduleIDs, mkID(moduleIDs))
        classModule.put(className, id)
      }
    }
  }
}
