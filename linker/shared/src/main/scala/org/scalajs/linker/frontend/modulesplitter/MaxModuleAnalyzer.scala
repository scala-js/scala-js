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

import scala.annotation.tailrec

import scala.collection.immutable
import scala.collection.mutable

import org.scalajs.ir.Names.ClassName
import org.scalajs.linker.standard.ModuleSet.ModuleID

/** Build fewest (largest) possible modules (without reaching unnecessary code).
 *
 *  Calculates a transitive closure over the dependency graph for each public
 *  module. After that, each class ends up with set of "tags": one "tag" for
 *  each public module it can be reached by. We then create a module for each
 *  distinct set of tags.
 */
private[modulesplitter] final class MaxModuleAnalyzer extends ModuleAnalyzer {
  import MaxModuleAnalyzer._

  def analyze(info: ModuleAnalyzer.DependencyInfo): ModuleAnalyzer.Analysis = {
    val hasDynDeps = info.classDependencies.exists(_._2.dynamicDependencies.nonEmpty)

    if (info.publicModuleDependencies.size == 1 && !hasDynDeps) {
      // Fast path.
      new SingleModuleAnalysis(info.publicModuleDependencies.head._1)
    } else {
      val allTags = new Tagger(info).tagAll()

      val moduleIDs =
        buildModuleIDs(info.publicModuleDependencies.keys, allTags.flatMap(_._2._2))

      val moduleMap = allTags.map {
        case (className, tags) => className -> moduleIDs(tags)
      }.toMap

      new FullAnalysis(moduleMap)
    }
  }

  private def buildModuleIDs(publicIDsUnordered: Iterable[ModuleID],
      dynamicIDsUnordered: Iterable[ClassName]): Map[(Set[ModuleID], Set[ClassName]), ModuleID] = {
    /* We build the new module IDs independent of the actually present
     * modules to ensure stability.
     *
     * We sort the ModuleIDs to not depend on map iteration order (or the
     * order of the input files).
     *
     * All of this is to avoid module ID collisions, for example with the
     * following set of public modules: {`a`, `b`, `a-b`}.
     */
    val publicIDs = sortedSet(publicIDsUnordered)(Ordering.by[ModuleID, String](_.id))
    val dynamicIDs = sortedSet(dynamicIDsUnordered)

    val seenIDs = mutable.Set.empty[ModuleID]

    val tups = for {
      dynamicModules <- dynamicIDs.subsets()
      publicModules <- publicIDs.subsets()
      if publicModules.nonEmpty || dynamicModules.nonEmpty
    } yield {
      var candidate = ModuleID((
          publicModules.toList.map(_.id) ++
          dynamicModules.toList.map(_.nameString)
      ).mkString("-"))

      while (seenIDs.contains(candidate))
        candidate = ModuleID(candidate.id + "$")

      seenIDs.add(candidate)

      (publicModules, dynamicModules) -> candidate
    }

    tups.toMap
  }

  private def sortedSet[T: Ordering](s: Iterable[T]): immutable.SortedSet[T] = {
    // Best way I could find to create a SortedSet from an Iterable (i.e. not Seq) :-/
    val b = immutable.SortedSet.newBuilder[T]
    s.foreach(b += _)
    b.result()
  }
}

private object MaxModuleAnalyzer {

  private final class SingleModuleAnalysis(moduleID: ModuleID)
      extends ModuleAnalyzer.Analysis {
    def moduleForClass(className: ClassName): Option[ModuleID] =
      Some(moduleID)
  }

  private final class FullAnalysis(map: Map[ClassName, ModuleID])
      extends ModuleAnalyzer.Analysis {
    def moduleForClass(className: ClassName): Option[ModuleID] =
      map.get(className)
  }

  /** Tagger performs the actual grouping of classes into modules.
   *
   *  To group classes into modules appropriately, we want to know for
   *  each class, "how" it can be reached. In practice, this means we
   *  record the path from the original public module and every
   *  dynamic import hop we made.
   *
   *  Of all these paths, we only care about the "simplest" ones. Or
   *  more formally, the minimum prefixes of all paths. For example,
   *  if a class is reachable by the following paths:
   *
   *  - a -> b
   *  - a -> b -> c
   *  - d -> c
   *  - d
   *
   *  We really only care about:
   *
   *  - a -> b
   *  - d
   *
   *  Because if we reach the class through a path that goes through
   *  `c`, it is necessarily already loaded.
   *
   *  Once we have obtained this minimal set of paths, we use the last
   *  element of each path to determine the final module
   *  grouping. This is because these have an actual static dependency
   *  on the node in question.
   *
   *  Merging these tags into a single `ModuleID` is delegated to the
   *  caller.
   */
  private final class Tagger(infos: ModuleAnalyzer.DependencyInfo) {
    private[this] val allPaths = mutable.Map.empty[ClassName, Paths]

    def tagAll(): scala.collection.Map[ClassName, (Set[ModuleID], Set[ClassName])] = {
      tagEntryPoints()
      allPaths.map { case (className, paths) => className -> paths.tags() }
    }

    private def tag(className: ClassName, pathRoot: ModuleID, pathSteps: List[ClassName]): Unit = {
      val updated = allPaths
        .getOrElseUpdate(className, new Paths)
        .put(pathRoot, pathSteps)

      if (updated) {
        val classInfo = infos.classDependencies(className)
        classInfo
          .staticDependencies
          .foreach(tag(_, pathRoot, pathSteps))

        classInfo
          .dynamicDependencies
          .foreach(c => tag(c, pathRoot, pathSteps :+ c))
      }
    }

    private def tagEntryPoints(): Unit = {
      for {
        (moduleID, deps) <- infos.publicModuleDependencies
        className <- deps
      } {
        tag(className, moduleID, Nil)
      }
    }
  }

  /** "Interesting" paths that can lead to a given class.
   *
   *  "Interesting" in this context means:
   *  - All direct paths from a public dependency.
   *  - All non-empty, mutually prefix-free paths of dynamic import hops.
   */
  private final class Paths {
    private val direct = mutable.Set.empty[ModuleID]
    private val dynamic = mutable.Map.empty[ModuleID, DynamicPaths]

    def put(pathRoot: ModuleID, pathSteps: List[ClassName]): Boolean = {
      if (pathSteps.isEmpty) {
        direct.add(pathRoot)
      } else {
        dynamic
          .getOrElseUpdate(pathRoot, new DynamicPaths)
          .put(pathSteps)
      }
    }

    def tags(): (Set[ModuleID], Set[ClassName]) = {
      /* Remove dynamic paths to class that are also reached by a public module.
       * However, only do this if there are other tags as well. Otherwise, this
       * class will end up in a public module, but the dynamically loaded module
       * will try to import it (but importing public modules is forbidden).
       */
      if (direct.size > 1 || direct != dynamic.keySet) {
        direct.foreach(dynamic.remove(_))
      }

      val endsBuilder = Set.newBuilder[ClassName]
      dynamic.values.foreach(_.ends(endsBuilder))
      (direct.toSet, endsBuilder.result())
    }
  }

  /** Set of shortest, mutually prefix-free paths of dynamic import hops */
  private final class DynamicPaths {
    private val content = mutable.Map.empty[ClassName, DynamicPaths]

    @tailrec
    def put(path: List[ClassName]): Boolean = {
      val h :: t = path

      if (content.get(h).exists(_.content.isEmpty)) {
        // shorter or equal path already exists.
        false
      } else if (t.isEmpty) {
        // the path we put stops here, prune longer paths (if any).
        content.put(h, new DynamicPaths)
        true
      } else {
        // there are other paths, recurse.
        content
          .getOrElseUpdate(h, new DynamicPaths)
          .put(t)
      }
    }

    /** Populates `builder` with the ends of all paths. */
    def ends(builder: mutable.Builder[ClassName, Set[ClassName]]): Unit = {
      for ((h, t) <- content) {
        if (t.content.isEmpty)
          builder += h
        else
          t.ends(builder)
      }
    }
  }
}
