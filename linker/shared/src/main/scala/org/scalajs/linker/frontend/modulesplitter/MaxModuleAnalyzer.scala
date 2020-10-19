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
    private[this] val allPaths =
      mutable.Map.empty[ClassName, Paths[ModuleID, ClassName]]

    def tagAll(): scala.collection.Map[ClassName, (Set[ModuleID], Set[ClassName])] = {
      tagEntryPoints()
      allPaths.map { case (className, paths) => className -> paths.ends }
    }

    private def tag(className: ClassName, pathRoot: ModuleID, pathSteps: List[ClassName]): Unit = {
      val updated = allPaths
        .getOrElseUpdate(className, Paths.empty)
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

  /** Set of shortest, mutually prefix-free paths. */
  private final class Paths[H, T] private (
      private val content: mutable.Map[H, Paths[T, T]]) {

    /* cannot make this tailrec, because the type parameters change over the
     * recursion. 2.11 does not support this.
     */
    def put(h: H, t: List[T]): Boolean = {
      if (content.get(h).exists(_.isEmpty)) {
        // shorter or equal path already exists.
        false
      } else if (t.isEmpty) {
        // the path we put stops here, prune longer paths (if any).
        content.put(h, Paths.empty)
        true
      } else {
        // there are other paths, recurse.
        content
          .getOrElseUpdate(h, Paths.empty)
          .put(t.head, t.tail)
      }
    }

    /** Returns the ends of all paths. */
    def ends: (Set[H], Set[T]) = {
      val hBuilder = Set.newBuilder[H]
      val tBuilder = Set.newBuilder[T]

      content.foreach {
        case (h, t) if t.isEmpty =>
          hBuilder += h

        case (_, t) =>
          val (ts0, ts1) = t.ends
          tBuilder ++= ts0
          tBuilder ++= ts1
      }

      (hBuilder.result(), tBuilder.result())
    }

    private def isEmpty: Boolean = content.isEmpty
  }

  private final object Paths {
    def empty[H, T]: Paths[H, T] = new Paths(mutable.Map.empty)
  }
}
