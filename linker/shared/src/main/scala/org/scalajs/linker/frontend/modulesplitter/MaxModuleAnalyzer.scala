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

import java.nio.charset.StandardCharsets

import org.scalajs.ir.Names.ClassName
import org.scalajs.ir.SHA1
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
      val prefix = internalModuleIDPrefix(info.publicModuleDependencies.keys)
      val moduleMap = new Tagger(info).tagAll(prefix)

      new FullAnalysis(moduleMap)
    }
  }

  /** Create a prefix for internal modules.
   *
   *  Chosen such that it is not a prefix of any public module ID.
   *  This ensures that a generated internal module ID never collides with a
   *  public module ID.
   */
  private def internalModuleIDPrefix(publicIDs: Iterable[ModuleID]): String = {
    Iterator
      .iterate("internal-")(_ + "-")
      .find(p => !publicIDs.exists(_.id.startsWith(p)))
      .get
  }
}

private object MaxModuleAnalyzer {

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

    def tagAll(internalModuleIDPrefix: String): scala.collection.Map[ClassName, ModuleID] = {
      tagEntryPoints()
      allPaths.map { case (className, paths) =>
        className -> paths.moduleID(internalModuleIDPrefix)
      }
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

    def moduleID(internalModuleIDPrefix: String): ModuleID = {
      if (direct.size == 1 && dynamic.isEmpty) {
        /* Class is only used by a single public module. Put it there.
         *
         * Note that we must not do this if there are any dynamic modules
         * requiring this class. Otherwise, the dynamically loaded module
         * will try to import the public module (but importing public modules is
         * forbidden).
         */
        direct.head
      } else {
        /* Class is used by multiple public modules and/or dynamic edges.
         * Create a module ID grouping it with other classes that have the same
         * dependees.
         */
        val digestBuilder = new SHA1.DigestBuilder

        // Public modules using this.
        for (id <- direct.toList.sortBy(_.id))
          digestBuilder.update(id.id.getBytes(StandardCharsets.UTF_8))

        // Dynamic modules using this.
        for (className <- dynamicEnds)
          digestBuilder.updateUTF8String(className.encoded)

        // Build a hex string of the hash with the right prefix.
        @inline def hexDigit(digit: Int): Char =
          Character.forDigit(digit & 0x0f, 16)

        val id = new java.lang.StringBuilder(internalModuleIDPrefix)

        for (b <- digestBuilder.finalizeDigest()) {
          id.append(hexDigit(b >> 4))
          id.append(hexDigit(b))
        }

        ModuleID(id.toString())
      }
    }

    private def dynamicEnds: immutable.SortedSet[ClassName] = {
      val builder = immutable.SortedSet.newBuilder[ClassName]
      /* We ignore paths that originate in a module that imports this class
       * directly: They are irrelevant for the final ID.
       *
       * However, they are important to ensure we do not attempt to import a
       * public module (see the comment in moduleID); therefore, we only filter
       * them here.
       */
      for ((h, t) <- dynamic if !direct.contains(h))
        t.ends(builder)
      builder.result()
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
