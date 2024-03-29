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
import java.nio.ByteBuffer

import org.scalajs.ir.Names.ClassName
import org.scalajs.ir.SHA1
import org.scalajs.linker.standard.ModuleSet.ModuleID

import InternalModuleIDGenerator.ForDigests

/** Tagger groups classes into coarse modules.
 *
 *  It is the primary mechanism for the FewestModulesAnalyzer but also used
 *  by the SmallModulesForAnalyzer.
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
 *
 *  == Class Exclusion ==
 *  Classes can be excluded from the modules generated by Tagger.
 *
 *  For excluded classes, the Tagger assumes that they are in a module
 *  provided by a different part of the overall analysis.
 *
 *  The (transitive) dependencies of the class are nevertheless taken into
 *  account and tagged as appropriate.
 *  In particular, to avoid cycles and excessive splitting alike (see #4835),
 *  we need to introduce an additonal tagging mechanism.
 *
 *  To illustrate the problem, take the following dependency graph as an example
 *
 *    a -> b -> c
 *
 *  where B is excluded. Naively, we would want to group a and c together into a'.
 *  However, this would lead to a circular dependency between a' and c.
 *
 *  Nevertheless, in the absence of b, or if b is not an excluded class, we'd
 *  want to perform the grouping to avoid unnecessary splitting.
 *
 *  We achieve this by tracking an additional tag, representing the maximum
 *  number of hops from an excluded class (aka fine) to a non-excluded class
 *  (aka coarse) class for any path from an entrypoint to the given class.
 *
 *  We then only permit grouping coarse classes with the same tag. This avoids
 *  the creation of cycles.
 *
 *  The following is a proof that this strategy avoids cycles.
 *
 *  Given
 *
 *  G = (V, E), acyclic, V = F ∪ C, F ∩ C = ∅
 *    the original dependency graph,
 *    F: set of fine classes,
 *    C: set of coarse classes
 *
 *  t : V → ℕ (the maxExcludedHopCount tag)
 *    ∀ (v1, v2) ∈ E : t(v1) ≤ t(v2)
 *    ∀ (f, c) ∈ E : f ∈ F, c ∈ C ⇒ t(f) < t(c)
 *
 *  Define
 *
 *  G' = (V', E'), V' = F ∪ C' (the new grouped graph)
 *
 *  C' = { n ∈ ℕ | ∃ c ∈ C : t(c) = n }
 *
 *  E' = { (f1, f2) ∈ E | f1, f2 ∈ F } ∪
 *       { (f, n) | f ∈ F, ∃ c ∈ C : (f, c) ∈ E : t(c) = n } ∪
 *       { (n, f) | f ∈ F, ∃ c ∈ C : (c, f) ∈ E : t(c) = n } ∪
 *       { (n, m) | n ≠ m, ∃ c1, c2 ∈ C : (c1, c2) ∈ E : t(c1) = n, t(c2) = m }
 *
 *  t' : V' → ℕ:
 *
 *  t'(f) = t(f)  (if f ∈ F)
 *  t'(n) = n     (if n ∈ C')
 *
 *  Lemma 1 (unproven)
 *
 *  ∀ (v1, v2) ∈ E' : t'(v1) ≤ t'(v2)
 *
 *  Lemma 2 (unproven)
 *
 *  ∀ (f, n) ∈ E' : f ∈ F, n ∈ C' : t'(f) < t'(n)
 *
 *  Lemma 3
 *
 *  ∀ (n, m) ∈ E' : n,m ∈ C' ⇒ t'(n) < t'(m)
 *
 *  Follows from Lemma 1 and (n, m) ∈ E' ⇒ n ≠ m (by definition).
 *
 *  Theorem
 *
 *  G' is acyclic
 *
 *  Proof by contradiction.
 *
 *  Assume ∃ p = x1, ..., xn (x1 = xn, n > 1, xi ∈ V)
 *
 *  ∃ xi ∈ C' by contradiction: ∀ xi ∈ F ⇒ p is a cycle in G
 *
 *  ∃ xi ∈ F by contradiction: ∀ xi ∈ C' ⇒
 *    t'(xi) increases strictly monotonically (by Lemma 3),
 *    but x1 = xn ⇒ t'(x1) = t'(xn)
 *
 *  Therefore,
 *
 *  ∃ (xi, xj) ∈ p : xi ∈ F, xj ∈ C'
 *
 *  Therefore (by Lemma 1)
 *
 *  t'(x1) ≤ ... ≤ t'(xi) < t'(xj) ≤ ... ≤ t'(xn) ⇒ t'(x1) < t'(xn)
 *
 *  But x1 = xn ⇒ t'(x1) = t'(xn), which is a contradiction.
 *
 *  Therefore, G' is acyclic.
 */
private class Tagger(infos: ModuleAnalyzer.DependencyInfo,
    excludedClasses: scala.collection.Set[ClassName] = Set.empty) {
  import Tagger._

  private[this] val allPaths = mutable.Map.empty[ClassName, Paths]

  final def tagAll(modulesToAvoid: Iterable[ModuleID]): scala.collection.Map[ClassName, ModuleID] = {
    val internalModIDGenerator = new InternalModuleIDGenerator.ForDigests(modulesToAvoid)
    tagEntryPoints()
    for {
      (className, paths) <- allPaths
      if !excludedClasses.contains(className)
    } yield {
      className -> paths.moduleID(internalModIDGenerator)
    }
  }

  private def tag(className: ClassName, pathRoot: ModuleID, pathSteps: List[ClassName],
      excludedHopCount: Int, fromExcluded: Boolean): Unit = {
    val isExcluded = excludedClasses.contains(className)

    val newExcludedHopCount =
      if (fromExcluded && !isExcluded) excludedHopCount + 1 // hop from fine to coarse
      else excludedHopCount

    val updated = allPaths
      .getOrElseUpdate(className, new Paths)
      .put(pathRoot, pathSteps, newExcludedHopCount)

    if (updated) {
      val classInfo = infos.classDependencies(className)
      classInfo
        .staticDependencies
        .foreach(staticEdge(_, pathRoot, pathSteps, newExcludedHopCount, fromExcluded = isExcluded))

      classInfo
        .dynamicDependencies
        .foreach(dynamicEdge(_, pathRoot, pathSteps, newExcludedHopCount, fromExcluded = isExcluded))
    }
  }

  private def staticEdge(className: ClassName, pathRoot: ModuleID, pathSteps: List[ClassName],
      excludedHopCount: Int, fromExcluded: Boolean): Unit = {
    tag(className, pathRoot, pathSteps, excludedHopCount, fromExcluded)
  }

  private def dynamicEdge(className: ClassName, pathRoot: ModuleID, pathSteps: List[ClassName],
      excludedHopCount: Int, fromExcluded: Boolean): Unit = {
    tag(className, pathRoot, pathSteps :+ className, excludedHopCount, fromExcluded)
  }

  private def tagEntryPoints(): Unit = {
    for {
      (moduleID, deps) <- infos.publicModuleDependencies
      className <- deps
    } {
      staticEdge(className, pathRoot = moduleID, pathSteps = Nil,
          excludedHopCount = 0, fromExcluded = false)
    }
  }
}

private object Tagger {

  /** "Interesting" paths that can lead to a given class.
   *
   *  "Interesting" in this context means:
   *  - All direct paths from a public dependency.
   *  - All non-empty, mutually prefix-free paths of dynamic import hops.
   */
  private final class Paths {
    private var maxExcludedHopCount = 0
    private val direct = mutable.Set.empty[ModuleID]
    private val dynamic = mutable.Map.empty[ModuleID, DynamicPaths]

    def put(pathRoot: ModuleID, pathSteps: List[ClassName], excludedHopCount: Int): Boolean = {
      val hopCountsChanged = excludedHopCount > maxExcludedHopCount

      if (hopCountsChanged)
        maxExcludedHopCount = excludedHopCount

      val stepsChanged = if (pathSteps.isEmpty) {
        direct.add(pathRoot)
      } else {
        dynamic
          .getOrElseUpdate(pathRoot, new DynamicPaths)
          .put(pathSteps)
      }
      hopCountsChanged || stepsChanged
    }

    def moduleID(internalModIDGenerator: ForDigests): ModuleID = {
      if (direct.size == 1 && dynamic.isEmpty && maxExcludedHopCount == 0) {
        /* Class is only used by a single public module. Put it there.
         *
         * Note that we must not do this if there are any dynamic or excluded
         * modules requiring this class. Otherwise, the dynamically loaded module
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

        // Excluded hop counts (exclude 0 for fast path in FewestModules mode)
        if (maxExcludedHopCount > 0)
          digestBuilder.update(intToBytes(maxExcludedHopCount))

        // Public modules using this.
        for (id <- direct.toList.sortBy(_.id))
          digestBuilder.update(id.id.getBytes(StandardCharsets.UTF_8))

        // Dynamic modules using this.
        for (className <- dynamicEnds)
          digestBuilder.updateUTF8String(className.encoded)

        internalModIDGenerator.forDigest(digestBuilder.finalizeDigest())
      }
    }

    private def intToBytes(x: Int): Array[Byte] = {
      val result = new Array[Byte](4)
      val buf = ByteBuffer.wrap(result)
      buf.putInt(x)
      result
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
