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
import org.scalajs.ir.SHA1
import org.scalajs.linker.frontend.modulesplitter.InternalModuleIDGenerator.ForDigests
import org.scalajs.linker.standard.ModuleSet.ModuleID

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import scala.annotation.tailrec
import scala.collection.{immutable, mutable}

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
 *  we need to introduce an additional tagging mechanism.
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
    } yield {
      className -> paths.moduleID(internalModIDGenerator)
    }
  }

  /**
   * Tags all of the given classes with the given path iff it is shorter than the existing path.
   *
   * This will recursively tag all static dependencies (which by definition do not contribute to the path).
   *
   * A class that only has a direct path may be re-tagged with a dynamic path. In this case, all its dependencies are
   * also re-tagged where appropriate.
   *
   * @param classNames the starting set of classes to tag
   * @param pathRoot the root of the path
   * @param pathSteps the steps that make up path
   * @param nextSteps the accumulated result
   * @return the next steps to traverse
   */
  @tailrec
  private def tag(classNames: Set[ClassName], pathRoot: ModuleID, pathSteps: List[ClassName],
                  nextSteps: Set[ClassName]): Set[ClassName] = {
    classNames.headOption match {
      case Some(className) => allPaths.get(className) match {
        case Some(paths) if !paths.hasDynamic && pathSteps.nonEmpty =>
          // Special case that visits static dependencies again when the first dynamic dependency is found so as to
          // ensure that they are not thought to only be used by a single public module.
          paths.put(pathRoot, pathSteps)
          val classInfo = infos.classDependencies(className)
          tag(classNames.tail ++ classInfo.staticDependencies, pathRoot, pathSteps, nextSteps)
        case None =>
          val paths = new Paths(trackExcludedHopCounts = excludedClasses.nonEmpty)
          paths.put(pathRoot, pathSteps)
          allPaths.put(className, paths)
          // Consider dependencies the first time we encounter them as this is the shortest path there will be.
          val classInfo = infos.classDependencies(className)
          tag(classNames.tail ++ classInfo.staticDependencies, pathRoot, pathSteps,
            nextSteps ++ classInfo.dynamicDependencies)
        case Some(paths) =>
          paths.put(pathRoot, pathSteps)
          // Otherwise do not consider dependencies again as there is no more information to find.
          tag(classNames.tail, pathRoot, pathSteps, nextSteps)
      }
      case None => nextSteps
    }
  }

  /**
   * Tags each step relative to the current path and tags them.
   *
   * Once all of the given steps (and their static dependencies) have been tagged it repeats on the next steps until all
   * dependencies have been tagged.
   *
   * @param classNames the steps to tag and traverse
   * @param pathRoot the root of the path
   * @param pathSteps the steps that make up path
   * @param acc the accumulator
   */
  @tailrec
  private def tagNextSteps(classNames: Set[ClassName], pathRoot: ModuleID, pathSteps: List[ClassName],
                           acc: List[(List[ClassName], Set[ClassName])]): Unit = {
    classNames.headOption match {
      case Some(className) =>
        val nextPathSteps = pathSteps :+ className
        val nextSteps = tag(Set(className), pathRoot, nextPathSteps, Set.empty)
        val nextAcc = nextPathSteps -> nextSteps :: acc
        tagNextSteps(classNames.tail, pathRoot, pathSteps, nextAcc)
      case None => acc match {
        case (pathSteps, classNames) :: nextAcc =>
          tagNextSteps(classNames, pathRoot, pathSteps, nextAcc)
        case Nil => ()
      }
    }
  }

  /**
   * Performs a full traversal of the dependencies to re-tag the paths with the maximum excluded hop count.
   *
   * This will traverse dependencies repeatedly if a prefix is found with a larger excluded hop count.
   *
   * @param className the starting class to tag
   * @param excludedHopCount the excluded hop count so far
   * @param fromExcluded whether the previous step was excluded
   */
  private def updateExcludedHopCounts(className: ClassName, excludedHopCount: Int, fromExcluded: Boolean): Unit = {
    val isExcluded = excludedClasses.contains(className)

    val newExcludedHopCount =
      if (fromExcluded && !isExcluded) excludedHopCount + 1 // hop from fine to coarse
      else excludedHopCount

    val updated = allPaths(className).updateExcludedHopCount(excludedHopCount)

    if (updated) {
      val classInfo = infos.classDependencies(className)
      classInfo
        .staticDependencies
        .foreach(updateExcludedHopCounts(_, newExcludedHopCount, fromExcluded = isExcluded))
      classInfo
        .dynamicDependencies
        .foreach(updateExcludedHopCounts(_, newExcludedHopCount, fromExcluded = isExcluded))
    }
  }

  private def tagEntryPoints(): Unit = {
    infos.publicModuleDependencies.foreach {
      case (moduleID, deps) =>
        // We need to be careful with memory usage here. There is a contention between finding the shortest path and
        // finding the maximum excluded hop count. For the former it is best to do a breadth first traversal but for the
        // later we do a depth first traversal.
        val nextSteps = tag(classNames = deps, pathRoot = moduleID, pathSteps = Nil, nextSteps = Set.empty)
        tagNextSteps(classNames = nextSteps, pathRoot = moduleID, pathSteps = Nil, acc = Nil)
        // Only needed when there are excluded classes.
        if (excludedClasses.nonEmpty) {
          deps.foreach(updateExcludedHopCounts(_, excludedHopCount = 0, fromExcluded = false))
        }
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
  private final class Paths(trackExcludedHopCounts: Boolean) {
    // Start at -1 so that when we re-tag we consider the first time it is set to 0 as an update.
    private var maxExcludedHopCount = if (trackExcludedHopCounts) -1 else 0
    private val direct = mutable.Set.empty[ModuleID]
    private val dynamic = mutable.Map.empty[ModuleID, DynamicPaths]

    def hasDynamic: Boolean = dynamic.nonEmpty

    def put(pathRoot: ModuleID, pathSteps: List[ClassName]): Unit = {
      if (pathSteps.isEmpty) {
        direct.add(pathRoot)
      } else {
        dynamic
          .getOrElseUpdate(pathRoot, new DynamicPaths)
          .put(pathSteps)
      }
    }

    def updateExcludedHopCount(excludedHopCount: Int): Boolean = {
      val hopCountsChanged = excludedHopCount > maxExcludedHopCount
      if (hopCountsChanged)
        maxExcludedHopCount = excludedHopCount
      hopCountsChanged
    }

    def moduleID(internalModIDGenerator: ForDigests): ModuleID = {
      assert(maxExcludedHopCount >= 0, "Maximum excluded hop count has not been calculated")
      if (direct.size == 1 && dynamic.isEmpty && maxExcludedHopCount <= 0) {
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
