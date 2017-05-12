package org.scalajs.jsdependencies.core

import scala.collection.mutable

import org.scalajs.core.tools.io.VirtualJSFile
import JSLibResolveException.Problem

object DependencyResolver {

  type DependencyFilter =
    Traversable[FlatJSDependency] => Traversable[FlatJSDependency]

  /** Constructs an ordered list of JS libraries to include. Fails if:
   *  - Resource names do not identify a unique resource on the classpath
   *  - Dependencies have cycles
   *  - Not all dependencies are available
   */
  def resolveDependencies(
      manifests: Traversable[JSDependencyManifest],
      availableLibs: Map[String, VirtualJSFile],
      dependencyFilter: DependencyFilter): List[ResolvedJSDependency] = {

    val resourceNames = collectAllResourceNames(manifests)
    val resolvedJSLibs =
      resolveAllResourceNames(resourceNames, availableLibs.keys)

    val allFlatDeps = for {
      manifest <- manifests
      dep <- manifest.libDeps
    } yield {
      new FlatJSDependency(
          manifest.origin,
          resolvedJSLibs(dep.resourceName),
          dep.dependencies.map(resolvedJSLibs),
          dep.commonJSName,
          dep.minifiedResourceName.map(resolvedJSLibs))
    }

    val flatDeps = dependencyFilter(allFlatDeps)
    val includeList = createIncludeList(flatDeps)

    for (info <- includeList) yield {
      new ResolvedJSDependency(availableLibs(info.relPath),
          info.relPathMinified.map(availableLibs), info)
    }
  }

  /** Collects all the resource names mentioned in the manifests.
   *  @param manifests to collect from
   *  @return Map from resource name to the list of origins mentioning them
   */
  private def collectAllResourceNames(
      manifests: Traversable[JSDependencyManifest]): Map[String, List[Origin]] = {

    def allResources(dep: JSDependency) =
      dep.resourceName :: dep.dependencies ::: dep.minifiedResourceName.toList

    val nameOriginPairs = for {
      manifest <- manifests.toList
      dep <- manifest.libDeps
      resourceName <- allResources(dep)
    } yield (resourceName, manifest.origin)

    nameOriginPairs.groupBy(_._1).mapValues(_.map(_._2))
  }

  /** Resolves all the resource names wrt to the current classpath.
   *  @throws JSLibResolveException if any of the resource names cannot be resolved
   *  @return Map from resource name to relative path
   */
  private def resolveAllResourceNames(
      allResourceNames: Map[String, List[Origin]],
      relPaths: Traversable[String]): Map[String, String] = {
    val problems = mutable.ListBuffer.empty[Problem]
    val resolvedLibs = Map.newBuilder[String, String]

    for ((resourceName, origins) <- allResourceNames) {
      resolveResourceName(resourceName, origins, relPaths).fold[Unit](
          problems += _,
          resolvedLibs += resourceName -> _)
    }

    if (problems.nonEmpty)
      throw new JSLibResolveException(problems.toList)
    else
      resolvedLibs.result()
  }

  /** Resolves one resource name wrt to the current classpath. */
  private def resolveResourceName(resourceName: String, origins: List[Origin],
      relPaths: Traversable[String]): Either[Problem, String] = {
    val candidates = (relPaths collect {
      case relPath if ("/" + relPath).endsWith("/" + resourceName) =>
        relPath
    }).toList

    candidates match {
      case relPath :: Nil =>
        Right(relPath)
      case _ =>
        Left(new Problem(resourceName, candidates, origins))
    }
  }

  /** Create a sorted include list for js libs */
  private def createIncludeList(
      flatDeps: Traversable[FlatJSDependency]): List[ResolutionInfo] = {
    val jsDeps = mergeManifests(flatDeps)

    // Verify all dependencies are met
    for {
      lib <- flatDeps
      dep <- lib.dependencies
      if !jsDeps.contains(dep)
    } throw new MissingDependencyException(lib, dep)

    // Sort according to dependencies and return

    // Very simple O(n²) topological sort for elements assumed to be distinct
    // Copied :( from GenJSExports (but different exception)
    @scala.annotation.tailrec
    def loop(coll: List[ResolutionInfo],
      acc: List[ResolutionInfo]): List[ResolutionInfo] = {

      if (coll.isEmpty) acc
      else if (coll.tail.isEmpty) coll.head :: acc
      else {
        val (selected, pending) = coll.partition { x =>
          coll forall { y => (x eq y) || !y.dependencies.contains(x.relPath) }
        }

        if (selected.nonEmpty)
          loop(pending, selected ::: acc)
        else
          throw new CyclicDependencyException(pending)
      }
    }

    loop(jsDeps.values.toList, Nil)
  }

  /** Merges multiple JSDependencyManifests into a map of map:
   *  resourceName -> ResolutionInfo
   */
  private def mergeManifests(flatDeps: Traversable[FlatJSDependency]) = {
    checkCommonJSNameConflicts(flatDeps)

    val byRelPath = flatDeps.groupBy(_.relPath)

    checkMinifiedJSConflicts(byRelPath)

    byRelPath.mapValues { sameName =>
      new ResolutionInfo(
        relPath = sameName.head.relPath,
        dependencies = sameName.flatMap(_.dependencies).toSet,
        origins = sameName.map(_.origin).toList,
        commonJSName = sameName.flatMap(_.commonJSName).headOption,
        relPathMinified = sameName.flatMap(_.relPathMinified).headOption
      )
    }
  }

  private def checkCommonJSNameConflicts(flatDeps: Traversable[FlatJSDependency]) = {
    @inline
    def hasConflict(x: FlatJSDependency, y: FlatJSDependency) = (
        x.commonJSName.isDefined &&
        y.commonJSName.isDefined &&
        (x.relPath == y.relPath ^ x.commonJSName == y.commonJSName)
    )

    val conflicts = for {
      dep <- flatDeps
      if flatDeps.exists(hasConflict(dep, _))
    } yield dep

    if (conflicts.nonEmpty)
      throw new ConflictingNameException(conflicts.toList)
  }

  private def checkMinifiedJSConflicts(
      byRelPath: Map[String, Traversable[FlatJSDependency]]) = {

    @inline
    def hasConflict(x: FlatJSDependency, y: FlatJSDependency) = (
        x.relPathMinified.isDefined &&
        y.relPathMinified.isDefined &&
        x.relPathMinified != y.relPathMinified
    )

    val conflicts = for {
      (_, deps) <- byRelPath
      x <- deps if deps.exists(y => hasConflict(x, y))
    } yield x

    if (conflicts.nonEmpty)
      throw new ConflictingMinifiedJSException(conflicts.toList)
  }

}
