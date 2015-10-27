/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.classpath

import scala.collection.immutable.{Seq, Traversable}
import scala.collection.mutable

import org.scalajs.core.tools.jsdep._
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.corelib.CoreJSLibs

import JSLibResolveException.Problem

/** A partial Scala.js classpath is a collection of:
 *  - Scala.js binary files *.sjsir
 *  - Native JavaScript libraries
 *  - Description of dependencies on other JavaScript libraries
 *
 *  PartialClasspaths can be combined (using [[merge]]) and eventually resolved
 *  to an [[IRClasspath]].
 */
final class PartialClasspath(
    /** Description of JS libraries the content of this classpath depends on */
    val dependencies: Traversable[JSDependencyManifest],
    /** JS libraries this partial classpath provides */
    val availableLibs: Map[String, VirtualJSFile],
    /** Scala.js IR contained in this PartialClasspath (unordered) */
    val scalaJSIR: Traversable[VirtualScalaJSIRFile],
    val version: Option[String]
) {
  import PartialClasspath.{DependencyFilter, ManifestFilter}

  /** Merges another [[PartialClasspath]] with this one. This means:
   *  - Concatenate/merge dependencies
   *  - Merge availableLibs (libs in that shadow libs in this)
   *  - Merge Scala.js IR
   */
  def merge(that: PartialClasspath): PartialClasspath = {
    new PartialClasspath(
        this.dependencies  ++ that.dependencies,
        this.availableLibs ++ that.availableLibs,
        this.scalaJSIR     ++ that.scalaJSIR,
        CacheUtils.joinVersions(this.version, that.version))
  }

  /** Construct a [[IRClasspath]] out of this [[PartialClasspath]] by
   *  resolving library dependencies (and failing if they are not met)
   */
  @deprecated("Use the version with manifestFilter instead", "0.6.3")
  def resolve(filter: DependencyFilter): IRClasspath = resolve(filter, identity)

  /** Construct a [[IRClasspath]] out of this [[PartialClasspath]] by
   *  resolving library dependencies (and failing if they are not met)
   */
  def resolve(dependencyFilter: DependencyFilter = identity,
      manifestFilter: ManifestFilter = identity): IRClasspath = {
    val jsLibs = resolveDependencies(dependencyFilter, manifestFilter)
    new IRClasspath(jsLibs, mergeCompliance(), scalaJSIR,
        dependencies.exists(_.requiresDOM), version)
  }

  /** Constructs an ordered list of JS libraries to include. Fails if:
   *  - Resource names do not identify a unique resource on the classpath
   *  - Dependencies have cycles
   *  - Not all dependencies are available
   */
  private def resolveDependencies(
      dependencyFilter: DependencyFilter,
      manifestFilter: ManifestFilter): List[ResolvedJSDependency] = {

    val filteredManifests = manifestFilter(dependencies)
    val resourceNames = collectAllResourceNames(filteredManifests)
    val resolvedJSLibs = resolveAllResourceNames(resourceNames)

    val allFlatDeps = for {
      manifest <- filteredManifests
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
    val includeList = JSDependencyManifest.createIncludeList(flatDeps)

    for (info <- includeList)
      yield new ResolvedJSDependency(availableLibs(info.relPath),
          info.relPathMinified.map(availableLibs), info)
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
      allResourceNames: Map[String, List[Origin]]): Map[String, String] = {
    val problems = mutable.ListBuffer.empty[Problem]
    val resolvedLibs = Map.newBuilder[String, String]

    for ((resourceName, origins) <- allResourceNames) {
      resolveResourceName(resourceName, origins).fold[Unit](
          problems += _,
          resolvedLibs += resourceName -> _)
    }

    if (problems.nonEmpty)
      throw new JSLibResolveException(problems.toList)
    else
      resolvedLibs.result()
  }

  /** Resolves one resource name wrt to the current classpath. */
  private def resolveResourceName(resourceName: String,
      origins: List[Origin]): Either[Problem, String] = {
    val candidates = (availableLibs.keys collect {
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

  protected def mergeCompliance(): Traversable[ComplianceRequirement] = {
    val flatTups = for {
      dependency <- dependencies
      semantics  <- dependency.compliantSemantics
    } yield (semantics, dependency.origin)

    for {
      (semantics, tups) <- flatTups.groupBy(_._1)
    } yield new ComplianceRequirement(semantics, tups.map(_._2).toList)
  }

}

object PartialClasspath {

  type DependencyFilter =
    Traversable[FlatJSDependency] => Traversable[FlatJSDependency]

  type ManifestFilter = ManifestFilters.ManifestFilter

  /** Creates an empty PartialClasspath */
  def empty: PartialClasspath =
    new PartialClasspath(Nil, Map.empty, Nil, Some(""))
}
