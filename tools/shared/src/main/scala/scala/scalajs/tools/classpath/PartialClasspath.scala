/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.classpath

import scala.collection.immutable.{Seq, Traversable}

import scala.scalajs.tools.jsdep._
import scala.scalajs.tools.io._
import scala.scalajs.tools.corelib.CoreJSLibs

/** A classpath to which other elements still may be added. */
abstract class PartialClasspath(
    /** Description of JS libraries the content of this classpath depends on */
    val dependencies: Traversable[JSDependencyManifest],
    /** JS libraries this partial classpath provides */
    val availableLibs: Map[String, VirtualJSFile],
    val version: Option[String]
) {
  import PartialClasspath.DependencyFilter

  /** Ordered Scala.js code in this PartialClasspath (packaged or IR) */
  def scalaJSCode: Seq[VirtualJSFile]

  /** Appends another PartialClasspath to this one. This means:
   *  - Concatenate/merge dependencies
   *  - Merge availableLibs (libs in that shadow libs in this)
   *  - Concatenate scalaJSCode (maintaining order)
   *
   *  Also have a look at [[PartialIRClasspath#merge]]
   */
  def append(that: PartialClasspath): PartialClasspath = {
    PartialClasspath(
        this.dependencies  ++ that.dependencies,
        this.availableLibs ++ that.availableLibs,
        this.scalaJSCode   ++ that.scalaJSCode,
        CacheUtils.joinVersions(this.version, that.version))
  }

  /** Construct a CompleteClasspath out of this PartialClasspath by:
   *  - Resolving library dependencies (and failing if they are not met)
   *  - Adding the CoreJSLib
   *
   *  PartialIRClasspath overrides this to return a CompleteIRClasspath
   */
  def resolve(filter: DependencyFilter = identity): CompleteCIClasspath = {
    CompleteCIClasspath(resolveDependencies(filter),
        CoreJSLibs.libs ++ scalaJSCode, dependencies.exists(_.requiresDOM),
        version)
  }

  /** Constructs an ordered list of JS libraries to include. Fails if:
   *  - Dependencies have cycles
   *  - Not all dependencies are available
   */
  protected def resolveDependencies(
      filter: DependencyFilter): List[(VirtualJSFile, ResolutionInfo)] = {
    val flatDeps = filter(dependencies.flatMap(_.flatten))
    val includeList = JSDependencyManifest.createIncludeList(flatDeps)

    val missingDeps = includeList.filterNot { info =>
      availableLibs.contains(info.resourceName)
    }

    if (missingDeps.nonEmpty)
      throw new MissingJSLibException(missingDeps)

    for (info <- includeList)
      yield (availableLibs(info.resourceName), info)
  }

}

object PartialClasspath {

  type DependencyFilter =
    Traversable[FlatJSDependency] => Traversable[FlatJSDependency]

  private class SimplePartialClasspath(
      dependencies: Traversable[JSDependencyManifest],
      availableLibs: Map[String, VirtualJSFile],
      val scalaJSCode: Seq[VirtualJSFile],
      version: Option[String]
  ) extends PartialClasspath(dependencies, availableLibs, version)

  /** Creates a PartialClasspath with the specified contents. */
  def apply(deps: Traversable[JSDependencyManifest],
      availableLibs: Map[String, VirtualJSFile],
      scalaJSCode: Seq[VirtualJSFile],
      version: Option[String]): PartialClasspath =
    new SimplePartialClasspath(deps, availableLibs, scalaJSCode, version)

  /** Creates an empty PartialClasspath */
  def empty: PartialClasspath =
    new SimplePartialClasspath(Nil, Map.empty, Nil, Some(""))
}
