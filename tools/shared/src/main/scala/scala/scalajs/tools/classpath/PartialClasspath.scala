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

/** A partial Scala.js classpath is a collection of:
 *  - Scala.js binary files *.sjsir
 *  - Native JavaScript libraries
 *  - Description of dependencies on other JavaScript libraries
 *
 *  PartialClasspaths can be combined (using [[merge]]) and eventually resolved
 *  to a [[CompleteIRClasspath]]
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
  import PartialClasspath.DependencyFilter

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
  def resolve(filter: DependencyFilter = identity): IRClasspath = {
    new IRClasspath(resolveDependencies(filter), scalaJSIR,
        dependencies.exists(_.requiresDOM), version)
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

  /** Creates an empty PartialClasspath */
  def empty: PartialClasspath =
    new PartialClasspath(Nil, Map.empty, Nil, Some(""))
}
