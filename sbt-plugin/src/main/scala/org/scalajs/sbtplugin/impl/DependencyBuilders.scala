/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.sbtplugin
package impl

import scala.language.implicitConversions
import scala.language.experimental.macros

import sbt._

trait DependencyBuilders {
  @deprecated(
      "Use org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport." +
      "toPlatformDepsGroupID",
      "0.6.23")
  final def toScalaJSGroupID(groupID: String): ScalaJSGroupID = {
    require(groupID.trim.nonEmpty, "Group ID cannot be empty.")
    new ScalaJSGroupID(groupID)
  }

  /**
   *  Dummy builder to allow declaractions like:
   *
   *  {{{
   *  RuntimeDOM % "test"
   *  }}}
   */
  @deprecated(
      "Requesting a DOM-enabled JS env with `jsDependencies += RuntimeDOM` " +
      "or `requiresDOM := true` will not be supported in Scala.js 1.x. " +
      "Instead, explicitly select a suitable JS with `jsEnv`, e.g., " +
      "`jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv`.",
      "0.6.20")
  val RuntimeDOM = org.scalajs.sbtplugin.RuntimeDOMDep(None)

  /**
   *  Builder to allow declarations like:
   *
   *  {{{
   *  ProvidedJS / "foo.js"
   *  ProvidedJS / "foo.js" % "test"
   *  }}}
   */
  object ProvidedJS {
    def /(name: String): ProvidedJSModuleID = ProvidedJSModuleID(name, None)
  }

  /**
   *  Builder to allow declarations like:
   *
   *  {{{
   *  "org.webjars" % "jquery" % "1.10.2" / "jquery.js"
   *  "org.webjars" % "jquery" % "1.10.2" / "jquery.js" % "test"
   *  }}}
   */
  implicit class JSModuleIDBuilder(module: ModuleID) {
    def /(name: String): JarJSModuleID = JarJSModuleID(module, name)
  }
}

final class ScalaJSGroupIDForce private[sbtplugin] (private val groupID: String) {
  def %%%!(artifactID: String): CrossGroupArtifactID = {
    require(artifactID.trim.nonEmpty, "Artifact ID cannot be empty.")
    new CrossGroupArtifactID(groupID, artifactID, ScalaJSCrossVersion.binary)
  }
}

final class ScalaJSGroupID private[sbtplugin] (private val groupID: String) {
  def %%%(artifactID: String): CrossGroupArtifactID =
    macro ScalaJSGroupID.auto_impl

  def %%%!(artifactID: String): CrossGroupArtifactID =
    ScalaJSGroupID.withCross(this, artifactID, ScalaJSCrossVersion.binary)
}

object ScalaJSGroupID {
  import scala.reflect.macros.Context

  /** Internal. Used by the macro implementing [[ScalaJSGroupID.%%%]]. Use:
   *  {{{
   *  ("a" % artifactID % revision).cross(cross)
   *  }}}
   *  instead.
   */
  def withCross(groupID: ScalaJSGroupID, artifactID: String,
      cross: CrossVersion): CrossGroupArtifactID = {
    require(artifactID.trim.nonEmpty, "Artifact ID cannot be empty.")
    new CrossGroupArtifactID(groupID.groupID, artifactID, cross)
  }

  def auto_impl(c: Context { type PrefixType = ScalaJSGroupID })(
      artifactID: c.Expr[String]): c.Expr[CrossGroupArtifactID] = {
    import c.universe._

    // Hack to work around bug in sbt macros (wrong way of collecting local
    // definitions)
    val keysSym = rootMirror.staticModule(
        "_root_.org.scalajs.sbtplugin.ScalaJSPlugin.AutoImport")
    val keys = c.Expr[ScalaJSPlugin.AutoImport.type](Ident(keysSym))

    reify {
      val cross = {
        if (keys.splice.isScalaJSProject.value)
          ScalaJSCrossVersion.binary
        else
          CrossVersion.binary
      }
      ScalaJSGroupID.withCross(c.prefix.splice, artifactID.splice, cross)
    }
  }

}

final class CrossGroupArtifactID(groupID: String,
    artifactID: String, crossVersion: CrossVersion) {
  def %(revision: String): ModuleID = {
    require(revision.trim.nonEmpty, "Revision cannot be empty.")
    ModuleID(groupID, artifactID, revision).cross(crossVersion)
  }
}
