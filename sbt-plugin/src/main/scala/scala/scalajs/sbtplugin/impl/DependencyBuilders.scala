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

import StringUtilities.nonEmpty

trait DependencyBuilders {
  final implicit def toScalaJSGroupID(groupID: String): ScalaJSGroupID = {
    nonEmpty(groupID, "Group ID")
    new ScalaJSGroupID(groupID)
  }

  /**
   *  Dummy builder to allow declaractions like:
   *
   *  {{{
   *  RuntimeDOM % "test"
   *  }}}
   */
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
    nonEmpty(artifactID, "Artifact ID")
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
    nonEmpty(revision, "Revision")
    ModuleID(groupID, artifactID, revision).cross(crossVersion)
  }
}
