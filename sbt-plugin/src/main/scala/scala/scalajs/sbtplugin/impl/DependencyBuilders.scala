/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin
package impl

import scala.language.implicitConversions

import sbt._

import StringUtilities.nonEmpty

trait DependencyBuilders {
  final implicit def toScalaJSGroupID(groupID: String): ScalaJSGroupID = {
    nonEmpty(groupID, "Group ID")
    new ScalaJSGroupID(groupID)
  }

  /** Builder to allow for stuff like:
   *
   *    ProvidedJS / "foo.js"
   *    ProvidedJS / "foo.js" % "test"
   *
   */
  object ProvidedJS {
    def /(name: String): ProvidedJSModuleID = ProvidedJSModuleID(name, None)
  }

  /** Builder to allow for stuff like:
   *
   *    "org.webjars" % "jquery" % "1.10.2" / "jquery.js"
   *    "org.webjars" % "jquery" % "1.10.2" / "jquery.js" % "test"
   *
   */
  implicit class JSModuleIDBuilder(module: ModuleID) {
    def /(name: String): JarJSModuleID = JarJSModuleID(module, name)
  }
}

final class ScalaJSGroupID private[sbtplugin] (groupID: String) {
  def %%%(artifactID: String): ScalaJSGroupArtifactID = {
    nonEmpty(artifactID, "Artifact ID")
    new ScalaJSGroupArtifactID(groupID, artifactID, ScalaJSCrossVersion.binary)
  }
}

final class ScalaJSGroupArtifactID private[sbtplugin] (groupID: String,
    artifactID: String, crossVersion: CrossVersion) {
  def %(revision: String): ModuleID = {
    nonEmpty(revision, "Revision")
    ModuleID(groupID, artifactID, revision).cross(crossVersion)
  }
}
