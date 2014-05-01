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
