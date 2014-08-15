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

  final implicit def toAutoGroupID(groupID: String): AutoGroupID = {
    nonEmpty(groupID, "Group ID")
    new AutoGroupID(groupID)
  }

  /** Lift the % operator on ModuleID in the [[Def.Initialize]] domain.
   *  Allows for the `% "test"` in
   *  {{{
   *  "org.example" %?% "test-lib" % "0.1" % "test"
   *  }}}
   */
  final implicit class InitModuleIDConfigurable(mod: Def.Initialize[ModuleID]) {
    def %(configuration: Configuration): Def.Initialize[ModuleID] =
      mod(_ % configuration)
    def %(configurations: String): Def.Initialize[ModuleID] =
      mod(_ % configurations)
  }

  /** Automatically joins a `Seq[Def.Initialize[ModuleID]] into a
   *  `Def.Initialize[Seq[ModuleID]]`
   *
   *  Allows to write:
   *  {{{
   *  libraryDependencies <++= Seq(
   *    "org.example" %?% "blah-lib" % "0.2",
   *    "org.example" %?% "test-lib" % "0.1" % "test"
   *  )
   *  }}}
   */
  final implicit def joinModInitSeq(
      mods: Seq[Def.Initialize[ModuleID]]): Def.Initialize[Seq[ModuleID]] =
    Def.Initialize.join(mods)

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

final class AutoGroupID private[sbtplugin] (groupID: String) {
  def %?%(artifactID: String): AutoGroupArtifactID = {
    nonEmpty(artifactID, "Artifact ID")
    new AutoGroupArtifactID(groupID, artifactID)
  }
}

final class AutoGroupArtifactID private[sbtplugin] (groupID: String,
    artifactID: String) {
  def %(revision: String): Def.Initialize[ModuleID] = {
    nonEmpty(revision, "Revision")

    val moduleId = ModuleID(groupID, artifactID, revision)

    // If the jsDependencies key is set (to something), we are in a Scala.js
    // project. Otherwise we are in a JVM project.
    ScalaJSPlugin.ScalaJSKeys.jsDependencies.? { opt =>
      if (opt.isDefined)
        moduleId.cross(ScalaJSCrossVersion.binary)
      else
        moduleId.cross(CrossVersion.binary)
    }
  }
}
