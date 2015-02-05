/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.sbtplugin.cross

import org.scalajs.sbtplugin.ScalaJSPlugin

import scala.language.implicitConversions
import scala.language.experimental.macros

import scala.reflect.macros.Context

import sbt._
import Keys._
import Project.projectToRef

import java.io.File

/** A convenience structure that creates a JVM and a Scala.js project under the
 *  hood and forwards common operations to it.
 *
 *  <h2>Basic Usage</h2>
 *  In your `build.sbt`, use [[CrossProject]] as follows:
 *  {{{
 *  lazy val p1 = crossProject.
 *    settings(
 *      name := "test", // default name would be p1
 *      libraryDependencies += "org.example" %%% "test" % "0.1"
 *    ).
 *    jvmSettings(
 *      libraryDependencies += "org.example" %% "jvm-specific" % "0.1"
 *    ).
 *    jsSettings(
 *      libraryDependencies += "org.example" %%% "js-specific" % "0.1",
 *      jsDependencies += "org.example" %% "js-thing" % "0.1" / "foo.js"
 *    )
 *
 *  // Needed, so sbt finds the projects
 *  lazy val p1JVM = p1.jvm
 *  lazy val p1JS = p1.js
 *
 *  lazy val p2 = crossProject.crossType(CrossType.Pure).dependsOn(p1 % "test")
 *
 *  // Needed, so sbt finds the projects
 *  lazy val p2JVM = p2.jvm
 *  lazy val p2JS = p2.js
 *  }}}
 *
 *  <h2>CrossProject types</h2>
 *  There are three built-in types of [[CrossProject]]s. Each of them
 *  corresponds to a concrete subclass of [[CrossType]]:
 *
 *  <h3>Full CrossProject ([[CrossType.Full]])</h3>
 *  A CrossProject that has both shared and individual JVM/JS sources.
 *  This is the default.
 *
 *  The directory structure is as follows:
 *
 *  <pre>
 *  project/
 *    shared/
 *      src/
 *        main/
 *        test/
 *    jvm/
 *      src/
 *        main/
 *        test/
 *    js/
 *      src/
 *        main/
 *        test/
 *  </pre>
 *
 *  The shared source tree is included in both the JVM and the JS project.
 *
 *  <h3>Pure CrossProject ([[CrossType.Pure]])</h3>
 *  A CrossProject that does not have individual JVM/JS sources.
 *
 *  The directory structure is as follows:
 *
 *  <pre>
 *  project/
 *    src/
 *      main/
 *      test/
 *    .jvm/
 *    .js/
 *  </pre>
 *
 *  The source tree is included in both the JVM and the JS project. The hidden
 *  folders are the true project roots in sbt's terms.
 *
 *  <h3>Dummy CrossProject ([[CrossType.Dummy]])</h3>
 *  A CrossProject that does not have shared JVM/JS sources. It is useful, since
 *  it can still be used for dependency tracking and aggregation.
 *
 *  The directory structure is as follows:
 *
 *  <pre>
 *  project/
 *    jvm/
 *      src/
 *        main/
 *        test/
 *    js/
 *      src/
 *        main/
 *        test/
 *  </pre>
 *
 *  <h2>Eclipse Support</h2>
 *  Note that by default, the sbteclipse plugin uses sbt's project names to name
 *  the Eclipse projects it generates. Since the CrossProject generates two
 *  projects with the same name, this may result in a conflict when importing
 *  the projects into Eclipse.
 *
 *  You can configure sbteclipse to
 *  [https://github.com/typesafehub/sbteclipse/wiki/Using-sbteclipse#useprojectid
 *  use the project ID] instead (which is unique in sbt as well):
 *
 *  {{{
 *  EclipseKeys.useProjectId := true
 *  }}}
 *
 *  Alternatively, you can of course also just import one of the two projects
 *  into your Eclipse.
 *
 *  <h2>IntelliJ IDEA Support</h2>
 *  While CrossProject works out of the box with Eclipse and the sbt eclipse
 *  plugin, it does not with IntelliJ IDEA due to its missing support for shared
 *  source directories.
 *
 *  To fix this, you should add symlinks in the hierarchy to the shared source
 *  directory and include them in your imported IntelliJ IDEA project (but not
 *  in sbt). The recommended structure is as follows (for a Full CrossProject):
 *
 *  <pre>
 *  project/
 *    shared/
 *      src/
 *        main/
 *        test/
 *    jvm/
 *      src/
 *        main/
 *        test/
 *        idea-shared-main/ --> project/shared/src/main
 *        idea-shared-test/ --> project/shared/src/test
 *    js/
 *      src/
 *        main/
 *        test/
 *        idea-shared-main/ --> project/shared/src/main
 *        idea-shared-test/ --> project/shared/src/test
 *  </pre>
 *
 *  Note that we do not recommend to put the symlinks in version control, since
 *  they do not work on Windows (Git, for example, just ignores their existence
 *  when cloning).
 *
 *  <h2>Pitfalls to Avoid</h2>
 *
 *  <h3>Altering a contained Project outside the CrossProject</h3>
 *  Since sbt projects are immutable structures, it is important that you do not
 *  "mutate" (i.e. create a new Project) outside of the CrossProject.
 *
 *  <h4>DON'T</h4>
 *  {{{
 *  lazy val p1 = crossProject
 *
 *  lazy val p1JVM = p1.jvm
 *  lazy val p1JS = p1.js.settings(jsDependencies += RuntimeDOM)
 *
 *  // Now we have p1JS != p1.js... Dependency tracking will not work anymore.
 *  }}}
 *
 *  <h4>DO</h4>
 *  {{{
 *  lazy val p1 = crossProject.
 *    jsSettings(jsDependencies += RuntimeDOM)
 *
 *  lazy val p1JVM = p1.jvm
 *  lazy val p1JS = p1.js
 *  }}}
 *
 *  <h3>Manually setting the base of a contained Project</h3>
 *  CrossProject puts its contained projects in a given directory structure. If
 *  you try to work around that, things will fail (and non-existing directories
 *  will be referenced). If you want to put your projects in a different
 *  directory structure, you are encouraged to implement your own subclass of
 *  [[CrossType]].
 *
 *  <h4>DON'T</h4>
 *  {{{
 *  lazy val p1 = crossProject.jsConfigure(_.in(file("myJSDir")))
 *  }}}
 *
 *  <h4>DO</h4>
 *  Implement your own subclass (sub-object) of [[CrossType]].
 *
 */
final class CrossProject private (
    crossType: CrossType,
    val jvm: Project,
    val js: Project
) {

  import CrossProject._

  // Transformers for inner projects

  /** Transform the underlying JVM project */
  def jvmConfigure(transformer: Project => Project): CrossProject =
    copy(jvm = transformer(jvm))

  /** Transform the underlying JS project */
  def jsConfigure(transformer: Project => Project): CrossProject =
    copy(js = transformer(js))

  /** Add settings specific to the underlying JVM project */
  def jvmSettings(ss: Def.Setting[_]*): CrossProject =
    jvmConfigure(_.settings(ss: _*))

  /** Add settings specific to the underlying JS project */
  def jsSettings(ss: Def.Setting[_]*): CrossProject =
    jsConfigure(_.settings(ss: _*))

  // Concrete alteration members

  def aggregate(refs: CrossProject*): CrossProject = {
    copy(
        jvm.aggregate(refs.map(_.jvm: ProjectReference): _*),
        js.aggregate(refs.map(_.js: ProjectReference): _*))
  }

  def configs(cs: Configuration*): CrossProject =
    copy(jvm.configs(cs: _*), js.configs(cs: _*))

  def configure(transforms: (CrossProject => CrossProject)*): CrossProject =
    transforms.foldLeft(this)((p, t) => t(p))

  def dependsOn(deps: CrossClasspathDependency*): CrossProject =
    copy(jvm.dependsOn(deps.map(_.jvm): _*), js.dependsOn(deps.map(_.js): _*))

  def disablePlugins(ps: AutoPlugin*): CrossProject =
    copy(jvm.disablePlugins(ps: _*), js.disablePlugins(ps: _*))

  def enablePlugins(ns: Plugins*): CrossProject =
    copy(jvm.enablePlugins(ns: _*), js.enablePlugins(ns: _*))

  def in(dir: File): CrossProject =
    copy(jvm.in(crossType.jvmDir(dir)), js.in(crossType.jsDir(dir)))

  def overrideConfigs(cs: Configuration*): CrossProject =
    copy(jvm.overrideConfigs(cs: _*), js.overrideConfigs(cs: _*))

  /** Configures how settings from other sources, such as .sbt files, are
   *  appended to the explicitly specified settings for this project.
   *
   *  Note: If you disable AutoPlugins here, Scala.js will not work
   */
  def settingSets(select: AddSettings*): CrossProject =
    copy(jvm.settingSets(select: _*), js.settingSets(select: _*))

  def settings(ss: Def.Setting[_]*): CrossProject =
    copy(jvm.settings(ss: _*), js.settings(ss: _*))

  override def toString(): String = s"CrossProject(jvm = $jvm, js = $js)"

  // Helpers

  private def copy(jvm: Project = jvm, js: Project = js): CrossProject =
    new CrossProject(crossType, jvm, js)

}

object CrossProject extends CrossProjectExtra {

  def apply(id: String, base: File, crossType: CrossType): CrossProject = {
    CrossProject(id + "JVM", id + "JS", base, crossType).
      settings(name := id)
  }

  def apply(jvmId: String, jsId: String, base: File,
      crossType: CrossType): CrossProject = {

    val sss = sharedSrcSettings(crossType)

    val jvm = Project(jvmId, crossType.jvmDir(base)).
      settings(sss: _*)

    val js = Project(jsId, crossType.jsDir(base)).
      settings(sss: _*).
      enablePlugins(ScalaJSPlugin)

    new CrossProject(crossType, jvm, js)
  }

  private def sharedSrcSettings(crossType: CrossType) = Seq(
      unmanagedSourceDirectories in Compile ++=
        crossType.sharedSrcDir(baseDirectory.value, "main").toSeq,
      unmanagedSourceDirectories in Test ++=
        crossType.sharedSrcDir(baseDirectory.value, "test").toSeq
  )

  final class Builder(id: String, base: File) {
    def crossType(crossType: CrossType): CrossProject =
      CrossProject(id, base, crossType)
  }

  def crossProject_impl(c: Context): c.Expr[Builder] = {
    import c.universe._
    val enclosingValName = MacroUtils.definingValName(c, methodName =>
      s"""$methodName must be directly assigned to a val, such as `val x = $methodName`.""")
    val name = c.Expr[String](Literal(Constant(enclosingValName)))
    reify { new Builder(name.splice, new File(name.splice)) }
  }

}

trait CrossProjectExtra {

  def crossProject: CrossProject.Builder = macro CrossProject.crossProject_impl

  implicit def crossProjectFromBuilder(
      builder: CrossProject.Builder): CrossProject = {
    builder.crossType(CrossType.Full)
  }

  implicit def crossClasspathDependencyConstructor(
      cp: CrossProject): CrossClasspathDependency.Constructor =
    new CrossClasspathDependency.Constructor(cp)

  implicit def crossClasspathDependency(
      cp: CrossProject): CrossClasspathDependency =
    new CrossClasspathDependency(cp, None)
}
