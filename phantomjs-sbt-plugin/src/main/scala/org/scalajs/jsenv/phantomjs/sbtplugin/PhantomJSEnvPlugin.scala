/*                     __                                                   *\
**     ________ ___   / /  ___      __ ____  PhantomJS support for Scala.js **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL       **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    https://www.scala-js.org/      **
** /____/\___/_/ |_/____/_/ | |__/ /____/                                   **
**                          |/____/                                         **
\*                                                                          */

package org.scalajs.jsenv.phantomjs.sbtplugin

import sbt._
import sbt.Keys._

import java.net.URLClassLoader

import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

import org.scalajs.jsenv._
import org.scalajs.jsenv.phantomjs._

/** An sbt plugin that simplifies the setup of [[PhantomJSEnv]]s.
 *
 *  There is no need to use `enablePlugins(PhantomJSEnvPlugin)`, as this plugin
 *  is automatically triggered by Scala.js projects.
 *
 *  Usually, one only needs to use the
 *  [[PhantomJSEnvPlugin.autoImport.PhantomJSEnv]] method.
 */
object PhantomJSEnvPlugin extends AutoPlugin {
  override def requires: Plugins = ScalaJSPlugin
  override def trigger: PluginTrigger = allRequirements

  object autoImport {
    /** Class loader for PhantomJSEnv, used to load jetty8.
     *
     *  Usually, you should not need to use `scalaJSPhantomJSClassLoader`
     *  directly. Instead, use the `PhantomJSEnv()` function.
     */
    val scalaJSPhantomJSClassLoader: TaskKey[ClassLoader] = {
      TaskKey[ClassLoader](
          "scalaJSPhantomJSClassLoader",
          "Private class loader to load jetty8 without polluting the " +
          "classpath. Only use this as the `jettyClassLoader` argument of " +
          "a PhantomJSEnv.",
          KeyRanks.Invisible)
    }

    /** An [[sbt.Def.Initialize Def.Initialize]] for a [[PhantomJSEnv]].
     *
     *  Use this to specify in your build that you would like to run and/or
     *  test a project with PhantomJS:
     *
     *  {{{
     *  jsEnv := PhantomJSEnv().value
     *  }}}
     *
     *  Note that the resulting [[sbt.Def.Setting Setting]] is not scoped at
     *  all, but must be scoped in a project that has the ScalaJSPlugin enabled
     *  to work properly.
     *  Therefore, either put the upper line in your project settings (common
     *  case) or scope it manually, using
     *  [[sbt.ProjectExtra.inScope[* Project.inScope]].
     */
    def PhantomJSEnv(
        executable: String = "phantomjs",
        args: Seq[String] = Seq.empty,
        env: Map[String, String] = Map.empty,
        autoExit: Boolean = true
    ): Def.Initialize[Task[PhantomJSEnv]] = Def.task {
      val loader = scalaJSPhantomJSClassLoader.value
      new PhantomJSEnv(executable, args, env, autoExit, loader)
    }
  }

  import autoImport._

  val phantomJSJettyModules: Seq[ModuleID] = Seq(
      "org.eclipse.jetty" % "jetty-websocket" % "8.1.16.v20140903",
      "org.eclipse.jetty" % "jetty-server" % "8.1.16.v20140903"
  )

  override def projectSettings: Seq[Setting[_]] = Seq(
    /* Depend on jetty artifacts in a dummy configuration to be able to inject
     * them into the PhantomJS runner if necessary.
     * See scalaJSPhantomJSClassLoader.
     */
    ivyConfigurations += config("phantom-js-jetty").hide,
    libraryDependencies ++= phantomJSJettyModules.map(_ % "phantom-js-jetty"),

    scalaJSPhantomJSClassLoader := {
      val report = update.value
      val jars = report.select(configurationFilter("phantom-js-jetty"))

      val jettyLoader =
        new URLClassLoader(jars.map(_.toURI.toURL).toArray, null)

      new PhantomJettyClassLoader(jettyLoader, getClass.getClassLoader)
    }
  )

}
