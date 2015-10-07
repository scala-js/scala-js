/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.sbtplugin

import sbt._
import sbt.Keys._

object ScalaJSJUnitPlugin extends AutoPlugin {
  override def requires: Plugins = ScalaJSPlugin

  import ScalaJSPlugin.autoImport._

  override def projectSettings: Seq[Setting[_]] = Seq(
    /* The `scala-js-test-plugin` configuration adds a plugin only to the `test`
     * configuration. It is a refinement of the `plugin` configuration which adds
     * it to both `compile` and `test`.
     */
    ivyConfigurations += config("scala-js-test-plugin").hide,
    libraryDependencies ++= Seq(
        "org.scala-js" % "scalajs-junit-test-plugin" % scalaJSVersion %
        "scala-js-test-plugin" cross CrossVersion.full,
        "org.scala-js" %% "scalajs-junit-test-runtime" % scalaJSVersion  % "test"),
    scalacOptions in Test ++= {
      val report = update.value
      val jars = report.select(configurationFilter("scala-js-test-plugin"))
      for {
        jar <- jars
        jarPath = jar.getPath
        // This is a hack to filter out the dependencies of the plugins
        if jarPath.contains("plugin")
      } yield {
        s"-Xplugin:$jarPath"
      }
    }
  )
}
