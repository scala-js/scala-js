/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.sbtplugin

import sbt._
import sbt.Keys._

object ScalaJSJUnitPlugin extends AutoPlugin {
  override def requires: Plugins = ScalaJSPlugin

  import ScalaJSPlugin.autoImport._

  /* As of sbt 1, a `config()` must be assigned to a `val` starting with an
   * uppercase letter, which will become the "id" of the configuration.
   */
  val ScalaJSTestPlugin = config("scala-js-test-plugin").hide

  override def projectSettings: Seq[Setting[_]] = Seq(
    /* The `scala-js-test-plugin` configuration adds a plugin only to the `test`
     * configuration. It is a refinement of the `plugin` configuration which adds
     * it to both `compile` and `test`.
     */
    ivyConfigurations += ScalaJSTestPlugin,
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
