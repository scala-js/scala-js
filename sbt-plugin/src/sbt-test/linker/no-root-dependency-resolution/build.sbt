/* This scripted test is pinned to sbt 1.x in project/build.properties.
 * sbt 2.0.0-RC9+ requires LocalRootProject / dependencyResolution,
 * for https://github.com/sbt/sbt/pull/8459.
 * otherwise, the build fails earlier in sbt startup.
 */

name := "Scala.js sbt test"

ThisBuild / version := scalaJSVersion
ThisBuild / scalaVersion := "2.12.21"

// Disable the IvyPlugin on the root project
disablePlugins(sbt.plugins.IvyPlugin)

lazy val `my-project` = project
  .enablePlugins(ScalaJSPlugin)
  .settings(scalaJSUseMainModuleInitializer := true)
