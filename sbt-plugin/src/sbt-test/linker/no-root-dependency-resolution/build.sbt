name := "Scala.js sbt test"

ThisBuild / version := scalaJSVersion
ThisBuild / scalaVersion := "2.12.20"

// Disable the IvyPlugin on the root project
disablePlugins(sbt.plugins.IvyPlugin)

lazy val `my-project` = project
  .enablePlugins(ScalaJSPlugin)
  .settings(scalaJSUseMainModuleInitializer := true)
