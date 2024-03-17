name := "Scala.js sbt test"

version in ThisBuild := scalaJSVersion
scalaVersion in ThisBuild := "2.12.19"

// Disable the IvyPlugin on the root project
disablePlugins(sbt.plugins.IvyPlugin)

lazy val `my-project` = project
  .enablePlugins(ScalaJSPlugin)
  .settings(scalaJSUseMainModuleInitializer := true)
