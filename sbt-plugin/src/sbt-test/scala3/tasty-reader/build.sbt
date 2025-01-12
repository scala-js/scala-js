lazy val root = project.in(file("."))

lazy val testlib = project.in(file("testlib"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaVersion := "3.3.4"
  )

lazy val app = project.in(file("app"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(testlib)
  .settings(
    scalaVersion := "2.13.16",
    scalacOptions += "-Ytasty-reader",
    scalaJSUseMainModuleInitializer := true
  )
