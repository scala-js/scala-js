lazy val root = project.in(file("."))

lazy val testlib = project.in(file("testlib"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaVersion := "3.1.3"
  )

lazy val app = project.in(file("app"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(testlib)
  .settings(
    scalaVersion := "2.13.8",
    scalacOptions += "-Ytasty-reader",
    scalaJSUseMainModuleInitializer := true
  )
