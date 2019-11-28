name := "Scala.js sbt test"

version := scalaJSVersion

val versionSettings = Seq(
  version := scalaJSVersion,
  scalaVersion := "2.11.12"
)

lazy val noDOM = project.settings(versionSettings: _*).
  enablePlugins(ScalaJSPlugin).
  enablePlugins(ScalaJSJUnitPlugin).
  settings(
    name := "Scala.js sbt test w/o DOM",
    scalaJSUseMainModuleInitializer := true,
  )


