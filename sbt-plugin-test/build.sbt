name := "Scala.js sbt test"

version := scalaJSVersion

val versionSettings = Seq(
  version := scalaJSVersion,
  scalaVersion := "2.11.12"
)

val baseSettings = versionSettings ++ Seq(
  // Test that non-existent classpath entries are allowed - #2198
  fullClasspath in Compile += (baseDirectory in LocalProject("root")).value /
    "non-existent-directory-please-dont-ever-create-this"
)

lazy val noDOM = project.settings(baseSettings: _*).
  enablePlugins(ScalaJSPlugin).
  enablePlugins(ScalaJSJUnitPlugin).
  settings(
    name := "Scala.js sbt test w/o DOM",
    scalaJSUseMainModuleInitializer := true,
  )


