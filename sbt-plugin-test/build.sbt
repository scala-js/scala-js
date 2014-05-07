name := "Scala.js SBT test"

version := scalaJSVersion

val baseSettings = scalaJSSettings ++ Seq(
  version := scalaJSVersion,
  scalaVersion := "2.11.0",
  libraryDependencies +=
    "org.scala-lang.modules.scalajs" %% "scalajs-jasmine-test-framework" % scalaJSVersion % "test"
)

lazy val root = project.in(file(".")).aggregate(noDOM, withDOM)

lazy val noDOM = project.settings(baseSettings: _*).settings(
  name := "Scala.js SBT test w/o DOM"
)

lazy val withDOM = project.settings(baseSettings: _*).settings(
  ScalaJSKeys.requiresDOM := true,
  ScalaJSKeys.jsDependencies +=
    "org.webjars" % "jquery" % "1.10.2" / "jquery.js"
)
