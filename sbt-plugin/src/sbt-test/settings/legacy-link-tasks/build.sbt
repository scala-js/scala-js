version := scalaJSVersion
scalaVersion := "2.12.21"

enablePlugins(ScalaJSPlugin)

scalaJSUseMainModuleInitializer := true

(artifactPath in fastOptJS in Compile) :=
  baseDirectory.value / "my-fast.js"

(artifactPath in fullOptJS in Compile) :=
  baseDirectory.value / "my-full.js"
