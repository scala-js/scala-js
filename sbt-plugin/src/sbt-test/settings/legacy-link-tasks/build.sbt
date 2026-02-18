import sbtcompat.PluginCompat

version := scalaJSVersion
scalaVersion := "2.12.21"

enablePlugins(ScalaJSPlugin)

scalaJSUseMainModuleInitializer := true

Compile / fastOptJS / artifactPath := {
  implicit val conv: xsbti.FileConverter = fileConverter.value
  PluginCompat.toArtifactPath(baseDirectory.value / "my-fast.js")
}

Compile / fullOptJS / artifactPath := {
  implicit val conv: xsbti.FileConverter = fileConverter.value
  PluginCompat.toArtifactPath(baseDirectory.value / "my-full.js")
}
