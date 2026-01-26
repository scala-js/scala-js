version := scalaJSVersion
scalaVersion := "2.12.21"

enablePlugins(ScalaJSPlugin)

scalaJSUseMainModuleInitializer := true

// in sbt2 artifactPath VirtualFileRef
Compile / fastOptJS / artifactPath := {
  val conv = fileConverter.value
  conv.toVirtualFile((baseDirectory.value / "my-fast.js").toPath)
}

Compile / fullOptJS / artifactPath := {
  val conv = fileConverter.value
  conv.toVirtualFile((baseDirectory.value / "my-full.js").toPath)
}
