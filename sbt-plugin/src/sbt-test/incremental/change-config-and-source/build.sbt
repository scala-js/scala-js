name := "change-config-and-source"

scalaVersion := "2.12.21"

enablePlugins(ScalaJSPlugin)

scalaJSUseMainModuleInitializer := true

Compile / fastLinkJS / scalaJSLinkerOutputDirectory := baseDirectory.value / "target" / "fastopt"
