enablePlugins(ScalaJSPlugin)

scalaVersion := "3.0.0-M1"

// Test withDottyCompat for %%% dependencies
libraryDependencies +=
  ("org.scala-js" %%% "scalajs-ir" % scalaJSVersion).withDottyCompat(scalaVersion.value)

scalaJSUseMainModuleInitializer := true
