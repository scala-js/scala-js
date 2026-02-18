enablePlugins(ScalaJSPlugin)

scalaVersion := "3.3.4"

// Test CrossVersion.for3Use2_13 for %% dependencies
// In sbt2, there's no %%%, %% does the same job for %%% in sbt1.
libraryDependencies +=
  ("org.scala-js" %% "scalajs-ir" % scalaJSVersion).cross(CrossVersion.for3Use2_13)

scalaJSUseMainModuleInitializer := true
