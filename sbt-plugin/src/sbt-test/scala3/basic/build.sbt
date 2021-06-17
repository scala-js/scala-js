enablePlugins(ScalaJSPlugin)

scalaVersion := "3.0.0"

// Test CrossVersion.for3Use2_13 for %%% dependencies
libraryDependencies +=
  ("org.scala-js" %%% "scalajs-ir" % scalaJSVersion).cross(CrossVersion.for3Use2_13)

scalaJSUseMainModuleInitializer := true

// Work around #4368
ThisBuild / useCoursier := false
