enablePlugins(ScalaJSPlugin)

scalaVersion := "3.7.4"

// In sbt 2, %% resolves to the Scala.js artifact (scalajs-dom_sjs1_3)
// for Scala.js projects. This replaces the %%% operator from sbt 1.
libraryDependencies += "org.scala-js" %% "scalajs-dom" % "2.8.0"

scalaJSUseMainModuleInitializer := true
