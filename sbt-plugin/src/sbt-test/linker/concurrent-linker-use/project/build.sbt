val scalaJSVersion = sys.props("plugin.version")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % scalaJSVersion)
addSbtPlugin("com.github.sbt" % "sbt2-compat" % "0.1.0")

// see: anonfunction-compat/project/build.sbt
libraryDependencies += ("org.scala-js" %% "scalajs-linker" % scalaJSVersion)
  .cross(CrossVersion.for3Use2_13)
  .exclude("org.scala-js", "scalajs-linker-interface_2.13")
  .exclude("org.scala-js", "scalajs-ir_2.13")
  .exclude("org.scala-js", "scalajs-logging_2.13")
