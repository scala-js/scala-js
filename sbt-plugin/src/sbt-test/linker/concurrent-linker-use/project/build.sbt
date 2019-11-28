val scalaJSVersion = sys.props("plugin.version")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % scalaJSVersion)

libraryDependencies += "org.scala-js" %% "scalajs-linker" % scalaJSVersion
