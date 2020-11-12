val scalaJSVersion = sys.props("plugin.version")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % scalaJSVersion)
addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.4.5")
