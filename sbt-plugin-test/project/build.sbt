addSbtPlugin("org.scala-js" % "sbt-scalajs" %
  org.scalajs.core.ir.ScalaJSVersions.current)

addSbtPlugin("org.scala-js" % "sbt-scalajs-env-phantomjs" %
  org.scalajs.core.ir.ScalaJSVersions.current)

libraryDependencies += "org.eclipse.jetty" % "jetty-server" % "9.2.3.v20140905"
